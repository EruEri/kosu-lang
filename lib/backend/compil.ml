(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
(*                                                                                            *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms            *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kosu.         *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module type LinkerOption = sig
  type linker_option

  val ld_command : string
  val options : linker_option list
  val raw_args : linker_option list
  val string_of_option : linker_option -> string

  val disable : string option
  (** Message to output if the native compilation pipeline is disable*)

  val should_create_entry_point : string option
end

module Make (Codegen : Codegen.S) (LD : LinkerOption) = struct
  let output_file = "a.out"

  let generate_asm_only tac_program () =
    let _files = Codegen.compile_asm_from_tac tac_program in
    0

  let run_command ?(flush = false) ~verbose cmd =
    let () =
      if verbose then
        Printf.printf "%s\n%s" cmd
          ( if flush then
              "%!"
            else
              ""
          )
      else
        ()
    in
    Sys.command cmd

  let pkg_config ~verbose ~cflags ~clibs ~pkg_config_names () =
    pkg_config_names
    |> List.fold_left
         (fun acc_pkg libname ->
           let pkg =
             Util.PkgConfig.of_command ~verbose ~cflags ~clibs ~libname ()
           in
           Util.PkgConfig.union acc_pkg pkg
         )
         Util.PkgConfig.empty

  let pkg_append_lib libs pkg_config =
    libs
    |> List.fold_left
         (fun acc_pkg clib ->
           let format_clib = Printf.sprintf "-l%s" clib in
           Util.PkgConfig.add_linked_libs format_clib acc_pkg
         )
         pkg_config

  let cc_compilation ?(debug = false) ?(ccol = []) ?(cclib = [])
      ~pkg_config_names ~verbose ~outfile ~other tac_prgram =
    let c_obj_files =
      ccol
      |> List.map (fun s ->
             let tmp_name = Filename.temp_file s ".o" in
             let cc_cmd = Printf.sprintf "cc -c -o %s %s" tmp_name s in
             let code = run_command ~verbose cc_cmd in
             if code = 0 then
               Ok tmp_name
             else
               Error code
         )
    in
    let error_code =
      c_obj_files
      |> List.find_map (function
           | Error code when code <> 0 ->
               Some code
           | _ ->
               None
           )
    in
    match error_code with
    | Some s ->
        s
    | None ->
        let asm_files =
          Codegen.compile_asm_from_tac_tmp ~start:None tac_prgram
        in
        let obj_file = c_obj_files |> List.map Result.get_ok in
        let pkg_configs =
          pkg_config ~verbose ~cflags:true ~clibs:true ~pkg_config_names ()
        in

        let pkg_configs = pkg_append_lib cclib pkg_configs in
        let cmd =
          Printf.sprintf "cc %s -o %s %s %s %s"
            ( if debug then
                "-g"
              else
                ""
            )
            outfile
            (asm_files |> String.concat " ")
            (obj_file @ other |> String.concat " ")
            (Util.PkgConfig.to_string pkg_configs)
        in
        run_command ~verbose cmd

  let native_compilation ?(debug = false) ?(ccol = []) ?(cclib = [])
      ~pkg_config_names ~verbose ~outfile ~other tac_prgram =
    let () =
      match LD.disable with
      | Some message ->
          Printf.eprintf "%s\n" message;
          exit 1
      | None ->
          ()
    in
    let _ = ignore debug in
    let out_file = outfile in
    let pkg =
      pkg_config ~verbose ~cflags:(ccol <> []) ~clibs:true ~pkg_config_names ()
    in
    let c_obj_files =
      ccol
      |> List.map (fun s ->
             let tmp_name = Filename.temp_file s ".o" in
             let cmd =
               Printf.sprintf "cc -c -o %s %s %s" tmp_name s
                 (Util.PkgConfig.cc_flags_format pkg)
             in
             let code = run_command ~verbose cmd in
             if code = 0 then
               tmp_name
             else
               exit code
         )
    in

    let kosu_asm_files =
      Codegen.compile_asm_from_tac_tmp ~start:LD.should_create_entry_point
        tac_prgram
    in
    let ccol_obj_files = c_obj_files in

    let other_object_files, other_asm_files =
      other
      |> List.filter (fun file ->
             Util.is_asm_file file || Util.is_object_file file
         )
      |> List.partition_map (fun file ->
             if Util.is_object_file file then
               Either.left file
             else
               Either.right file
         )
    in

    let asm_files = kosu_asm_files @ other_asm_files in
    let asm_to_object_files =
      asm_files
      |> List.map (fun file ->
             let basename = Filename.basename file in
             let tmp_file = Filename.temp_file basename ".o" in
             let cmd = Printf.sprintf "as -o %s %s" tmp_file file in
             let code = run_command ~verbose cmd in
             if code <> 0 then
               exit code
             else
               tmp_file
         )
    in

    let objects_files =
      ccol_obj_files @ other_object_files @ asm_to_object_files
    in
    let string_of_objects_files = objects_files |> String.concat " " in
    let options =
      LD.options
      |> List.map LD.string_of_option
      |> List.map (( ^ ) "-")
      |> String.concat " "
    in

    let pkg = pkg_append_lib cclib pkg in

    let raw_args =
      LD.raw_args |> List.map LD.string_of_option |> String.concat " "
    in

    let ld_cmd =
      Printf.sprintf "%s %s -o %s %s %s %s" LD.ld_command options out_file
        raw_args string_of_objects_files
        (Util.PkgConfig.linker_flags_format pkg)
    in
    let code = run_command ~verbose ld_cmd in
    code

  let compilation ~cc =
    if cc then
      cc_compilation
    else
      native_compilation
end

(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
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

open KosuFrontend.Astvalidation
open KosuIrTyped
open KosuIrTAC
open KosuCli


let () = KosuFrontend.Registerexn.register_kosu_error ()
let code =
  Clap.description "kosuc - The Kosu compiler";

  let output = Clap.optional_string ~long:"output" ~short:'o' () in

  let is_target_asm =
    Clap.flag ~set_short:'S' ~description:"Produce an assembly file" false
  in

  let _is_without_link =
    Clap.flag ~set_short:'c' ~description:"Produce an object file" false
  in

  let target_archi =
    Clap.mandatory Cli.archi_clap_type ~long:"target"
      ~description:"Architecture compilation target" ~placeholder:"Target" ()
  in
  let cc =
    Clap.flag ~set_long:"cc"
      ~description:"Generate executable by using a C compiler" false
  in
  let ccol =
    Clap.list_string ~long:"ccol"
      ~description:
        "Invoke the default C compiler to generate object file and link those \
         files"
      ~placeholder:"C Files" ()
  in

  let files = Clap.list_string ~description:"files" ~placeholder:"FILES" () in

  let kosu_files, other_files =
    files |> List.partition (fun s -> s |> Filename.extension |> ( = ) ".kosu")
  in

  let () = Clap.close () in

  let modules_opt = Cli.files_to_ast_program kosu_files in

  let tac_program =
    match modules_opt with
    | Error e -> (
        match e with
        | No_input_file -> raise (Invalid_argument "no Input file")
        | File_error (s, exn) ->
            Printf.eprintf "%s\n" s;
            raise exn
        | Filename_error _ -> raise (Invalid_argument "Filename Error")
        | Lexer_Error e -> raise e)
    | Ok modules -> (
        match valide_program modules with
        | filename, Error e ->
            (* Printf.eprintf "\nFile \"%s\", %s\n" filename (Kosu_frontend.Pprint.string_of_validation_error e); *)
            raise (Error.Validation_error (filename, e))
        | _, Ok () ->
            let typed_program =
              try Asttyconvert.from_program modules
              with KosuFrontend.Ast.Error.Ast_error e ->
                let () =
                  Printf.printf "%s\n"
                    (KosuFrontend.Pprinterr.string_of_ast_error e)
                in
                failwith "failwith"
            in
            let tac_program =
              Asttacconv.tac_program_of_rprogram typed_program
            in
            tac_program)
  in

  let code =
    match is_target_asm with
    | true -> (
        match target_archi with
        | Cli.Arm64e ->
            let _files =
              Aarch64.Aarch64Codegen.compile_asm_from_tac tac_program
            in
            0
        | Cli.X86_64 -> failwith "X86_64 Support to do")
    | false -> (
        let c_obj_files = Cli.ccol_compilation ccol in
        let error_code = Cli.find_error_code_opt c_obj_files in
        if Option.is_some error_code then Option.get error_code
        else
          let obj_file = c_obj_files |> List.map Result.get_ok in

          let outfile = output |> Option.value ~default:"a.out" in
          match target_archi with
          | Cli.X86_64 -> failwith "X86_64 Support to do"
          | Cli.Arm64e ->
              if cc then
                let asm_file =
                  Aarch64.Aarch64Codegen.compile_asm_from_tac_tmp tac_program
                in
                Cli.cc_compilation outfile ~asm:asm_file
                  ~other:(other_files @ obj_file)
              else failwith "Native compiling pipeline with as and ld to do")
  in
  code

let () = exit code

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

type filename_error = Mutiple_dot_in_filename | No_extension | Unknow_error
type archi_target = Arm64e | X86_64m | X86_64

let std_global_variable = "KOSU_STD_PATH"
let std_path = Sys.getenv_opt std_global_variable
let is_kosu_file file = file |> Filename.extension |> ( = ) ".kosu"

let archi_parse = function
  | "x86_64m" -> Some X86_64m
  | "x86_64" -> Some X86_64
  | "arm64e" -> Some Arm64e
  | _ -> None

let string_of_archi = function
  | X86_64m | X86_64 -> "x86_64"
  | Arm64e -> "arm64e"

let archi_clap_type =
  Clap.typ ~name:"target" ~dummy:X86_64 ~parse:archi_parse ~show:string_of_archi

let cc_compilation outputfile ~asm ~other =
  Sys.command
    (Printf.sprintf "cc -g -o %s %s %s" outputfile
       (asm |> String.concat " ")
       (other |> String.concat " "))

let ccol_compilation cfiles =
  cfiles
  |> List.map (fun s ->
         let tmp_name = Filename.temp_file s ".o" in
         let code = Sys.command (Printf.sprintf "cc -c -o %s %s" tmp_name s) in
         if code == 0 then Ok tmp_name else Error code)

let find_error_code_opt l =
  l
  |> List.find_map (function
       | Error code when code <> 0 -> Some code
       | _ -> None)

type cli_error =
  | No_input_file
  | Lexer_Error of exn
  | File_error of string * exn
  | Filename_error of filename_error

let ( >>= ) = Result.bind

let f s =
  String.concat "::"
  @@ List.map String.capitalize_ascii
  @@ String.split_on_char '/' s

let convert_filename_to_path filename =
  filename |> String.split_on_char '.'
  |> (function
       | [ t; _ ] -> Ok t
       | [ _ ] -> Error No_extension
       | _ :: _ :: _ -> Error Mutiple_dot_in_filename
       | _ -> Error Unknow_error)
  |> Result.map f

let module_path_of_file filename =
  let chomped_filename =
    match std_path with
    | None -> filename
    | Some path ->
        if String.starts_with ~prefix:path filename then
          let filename_len = String.length filename in
          let dir_sep_len = String.length @@ Filename.dir_sep in
          let path_len = String.length path in
          let prefix_len = path_len + dir_sep_len in
          String.sub filename prefix_len (filename_len - prefix_len)
        else filename
  in
  (* let () = Printf.printf "filename = %s\nchomped = %s\n" filename  chomped_filename in *)
  let open KosuFrontend in
  let open KosuFrontend.Ast in
  let ( >>= ) = Result.bind in
  let lexbuf_res =
    try
      let file = open_in filename in
      let source = Lexing.from_channel file in
      at_exit (fun () -> close_in file);
      source |> Result.ok
    with e -> Error (File_error (filename, e))
  in
  lexbuf_res >>= fun lexbuf ->
  KosuParser.parse lexbuf (Parser.Incremental.modul lexbuf.lex_curr_p)
  |> Result.map_error (fun lexer_error ->
         Lexer_Error (Lexer.Lexer_Error { filename; error = lexer_error }))
  >>= fun _module ->
  chomped_filename |> convert_filename_to_path
  |> Result.map (fun path -> { filename; module_path = { path; _module } })
  |> Result.map_error (fun e -> Filename_error e)

(**
    Takes all the kosuc files and transform into the ast.
    It also revomes all the implicit Module type function with the function
    [Kosu_frontend.Astvalidation.Help.program_remove_implicit_type_path]
*)
let files_to_ast_program (files : string list) =
  files |> List.map module_path_of_file |> function
  | [] -> Error No_input_file
  | l -> (
      match
        l
        |> List.find_map (fun s -> match s with Error e -> Some e | _ -> None)
      with
      | None ->
          Ok
            (l |> List.map Result.get_ok
           |> KosuFrontend.Astvalidation.Help.program_remove_implicit_type_path
            )
      | Some error -> Error error)

let rec fetch_kosu_file direname () =
  let file_in_dir = Sys.readdir direname in
  let kosu_files =
    file_in_dir
    |> Array.fold_left
         (fun acc_kosu_files file ->
           let file = Printf.sprintf "%s%s%s" direname Filename.dir_sep file in
           if Sys.is_directory file then
             acc_kosu_files @ fetch_kosu_file file ()
           else if is_kosu_file file then file :: acc_kosu_files
           else acc_kosu_files)
         []
  in
  kosu_files

let fetch_std_file ~no_std () =
  if no_std || Option.is_none std_path then []
  else
    let std_path = Option.get std_path in
    fetch_kosu_file std_path ()

let parse_library_link_name libname =
  let prefix_lib = "lib" in
  let lib_strlen = String.length prefix_lib in
  if String.starts_with ~prefix:prefix_lib libname then
    String.sub libname lib_strlen
      (libname |> String.length |> ( + ) (-lib_strlen))
  else libname

module Cli = struct
  open Cmdliner
  open KosuFrontend.Astvalidation
  open KosuIrTyped
  open KosuIrTAC

  module Mac0SX86 =
    KosuBackend.Codegen.Make
      (KosuBackend.X86_64.X86_64Codegen.Codegen
         (KosuBackend.X86_64.X86_64AsmSpecImpl.X86MacOsAsmSpec))

  module LinuxX86 =
    KosuBackend.Codegen.Make
      (KosuBackend.X86_64.X86_64Codegen.Codegen
         (KosuBackend.X86_64.X86_64AsmSpecImpl.X86_64LinuxAsmSpec))

  module MacOSAarch64 =
    KosuBackend.Codegen.Make
      (KosuBackend.Aarch64.Aarch64Codegen.Codegen
         (KosuBackend.Aarch64.Aarch64AsmSpecImpl.MacOSAarch64AsmSpec))

  let name = "kosuc"
  let version = "not-even-alpha"

  type cmd = {
    target_archi : archi_target;
    no_std : bool;
    is_target_asm : bool;
    cc : bool;
    verbose : bool;
    output : string;
    pkg_configs : string list;
    ccol : string list;
    cclib : string list;
    files : string list;
  }

  let default_outfile = "a.out"

  let target_enum =
    [ ("arm64e", Arm64e); ("x86_64", X86_64); ("x86_64m", X86_64m) ]

  let target_archi_term =
    Arg.(
      required
      & opt (some & enum target_enum) None
      & info ~docv:"Assembly Target"
          ~doc:(doc_alts_enum ~quoted:true target_enum)
          [ "t"; "target" ])

  let no_std_term =
    Arg.(
      value & flag
      & info [ "no-std" ] ~doc:"Don't include the standard librairy")

  let verbose_term =
    Arg.(
      value & flag
      & info [ "V"; "verbose" ] ~doc:"Print each command before it execution")

  let cc_term =
    Arg.(
      value & flag
      & info [ "cc" ] ~doc:"Generate executable by using a C compiler")

  let target_asm_term =
    Arg.(value & flag & info [ "S" ] ~doc:"Produce assembly files")

  let pkg_config_term =
    Arg.(
      value & opt_all string []
      & info
          [ "pkg-config"; "pkg-c"; "pc" ]
          ~docv:"libname"
          ~doc:
            "Invoke $(b,pkg-config)(1) to retreive compilation flags and libs")

  let cclib_term =
    Arg.(
      value & opt_all string []
      & info [ "l" ] ~docv:"libname" ~doc:"Pass $(i,libname) to the linker")

  let output_term =
    Arg.(
      value & opt string default_outfile
      & info [ "o"; "output" ] ~docv:"EXECUTABLE NAME"
          ~doc:"Specify the name of the file producted by the linker")

  let ccol_term =
    Arg.(
      value
      & Arg.opt_all non_dir_file []
      & info [ "ccol" ] ~docv:"C FILES"
          ~doc:
            "Invoke the default C compiler to generate object file and link \
             those files")

  let files_term =
    Arg.(
      non_empty
      & pos_all Arg.non_dir_file []
      & info [] ~docv:"FILES"
          ~doc:
            "Input files of the compiler. Kosu files must have the extension \
             .kosu. Files ending  \n\
            \  with .o are treated as object files to be passed to the linker. \
             If --cc flag is set, any files recognized by the $(b,cc(1)) can \
             be passed. \n\
            \  ")

  let cmd_term run =
    let combine target_archi no_std verbose cc is_target_asm output pkg_configs
        ccol cclib files =
      run
      @@ {
           target_archi;
           no_std;
           verbose;
           is_target_asm;
           cc;
           output;
           pkg_configs;
           cclib;
           ccol;
           files;
         }
    in
    Term.(
      const combine $ target_archi_term $ no_std_term $ verbose_term $ cc_term
      $ target_asm_term $ output_term $ pkg_config_term $ ccol_term $ cclib_term
      $ files_term)

  let kosuc_doc = "The Kosu compiler"

  let kosuc_man =
    [
      `S Manpage.s_description;
      `P
        "Kosu is (or will be at least I hope) a statically-typed, \
         expression-oriented language.";
      `P
        "The philosophy of Kosu is to have as control over memory as C (manual \
         memory management, pointers) while having some higher features like \
         generics or sum type.";
      `S Manpage.s_environment;
      `I
        ( Printf.sprintf "$(b,%s)" std_global_variable,
          "If this environment variable is present, kosu files inside the \
           folder are recessively included in the compilation except if \
           --no-std is present" );
      `S Manpage.s_see_also;
      `P "$(b,cc)(1), $(b,as)(1), $(b,ld)(1)";
      `Noblank;
      `P "Repository:  https://github.com/EruEri/kosu-lang";
      `S Manpage.s_authors;
      `P "Yves Ndiaye";
      `S "COPYRIGHT";
      `P "Yves Ndiaye";
      `S "LICENSE";
      `P "Kosuc is distributed under the GNU GPL-3.0";
    ]

  let kosuc run =
    let info = Cmd.info ~doc:kosuc_doc ~man:kosuc_man ~version name in
    Cmd.v info (cmd_term run)

  let run cmd =
    let {
      target_archi;
      no_std;
      verbose;
      is_target_asm;
      cc;
      pkg_configs;
      output;
      ccol;
      files;
      cclib;
    } =
      cmd
    in
    let module Codegen = (val match target_archi with
                              | X86_64 -> (module LinuxX86)
                              | X86_64m -> (module Mac0SX86)
                              | Arm64e -> (module MacOSAarch64)
                            : KosuBackend.Codegen.S)
    in
    let module LinkerOption = (val match target_archi with
                                   | X86_64m | Arm64e ->
                                       (module LdSpec.MacOSLdSpec)
                                   | X86_64 -> (module LdSpec.LinuxLdSpec)
                                 : KosuBackend.Compil.LinkerOption)
    in
    let module Compiler = KosuBackend.Compil.Make (Codegen) (LinkerOption) in
    let cclib = cclib |> List.map parse_library_link_name in

    let kosu_files, other_files = files |> List.partition is_kosu_file in

    let std_file = fetch_std_file ~no_std () in

    let modules_opt = files_to_ast_program (kosu_files @ std_file) in

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
                  failwith "Error while typing ast: Shouldn't append"
              in
              let tac_program =
                Asttacconv.tac_program_of_rprogram typed_program
              in
              tac_program)
    in

    let _code =
      match is_target_asm with
      | true -> Compiler.generate_asm_only tac_program ()
      | false ->
          let compilation = Compiler.compilation ~cc in
          compilation ~outfile:output ~debug:true ~ccol ~other:other_files
            ~cclib ~verbose ~pkg_config_names:pkg_configs tac_program
    in
    ()

  let eval () = run |> kosuc |> Cmd.eval ~catch:true
end

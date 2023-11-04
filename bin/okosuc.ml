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

open Cmdliner

let name = "okosuc"

type cmd = { config : bool; files : string list }

let files_term =
  Arg.(
    value
    & pos_all Arg.non_dir_file []
    & info [] ~docv:"FILES" ~doc:"Compiler Input files"
  )

let cmd_term run =
  let combine config files = run @@ { config; files } in
  Term.(const combine $ CliCommon.config_term $ files_term)

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
    `P "Kosu files must have the extension .kosu.";
    `P
      "Files ending with .c are treated as C files, are compiled to object \
       files with $(b,cc)(1)\n\
      \      and linked to the program";
    `P
      "Files ending with .s are treated as Assembly file, are assembled with \
       $(b,as)(1)\n\
      \      and linked to the program";
    `P
      "Files ending with .o are treated as object files to be passed to the \
       linker.";
    `P
      "If --cc flag is set, files ending with .c, .s or .o are passed as it is \
       to $(b,cc)(1)";
    `S Manpage.s_environment;
    `I
      ( Printf.sprintf "$(b,%s)" "DUMMY",
        "If this environment variable is present, kosu files inside the folder \
         are recessively included in the compilation except if --no-std is \
         present"
      );
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
  let info =
    Cmd.info ~doc:kosuc_doc ~man:kosuc_man ~version:CliCommon.fversion name
  in
  Cmd.v info (cmd_term run)

let run cmd =
  let { files; config } = cmd in
  let () =
    match () with
    | _ when config ->
        let () = CliCommon.kosu_config_print () in
        ()
    | _ when files = [] ->
        ()
    | () ->
        let ( `KosuFile kosu_files,
              `CFile _c_files,
              `ObjFile _object_files,
              `AssemblyFile _assembly_files ) =
          match CliCommon.input_file files with
          | Ok e ->
              e
          | Error e ->
              raise @@ KosuFrontendAlt.Error.Exn.unsupported_file e
        in

        let () = KosuFrontendAlt.Error.register_exn () in
        let kosu_program =
          match KosuFrontendAlt.Parsing.kosu_program kosu_files with
          | Ok kosu_program ->
              kosu_program
          | Error e ->
              raise @@ KosuFrontendAlt.Error.Exn.analytics_error e
        in
        let () =
          match KosuFrontendAlt.Validation.validate kosu_program with
          | Ok () ->
              ()
          | Error e ->
              raise @@ KosuFrontendAlt.Error.Exn.kosu_error e
        in
        ()
  in
  ()

let eval () = run |> kosuc |> Cmd.eval ~catch:true
let () = exit @@ eval ()

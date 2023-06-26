(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
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

let name = "repl"

type cmd = { modules_files : string list; interpreted_file : string option }

let interpreted_file_term =
  Arg.(
    value
    & opt (some file) None
    & info [ "e"; "eval" ] ~docv:"FILE"
        ~doc:
          "If $(b,docv) is given, phrases are read instead of reading from the \
           stdin"
  )

let modules_files_term =
  Arg.(
    value & pos_all file []
    & info [] ~docv:"FILES" ~doc:"Compile the files to be loaded by the repl"
  )

let cmd_term run =
  let combine modules_files interpreted_file =
    run @@ { modules_files; interpreted_file }
  in
  Term.(const combine $ modules_files_term $ interpreted_file_term)

let repl_doc = "WIP: Interpret through a read-eval-print-loop (REPL)"

let repl_man =
  [
    `S Manpage.s_description;
    `P "kosu-repl allows you to evaluate phrases in a REPL";
    `P "currently, the repl can only type expression";
    `S Manpage.s_see_also;
    `Noblank;
  ]

let repl_main cmd =
  (* let () = Printf.printf "nb file : %u\n" (List.length cmd.modules_files) in
     let () = Printf.printf "Is some : %b\n" (Option.is_some cmd.interpreted_file) in *)
  let () = ignore cmd in
  CliCommon.DefaultFront.KosuFrontInterpret.repl
    ~welcome:(Printf.sprintf "Kosu version %s" CliCommon.version)
    ()

let repl =
  let info = Cmd.info ~doc:repl_doc ~man:repl_man name in
  Cmd.v info (cmd_term repl_main)

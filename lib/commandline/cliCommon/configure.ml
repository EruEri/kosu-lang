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

let name = "configure"

type t = {
  arch : string;
  os : string;
  os_extension : string;
  cc : string;
  linker_options : string list;
  linker_raw_args : string list;
  commit_hash : string;
  branch : string;
  headers : string;
  core_path : string;
  runtime_path : string;
}

let arch_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "a"; "arch" ] ~doc:"architecture Target"
  )

let os_term =
  Arg.(required & opt (some string) None & info [ "o"; "os" ] ~doc:"Os Target")

let cc_term =
  Arg.(required & opt (some string) None & info [ "cc" ] ~doc:"C compiler")

let os_extension_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "os-extension" ] ~doc:"Os Extension"
  )

let _assembler_term =
  Arg.(required & opt (some string) None & info [ "as" ] ~doc:"Assembler used")

let linker_options_term =
  Arg.(value & opt (list string) [] & info [ "lo" ] ~doc:"Linker options")

let linker_raw_args_term =
  Arg.(value & opt (list string) [] & info [ "lar" ] ~doc:"Linker raw args")

let branch_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "b"; "branch" ] ~doc:"Compilation branch name"
  )

let commit_hash_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "hash" ] ~doc:"Compilation hash commit"
  )

let headers_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "H"; "headers" ] ~doc:"Kosu headers location"
  )

let core_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "c"; "core" ] ~doc:"Kosu core function location"
  )

let runtime_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "r"; "runtime" ] ~doc:"Kosu runtime library location"
  )

let cmd_term run =
  let combine arch os os_extension cc linker_options linker_raw_args commit_hash
      branch headers core_path runtime_path =
    run
    @@ {
         arch;
         os;
         os_extension;
         cc;
         linker_options;
         linker_raw_args;
         commit_hash;
         branch;
         headers;
         core_path;
         runtime_path;
       }
  in
  Term.(
    const combine $ arch_term $ os_term $ os_extension_term $ cc_term
    $ linker_options_term $ linker_raw_args_term $ commit_hash_term
    $ branch_term $ headers_term $ core_term $ runtime_term
  )

let doc = "Configure kosu compilation option"

let configure run =
  let info = Cmd.info ~doc name in
  Cmd.v info (cmd_term run)

let run cmd =
  let {
    arch;
    os;
    os_extension;
    cc;
    linker_options;
    linker_raw_args;
    commit_hash;
    branch;
    headers;
    core_path;
    runtime_path;
  } =
    cmd
  in
  let s_linker_option =
    linker_options
    |> List.map @@ Printf.sprintf "\"%s\""
    |> String.concat "; " |> Printf.sprintf "[%s]"
  in
  let s_linker_raw_args =
    linker_raw_args
    |> List.map @@ Printf.sprintf "\"%s\""
    |> String.concat "; " |> Printf.sprintf "[%s]"
  in
  let () =
    Printf.printf
      {| 
let kosu_target_arch = "%s"
let kosu_target_os = "%s"
let kosu_target_os_extentsion = "%s"
let kosu_target_cc = "%s" 
let kosu_target_hash = "%s"
let kosu_target_branch = "%s"
let kosu_target_headers = "%s"
let kosu_target_core_path = "%s"
let kosu_target_runtime_path = "%s"
let kosu_target_linker_option = %s
let kosu_target_linker_args = %s
    |}
      arch os os_extension cc commit_hash branch headers core_path runtime_path
      s_linker_option s_linker_raw_args
  in
  ()

let eval = Cmd.eval @@ configure @@ run
let () = exit eval

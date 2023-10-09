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
  hash : string;
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

let commit_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "h"; "hash" ] ~doc:"Compilation commit hash"
  )

let branch_term =
  Arg.(
    required
    & opt (some string) None
    & info [ "b"; "branch" ] ~doc:"Compilation branch name"
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

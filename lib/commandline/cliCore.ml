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

type architecture = Arm64 | X86_64
type os = Macos | Linux | FreeBSD

let commit_hash () = KosuHash.commit_hash

let version =
  let commit_hash =
    () |> commit_hash
    |> Option.map (Printf.sprintf "[%s]")
    |> Option.value ~default:""
  in
  let v =
    match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v
  in
  Printf.sprintf "%s %s" v commit_hash

let std_global_variable = "KOSU_STD_PATH"
let architecture_global_variable = "KOSU_TARGET_ARCHI"
let os_global_variable = "KOSU_TARGET_OS"
let std_path = Sys.getenv_opt std_global_variable
let is_kosu_file file = file |> Filename.extension |> ( = ) ".kosu"

let string_of_enum ?(splitter = "|") ?(quoted = false) enum =
  let f = if quoted then Cmdliner.Arg.doc_quote else Fun.id in
  enum |> List.map (fun (elt, _) -> f elt) |> String.concat splitter

module DefaultFront = struct
  module ValidationRule : KosuFrontend.KosuValidationRule = struct end

  module TypeCheckerRule : KosuFrontend.TypeCheckerRule = struct
    let allow_generics_in_variadic = false
  end

  module Compilation_Files : KosuFrontend.Compilation_Files = struct
    let std_global_variable = std_global_variable
  end

  module KosuFront =
    KosuFrontend.Make (Compilation_Files) (ValidationRule) (TypeCheckerRule)

  module KosuFrontInterpret =
    KosuInterpreter.Make (Compilation_Files) (ValidationRule) (TypeCheckerRule)

  module Asttyconvert = KosuIrTyped.Asttyconvert.Make (TypeCheckerRule)
end

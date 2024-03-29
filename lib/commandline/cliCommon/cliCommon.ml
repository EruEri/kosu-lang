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
type large_architecture = LArm64 | LX86_64 | LKVM
type suppoted_file = SF_Kosu | SF_C | SF_Object | SF_Assembly

let extension_list =
  [ (SF_C, ".c"); (SF_Object, ".o"); (SF_Kosu, ".kosu"); (SF_Assembly, ".s") ]

let rev_extension_list =
  let swap (a, b) = (b, a) in
  List.map swap extension_list

let rec input_file ~kosu ~c ~co ~assembly = function
  | [] ->
      Ok (List.rev kosu, List.rev c, List.rev co, List.rev assembly)
  | t :: q ->
      let ( let* ) = Result.bind in
      let filekind = List.assoc_opt (Filename.extension t) rev_extension_list in
      let* kind =
        match filekind with None -> Error t | Some kind -> Ok kind
      in
      let kosu, c, co, assembly =
        match kind with
        | SF_C ->
            (kosu, t :: c, co, assembly)
        | SF_Object ->
            (kosu, c, t :: co, assembly)
        | SF_Kosu ->
            (t :: kosu, c, co, assembly)
        | SF_Assembly ->
            (kosu, c, co, t :: assembly)
      in
      input_file ~kosu ~c ~co ~assembly q

let input_file files =
  match input_file ~kosu:[] ~c:[] ~co:[] ~assembly:[] files with
  | Ok (kosu, c, co, assembly) ->
      Ok (`KosuFile kosu, `CFile c, `ObjFile co, `AssemblyFile assembly)
  | Error _ as e ->
      e

let large_architecture_enum =
  [ ("arm64", LArm64); ("x86_64", LX86_64); ("kvm", LKVM) ]

let architecture_enum = [ ("arm64", Arm64); ("x86_64", X86_64) ]
let os_enum = [ ("freebsd", FreeBSD); ("linux", Linux); ("macos", Macos) ]
let commit_hash () = KosuHash.commit_hash

let version =
  let commit_hash =
    () |> commit_hash
    |> Option.map (Printf.sprintf "[%s]")
    |> Option.value ~default:""
  in
  let v =
    match Build_info.V1.version () with
    | None ->
        "n/a"
    | Some v ->
        Build_info.V1.Version.to_string v
  in
  Printf.sprintf "%s %s" v commit_hash
(* Printf.sprintf "%s-next %s" v commit_hash *)

let std_global_variable = "KOSU_STD_PATH"
let architecture_global_variable = "KOSU_TARGET_ARCH"
let os_global_variable = "KOSU_TARGET_OS"
let std_path = Sys.getenv_opt std_global_variable
let is_kosu_file file = file |> Filename.extension |> ( = ) ".kosu"

let string_of_enum ?(splitter = "|") ?(quoted = false) enum =
  let f =
    if quoted then
      Cmdliner.Arg.doc_quote
    else
      Fun.id
  in
  enum |> List.map (fun (elt, _) -> f elt) |> String.concat splitter

let rec fetch_kosu_file direname () =
  let file_in_dir = Sys.readdir direname in
  let kosu_files =
    file_in_dir
    |> Array.fold_left
         (fun acc_kosu_files file ->
           let file = Printf.sprintf "%s%s%s" direname Filename.dir_sep file in
           if Sys.is_directory file then
             acc_kosu_files @ fetch_kosu_file file ()
           else if is_kosu_file file then
             file :: acc_kosu_files
           else
             acc_kosu_files
         )
         []
  in
  kosu_files

let fetch_std_file ~no_std () =
  if no_std || Option.is_none std_path then
    []
  else
    let std_path = Option.get std_path in
    fetch_kosu_file std_path ()

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
    KosuRepl.Make (Compilation_Files) (ValidationRule) (TypeCheckerRule)

  module Asttyconvert = KosuIrTyped.Asttyconvert.Make (TypeCheckerRule)
end

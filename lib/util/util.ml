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

module Args = Args
module Occurence = Occurence
module Io = Io
module PkgConfig = PkgConfig
module Checksum = Checksum
module Ulist = Ulist
module Position = Position

type stringlit_label = SLit of string
type floatlit_label = FLit of string
type coordinate = { line : int; column : int }

let couple a b = (a, b)

let is_what_file ~extension filename =
  filename |> Filename.extension |> ( = ) extension

let is_object_file = is_what_file ~extension:".o"

let is_asm_file filename =
  filename |> Filename.extension |> String.lowercase_ascii |> ( = ) ".s"

let rec string_of_chars_aux count result char =
  if count <= 0 then
    result
  else
    string_of_chars_aux (count - 1) (Printf.sprintf "%c%s" char result) char

let string_of_chars count = string_of_chars_aux count ""

let string_of_module_path path =
  if path = String.empty then
    String.empty
  else
    Printf.sprintf "%s::" path

let dummy_generic_map generic_names parametrics_types =
  let list_len = parametrics_types |> List.length in
  let dummy_list = List.init list_len (fun _ -> ()) in
  let dummy_parametrics = List.combine dummy_list parametrics_types in
  let generics_mapped = List.combine generic_names dummy_parametrics in
  Hashtbl.of_seq (generics_mapped |> List.to_seq)

module Operator = struct
  let ( >? ) o v = Option.value ~default:v o
end

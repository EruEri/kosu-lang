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

type ffi_type =
  | FFI_S8
  | FFI_U8
  | FFI_S16
  | FFI_U16
  | FFI_S32
  | FFI_U32
  | FFI_S64
  | FFI_U64
  | FFI_Pointer
  | FFI_Struct of ffi_type list

type address_offset = Off_Reg of int | Off_value of int
type address = { base_reg : int; offset : address_offset }

type ccall_entry = {
  function_name : string;
  arity : int;
  dynlib_entry : int;
  args : address list;
  ty_args : ffi_type list;
  ty_return : ffi_type;
}

let rec string_of_ffi_type = function
  | FFI_S8 ->
      "s8"
  | FFI_U8 ->
      "u8"
  | FFI_S16 ->
      "s16"
  | FFI_U16 ->
      "u16"
  | FFI_S32 ->
      "s32"
  | FFI_U32 ->
      "u32"
  | FFI_S64 ->
      "s64"
  | FFI_U64 ->
      "u64"
  | FFI_Pointer ->
      "ptr"
  | FFI_Struct types ->
      Printf.sprintf "{ %s }"
        (types |> List.map string_of_ffi_type |> String.concat " ")

let string_of_address_offset = function
  | Off_Reg n ->
      Printf.sprintf "# %u" n
  | Off_value n ->
      Printf.sprintf "$ %d " n

let string_of_address = function
  | { base_reg; offset } ->
      Printf.sprintf "%u(%s)" base_reg @@ string_of_address_offset offset

let single_quoted = Printf.sprintf "\'%s\'"
let quoted = Printf.sprintf "\"%s\""

let string_of_ccall_entry = function
  | { function_name; arity; dynlib_entry; args; ty_args; ty_return } ->
      Printf.sprintf "(%s %d %u (%s) (%s) %s)"
        (single_quoted function_name)
        arity dynlib_entry
        (args |> List.map string_of_address |> String.concat " ")
        (ty_args |> List.map string_of_ffi_type |> String.concat " ")
        (string_of_ffi_type ty_return)

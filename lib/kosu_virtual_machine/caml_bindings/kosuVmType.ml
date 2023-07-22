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
  | FFI_F32
  | FFI_F64
  | FFI_Pointer
  | FFI_Struct of ffi_type list

type address_offset = Off_Reg of int64 | Off_value of int64

type args =
  | ArgsValue of int64
  | ArgsAddr of { base_reg : int64; offset : address_offset }
  | ArgsPcReal of int

type 'arg ccall_entry = {
  function_name : string;
  arity : int64;
  args : 'arg list;
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
  | FFI_F32 ->
      "f32"
  | FFI_F64 ->
      "f64"
  | FFI_Struct types ->
      Printf.sprintf "{ %s }"
        (types |> List.map string_of_ffi_type |> String.concat " ")

let string_of_address_offset = function
  | Off_Reg n ->
      Printf.sprintf "# %Lu" n
  | Off_value n ->
      Printf.sprintf "$ %Ld " n

let string_of_args = function
  | ArgsValue value ->
      Printf.sprintf "$%Ld" value
  | ArgsAddr { base_reg; offset } ->
      Printf.sprintf "# %Ld(%s)" base_reg @@ string_of_address_offset offset
  | ArgsPcReal int ->
      Printf.sprintf "#$ %d" int

let single_quoted = Printf.sprintf "\'%s\'"
let quoted = Printf.sprintf "\"%s\""

let string_of_ccall_entry = function
  | { function_name; arity; args; ty_args; ty_return } ->
      Printf.sprintf "(%s %Ld (%s) (%s) %s)"
        (single_quoted function_name)
        arity
        (args |> List.map string_of_args |> String.concat " ")
        (ty_args |> List.map string_of_ffi_type |> String.concat " ")
        (string_of_ffi_type ty_return)

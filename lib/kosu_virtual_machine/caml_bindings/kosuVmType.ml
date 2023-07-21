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

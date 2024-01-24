(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
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

open TyzuBase

type tyzu_variable_polymorphic = ForAllVar of string

type tyzu_function_schema = {
  poly_vars : tyzu_variable_polymorphic list;
  parameters_type : tyzu_type list;
  return_type : tyzu_type;
}

and tyzu_type =
  | TyzuIdentifier of {
      module_resolver : module_resolver;
      parametrics_type : tyzu_type list;
      name : string;
    }
  | TyzuPolymorphic of tyzu_variable_polymorphic
  | TyzuPointer of { pointer_state : pointer_state; pointee_type : tyzu_type }
  | TyzuInteger of integer_info
  | TyzuFloat of fsize option
  | TyzuFunctionPtr of tyzu_function_schema
  | TyzuClosure of tyzu_function_schema
  | TyzuArray of { ktype : tyzu_type; size : int64 }
  | TyzuTuple of tyzu_type list
  | TyzuOpaque of { module_resolver : module_resolver; name : string }
  | TyzuOrdered
  | TyzuStringLit
  | TyzuChar
  | TyzuBool
  | TyzuUnit

type 'a typed = { elememt : 'a; tysu_type : tyzu_type }

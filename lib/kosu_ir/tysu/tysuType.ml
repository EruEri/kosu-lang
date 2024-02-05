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

open TysuBase

type tysu_variable_polymorphic = ForAllVar of string

type tysu_function_schema = {
  poly_vars : tysu_variable_polymorphic list;
  parameters_type : tysu_type list;
  return_type : tysu_type;
}

and tysu_type =
  | TysuIdentifier of {
      module_resolver : module_resolver;
      parametrics_type : tysu_type list;
      name : string;
    }
  | TysuPolymorphic of tysu_variable_polymorphic
  | TysuPointer of { pointer_state : pointer_state; pointee_type : tysu_type }
  | TysuInteger of integer_info
  | TysuFloat of fsize
  | TysuFunctionPtr of tysu_function_schema
  | TysuClosure of tysu_function_schema
  | TysuArray of { tysu_type : tysu_type; size : int64 }
  | TysuTuple of tysu_type list
  | TysuOpaque of { module_resolver : module_resolver; name : string }
  | TysuOrdered
  | TysuStringLit
  | TysuChar
  | TysuBool
  | TysuUnit

type 'a typed = { element : 'a; tysu_type : tysu_type }

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

type t =
  | KosuCompilerVariableFound of string
  | EnumNotIntegerTagSize of
      KosuFrontendAlt.Type.TyLoc.kosu_loctype Util.Position.location

exception TysuError of t

let tysu_error e = TysuError e
let kosu_compiler_variable_found s = tysu_error @@ KosuCompilerVariableFound s
let enum_not_integer_tag_size e = tysu_error @@ EnumNotIntegerTagSize e

let to_string = function
  | KosuCompilerVariableFound s ->
      Printf.sprintf "CompilerPolymorphic Variable found : \"%s\"" s
  | EnumNotIntegerTagSize _ ->
      Printf.sprintf "Enum tag not integer type"

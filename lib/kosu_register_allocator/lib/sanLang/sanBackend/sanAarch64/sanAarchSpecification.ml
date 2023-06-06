(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module type Aarch64AsmSpecification = sig
  type address_load_style = MacOS | Other

  val main : string
  val function_directives : string -> string list
  val adrp_style : address_load_style

  val constant_directives :
    string ->
    [ `IntVal of int64 | `StrVal of string ] ->
    string list

  val comment_prefix : string
  val string_litteral_section_start : string
  val string_litteral_section_end : string
  val string_litteral_directive : string
  val label_prefix : string
  val label_of_function: string -> string
end

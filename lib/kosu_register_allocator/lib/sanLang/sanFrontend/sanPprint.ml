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

open SanAst

let string_of_san_type = function
  | Ssize ->
      "ssize"
  | Boolean ->
      "bool"
  | Unit ->
      "unit"
  | Stringl ->
      "stringl"

let symbole_of_unary unary =
  match unary with TacNot -> "not" | TacUminus -> "(-.)"

let symbole_of_binary binary =
  match binary with
  | TacSelf TacAdd ->
      "+"
  | TacSelf TacMinus ->
      "-"
  | TacSelf TacMult ->
      "*"
  | TacSelf TacDiv ->
      "/"
  | TacSelf TacBitwiseAnd ->
      "&"
  | TacSelf TacBitwiseOr ->
      "|"
  | TacSelf TacBitwiseXor ->
      "^"
  | TacSelf TacShiftLeft ->
      "<<"
  | TacSelf TacShiftRight ->
      ">>"
  | TacBool TacAnd ->
      "&&"
  | TacBool TacOr ->
      "||"
  | TacBool TacSup ->
      ">"
  | TacBool TacSupEq ->
      ">="
  | TacBool TacInf ->
      "<"
  | TacBool TacInfEq ->
      "<="
  | TacBool TacEqual ->
      "=="
  | TacBool TacDiff ->
      "!="

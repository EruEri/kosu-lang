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


open SanPosition

type san_type = Ssize | Stringl | Boolean | Unit
type signature = san_type loc list * san_type loc

type atom =
  | String of string
  | Int of Nativeint.t
  | Variable of string
  | Boolean of bool

type tac_binop_bool =
  | TacOr
  | TacSup
  | TacSupEq
  | TacInf
  | TacInfEq
  | TacEqual
  | TacDiff
  | TacAnd

type tac_binop_self =
  | TacAdd
  | TacMinus
  | TacMult
  | TacDiv
  | TacBitwiseOr
  | TacBitwiseAnd
  | TacBitwiseXor
  | TacShiftLeft
  | TacShiftRight

type tac_binop = TacSelf of tac_binop_self | TacBool of tac_binop_bool
type tac_unop = TacNot | TacUminus
type binary = { binop : tac_binop; blhs : atom loc; brhs : atom loc }
type unary = { unop : tac_unop; atom : atom loc}
type fn_call = { fn_name : string loc; parameters : atom loc list }

type san_rvalue =
  | RVExpr of atom loc
  | RVUnary of unary
  | RVBinary of binary
  | RVFunctionCall of fn_call
  | RVDiscard of san_type loc
  | RVLater of san_type loc

type san_statement = SSDeclaration of string loc * san_rvalue loc

type san_ending =
  | SE_return of atom loc
  | SE_If of { expr : atom loc; if_label : string loc; else_label : string loc }

type san_basic_block = {
  label : string loc;
  statements : san_statement list;
  ending : san_ending option;
}

type san_function = {
  fn_name : string loc;
  parameters : (string loc * san_type loc) list;
  return_type : san_type loc;
  san_basic_blocks : san_basic_block list;
}

type san_node =
  | External of {
      fn_name : string loc;
      signature : signature;
      cname : string loc option;
    }
  | Declaration of san_function

type san_module = san_node list

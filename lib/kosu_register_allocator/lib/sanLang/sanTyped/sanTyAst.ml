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

open SanFrontend.SanAst

type san_type = SanFrontend.SanAst.san_type
type atom = SanFrontend.SanAst.atom

type typed_atom = { atom_type : san_type; atom : atom } 

type ty_unary = {
  unop: tac_unop;
  ty_atom: typed_atom;
}

type ty_binary = {
  binop: tac_binop;
  tylhs: typed_atom;
  tyrhs: typed_atom;
}

type ty_fncall = {
  fn_name: string;
  parameters: typed_atom list
}

type ty_san_rvalue = 
  | TyRVExpr of typed_atom
  | TYRVUnary of ty_unary
  | TYRVBinary of ty_binary
  | TyRVFunctionCall of ty_fncall
  | TyRVDiscard of san_type 
  | TYRVLater of san_type

type typed_san_rvalue = {
  san_rvalue: ty_san_rvalue;
  san_type: san_type
}

type ty_san_statement = TySSDeclaration of string * typed_san_rvalue

type ty_san_ending =
  | TySE_return of typed_atom
  | TYSE_If of { expr : typed_atom; if_label : string ; else_label : string }

type ty_san_basic_block = {
    label : string;
    statements : ty_san_statement list;
    ending : ty_san_ending option;
}

type ty_san_function = {
  fn_name : string;
  parameters : (string * san_type) list;
  return_type : san_type;
  san_basic_blocks : ty_san_basic_block list;
  locals: (string * san_type) list
}

type tysan_node =
  | TyExternal of {
      fn_name : string;
      signature : (san_type list * san_type);
      cname : string option;
    }
  | TyDeclaration of ty_san_function

type tysan_module = tysan_node list
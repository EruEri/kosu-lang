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
open SanFrontend.SanPprint
open SanTyAst
open Printf

let string_of_atom = function
  | Variable v -> v
  | Int i -> sprintf "%nd" i
  | Boolean b -> string_of_bool b
  | String s -> sprintf "\"%s\"" s

let string_of_typed_atom tyatom =
  sprintf "%s : %s"
    (string_of_atom tyatom.atom)
    (string_of_san_type tyatom.atom_type)

let string_of_ty_san_rvlue = function
  | TyRVExpr typed_atom -> string_of_typed_atom typed_atom
  | TYRVUnary { unop; ty_atom } ->
      sprintf "%s %s" (symbole_of_unary unop) (string_of_atom ty_atom.atom)
  | TYRVBinary { binop; tylhs; tyrhs } ->
      sprintf "%s %s %s"
        (string_of_atom tylhs.atom)
        (symbole_of_binary binop)
        (string_of_atom tyrhs.atom)
  | TyRVFunctionCall { fn_name; parameters } ->
      sprintf "%s(%s)" fn_name
        (parameters
        |> List.map (fun tyatom -> string_of_atom tyatom.atom)
        |> String.concat ", ")
  | TyRVDiscard ty -> sprintf "discard : %s" (string_of_san_type ty)
  | TYRVLater ty -> sprintf "lateinit : %s" (string_of_san_type ty)

let string_of_typed_san_rvalue rvalue =
  sprintf "%s : %s"
    (string_of_san_type rvalue.san_type)
    (string_of_ty_san_rvlue rvalue.san_rvalue)

let string_of_statememnt = function
  | TySSDeclaration (s, rvalue) ->
      sprintf "%s : %s = %s" s
        (string_of_san_type rvalue.san_type)
        (string_of_ty_san_rvlue rvalue.san_rvalue)

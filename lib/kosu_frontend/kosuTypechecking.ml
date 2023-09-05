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

open Position
open KosuAst
open KosuType


let rec typeof (kosu_env: KosuEnv.kosu_env) (expr: KosuAst.kosu_expression location) = 
  match expr.value with
  | EEmpty -> 
    kosu_env, Ty.TyUnit
  | ETrue | EFalse -> 
    kosu_env, TyBool
  | ECmpLess | ECmpEqual | ECmpGreater ->
    kosu_env, TyOrdered
  | ENullptr -> 
    let fresh_type = Ty.fresh_variable_type () in
    kosu_env, TyPointer {pointee_type = fresh_type; pointer_state = Const}
  | EStringl _ ->
    kosu_env, TyStringLit
  | EChar _ ->
    kosu_env, TyChar
  | EInteger {signedness; isize; ivalue = _} -> 
    kosu_env, TyInteger (signedness, isize)
  | EFloat {fsize; fvalue = _} -> 
    kosu_env, TyFloat fsize
  | ESizeof _ ->
    failwith "SIZEOF: TODO"
  | EFieldAccess {first_expr; field } ->
    let kosu_env, typeof = typeof kosu_env first_expr in
    failwith ""
  | EArrayAccess {array_expr; index_expr} ->
    failwith "TODO: Array"
  | ETupleAccess {first_expr; index} -> 
    failwith "TUPLE"
  | EConstIdentifier {module_resolver; identifier} ->
    failwith ""
  | EIdentifier {module_resolver; id} -> 
    failwith ""
  | EStruct {module_resolver; struct_name; fields} ->
    failwith ""
  | EEnum {
    module_resolver; enum_name;
    variant;
    assoc_exprs;
  } -> 
    failwith ""
  | EBlock block -> 
    typeof_block kosu_env block
  | EDeref expr ->
    let kosu_env, ty = typeof kosu_env expr in
    failwith ""
  | ETuple exprs -> 
    failwith ""
  | EArray exprs ->
    failwith ""
  | EBuiltinFunctionCall {fn_name; parameters} ->
    failwith ""
  | EFunctionCall {module_resolver; generics_resolver; fn_name; parameters} ->
    failwith ""
  | EWhile {condition_expr; body} ->
    failwith ""
  | ECases {cases; else_body} ->
    failwith ""
  | EMatch {expression; patterns} ->
    failwith ""
  | EAnonFunction {kind; parameters; body} -> 
    failwith ""
and typeof_block kosu_env block = failwith ""
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

(**
    [is_static_expression expr] determines if an expression can be evaluated a compile time
*)
let rec is_static_expression :
    KosuAst.kosu_expression Position.location ->
    (unit, KosuAst.kosu_expression Position.location) Result.t =
 fun expr ->
  let ( let* ) = Result.bind in
  let ok = Ok () in
  let err = Result.error in
  match expr.value with
  | EEmpty
  | ETrue
  | EFalse
  | ENullptr _
  | ECmpLess
  | ECmpEqual
  | ECmpGreater
  | EStringl _
  | EChar _
  | EInteger _
  | EConstIdentifier _
  | EFloat _ ->
      ok
  | ESizeof either ->
      let r =
        match either with
        | Either.Left _ ->
            ok
        | Right expr ->
            is_static_expression expr
      in
      r
  | EDeref first_expr
  | EFieldAccess { first_expr; field = _ }
  | ETupleAccess { first_expr; index = _ } ->
      is_static_expression first_expr
  | EArrayAccess { array_expr; index_expr } ->
      let* () = is_static_expression array_expr in
      is_static_expression index_expr
  | EIdentifier { module_resolver; id = _ } ->
      let r =
        match module_resolver with
        | ModuleResolverLoc [] ->
            err expr
        | ModuleResolverLoc (_ :: _) ->
            ok
      in
      r
  | EStruct { module_resolver = _; struct_name = _; fields } ->
      Util.Ulist.fold_ok
        (fun () (_, expr) -> is_static_expression expr)
        () fields
  | EEnum
      { assoc_exprs = exprs; module_resolver = _; enum_name = _; variant = _ }
  | ETuple exprs
  | EArray exprs ->
      Util.Ulist.fold_ok (fun () -> is_static_expression) () exprs
  | EBlock kosu_block ->
      let r =
        match kosu_block with
        | { kosu_stmts = []; kosu_expr } ->
            is_static_expression kosu_expr
        | { kosu_stmts = _ :: _; kosu_expr = _ } ->
            err expr
      in
      r
  | EBuiltinFunctionCall _
  | EFunctionCall _
  | EWhile _
  | ECases _
  | EMatch _
  | EAnonFunction _ ->
      err expr

(**
    [last_expression expr] returns the last expression of mono block expression such as
    [KosuAst.EBlock _] or [KosuAst.EWhile _] or [expr] if others
*)
let rec last_expression expr =
  let open Position in
  let open KosuAst in
  match expr.value with
  | EBlock block ->
      last_expression block.kosu_expr
  | EWhile { body; _ } ->
      last_expression body.kosu_expr
  | EEmpty
  | ETrue
  | EFalse
  | ENullptr _
  | ECmpLess
  | ECmpEqual
  | ECmpGreater
  | EStringl _
  | EChar _
  | EInteger _
  | EFloat _
  | ESizeof _
  | EFieldAccess _
  | EArrayAccess _
  | ETupleAccess _
  | EConstIdentifier _
  | EIdentifier _
  | EStruct _
  | EEnum _
  | EDeref _
  | ETuple _
  | EArray _
  | EBuiltinFunctionCall _
  | EFunctionCall _
  | ECases _
  | EMatch _
  | EAnonFunction _ ->
      expr

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
open KosuError

let rec typeof (kosu_env : KosuEnv.kosu_env)
    (expr : KosuAst.kosu_expression location) =
  match expr.value with
  | EEmpty ->
      (kosu_env, Ty.TyUnit)
  | ETrue | EFalse ->
      (kosu_env, TyBool)
  | ECmpLess | ECmpEqual | ECmpGreater ->
      (kosu_env, TyOrdered)
  | ENullptr ->
      let fresh_type = Ty.fresh_variable_type () in
      (kosu_env, TyPointer { pointee_type = fresh_type; pointer_state = Const })
  | EStringl _ ->
      (kosu_env, TyStringLit)
  | EChar _ ->
      (kosu_env, TyChar)
  | EInteger { integer_info; ivalue = _ } ->
      (kosu_env, TyInteger integer_info)
  | EFloat { fsize; fvalue = _ } ->
      (kosu_env, TyFloat fsize)
  | ESizeof either ->
      let kosu_env =
        match either with
        | Either.Left kosu_type ->
            let ty = KosuUtil.Ty.of_tyloc' kosu_type in
            let () =
              match KosuUtil.Ty.contains_polymorphic ty with
              | true ->
                  raise @@ sizeof_polytype kosu_type.position
              | false ->
                  ()
            in
            kosu_env
        | Either.Right rhs ->
            let ext_env, _ = typeof kosu_env rhs in
            KosuEnv.merge_constraint ext_env kosu_env
      in
      (kosu_env, KosuUtil.(Ty.of_tyloc LocType.usize))
  | EFieldAccess { first_expr; field } ->
      (* instanciante todo*)
      let env, typeof = typeof kosu_env first_expr in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      failwith ""
  | EArrayAccess { array_expr; index_expr } ->
      failwith "TODO: Array"
  | ETupleAccess { first_expr; index } ->
      failwith "TUPLE"
  | EConstIdentifier { module_resolver; identifier } ->
      let const_decl =
        match
          KosuEnv.find_const_declaration (module_resolver, identifier) kosu_env
        with
        | Some decl ->
            decl
        | None ->
            raise @@ failwith ""
      in
      failwith ""
  | EIdentifier { module_resolver = ModuleResolverLoc modules; id } ->
      let _ =
        match modules with
        | [] ->
            failwith "Find module in first case"
        | _ :: _ as modules ->
            failwith "Find speficique module"
      in
      failwith ""
  | EStruct { module_resolver; struct_name; fields } ->
      failwith ""
  | EEnum { module_resolver; enum_name; variant; assoc_exprs } ->
      failwith ""
  | EBlock block ->
      typeof_block kosu_env block
  | EDeref expr ->
      let kosu_env, ty = typeof kosu_env expr in
      let pointee_type =
        match ty with
        | TyPointer { pointee_type; pointer_state = _ } ->
            pointee_type
        | _ ->
            failwith ""
      in
      (kosu_env, pointee_type)
  | ETuple exprs ->
      let kosu_env, types =
        List.fold_left_map
          (fun kosu_env expr ->
            let env, ty = typeof kosu_env expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, ty)
          )
          kosu_env exprs
      in
      (kosu_env, TyTuple types)
  | EArray exprs ->
      let fresh_variable = Ty.fresh_variable_type () in
      let length = List.length exprs in
      let kosu_env =
        List.fold_left
          (fun kosu_env expr ->
            let env, ty = typeof kosu_env expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~lhs:fresh_variable ~rhs:ty expr
                kosu_env
            in
            kosu_env
          )
          kosu_env exprs
      in
      (kosu_env, TyArray { size = Int64.of_int length; ktype = fresh_variable })
  | EBuiltinFunctionCall { fn_name; parameters } ->
      failwith ""
  | EFunctionCall { module_resolver; generics_resolver; fn_name; parameters } ->
      failwith ""
  | EWhile { condition_expr; body } ->
      let env, ty = typeof kosu_env condition_expr in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~lhs:ty ~rhs:TyBool condition_expr
          kosu_env
      in
      let env, body_ty = typeof_block kosu_env body in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~lhs:body_ty ~rhs:TyUnit body.kosu_expr
          kosu_env
      in
      (kosu_env, TyUnit)
  | ECases { cases; else_body } ->
      let fresh_type = KosuType.Ty.fresh_variable_type () in
      let env, ty = typeof_block kosu_env else_body in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~lhs:fresh_type ~rhs:ty
          else_body.kosu_expr kosu_env
      in
      let kosu_env =
        cases
        |> List.fold_left
             (fun kosu_env (condition_expr, body) ->
               let env, ty = typeof kosu_env condition_expr in
               let kosu_env = KosuEnv.merge_constraint env kosu_env in
               let kosu_env =
                 KosuEnv.add_typing_constraint ~lhs:ty ~rhs:TyBool
                   condition_expr kosu_env
               in
               let env, body_ty = typeof_block kosu_env body in
               let kosu_env = KosuEnv.merge_constraint env kosu_env in
               let kosu_env =
                 KosuEnv.add_typing_constraint ~lhs:body_ty ~rhs:fresh_type
                   body.kosu_expr kosu_env
               in
               kosu_env
             )
             kosu_env
      in
      (kosu_env, fresh_type)
  | EMatch { expression; patterns } ->
      failwith ""
  | EAnonFunction { kind; parameters; body } ->
      failwith ""

and typeof_block kosu_env block = failwith ""

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

module PatternIdentifierBound = Set.Make (struct
  type t = string location * Ty.kosu_type

  let compare (lhs : t) (rhs : t) =
    String.compare (value @@ fst lhs) (value @@ fst rhs)
end)

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
              match
                KosuEnv.KosuTypeVariableSet.is_empty
                @@ KosuEnv.free_ty_variable ty kosu_env
              with
              | false ->
                  ()
              | true ->
                  raise @@ sizeof_polytype kosu_type.position
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
      let env, ty = typeof kosu_env array_expr in
      let ty_elt =
        match ty with
        | TyArray { ktype; size = _ } ->
            ktype
        | _ ->
            failwith "Array access of no array type"
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let env, ty = typeof kosu_env index_expr in
      let () =
        match ty with
        | TyInteger _ ->
            ()
        | _ ->
            failwith "Arrayy subscript with on integer type"
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      (kosu_env, ty_elt)
  | ETupleAccess { first_expr; index } ->
      let env, ty = typeof kosu_env first_expr in
      let ty_elts =
        match ty with
        | TyTuple elts ->
            elts
        | _ ->
            failwith "Tuple access of no tuple type"
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let ty =
        match List.nth_opt ty_elts (Int64.to_int index.value) with
        | Some ty ->
            ty
        | None ->
            failwith "Tuple out of bound index"
      in
      (kosu_env, ty)
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

and typeof_statement kosu_env (statement : KosuAst.kosu_statement location) =
  match statement.value with
  | SDeclaration { is_const; pattern; explicit_type; expression } ->
      failwith "DEclaration todo"
  | SAffection { is_deref; lvalue; expression } ->
      failwith "Affectation"
  | SDiscard expression ->
      let env, _ = typeof kosu_env expression in
      KosuEnv.merge_constraint env kosu_env
  | SOpen { module_resolver } ->
      let kosu_module =
        match KosuEnv.find_module_opt module_resolver kosu_env with
        | Some kosu_module ->
            kosu_module
        | None ->
            failwith "Module not found"
      in
      KosuEnv.add_module kosu_module kosu_env

and typeof_pattern scrutinee_type kosu_env
    (pattern : KosuAst.kosu_pattern location) =
  let open KosuType.Ty in
  match pattern.value with
  | PEmpty ->
      let ty = Ty.TyUnit in
      (* let kosu_env = KosuEnv.add_typing_constraint ~lhs:strutinee_type ~rhs:ty pattern kosu_env in *)
      ([], (kosu_env, ty))
  | PTrue | PFalse ->
      let ty = Ty.TyBool in
      (* let kosu_env = KosuEnv.add_typing_constraint ~lhs:strutinee_type ~rhs:ty pattern kosu_env in *)
      ([], (kosu_env, ty))
  | PCmpLess | PCmpEqual | PCmpGreater ->
      let ty = TyOrdered in
      (* let kosu_env = KosuEnv.add_typing_constraint ~lhs:strutinee_type ~rhs:ty pattern kosu_env in *)
      ([], (kosu_env, ty))
  | PNullptr ->
      let fresh_ty = fresh_variable_type () in
      let ty = TyPointer { pointer_state = Const; pointee_type = fresh_ty } in
      (* let kosu_env = KosuEnv.add_typing_constraint ~lhs:strutinee_type ~rhs:ty pattern kosu_env in *)
      ([], (kosu_env, ty))
  | PWildcard ->
      ([], (kosu_env, scrutinee_type))
  | PFloat _ ->
      let ty = TyFloat None in
      ([], (kosu_env, ty))
  | PChar _ ->
      let ty = TyChar in
      ([], (kosu_env, ty))
  | PInteger { value = _ } ->
      let ty = TyInteger None in
      ([], (kosu_env, ty))
  | PIdentifier identifier ->
      let bound = (identifier, scrutinee_type) in
      ([ bound ], (kosu_env, scrutinee_type))
  | PTuple patterns ->
      let module PIB = PatternIdentifierBound in
      let env, bound_typed =
        List.fold_left_map
          (fun kosu_env pattern ->
            let fresh_ty = fresh_variable_type () in
            let bound, (env, ty) = typeof_pattern fresh_ty kosu_env pattern in
            let env =
              KosuEnv.add_typing_constraint ~lhs:fresh_ty ~rhs:ty pattern env
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, (bound, ty))
          )
          kosu_env patterns
      in
      let bound_variable, tuples_elt_ty = List.split bound_typed in

      let indentifier_set =
        List.fold_left
          (fun set t_bounds_ty ->
            let comming = PIB.of_list t_bounds_ty in
            let intersection = PIB.inter comming set in
            let () =
              match PIB.is_empty intersection with
              | true ->
                  ()
              | false ->
                  raise @@ pattern_already_bound_identifier @@ List.map fst
                  @@ PIB.elements intersection
            in
            PIB.union comming set
          )
          PIB.empty bound_variable
      in

      let indentifiers = PIB.elements indentifier_set in

      let ty_pattern = TyTuple tuples_elt_ty in

      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      (indentifiers, (kosu_env, ty_pattern))
  | PCase { module_resolver; enum_name; variant; assoc_patterns } ->
      failwith ""
  | PRecord { module_resolver; struct_name; pfields } ->
      failwith ""
  | POr patterns ->
      let module PIB = PatternIdentifierBound in
      let env, bound_typed =
        List.fold_left_map
          (fun kosu_env pattern ->
            let fresh_ty = fresh_variable_type () in
            let bound, (env, ty) = typeof_pattern fresh_ty kosu_env pattern in
            let env =
              KosuEnv.add_typing_constraint ~lhs:fresh_ty ~rhs:ty pattern env
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, (bound, ty))
          )
          kosu_env patterns
      in
      let bound_variable, tuples_elt_ty = List.split bound_typed in

      let x_bounds, xs_bounds =
        match bound_variable with
        | [] ->
            failwith "Unreachable: Cannot be empty"
        | x :: xs ->
            (x, xs)
      in
      let first_set = PIB.of_list x_bounds in

      let indentifier_set =
        List.fold_left
          (fun set t_bounds_ty ->
            let comming = PIB.of_list t_bounds_ty in
            let () =
              match PIB.equal set comming with
              | true ->
                  ()
              | false ->
                  let inter1 = PIB.diff set comming in
                  let inter2 = PIB.diff comming set in
                  let diff = PIB.union inter1 inter2 in
                  raise @@ pattern_identifier_not_bound @@ List.map fst
                  @@ PIB.elements diff
            in
            set
          )
          first_set xs_bounds
      in

      let indentifiers = PIB.elements indentifier_set in

      let ty_pattern = TyTuple tuples_elt_ty in

      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      (indentifiers, (kosu_env, ty_pattern))
  | PAs { pas_pattern; pas_bound } ->
      let bound, (env, ty) =
        typeof_pattern scrutinee_type kosu_env pas_pattern
      in
      let bound =
        match
          List.exists
            (fun ((id : _ location), _) -> id.value = pas_bound.value)
            bound
        with
        | true ->
            raise @@ pattern_already_bound_identifier [ pas_bound ]
        | false ->
            (pas_bound, ty) :: bound
      in
      let env =
        KosuEnv.add_typing_constraint ~lhs:scrutinee_type ~rhs:ty pas_pattern
          env
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      (bound, (kosu_env, ty))

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

open KosuAst
open KosuType
open KosuError.Exn
open Position

module PatternIdentifierBound = Set.Make (struct
  type t = string location * KosuType.Ty.kosu_type

  let compare (lhs : t) (rhs : t) =
    String.compare (Position.value @@ fst lhs) (Position.value @@ fst rhs)
end)

module CapturedIdentifier = PatternIdentifierBound

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
      let default = Ty.TyInteger integer_info in
      let tuples =
        match integer_info with
        | Some _ ->
            (kosu_env, default)
        | None ->
            let t = Ty.fresh_variable_type () in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cexpected:default ~cfound:t expr
                kosu_env
            in
            (kosu_env, t)
      in
      tuples
  | EFloat { fsize; fvalue = _ } ->
      let default = Ty.TyFloat fsize in
      let tuple =
        match fsize with
        | Some _ ->
            (kosu_env, default)
        | None ->
            let t = Ty.fresh_variable_type () in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cexpected:default ~cfound:t expr
                kosu_env
            in
            (kosu_env, t)
      in
      tuple
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
      (kosu_env, KosuUtil.(Ty.of_tyloc TyLoc.usize))
  | EFieldAccess { first_expr; field = sfield } ->
      (* instanciante todo*)
      let env, ty_expr = typeof kosu_env first_expr in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let try_find_ty = function
        | Ty.TyIdentifier _ as ty ->
            Option.some @@ Either.left ty
        | Ty.TyPolymorphic p ->
            Option.some @@ Either.right p
        | _ ->
            raise @@ non_struct_type first_expr.position
      in
      let ty_solve =
        match KosuEnv.find_or_try_solve try_find_ty ty_expr kosu_env with
        | Some t ->
            t
        | None ->
            raise @@ cannot_infer_type first_expr.position
      in
      let struct_decl_opt =
        KosuEnv.find_struct_declaration_type ty_solve kosu_env
      in
      let module_resolver, struct_decl =
        match struct_decl_opt with
        | Some t ->
            t
        | None ->
            raise @@ cannot_find_struct_decl
            @@ Position.map (fun _ -> ty_solve) first_expr
      in
      let pametrics_type = KosuUtil.Ty.parametrics_type ty_expr in
      let struct_decl =
        match
          KosuUtil.Struct.substitution module_resolver pametrics_type
            struct_decl
        with
        | Some (raw_struct_decl, _) ->
            raw_struct_decl
        | None ->
            failwith "Fail to substitute type"
      in
      let ty =
        match KosuUtil.Struct.field sfield.value struct_decl with
        | Some ty ->
            ty
        | None ->
            raise @@ field_not_in_struct struct_decl sfield
      in
      (kosu_env, ty)
  | EArrayAccess { array_expr; index_expr } ->
      let env, ty = typeof kosu_env array_expr in
      let ty_elt =
        match ty with
        | TyArray { ktype; size = _ } ->
            ktype
        | TyPolymorphic ty ->
            let ty =
              match KosuEnv.try_solve ty kosu_env with
              | Some ty ->
                  (* let () = Printf.printf "field found = %s\n" (KosuPrint.string_of_kosu_type ty) in  *)
                  ty
              | None ->
                  failwith "After solve not goot type"
            in
            ty
        | _ ->
            raise @@ non_array_access array_expr.position
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let env, ty = typeof kosu_env index_expr in
      let kosu_env =
        match ty with
        | TyInteger _ ->
            kosu_env
        | TyPolymorphic _ ->
            KosuEnv.add_typing_constraint ~cfound:ty
              ~cexpected:(Ty.TyInteger None) index_expr kosu_env
        | _ ->
            raise @@ array_subscribe_not_integer index_expr.position
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      (kosu_env, ty_elt)
  | ETupleAccess { first_expr; index } ->
      let env, ty = typeof kosu_env first_expr in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let try_expect_tuple = function
        | Ty.TyTuple tes ->
            Option.some @@ Either.left tes
        | Ty.TyPolymorphic p ->
            Option.some @@ Either.right p
        | _ ->
            raise @@ non_tuple_access first_expr.position
      in
      let ty_elts =
        match KosuEnv.find_or_try_solve try_expect_tuple ty kosu_env with
        | Some t ->
            t
        | None ->
            raise @@ cannot_infer_type first_expr.position
      in
      let ty =
        match List.nth_opt ty_elts (Int64.to_int index.value) with
        | Some ty ->
            ty
        | None ->
            raise @@ index_out_of_bounds (List.length ty_elts) index
      in
      (kosu_env, ty)
  | EConstIdentifier { module_resolver; identifier } ->
      let _module_resolver, const_decl =
        match
          KosuEnv.find_const_declaration (module_resolver, identifier) kosu_env
        with
        | Some decl ->
            decl
        | None ->
            raise @@ unbound_constante module_resolver identifier
      in
      let ty = KosuUtil.Ty.of_tyloc' const_decl.explicit_type in
      (kosu_env, ty)
  | EIdentifier { module_resolver; id } ->
      let t = KosuEnv.find_identifier module_resolver id.value kosu_env in
      let t =
        match t with
        | Some t ->
            t
        | None ->
            raise @@ KosuError.Exn.unbound_identifier id
      in
      (kosu_env, t)
  | EStruct { module_resolver; struct_name; fields } ->
      let module_resolver, struct_decl =
        match
          KosuEnv.find_struct_declaration
            (module_resolver, struct_name)
            kosu_env
        with
        | Some decl ->
            decl
        | None ->
            failwith "No struct found"
      in
      let struct_decl, ty =
        KosuUtil.Struct.substitution_fresh ~fresh:Ty.fresh_variable_type
          module_resolver struct_decl
      in
      let combined_fields =
        match List.combine struct_decl.fields fields with
        | combined ->
            combined
        | exception _ ->
            failwith "Wrong field arrity"
      in

      let kosu_env =
        List.fold_left
          (fun kosu_env ((struct_name, struct_type), (expr_name, expr_expr)) ->
            let () =
              match struct_name = expr_name.value with
              | true ->
                  ()
              | false ->
                  failwith "Struct init not matching name"
            in
            let env, ty = typeof kosu_env expr_expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:struct_type
                expr_expr kosu_env
            in
            kosu_env
          )
          kosu_env combined_fields
      in
      (kosu_env, ty)
  | EEnum { module_resolver; enum_name; variant; assoc_exprs } ->
      let module_resolver, enum_decl =
        match
          KosuEnv.find_enum_declaration module_resolver
            (Option.map Position.value enum_name)
            variant kosu_env
        with
        | Some t ->
            t
        | None ->
            failwith "Cannot find the enum declaration"
      in
      let enum_decl, ty =
        KosuUtil.Enum.substitution_fresh ~fresh:Ty.fresh_variable_type
          module_resolver enum_decl
      in
      let enum_associated_type =
        match KosuUtil.Enum.assoc_types variant.value enum_decl with
        | Some assoc_types ->
            assoc_types
        | None ->
            failwith "No variant name"
      in
      let assoc_types_exprs =
        match List.combine enum_associated_type assoc_exprs with
        | combined ->
            combined
        | exception _ ->
            failwith "Wrong field arrity for enum"
      in
      let kosu_env =
        List.fold_left
          (fun kosu_env (enum_type, enum_expr) ->
            let env, ty = typeof kosu_env enum_expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:enum_type
                enum_expr kosu_env
            in
            kosu_env
          )
          kosu_env assoc_types_exprs
      in
      (kosu_env, ty)
  | EBlock block ->
      typeof_block kosu_env block
  | EDeref expr ->
      let kosu_env, ty = typeof kosu_env expr in
      let pointee_type =
        match ty with
        | TyPointer { pointee_type; pointer_state = _ } ->
            pointee_type
        | _ ->
            failwith "Should be pointer type"
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
              KosuEnv.add_typing_constraint ~cexpected:fresh_variable ~cfound:ty
                expr kosu_env
            in
            kosu_env
          )
          kosu_env exprs
      in
      (kosu_env, TyArray { size = Int64.of_int length; ktype = fresh_variable })
  | EBuiltinFunctionCall { fn_name; parameters } ->
      let () = ignore (fn_name, parameters) in
      failwith "TODO: Ebuiling duntion"
  | EFunctionCall { module_resolver; generics_resolver; fn_name; parameters } ->
      let t = KosuEnv.find_identifier module_resolver fn_name.value kosu_env in
      let t =
        match t with
        | Some t ->
            t
        | None ->
            raise @@ KosuError.Exn.unbound_identifier fn_name
      in

      let schema, _ =
        match t with
        | ( TyFunctionPtr schema
          | TyClosure schema
          | TyInnerClosureId (ClosureType { schema; _ }) ) as ty ->
            (schema, ty)
        | TyOrdered
        | TyStringLit
        | TyChar
        | TyBool
        | TyUnit
        | TyIdentifier { module_resolver = ModuleResolver_ _; _ }
        | TyPolymorphic (PolymorphicVar _ | CompilerPolymorphicVar _)
        | TyPointer _
        | TyInteger _
        | TyFloat _
        | TyArray _
        | TyTuple _
        | TyOpaque { module_resolver = ModuleResolver_ _; _ } ->
            failwith "Not a callable type"
      in

      let () =
        match Util.Ulist.are_same_length schema.parameters_type parameters with
        | true ->
            ()
        | false ->
            failwith "Not same length for callable"
      in

      let fresh_variables =
        List.map (fun _ -> Ty.fresh_variable_type ()) schema.poly_vars
      in
      let assoc_poly_fresh = List.combine schema.poly_vars fresh_variables in
      let kosu_env =
        match generics_resolver with
        | None ->
            kosu_env
        | Some tyes ->
            let () =
              match Util.Ulist.are_same_length tyes schema.poly_vars with
              | false ->
                  failwith "Unwrong generic explit size"
              | true ->
                  ()
            in
            tyes
            |> List.combine assoc_poly_fresh
            |> List.fold_left
                 (fun kosu_env ((_, fresh_ty), ty) ->
                   KosuEnv.add_typing_constraint
                     ~cfound:(KosuUtil.Ty.of_tyloc' ty) ~cexpected:fresh_ty ty
                     kosu_env
                 )
                 kosu_env
      in
      let schema = KosuUtil.Ty.ty_substitution_schema assoc_poly_fresh schema in

      let parameters = List.combine schema.parameters_type parameters in
      let kosu_env =
        List.fold_left
          (fun kosu_env (sig_ty, expr) ->
            let env, ty = typeof kosu_env expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            KosuEnv.add_typing_constraint ~cexpected:sig_ty ~cfound:ty expr
              kosu_env
          )
          kosu_env parameters
      in

      (kosu_env, schema.return_type)
  | EWhile { condition_expr; body } ->
      let env, ty = typeof kosu_env condition_expr in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:TyBool
          condition_expr kosu_env
      in
      let env, body_ty = typeof_block kosu_env body in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cfound:body_ty ~cexpected:TyUnit
          body.kosu_expr kosu_env
      in
      (kosu_env, TyUnit)
  | ECases { cases; else_body } ->
      let fresh_type = KosuType.Ty.fresh_variable_type () in
      let env, ty = typeof_block kosu_env else_body in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cexpected:fresh_type ~cfound:ty
          else_body.kosu_expr kosu_env
      in
      let kosu_env =
        List.fold_left
          (fun kosu_env (condition_expr, body) ->
            let env, ty = typeof kosu_env condition_expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:TyBool
                condition_expr kosu_env
            in
            let env, body_ty = typeof_block kosu_env body in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:body_ty
                ~cexpected:fresh_type body.kosu_expr kosu_env
            in
            kosu_env
          )
          kosu_env cases
      in
      (kosu_env, fresh_type)
  | EMatch { expression; patterns } ->
      let env, scrutinee_type = typeof kosu_env expression in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let fresh_variable_ty = Ty.fresh_variable_type () in
      let kosu_env =
        List.fold_left
          (fun kosu_env (kosu_pattern, kosu_block) ->
            let bound, (env, pattern_ty) =
              typeof_pattern scrutinee_type kosu_env kosu_pattern
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cexpected:scrutinee_type
                ~cfound:pattern_ty kosu_pattern kosu_env
            in
            let kosu_block_env =
              List.fold_left
                (fun env (id, bound_ty) ->
                  KosuEnv.add_variable true id bound_ty env
                )
                kosu_env bound
            in
            let block_env, block_ty = typeof_block kosu_block_env kosu_block in
            let block_env =
              KosuEnv.add_typing_constraint ~cexpected:fresh_variable_ty
                ~cfound:block_ty kosu_block.kosu_expr block_env
            in
            let kosu_env = KosuEnv.merge_constraint block_env kosu_env in
            kosu_env
          )
          kosu_env patterns
      in
      (kosu_env, fresh_variable_ty)
  | EAnonFunction { kind; parameters; body } ->
      let () = ignore (kind, parameters, body) in
      failwith "TODO: EAnonFunction"

and typeof_block kosu_env block =
  let kosu_env = List.fold_left typeof_statement kosu_env block.kosu_stmts in
  typeof kosu_env block.kosu_expr

and typeof_statement kosu_env (statement : KosuAst.kosu_statement location) =
  match statement.value with
  | SDeclaration { is_const; pattern; explicit_type; expression } ->
      let env, ety = typeof kosu_env expression in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        match explicit_type with
        | Some tly ->
            let ty = KosuUtil.Ty.of_tyloc' tly in
            KosuEnv.add_typing_constraint ~cexpected:ty ~cfound:ety expression
              kosu_env
        | None ->
            kosu_env
      in
      let bounds, (env, ty) = typeof_pattern ety kosu_env pattern in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:ety pattern kosu_env
      in
      let kosu_env =
        List.fold_left
          (fun kosu_env (id, ty) -> KosuEnv.add_variable is_const id ty kosu_env)
          kosu_env bounds
      in
      kosu_env
  | SAffection { is_deref; lvalue; expression } ->
      let find_identifier_type t env =
        match t with
        | Ty.TyIdentifier _ as t ->
            t
        | TyPolymorphic p ->
            let ty =
              match KosuEnv.try_solve p env with
              | Some (TyIdentifier _ as t) ->
                  t
              | Some _ | None ->
                  failwith "Not identifier type: after solving"
            in
            ty
        | _ ->
            failwith "Not ideintifiet type"
      in
      let env, ty = typeof kosu_env expression in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let (KosuLvalue { variable; fields }) = lvalue in
      let variable_info =
        match KosuEnv.assoc_type_opt variable.value kosu_env with
        | Some t ->
            t
        | None ->
            raise @@ KosuError.Exn.unbound_identifier variable
      in
      let () =
        match variable_info.is_const with
        | true when not is_deref ->
            failwith "Reassign const"
        | false | true ->
            ()
      in
      let kosu_env =
        match fields with
        | [] ->
            let expected_type =
              let t = Ty.fresh_variable_type () in
              match is_deref with
              | true ->
                  Ty.TyPointer { pointer_state = Mutable; pointee_type = t }
              | false ->
                  t
            in
            kosu_env
            |> KosuEnv.add_typing_constraint ~cfound:variable_info.kosu_type
                 ~cexpected:expected_type expression
            |> KosuEnv.add_typing_constraint ~cexpected:variable_info.kosu_type
                 ~cfound:ty variable
        | _ :: _ ->
            let struct_type =
              match is_deref with
              | false ->
                  let struct_type =
                    find_identifier_type variable_info.kosu_type kosu_env
                  in
                  struct_type
              | true ->
                  let try_expect_mutable_pointer kosu_type =
                    match kosu_type with
                    | Ty.TyPointer { pointer_state = Mutable; pointee_type } ->
                        Option.some @@ Either.left pointee_type
                    | Ty.TyPolymorphic p ->
                        Option.some @@ Either.right p
                    | Ty.TyPointer { pointer_state = Const; pointee_type = _ }
                      ->
                        failwith "Cannot deref pointer type"
                    | _ ->
                        failwith "Not pointer type"
                  in
                  let base_ty =
                    match
                      KosuEnv.find_or_try_solve try_expect_mutable_pointer
                        variable_info.kosu_type kosu_env
                    with
                    | Some t ->
                        t
                    | None ->
                        failwith "Cannot specialiste type"
                  in
                  find_identifier_type base_ty kosu_env
            in
            let ty_field =
              match KosuEnv.resolve_field_type fields struct_type kosu_env with
              | Ok t ->
                  t
              | Error e ->
                  raise e
            in
            KosuEnv.add_typing_constraint ~cexpected:ty_field ~cfound:ty
              expression kosu_env
      in
      kosu_env
  | SDiscard expression ->
      let env, _ = typeof kosu_env expression in
      KosuEnv.merge_constraint env kosu_env
  | SOpen { module_resolver } ->
      let kosu_module =
        match KosuEnv.find_module_opt module_resolver kosu_env with
        | Some kosu_module ->
            kosu_module
        | None ->
            raise @@ unbound_module module_resolver
      in
      KosuEnv.add_module kosu_module kosu_env

(**
    [typeof_pattern scrutinee_type kosu_env pattern] types the pattern [pattern] in the environment [kosu_env]
    with the scrutinee_type being [scrutinee_type]
*)
and typeof_pattern scrutinee_type kosu_env
    (pattern : KosuAst.kosu_pattern location) =
  (*Dont forget to raise if we try to bind an identifier to an existing variable in the*)
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
              KosuEnv.add_typing_constraint ~cexpected:fresh_ty ~cfound:ty
                pattern env
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
      let module PIB = PatternIdentifierBound in
      let name = Option.map Position.value enum_name in
      let module_loc, enum_decl =
        match
          KosuEnv.find_enum_declaration module_resolver name variant kosu_env
        with
        | Some l ->
            l
        | None ->
            failwith "No module found"
      in
      let enum_decl, enum_ty =
        KosuUtil.Enum.substitution_fresh ~fresh:Ty.fresh_variable_type
          module_loc enum_decl
      in
      let enum_associated_type =
        match KosuUtil.Enum.assoc_types variant.value enum_decl with
        | Some assoc_types ->
            assoc_types
        | None ->
            failwith "No variant name"
      in
      let assoc_types_exprs =
        match List.combine enum_associated_type assoc_patterns with
        | combined ->
            combined
        | exception _ ->
            failwith "Wrong field arrity for enum"
      in
      let env, bound_typed =
        List.fold_left_map
          (fun kosu_env (enum_type, enum_pattern) ->
            let fresh_ty = fresh_variable_type () in
            let bound, (env, ty) =
              typeof_pattern fresh_ty kosu_env enum_pattern
            in
            let env =
              KosuEnv.add_typing_constraint ~cexpected:fresh_ty ~cfound:ty
                enum_pattern env
            in
            let env =
              KosuEnv.add_typing_constraint ~cexpected:enum_type
                ~cfound:fresh_ty enum_pattern env
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, (bound, ty))
          )
          kosu_env assoc_types_exprs
      in
      let bound_variable, _ = List.split bound_typed in
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
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      (indentifiers, (kosu_env, enum_ty))
  | PRecord { module_resolver; struct_name; pfields } ->
      let module PIB = PatternIdentifierBound in
      let module_resolver, struct_decl =
        match
          KosuEnv.find_struct_declaration
            (module_resolver, struct_name)
            kosu_env
        with
        | Some decl ->
            decl
        | None ->
            failwith "No struct found"
      in
      let struct_decl, struct_ty =
        KosuUtil.Struct.substitution_fresh ~fresh:Ty.fresh_variable_type
          module_resolver struct_decl
      in
      let combined_fields =
        match List.combine struct_decl.fields pfields with
        | combined ->
            combined
        | exception _ ->
            failwith "Wrong field arrity"
      in
      let env, bound_variable =
        List.fold_left_map
          (fun kosu_env
               ((struct_name, struct_type), (pattern_name, pattern_pattern)) ->
            let () =
              match struct_name = pattern_name.value with
              | true ->
                  ()
              | false ->
                  failwith "Struct pattern init not matching name"
            in
            let fresh_ty = fresh_variable_type () in
            let bound, (env, ty) =
              typeof_pattern fresh_ty kosu_env pattern_pattern
            in
            let env =
              KosuEnv.add_typing_constraint ~cexpected:fresh_ty ~cfound:ty
                pattern_pattern env
            in
            let env =
              KosuEnv.add_typing_constraint ~cfound:fresh_ty
                ~cexpected:struct_type pattern_pattern env
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, bound)
          )
          kosu_env combined_fields
      in
      let indentifier_set =
        List.fold_left
          (fun set t_bounds_ty ->
            let () =
              Printf.printf "comming [%s]\n"
              @@ String.concat ", "
              @@ List.map (fun (s, _) -> s.value) t_bounds_ty
            in
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
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      (indentifiers, (kosu_env, struct_ty))
  | POr patterns ->
      let module PIB = PatternIdentifierBound in
      let fresh_ty = fresh_variable_type () in
      let env, bound_typed =
        List.fold_left_map
          (fun kosu_env pattern ->
            let bound, (env, ty) = typeof_pattern fresh_ty kosu_env pattern in
            let env =
              KosuEnv.add_typing_constraint ~cexpected:fresh_ty ~cfound:ty
                pattern env
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, (bound, ty))
          )
          kosu_env patterns
      in
      let bound_variable, _ = List.split bound_typed in

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
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cexpected:scrutinee_type ~cfound:fresh_ty
          pattern kosu_env
      in
      (indentifiers, (kosu_env, scrutinee_type))
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
        KosuEnv.add_typing_constraint ~cexpected:scrutinee_type ~cfound:ty
          pas_pattern env
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      (bound, (kosu_env, ty))

and free_variable_expression ~closure_env ~scope_env (expression : _ location) =
  let of_variable_info
      KosuEnv.KosuVariableInfo.{ identifier; kosu_type; is_const = _ } =
    (identifier, kosu_type)
  in
  (* capture should handle the resolve of identifier without module resolver *)
  let capture (identifier : _ location) =
    let in_clo_env = KosuEnv.assoc_type_opt identifier.value closure_env in
    let in_scope_env = KosuEnv.assoc_type_opt identifier.value scope_env in
    match (in_clo_env, in_scope_env) with
    | Some _, (Some _ | None) ->
        CapturedIdentifier.empty
    | None, Some scope_info ->
        CapturedIdentifier.singleton @@ of_variable_info scope_info
    | None, None ->
        failwith @@ "Undefine identifier " ^ identifier.value
  in
  match expression.value with
  | EIdentifier { module_resolver; id } ->
      let c =
        match KosuUtil.ModuleResolver.is_empty module_resolver with
        | true ->
            capture id
        | false ->
            (* Pointer toward a function symbol in a module so it's not captured*)
            CapturedIdentifier.empty
      in
      c
  | EEmpty
  | ETrue
  | EFalse
  | ENullptr
  | ECmpLess
  | ECmpEqual
  | ECmpGreater
  | EStringl _
  | EChar _
  | EInteger { integer_info = _; ivalue = _ }
  | EConstIdentifier { module_resolver = _; identifier = _ }
  | EFloat { fsize = _; fvalue = _ } ->
      CapturedIdentifier.empty
  | ESizeof either ->
      let captured =
        match either with
        | Either.Left _ ->
            CapturedIdentifier.empty
        | Either.Right expr ->
            free_variable_expression ~closure_env ~scope_env expr
      in
      captured
  | EFieldAccess { first_expr; field = _ }
  | ETupleAccess { first_expr; index = _ }
  | EDeref first_expr ->
      free_variable_expression ~closure_env ~scope_env first_expr
  | EArrayAccess { array_expr; index_expr } ->
      let set = free_variable_expression ~closure_env ~scope_env array_expr in
      let set2 = free_variable_expression ~closure_env ~scope_env index_expr in
      CapturedIdentifier.union set set2
  | EStruct { module_resolver = _; struct_name = _; fields } ->
      List.fold_left
        (fun set (_, expr) ->
          let s = free_variable_expression ~closure_env ~scope_env expr in
          CapturedIdentifier.union s set
        )
        CapturedIdentifier.empty fields
  | EEnum
      { assoc_exprs = exprs; module_resolver = _; enum_name = _; variant = _ }
  | ETuple exprs
  | EArray exprs
  | EBuiltinFunctionCall { parameters = exprs; fn_name = _ }
  | EFunctionCall
      {
        parameters = exprs;
        module_resolver = _;
        generics_resolver = _;
        fn_name = _;
      } ->
      List.fold_left
        (fun set expr ->
          let s = free_variable_expression ~closure_env ~scope_env expr in
          CapturedIdentifier.union s set
        )
        CapturedIdentifier.empty exprs
  | EBlock _ ->
      failwith ""
  | EAnonFunction { parameters; body; kind = _ } ->
      let () = ignore (parameters, body) in
      failwith "Anon function todo"
  | EWhile _ ->
      failwith "TODO WHILE"
  | ECases _ ->
      failwith "ECASES"
  | EMatch _ ->
      failwith ""

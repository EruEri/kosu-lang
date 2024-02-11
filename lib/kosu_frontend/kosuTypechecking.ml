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

let rec typeof ~tyfresh kosu_env
    (kosu_expression : _ KosuAst.kosu_expression location) =
  let ((_, ty) as ret), expression =
    typeof_expression ~tyfresh kosu_env
    @@ Position.map
         (fun { kosu_expression; _ } -> kosu_expression)
         kosu_expression
  in
  let kosu_expression =
    Position.map
      (fun _ -> { kosu_expression = expression; expression_associated = ty })
      kosu_expression
  in
  (ret, kosu_expression)

and typeof_expression ~tyfresh (kosu_env : KosuEnv.kosu_env)
    (expr : _ KosuAst.expression location) :
    (KosuEnv.kosu_env * Ty.kosu_type)
    * (Ty.kosu_type, Ty.kosu_type) KosuAst.expression =
  match expr.value with
  | EEmpty ->
      ((kosu_env, Ty.TyUnit), EEmpty)
  | (ETrue | EFalse) as expr ->
      ((kosu_env, TyBool), expr)
  | (ECmpLess | ECmpEqual | ECmpGreater) as expr ->
      ((kosu_env, TyOrdered), expr)
  | ENullptr { is_const } as expr ->
      let pointer_state =
        if is_const then
          Const
        else
          Mutable
      in
      let fresh_type = tyfresh () in
      ((kosu_env, TyPointer { pointee_type = fresh_type; pointer_state }), expr)
  | EStringl _ as expr ->
      ((kosu_env, TyStringLit), expr)
  | EChar _ as expr ->
      ((kosu_env, TyChar), expr)
  | EInteger { integer_info; ivalue = _ } as expresion_bound ->
      let default = Ty.TyInteger integer_info in
      let tuples =
        match integer_info with
        | Some _ ->
            (kosu_env, default)
        | None ->
            let t = tyfresh () in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cexpected:default ~cfound:t expr
                kosu_env
            in
            (kosu_env, t)
      in
      (tuples, expresion_bound)
  | EFloat { fsize; fvalue = _ } as expresion_bound ->
      let default = Ty.TyFloat fsize in
      let tuple =
        match fsize with
        | Some _ ->
            (kosu_env, default)
        | None ->
            let t = tyfresh () in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cexpected:default ~cfound:t expr
                kosu_env
            in
            (kosu_env, t)
      in
      (tuple, expresion_bound)
  | ESizeof either ->
      let kosu_env, ast =
        match either with
        | Either.Left kosu_type as self ->
            let _ = KosuUtil.Ty.of_tyloc' kosu_type in
            (kosu_env, self)
        | Either.Right rhs ->
            let (ext_env, _), ast = typeof ~tyfresh kosu_env rhs in
            let ast = Either.Right ast in
            let env = KosuEnv.merge_constraint ext_env kosu_env in
            (env, ast)
      in
      let ast = ESizeof ast in
      ((kosu_env, KosuUtil.(Ty.of_tyloc TyLoc.usize)), ast)
  | EFieldAccess { first_expr; field = sfield } ->
      (* instanciante todo*)
      let (env, ty_expr), ast = typeof ~tyfresh kosu_env first_expr in
      let ast = EFieldAccess { first_expr = ast; field = sfield } in
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
      ((kosu_env, ty), ast)
  | EArrayAccess { array_expr; index_expr } ->
      let (env, ty), ast_array_expr = typeof ~tyfresh kosu_env array_expr in
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
                  raise @@ cannot_infer_type array_expr.position
            in
            ty
        | _ ->
            raise @@ non_array_access array_expr.position
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let (env, ty), ast_index_expr = typeof ~tyfresh kosu_env index_expr in
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
      let ast =
        EArrayAccess
          { array_expr = ast_array_expr; index_expr = ast_index_expr }
      in
      ((kosu_env, ty_elt), ast)
  | ETupleAccess { first_expr; index } ->
      let (env, ty), ast_first_expr = typeof ~tyfresh kosu_env first_expr in
      let ast = ETupleAccess { first_expr = ast_first_expr; index } in
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
      ((kosu_env, ty), ast)
  | EConstIdentifier { module_resolver; identifier } as self ->
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
      ((kosu_env, ty), self)
  | EIdentifier { module_resolver; id } as self ->
      let t = KosuEnv.find_identifier module_resolver id.value kosu_env in
      let t =
        match t with
        | Some t ->
            t
        | None ->
            raise @@ KosuError.Exn.unbound_identifier id
      in
      ((kosu_env, t), self)
  | EStruct { module_resolver = module_resolver_loc; struct_name; fields } ->
      let module_resolver, struct_decl =
        match
          KosuEnv.find_struct_declaration
            (module_resolver_loc, struct_name)
            kosu_env
        with
        | Some decl ->
            decl
        | None ->
            raise @@ unbound_struct module_resolver_loc struct_name
      in
      let struct_decl, ty =
        KosuUtil.Struct.substitution_fresh ~fresh:tyfresh module_resolver
          struct_decl
      in
      let combined_fields =
        match List.combine struct_decl.fields fields with
        | combined ->
            combined
        | exception _ ->
            let expected = List.length struct_decl.fields in
            let found = List.length fields in
            raise @@ struct_wrong_arity struct_name expected found
      in

      let kosu_env, ast_fields =
        List.fold_left_map
          (fun kosu_env ((struct_name, struct_type), (expr_name, expr_expr)) ->
            let () =
              match struct_name = expr_name.value with
              | true ->
                  ()
              | false ->
                  raise @@ struct_init_wrong_field struct_name expr_name
            in
            let (env, ty), ast_expr = typeof ~tyfresh kosu_env expr_expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:struct_type
                expr_expr kosu_env
            in
            (kosu_env, (expr_name, ast_expr))
          )
          kosu_env combined_fields
      in
      let ast =
        EStruct
          {
            module_resolver = module_resolver_loc;
            struct_name;
            fields = ast_fields;
          }
      in
      ((kosu_env, ty), ast)
  | EEnum { module_resolver = re; enum_name; variant; assoc_exprs } ->
      let module_resolver, enum_decl =
        match
          KosuEnv.find_enum_declaration re
            (Option.map Position.value enum_name)
            variant kosu_env
        with
        | Some t ->
            t
        | None ->
            raise @@ unbound_enum re enum_name variant
      in
      let enum_decl, ty =
        KosuUtil.Enum.substitution_fresh ~fresh:tyfresh module_resolver
          enum_decl
      in
      let enum_associated_type =
        match KosuUtil.Enum.assoc_types variant.value enum_decl with
        | Some assoc_types ->
            assoc_types
        | None ->
            raise @@ unbound_enum re enum_name variant
      in
      let assoc_types_exprs =
        match List.combine enum_associated_type assoc_exprs with
        | combined ->
            combined
        | exception _ ->
            let expected = List.length enum_associated_type in
            let found = List.length assoc_exprs in
            raise @@ enum_variant_wrong_arity variant expected found
      in
      let kosu_env, ast_assoc_exprs =
        List.fold_left_map
          (fun kosu_env (enum_type, enum_expr) ->
            let (env, ty), ast_expr = typeof ~tyfresh kosu_env enum_expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:enum_type
                enum_expr kosu_env
            in
            (kosu_env, ast_expr)
          )
          kosu_env assoc_types_exprs
      in
      let ast =
        EEnum
          {
            module_resolver = re;
            enum_name;
            variant;
            assoc_exprs = ast_assoc_exprs;
          }
      in
      ((kosu_env, ty), ast)
  | EBlock block ->
      let ret, ast_block = typeof_block ~tyfresh kosu_env block in
      let ast = EBlock ast_block in
      (ret, ast)
  | EDeref expr ->
      let (kosu_env, ty), ast_expr = typeof ~tyfresh kosu_env expr in
      let ast = EDeref ast_expr in
      let try_find_ty = function
        | Ty.TyPointer { pointee_type; pointer_state = _ } ->
            Option.some @@ Either.left pointee_type
        | Ty.TyPolymorphic p ->
            Option.some @@ Either.right p
        | ty ->
            raise @@ deref_non_pointer @@ Position.map (fun _ -> ty) expr
      in
      let ty_solve =
        match KosuEnv.find_or_try_solve try_find_ty ty kosu_env with
        | Some t ->
            t
        | None ->
            raise @@ cannot_infer_type expr.position
      in
      ((kosu_env, ty_solve), ast)
  | ETuple exprs ->
      let kosu_env, types =
        List.fold_left_map
          (fun kosu_env expr ->
            let (env, ty), ast_expr = typeof ~tyfresh kosu_env expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, (ty, ast_expr))
          )
          kosu_env exprs
      in
      let types, ast_exprs = List.split types in
      let ast = ETuple ast_exprs in
      let ty = Ty.TyTuple types in
      ((kosu_env, ty), ast)
  | EArray exprs ->
      let fresh_variable = tyfresh () in
      let length = List.length exprs in
      let kosu_env, ast_exprs =
        List.fold_left_map
          (fun kosu_env expr ->
            let (env, ty), ast_expr = typeof ~tyfresh kosu_env expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cexpected:fresh_variable ~cfound:ty
                expr kosu_env
            in
            (kosu_env, ast_expr)
          )
          kosu_env exprs
      in
      let ast = EArray ast_exprs in
      let ty =
        Ty.TyArray { size = Int64.of_int length; ktype = fresh_variable }
      in
      ((kosu_env, ty), ast)
  | EBuiltinFunctionCall { fn_name; parameters } ->
      let builtin_function =
        Position.map_use
          (fun fn_name ->
            match KosuUtil.Builtin.of_string_opt fn_name.value with
            | Some f ->
                f
            | None ->
                raise @@ unbound_builtin_function fn_name
          )
          fn_name
      in
      let expected_arity = KosuUtil.Builtin.arity builtin_function.value in
      let args_count = List.length parameters in
      let () =
        if expected_arity <> args_count then
          raise
          @@ KosuError.Exn.callable_wrong_arity fn_name expected_arity
               args_count
      in
      let ret, ast_exprs =
        typeof_builin_functions ~tyfresh kosu_env builtin_function parameters
      in
      let ast = EBuiltinFunctionCall { fn_name; parameters = ast_exprs } in
      (ret, ast)
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
        | (TyFunctionPtr schema | TyClosure schema) as ty ->
            (schema, ty)
        | TyPolymorphic (PolymorphicVar _ | CompilerPolymorphicVar _) ->
            raise @@ cannot_infer_type fn_name.position
        | ( TyOrdered
          | TyStringLit
          | TyChar
          | TyBool
          | TyUnit
          | TyIdentifier { module_resolver = ModuleResolver_ _; _ }
          | TyPointer _
          | TyInteger _
          | TyFloat _
          | TyArray _
          | TyTuple _
          | TyOpaque { module_resolver = ModuleResolver_ _; _ } ) as ty ->
            raise @@ not_callable_type @@ Position.map (fun _ -> ty) fn_name
      in

      let () =
        match Util.Ulist.are_same_length schema.parameters_type parameters with
        | true ->
            ()
        | false ->
            let expected = List.length schema.parameters_type in
            let found = List.length parameters in
            raise @@ callable_wrong_arity fn_name expected found
      in

      let fresh _ = tyfresh () in

      let fresh_variables = List.map fresh schema.poly_vars in
      let assoc_poly_fresh = List.combine schema.poly_vars fresh_variables in
      let kosu_env =
        match generics_resolver with
        | None ->
            kosu_env
        | Some tyes ->
            let () =
              match Util.Ulist.are_same_length tyes schema.poly_vars with
              | false ->
                  let expected = List.length schema.poly_vars in
                  let found = List.length tyes in
                  raise @@ generics_resolver_wrong_arity fn_name expected found
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
      let kosu_env, ast_exprs =
        List.fold_left_map
          (fun kosu_env (sig_ty, expr) ->
            let (env, ty), ast_expr = typeof ~tyfresh kosu_env expr in
            let ty = KosuUtil.Ty.ty_instanciate ~fresh ty in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            ( KosuEnv.add_typing_constraint ~cexpected:sig_ty ~cfound:ty expr
                kosu_env,
              ast_expr
            )
          )
          kosu_env parameters
      in
      let ast =
        EFunctionCall
          {
            module_resolver;
            generics_resolver;
            fn_name;
            parameters = ast_exprs;
          }
      in
      ((kosu_env, schema.return_type), ast)
  | EWhile { condition_expr; body } ->
      let (env, ty), ast_condition_expr =
        typeof ~tyfresh kosu_env condition_expr
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:TyBool
          condition_expr kosu_env
      in
      let (env, body_ty), ast_body = typeof_block ~tyfresh kosu_env body in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cfound:body_ty ~cexpected:TyUnit
          body.kosu_expr kosu_env
      in
      let ast =
        EWhile { condition_expr = ast_condition_expr; body = ast_body }
      in
      ((kosu_env, TyUnit), ast)
  | ECases { cases; else_body } ->
      let fresh_type = tyfresh () in
      let (env, ty), ast_else_block =
        typeof_block ~tyfresh kosu_env else_body
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cexpected:fresh_type ~cfound:ty
          else_body.kosu_expr kosu_env
      in
      let kosu_env, ast_cases =
        List.fold_left_map
          (fun kosu_env (condition_expr, body) ->
            let (env, ty), ast_condition_expr =
              typeof ~tyfresh kosu_env condition_expr
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:TyBool
                condition_expr kosu_env
            in
            let (env, body_ty), ast_body =
              typeof_block ~tyfresh kosu_env body
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:body_ty
                ~cexpected:fresh_type body.kosu_expr kosu_env
            in
            let ast = (ast_condition_expr, ast_body) in
            (kosu_env, ast)
          )
          kosu_env cases
      in
      let ast = ECases { cases = ast_cases; else_body = ast_else_block } in
      ((kosu_env, fresh_type), ast)
  | EMatch { expression; patterns } ->
      let (env, scrutinee_type), ast_expression =
        typeof ~tyfresh kosu_env expression
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let fresh_variable_ty = tyfresh () in
      let kosu_env, ast_patterns =
        List.fold_left_map
          (fun kosu_env (kosu_pattern, kosu_block) ->
            let (bound, (env, pattern_ty)), ast_pattern =
              typeof_kosu_pattern ~tyfresh scrutinee_type kosu_env kosu_pattern
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
            let (block_env, block_ty), ast_block =
              typeof_block ~tyfresh kosu_block_env kosu_block
            in
            let block_env =
              KosuEnv.add_typing_constraint ~cexpected:fresh_variable_ty
                ~cfound:block_ty kosu_block.kosu_expr block_env
            in
            let kosu_env = KosuEnv.merge_constraint block_env kosu_env in
            (kosu_env, (ast_pattern, ast_block))
          )
          kosu_env patterns
      in
      let ast =
        EMatch { expression = ast_expression; patterns = ast_patterns }
      in
      ((kosu_env, fresh_variable_ty), ast)
  | EAnonFunction { kind; parameters; body } ->
      let variables, ty_variables =
        List.split
        @@ List.map
             (fun { aname = identifier; akosu_type; ais_var = is_var } ->
               let kosu_type =
                 match akosu_type with
                 | Some t ->
                     KosuUtil.Ty.of_tyloc' t
                 | None ->
                     tyfresh ()
               in
               ((identifier, not is_var, kosu_type), kosu_type)
             )
             parameters
      in
      let ast_parameters =
        List.map
          (fun (name, is_const, kosu_type) ->
            { aname = name; ais_var = not is_const; akosu_type = kosu_type }
          )
          variables
      in
      let closure_env = KosuEnv.rebind_env_variables variables kosu_env in
      let clo_variables =
        variables_expression ~tyfresh closure_env CapturedIdentifier.empty body
      in
      let clo_free_variables =
        CapturedIdentifier.filter
          (fun (identifier, _) ->
            List.exists
              (fun (clo_arg, _, _) -> clo_arg.value <> identifier.value)
              variables
          )
          clo_variables
      in

      let closure_kosu_env = KosuEnv.rebind_env_variables variables kosu_env in
      let (clo_env, ty_clo_ret), ast_body =
        typeof ~tyfresh closure_kosu_env body
      in
      let ast =
        EAnonFunction { kind; parameters = ast_parameters; body = ast_body }
      in

      let closure_solution = KosuEnv.solve closure_kosu_env in

      let kosu_env = KosuEnv.merge_constraint clo_env kosu_env in

      let constraints = KosuEnv.equations kosu_env in

      let closure_scheama =
        KosuTypeConstraint.to_schema constraints closure_solution ty_variables
          ty_clo_ret
      in

      let () = print_endline "Captured variable [START]" in
      let () =
        CapturedIdentifier.iter
          (fun (name, ty) ->
            Printf.printf "name = %s, ty: %s\n" name.value
            @@ KosuPrint.string_of_kosu_type ty
          )
          clo_free_variables
      in
      let () = print_endline "Captured variable [END]" in

      let ty =
        match kind with
        | KAClosure ->
            Ty.TyClosure closure_scheama
        | KAFunctionPointer
          when not @@ CapturedIdentifier.is_empty clo_free_variables ->
            let variables =
              clo_free_variables |> CapturedIdentifier.elements |> List.map fst
            in
            raise @@ KosuError.Exn.captured_variables_for_fnptr variables
        | KAFunctionPointer ->
            Ty.TyFunctionPtr closure_scheama
      in
      ((kosu_env, ty), ast)

and typeof_block ~tyfresh kosu_env block =
  let kosu_env, kosu_stmts =
    List.fold_left_map (typeof_statement ~tyfresh) kosu_env block.kosu_stmts
  in
  let ret, ast_expr = typeof ~tyfresh kosu_env block.kosu_expr in
  let block = { kosu_stmts; kosu_expr = ast_expr } in
  (ret, block)

and typeof_statement ~tyfresh kosu_env statement =
  let env, stmt = typeof_statement' ~tyfresh kosu_env statement.value in
  let stmt = Position.map (fun _ -> stmt) statement in
  (env, stmt)

and typeof_statement' ~tyfresh kosu_env (statement : _ KosuAst.kosu_statement) =
  match statement with
  | SDeclaration { is_const; pattern; explicit_type; expression } ->
      let (env, ety), ast_expression = typeof ~tyfresh kosu_env expression in
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
      let (bounds, (env, ty)), ast_pattern =
        typeof_kosu_pattern ~tyfresh ety kosu_env pattern
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:ety pattern kosu_env
      in
      let kosu_env =
        List.fold_left
          (fun kosu_env (id, ty) -> KosuEnv.add_variable is_const id ty kosu_env)
          kosu_env bounds
      in
      let ast =
        SDeclaration
          {
            is_const;
            pattern = ast_pattern;
            explicit_type;
            expression = ast_expression;
          }
      in
      (kosu_env, ast)
  | SAffection { is_deref; lvalue; expression } ->
      let find_identifier_type declaration_position reassign_position t env =
        match t with
        | Ty.TyIdentifier _ as t ->
            t
        | TyPolymorphic p ->
            let ty =
              match KosuEnv.try_solve p env with
              | Some (TyIdentifier _ as t) ->
                  t
              | Some (TyPolymorphic _) | None ->
                  raise @@ cannot_infer_type declaration_position.position
              | Some ty ->
                  raise
                  @@ reassign_no_struct_type_field declaration_position ty
                       reassign_position
            in
            ty
        | ty ->
            raise
            @@ reassign_no_struct_type_field declaration_position ty
                 reassign_position
      in
      let (env, ty), ast_expression = typeof ~tyfresh kosu_env expression in
      let ast = SAffection { is_deref; lvalue; expression = ast_expression } in
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
            raise @@ constant_reasign variable_info.identifier variable
        | false | true ->
            ()
      in
      let kosu_env =
        match fields with
        | [] ->
            let expected_type =
              let t = tyfresh () in
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
                    find_identifier_type variable_info.identifier variable
                      variable_info.kosu_type kosu_env
                  in
                  struct_type
              | true ->
                  let try_expect_mutable_pointer position kosu_type =
                    match kosu_type with
                    | Ty.TyPointer { pointer_state = Mutable; pointee_type } ->
                        Option.some @@ Either.left pointee_type
                    | Ty.TyPolymorphic p ->
                        Option.some @@ Either.right p
                    | Ty.TyPointer { pointer_state = Const; pointee_type = _ }
                      as ty ->
                        let located = Position.create position ty in
                        raise @@ expected_pointer (Some Mutable) located
                    | ty ->
                        let located = Position.create position ty in
                        raise @@ expected_pointer (Some Mutable) located
                  in
                  let base_ty =
                    match
                      KosuEnv.find_or_try_solve
                        (try_expect_mutable_pointer
                           variable_info.identifier.position
                        )
                        variable_info.kosu_type kosu_env
                    with
                    | Some t ->
                        t
                    | None ->
                        raise
                        @@ cannot_infer_type variable_info.identifier.position
                  in
                  find_identifier_type variable_info.identifier variable base_ty
                    kosu_env
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
      (kosu_env, ast)
  | SDiscard expression ->
      let (env, _), ast_expression = typeof ~tyfresh kosu_env expression in
      let ast = SDiscard ast_expression in
      let env = KosuEnv.merge_constraint env kosu_env in
      (env, ast)
  | SOpen { module_resolver } as self ->
      let kosu_module =
        match KosuEnv.find_module_opt module_resolver kosu_env with
        | Some kosu_module ->
            kosu_module
        | None ->
            raise @@ unbound_module module_resolver
      in
      let env = KosuEnv.add_module kosu_module kosu_env in
      (env, self)

and typeof_kosu_pattern ~tyfresh scrutine_type kosu_env kosu_pattern =
  let ((_, (_, ty)) as ret), pattern =
    typeof_pattern ~tyfresh scrutine_type kosu_env
    @@ Position.map (fun { kosu_pattern; _ } -> kosu_pattern) kosu_pattern
  in
  let kosu_pattern =
    Position.map
      (fun _ -> { kosu_pattern = pattern; pattern_associated = ty })
      kosu_pattern
  in
  (ret, kosu_pattern)

(**
    [typeof_pattern scrutinee_type kosu_env pattern] types the pattern [pattern] in the environment [kosu_env]
    with the scrutinee_type being [scrutinee_type]
*)
and typeof_pattern ~tyfresh scrutinee_type kosu_env
    (pattern : _ KosuAst.pattern location) =
  (*Dont forget to raise if we try to bind an identifier to an existing variable in the*)
  let open KosuType.Ty in
  match pattern.value with
  | PEmpty as self ->
      let ty = Ty.TyUnit in
      (([], (kosu_env, ty)), self)
  | (PTrue | PFalse) as ast ->
      let ty = Ty.TyBool in
      (([], (kosu_env, ty)), ast)
  | (PCmpLess | PCmpEqual | PCmpGreater) as ast ->
      let ty = TyOrdered in
      (([], (kosu_env, ty)), ast)
  | PNullptr as self ->
      let fresh_ty = tyfresh () in
      let ty = TyPointer { pointer_state = Const; pointee_type = fresh_ty } in
      (([], (kosu_env, ty)), self)
  | PWildcard as ast ->
      (([], (kosu_env, scrutinee_type)), ast)
  | PFloat _ as ast ->
      let ty = TyFloat None in
      (([], (kosu_env, ty)), ast)
  | PChar _ as ast ->
      let ty = TyChar in
      (([], (kosu_env, ty)), ast)
  | PInteger { value = _ } as ast ->
      let ty = TyInteger None in
      (([], (kosu_env, ty)), ast)
  | PIdentifier identifier as ast ->
      let bound = (identifier, scrutinee_type) in
      (([ bound ], (kosu_env, scrutinee_type)), ast)
  | PTuple patterns ->
      let module PIB = PatternIdentifierBound in
      let env, bound_typed =
        List.fold_left_map
          (fun kosu_env pattern ->
            let fresh_ty = tyfresh () in
            let (bound, (env, ty)), ast_pattern =
              typeof_kosu_pattern ~tyfresh fresh_ty kosu_env pattern
            in
            let env =
              KosuEnv.add_typing_constraint ~cexpected:fresh_ty ~cfound:ty
                pattern env
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, ((bound, ty), ast_pattern))
          )
          kosu_env patterns
      in
      let bound_typed, ast_patterns = List.split bound_typed in
      let bound_variable, tuples_elt_ty = List.split bound_typed in

      let ast = PTuple ast_patterns in

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
      ((indentifiers, (kosu_env, ty_pattern)), ast)
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
            raise @@ unbound_enum module_resolver enum_name variant
      in
      let enum_decl, enum_ty =
        KosuUtil.Enum.substitution_fresh ~fresh:tyfresh module_loc enum_decl
      in
      let enum_associated_type =
        match KosuUtil.Enum.assoc_types variant.value enum_decl with
        | Some assoc_types ->
            assoc_types
        | None ->
            raise @@ unbound_enum module_resolver enum_name variant
      in
      let assoc_types_exprs =
        match List.combine enum_associated_type assoc_patterns with
        | combined ->
            combined
        | exception _ ->
            let expected = List.length enum_associated_type in
            let found = List.length assoc_patterns in
            raise @@ enum_variant_wrong_arity variant expected found
      in
      let env, bound_typed =
        List.fold_left_map
          (fun kosu_env (enum_type, enum_pattern) ->
            let fresh_ty = tyfresh () in
            let (bound, (env, ty)), ast_pattern =
              typeof_kosu_pattern ~tyfresh fresh_ty kosu_env enum_pattern
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
            (kosu_env, (bound, ast_pattern))
          )
          kosu_env assoc_types_exprs
      in
      let bound_variable, ast_patterns = List.split bound_typed in
      let ast =
        PCase
          { module_resolver; enum_name; variant; assoc_patterns = ast_patterns }
      in
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
      ((indentifiers, (kosu_env, enum_ty)), ast)
  | PRecord { module_resolver = re; struct_name; pfields } ->
      let module PIB = PatternIdentifierBound in
      let module_resolver, struct_decl =
        match KosuEnv.find_struct_declaration (re, struct_name) kosu_env with
        | Some decl ->
            decl
        | None ->
            raise @@ unbound_struct re struct_name
      in
      let struct_decl, struct_ty =
        KosuUtil.Struct.substitution_fresh ~fresh:tyfresh module_resolver
          struct_decl
      in
      let combined_fields =
        match List.combine struct_decl.fields pfields with
        | combined ->
            combined
        | exception _ ->
            let expected = List.length struct_decl.fields in
            let found = List.length pfields in
            raise @@ struct_wrong_arity struct_name expected found
      in
      let env, bound_variables =
        List.fold_left_map
          (fun kosu_env
               ((struct_name, struct_type), (pattern_name, pattern_pattern)) ->
            let () =
              match struct_name = pattern_name.value with
              | true ->
                  ()
              | false ->
                  raise @@ struct_init_wrong_field struct_name pattern_name
            in
            let fresh_ty = tyfresh () in
            let (bound, (env, ty)), ast_pattern =
              typeof_kosu_pattern ~tyfresh fresh_ty kosu_env pattern_pattern
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
            (kosu_env, (bound, (pattern_name, ast_pattern)))
          )
          kosu_env combined_fields
      in
      let bound_variables, ast_patterns = List.split bound_variables in
      let ast =
        PRecord { module_resolver = re; struct_name; pfields = ast_patterns }
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
          PIB.empty bound_variables
      in
      let indentifiers = PIB.elements indentifier_set in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      ((indentifiers, (kosu_env, struct_ty)), ast)
  | POr patterns ->
      let module PIB = PatternIdentifierBound in
      let fresh_ty = tyfresh () in
      let env, bound_patterned =
        List.fold_left_map
          (fun kosu_env pattern ->
            let (bound, (env, ty)), ast_pattern =
              typeof_kosu_pattern ~tyfresh fresh_ty kosu_env pattern
            in
            let env =
              KosuEnv.add_typing_constraint ~cexpected:fresh_ty ~cfound:ty
                pattern env
            in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, (bound, ast_pattern))
          )
          kosu_env patterns
      in
      let bound_variables, ast_patterns = List.split bound_patterned in
      let ast = POr ast_patterns in
      let x_bounds, xs_bounds =
        match bound_variables with
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
      ((indentifiers, (kosu_env, scrutinee_type)), ast)
  | PAs { pas_pattern; pas_bound } ->
      let (bound, (env, ty)), ast_pattern =
        typeof_kosu_pattern ~tyfresh scrutinee_type kosu_env pas_pattern
      in
      let ast = PAs { pas_pattern = ast_pattern; pas_bound } in
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
      ((bound, (kosu_env, ty)), ast)

and typeof_builin_functions_args ~tyfresh kosu_env builtin expected args return
    =
  let fn_name = Position.map KosuUtil.Builtin.to_string builtin in
  let arity = KosuUtil.Builtin.arity builtin.value in
  let args =
    match List.combine expected args with
    | e ->
        e
    | exception _ ->
        raise
        @@ KosuError.Exn.callable_wrong_arity fn_name arity (List.length args)
  in
  let kosu_env, targs =
    List.fold_left_map
      (fun kosu_env (expected, expr) ->
        let (env, ty), ast_expr = typeof ~tyfresh kosu_env expr in
        let kosu_env = KosuEnv.merge_constraint env kosu_env in
        let cexpected =
          Position.map_use (fun expr -> expected expr.position ty) expr
        in
        let kosu_env =
          KosuEnv.add_typing_constraint ~cexpected:cexpected.value ~cfound:ty
            expr kosu_env
        in
        (kosu_env, (cexpected, ast_expr))
      )
      kosu_env args
  in
  let targs, ast_expr = List.split targs in
  let return_type = return targs in

  ((kosu_env, return_type), ast_expr)

and typeof_builin_functions ~tyfresh kosu_env builtin args =
  let farray_solve position = function
    | Ty.TyArray { ktype; size } ->
        Option.some @@ Either.left @@ (size, ktype)
    | TyPolymorphic e ->
        Option.some @@ Either.right e
    | ty ->
        raise @@ expected_array @@ Position.create position ty
  in
  let expected, return =
    match builtin.value with
    | StringLen ->
        let expected0 _ _ = KosuUtil.Ty.stringl in
        let expected = [ expected0 ] in
        let return_type _ = KosuUtil.Ty.usize in
        (expected, return_type)
    | StringlPtr ->
        let expected0 _ _ = KosuUtil.Ty.stringl in
        let expected = [ expected0 ] in
        let return_type _ = KosuUtil.Ty.(ptr_const s8) in
        (expected, return_type)
    | ArrayPtr ->
        let expected0 position ty =
          match
            KosuEnv.find_or_try_solve (farray_solve position) ty kosu_env
          with
          | Some _ ->
              ty
          | None ->
              raise @@ cannot_infer_type position
        in
        let expect = [ expected0 ] in
        let return_type args =
          let { value = ty; position } = List.hd args in
          match
            KosuEnv.find_or_try_solve (farray_solve position) ty kosu_env
          with
          | Some (_, ktype) ->
              KosuUtil.Ty.ptr_mut ktype
          | None ->
              raise @@ cannot_infer_type position
        in
        (expect, return_type)
    | ArrayLen ->
        let expected0 position ty =
          match
            KosuEnv.find_or_try_solve (farray_solve position) ty kosu_env
          with
          | Some _ ->
              ty
          | None ->
              raise @@ cannot_infer_type position
        in
        let expect = [ expected0 ] in
        (* match KosuEnv.find_or_try_solve (farray_solve ty.position) ty.value kosu_env with *)
        let return_type _ = KosuUtil.Ty.usize in
        (expect, return_type)
    | Exit ->
        let expected0 _ _ = KosuUtil.Ty.s32 in
        let expected = [ expected0 ] in
        let return_type _ = tyfresh () in
        (expected, return_type)
    | Alloc { const } ->
        let pointer_state =
          if const then
            Const
          else
            Mutable
        in
        let expected0 _ = Fun.id in
        let expected = [ expected0 ] in
        let return_type args =
          let pointee_type = List.hd args in
          Ty.TyPointer { pointer_state; pointee_type = pointee_type.value }
        in
        (expected, return_type)
    | Ralloc ->
        let expected0 position = function
          | Ty.TyPointer _ as ty ->
              ty
          | ty ->
              raise @@ expected_pointer None (Position.create position ty)
        in
        let expected1 _ _ = KosuUtil.Ty.usize in
        let expected = [ expected0; expected1 ] in
        let return_type args =
          let ty = List.hd args in
          match ty.value with
          | Ty.TyPointer { pointer_state = _; pointee_type } ->
              KosuUtil.Ty.ptr_mut pointee_type
          | _ ->
              raise @@ expected_pointer None ty
        in
        (expected, return_type)
  in
  typeof_builin_functions_args ~tyfresh kosu_env builtin expected args return

(**
  [variables_expression closure_env expression] returns all the variables used in [expression]
*)
and variables_expression ~tyfresh closure_env locals_variables
    (expression : _ location) =
  let of_variable_info identifier
      KosuEnv.{ identifier = _; kosu_type; is_const = _ } =
    (identifier, kosu_type)
  in
  (* Need to keep track of local variables, otherwise local variable of a anon function block *)
  (* Would be considered as captured variables *)
  (* capture should handle the resolve of identifier without module resolver *)
  let capture (identifier : _ location) =
    let is_local =
      CapturedIdentifier.exists
        (fun (id, _) -> id.value = identifier.value)
        locals_variables
    in
    if is_local then
      CapturedIdentifier.empty
    else
      let in_clo_env = KosuEnv.assoc_type_opt identifier.value closure_env in
      match in_clo_env with
      | Some scope_info ->
          CapturedIdentifier.singleton @@ of_variable_info identifier scope_info
      | None ->
          raise @@ KosuError.Exn.unbound_identifier identifier
  in
  match expression.value.kosu_expression with
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
  | ENullptr _
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
            variables_expression ~tyfresh closure_env locals_variables expr
      in
      captured
  | EFieldAccess { first_expr; field = _ }
  | ETupleAccess { first_expr; index = _ }
  | EDeref first_expr ->
      variables_expression ~tyfresh closure_env locals_variables first_expr
  | EArrayAccess { array_expr; index_expr } ->
      let set =
        variables_expression ~tyfresh closure_env locals_variables array_expr
      in
      let set2 =
        variables_expression ~tyfresh closure_env locals_variables index_expr
      in
      CapturedIdentifier.union set set2
  | EStruct { module_resolver = _; struct_name = _; fields } ->
      List.fold_left
        (fun set (_, expr) ->
          let s =
            variables_expression ~tyfresh closure_env locals_variables expr
          in
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
          let s =
            variables_expression ~tyfresh closure_env locals_variables expr
          in
          CapturedIdentifier.union s set
        )
        CapturedIdentifier.empty exprs
  | EBlock kosu_block ->
      variables_block ~tyfresh ~captured:CapturedIdentifier.empty closure_env
        locals_variables kosu_block
  | EAnonFunction { parameters; body; kind = _ } ->
      let variables, _ =
        List.split
        @@ List.map
             (fun { aname = identifier; akosu_type; ais_var = is_var } ->
               let kosu_type =
                 match akosu_type with
                 | Some t ->
                     KosuUtil.Ty.of_tyloc' t
                 | None ->
                     tyfresh ()
               in
               ((identifier, not is_var, kosu_type), kosu_type)
             )
             parameters
      in
      let closure_env = KosuEnv.rebind_env_variables variables closure_env in
      let clo_variables =
        variables_expression ~tyfresh closure_env CapturedIdentifier.empty body
      in
      let clo_free_variables =
        CapturedIdentifier.filter
          (fun (identifier, _) ->
            List.exists
              (fun (clo_arg, _, _) -> clo_arg.value <> identifier.value)
              variables
          )
          clo_variables
      in
      clo_free_variables
  | EWhile { condition_expr; body } ->
      let captured_expr =
        variables_expression ~tyfresh closure_env locals_variables
          condition_expr
      in
      let captured_body =
        variables_block ~tyfresh ~captured:CapturedIdentifier.empty closure_env
          locals_variables body
      in
      CapturedIdentifier.union captured_expr captured_body
  | ECases { cases; else_body } ->
      let captured =
        List.fold_left
          (fun captured (expr, body) ->
            let captured_expr =
              variables_expression ~tyfresh closure_env locals_variables expr
            in
            let captured_body =
              variables_block ~tyfresh ~captured:CapturedIdentifier.empty
                closure_env locals_variables body
            in
            let s = CapturedIdentifier.union captured_expr captured_body in
            CapturedIdentifier.union captured s
          )
          CapturedIdentifier.empty cases
      in
      variables_block ~tyfresh ~captured closure_env locals_variables else_body
  | EMatch { expression; patterns } ->
      let variables_exprs =
        variables_expression ~tyfresh closure_env locals_variables expression
      in
      List.fold_left
        (fun captured (pattern, block) ->
          let scrutine = tyfresh () in
          let (bound, (_, _)), _ =
            typeof_kosu_pattern ~tyfresh scrutine closure_env pattern
          in
          let block_locals = CapturedIdentifier.of_list bound in
          let block_locals =
            CapturedIdentifier.union locals_variables block_locals
          in
          let variables =
            List.map
              (fun (id, ty) ->
                let is_const = true in
                (id, is_const, ty)
              )
              bound
          in
          let closure_env =
            KosuEnv.rebind_env_variables variables closure_env
          in
          let captured_block =
            variables_block ~tyfresh ~captured closure_env block_locals block
          in
          captured_block
        )
        variables_exprs patterns

and variables_block ~tyfresh ~captured closure_env locals_variables block =
  let { kosu_stmts; kosu_expr } = block in
  let closure_env, captured, locals_variables =
    List.fold_left
      (fun (env, set, locals) stmt ->
        let env, nset, locals = variables_statement ~tyfresh env locals stmt in
        let set = CapturedIdentifier.union set nset in
        (env, set, locals)
      )
      (closure_env, captured, locals_variables)
      kosu_stmts
  in
  let captured_expr =
    variables_expression ~tyfresh closure_env locals_variables kosu_expr
  in
  let captured = CapturedIdentifier.union captured captured_expr in
  captured

and variables_statement ~tyfresh closure_env locals_variables statement =
  match statement.value with
  | SDeclaration { is_const; pattern; explicit_type; expression } ->
      let frees =
        variables_expression ~tyfresh closure_env locals_variables expression
      in
      let ty =
        Option.value ~default:(tyfresh ())
        @@ Option.map KosuUtil.Ty.of_tyloc' explicit_type
      in
      (* Maybe merge closure_env and scope_env *)
      let (pvariables, _), _ =
        typeof_kosu_pattern ~tyfresh ty closure_env pattern
      in
      let extend_locals = CapturedIdentifier.of_list pvariables in
      let locals_variables =
        CapturedIdentifier.union locals_variables extend_locals
      in
      let closure_env =
        List.fold_left
          (fun kosu_env (id, ty) -> KosuEnv.add_variable is_const id ty kosu_env)
          closure_env pvariables
      in
      (closure_env, frees, locals_variables)
  | SDiscard expression | SAffection { expression; _ } ->
      let captured =
        variables_expression ~tyfresh closure_env locals_variables expression
      in
      (closure_env, captured, locals_variables)
  | SOpen { module_resolver } ->
      let kosu_module =
        match KosuEnv.find_module_opt module_resolver closure_env with
        | Some kosu_module ->
            kosu_module
        | None ->
            raise @@ unbound_module module_resolver
      in
      let closure_env = KosuEnv.add_module kosu_module closure_env in
      (closure_env, CapturedIdentifier.empty, locals_variables)

let typeof (kosu_env : KosuEnv.kosu_env)
    (expr : _ KosuAst.kosu_expression location) =
  let counter = ref 0 in
  let fresh () =
    let n = !counter in
    let () = incr counter in
    let s = Printf.sprintf "'t%u" n in
    Ty.TyPolymorphic (CompilerPolymorphicVar s)
  in
  typeof ~tyfresh:fresh kosu_env expr

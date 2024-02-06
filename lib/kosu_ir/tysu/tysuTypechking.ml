(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
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

open Kosu.Ast
open Kosu.Type
open Kosu.Error.Exn
open Util.Position

module PatternIdentifierBound = Set.Make (struct
  type t = string location * Kosu.Type.Ty.kosu_type

  let compare (lhs : t) (rhs : t) =
    String.compare (value @@ fst lhs) (value @@ fst rhs)
end)

module CapturedIdentifier = PatternIdentifierBound

let error file line message =
  failwith @@ Printf.sprintf "File : %s, Line : %u: %s" file line message

let of_kosu_type_solved = KosuTysuBase.Tysu.of_kosu_type_solved

(* this function should do the same number of call to tyfresh than [KosuType] otherwise the compiler var name won't match *)
let rec typeof ~tyfresh solutions (kosu_env : Kosu.Env.kosu_env)
    (expr : Kosu.Ast.kosu_expression location) =
  match expr.value with
  | EEmpty ->
      let koty = Ty.TyUnit in
      let tyty = KosuTysuBase.Tysu.of_kosu_type koty in
      let tysu = TysuUtil.Type.typed TysuAst.EEmpty tyty in
      tysu
  | ETrue ->
      let kosu_type = Ty.TyBool in
      let tysu_type = KosuTysuBase.Tysu.of_kosu_type kosu_type in
      let tysu = TysuUtil.Type.typed TysuAst.ETrue tysu_type in
      tysu
  | EFalse ->
      let kosu_type = Ty.TyBool in
      let tysu_type = KosuTysuBase.Tysu.of_kosu_type kosu_type in
      let tysu = TysuUtil.Type.typed TysuAst.EFalse tysu_type in
      tysu
  | ECmpLess ->
      let kosu_type = Ty.TyOrdered in
      let tysu_type = KosuTysuBase.Tysu.of_kosu_type kosu_type in
      let tysu = TysuUtil.Type.typed TysuAst.ECmpLess tysu_type in
      tysu
  | ECmpEqual ->
      let kosu_type = Ty.TyOrdered in
      let tysu_type = KosuTysuBase.Tysu.of_kosu_type kosu_type in
      let tysu = TysuUtil.Type.typed TysuAst.ECmpEqual tysu_type in
      tysu
  | ECmpGreater ->
      let kosu_type = Ty.TyOrdered in
      let tysu_type = KosuTysuBase.Tysu.of_kosu_type kosu_type in
      let tysu = TysuUtil.Type.typed TysuAst.ECmpGreater tysu_type in
      tysu
  | ENullptr { is_const } ->
      let pointer_state =
        if is_const then
          Const
        else
          Mutable
      in
      let fresh_type = tyfresh () in
      let tysu_type =
        KosuTysuBase.Tysu.of_kosu_type_solved solutions expr.position fresh_type
      in
      TysuUtil.Type.typed (TysuAst.ENullptr { is_const })
      @@ TysuPointer { pointee_type = tysu_type; pointer_state }
  | EStringl s ->
      TysuUtil.Type.typed (TysuAst.EStringl s)
      @@ KosuTysuBase.Tysu.of_kosu_type Ty.TyStringLit
  | EChar c ->
      TysuUtil.Type.typed (TysuAst.EChar c)
      @@ KosuTysuBase.Tysu.of_kosu_type Ty.TyChar
  | EInteger { integer_info; ivalue } ->
      let tysu_type =
        match integer_info with
        | Some integer_info ->
            TysuType.TysuInteger integer_info
        | None ->
            let t = tyfresh () in
            KosuTysuBase.Tysu.of_kosu_type_solved solutions expr.position t
      in
      TysuUtil.Type.typed (TysuAst.EInteger ivalue) tysu_type
  | EFloat { fsize; fvalue } ->
      let tysu_type =
        match fsize with
        | Some integer_info ->
            TysuType.TysuFloat integer_info
        | None ->
            let t = tyfresh () in
            KosuTysuBase.Tysu.of_kosu_type_solved solutions expr.position t
      in
      TysuUtil.Type.typed (TysuAst.EFloat fvalue) tysu_type
  | ESizeof either ->
      let tysu_type =
        match either with
        | Either.Left kosu_type ->
            let ty = Kosu.Util.Ty.of_tyloc' kosu_type in
            let tysu_type =
              KosuTysuBase.Tysu.of_kosu_type_solved solutions kosu_type.position
                ty
            in
            tysu_type
        | Either.Right rhs ->
            let TysuType.{ element = _; tysu_type } =
              typeof ~tyfresh solutions kosu_env rhs
            in
            tysu_type
      in
      TysuUtil.Type.typed (TysuAst.ESizeof tysu_type)
      @@ KosuTysuBase.Tysu.of_kosu_type Kosu.Util.(Ty.of_tyloc TyLoc.usize)
  | EFieldAccess { first_expr; field = sfield } ->
      (* instanciante todo*)
      let first_expression = typeof ~tyfresh solutions kosu_env first_expr in
      let ty_expr = KosuTysuBase.Kosu.of_tysu_type first_expression.tysu_type in
      let struct_decl_opt =
        Kosu.Env.find_struct_declaration_type ty_expr kosu_env
      in
      let module_resolver, struct_decl = Option.get struct_decl_opt in
      let pametrics_type = Kosu.Util.Ty.parametrics_type ty_expr in
      let struct_decl =
        fst @@ Option.get
        @@ Kosu.Util.Struct.substitution module_resolver pametrics_type
             struct_decl
      in
      let ty = Option.get @@ Kosu.Util.Struct.field sfield.value struct_decl in
      let tysu_type = KosuTysuBase.Tysu.of_kosu_type ty in
      let tysu_expr =
        TysuAst.EFieldAccess
          { first_expr = first_expression; field = sfield.value }
      in
      let tysu_expr_typed = TysuUtil.Type.typed tysu_expr tysu_type in
      tysu_expr_typed
  | EArrayAccess { array_expr; index_expr } ->
      let array_expr = typeof ~tyfresh solutions kosu_env array_expr in
      let index_expr = typeof ~tyfresh solutions kosu_env index_expr in
      let tysu_type =
        match array_expr.tysu_type with
        | TysuArray { tysu_type; _ } ->
            tysu_type
        | _ ->
            error __FILE__ __LINE__ "EArray Access"
      in
      let tysu_expr = TysuAst.EArrayAccess { array_expr; index_expr } in
      let tysu_expr_typed = TysuUtil.Type.typed tysu_expr tysu_type in
      tysu_expr_typed
  | ETupleAccess { first_expr; index } ->
      let first_expr = typeof ~tyfresh solutions kosu_env first_expr in
      let tysu_type =
        match first_expr.tysu_type with
        | TysuTuple ttes ->
            ttes
        | _ ->
            error __FILE__ __LINE__ ""
      in
      let ty =
        Option.get @@ List.nth_opt tysu_type (Int64.to_int index.value)
      in
      let tysu_expr =
        TysuAst.ETupleAccess { first_expr; index = index.value }
      in
      TysuUtil.Type.typed tysu_expr ty
  | EConstIdentifier { module_resolver; identifier } ->
      let _, const_decl =
        Option.get
        @@ Kosu.Env.find_const_declaration
             (module_resolver, identifier)
             kosu_env
      in
      let ty =
        KosuTysuBase.Tysu.of_kosu_type
        @@ Kosu.Util.Ty.of_tyloc' const_decl.explicit_type
      in
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver
        @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver
      in
      let identifier = identifier.value in
      let tysu_expr =
        TysuAst.EConstIdentifier { module_resolver; identifier }
      in
      TysuUtil.Type.typed tysu_expr ty
  | EIdentifier { module_resolver; id } ->
      let t = Kosu.Env.find_identifier module_resolver id.value kosu_env in
      let ty = KosuTysuBase.Tysu.of_kosu_type @@ Option.get t in
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver
        @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver
      in
      let identifier = id.value in
      let tysu_expr =
        TysuAst.EIdentifier { module_resolver; id = identifier }
      in
      TysuUtil.Type.typed tysu_expr ty
  | EStruct { module_resolver; struct_name; fields } ->
      let module_resolver, struct_decl =
        Option.get
        @@ Kosu.Env.find_struct_declaration
             (module_resolver, struct_name)
             kosu_env
      in
      let struct_decl, ty =
        Kosu.Util.Struct.substitution_fresh ~fresh:tyfresh module_resolver
          struct_decl
      in
      let combined_fields = List.combine struct_decl.fields fields in
      let _, tysu_fields =
        List.fold_left_map
          (fun kosu_env ((struct_name, _), (_, expr_expr)) ->
            let tysu_expr = typeof ~tyfresh solutions kosu_env expr_expr in
            (kosu_env, (struct_name, tysu_expr))
          )
          kosu_env combined_fields
      in
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver module_resolver
      in
      let struct_name = struct_name.value in
      let tysu_expr =
        TysuAst.EStruct { module_resolver; struct_name; fields = tysu_fields }
      in
      let tysu_type =
        KosuTysuBase.Tysu.of_kosu_type_solved solutions expr.position ty
      in
      TysuUtil.Type.typed tysu_expr tysu_type
  | EEnum { module_resolver = re; enum_name; variant; assoc_exprs } ->
      let module_resolver, enum_decl =
        Option.get
        @@ Kosu.Env.find_enum_declaration re
             (Option.map Util.Position.value enum_name)
             variant kosu_env
      in
      let enum_decl, ty =
        Kosu.Util.Enum.substitution_fresh ~fresh:tyfresh module_resolver
          enum_decl
      in
      let enum_associated_type =
        Option.get @@ Kosu.Util.Enum.assoc_types variant.value enum_decl
      in
      let assoc_types_exprs = List.combine enum_associated_type assoc_exprs in
      let _, assoc_tysu_types =
        List.fold_left_map
          (fun kosu_env (_, enum_expr) ->
            let tysu_expr = typeof ~tyfresh solutions kosu_env enum_expr in
            (kosu_env, tysu_expr)
          )
          kosu_env assoc_types_exprs
      in
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver module_resolver
      in
      let enum_name = enum_decl.enum_name in
      let variant = variant.value in
      let tysu_expr =
        TysuAst.EEnum
          {
            module_resolver;
            enum_name;
            variant;
            assoc_exprs = assoc_tysu_types;
          }
      in
      let tysu_type = of_kosu_type_solved solutions expr.position ty in
      TysuUtil.Type.typed tysu_expr tysu_type
  | EBlock block ->
      typeof_block ~tyfresh kosu_env block
  | EDeref expr ->
      let kosu_env, ty = typeof ~tyfresh kosu_env expr in
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
      (kosu_env, ty_solve)
  | ETuple exprs ->
      let kosu_env, types =
        List.fold_left_map
          (fun kosu_env expr ->
            let env, ty = typeof ~tyfresh kosu_env expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            (kosu_env, ty)
          )
          kosu_env exprs
      in
      (kosu_env, TyTuple types)
  | EArray exprs ->
      let fresh_variable = tyfresh () in
      let length = List.length exprs in
      let kosu_env =
        List.fold_left
          (fun kosu_env expr ->
            let env, ty = typeof ~tyfresh kosu_env expr in
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
      let builtin_function =
        Position.map_use
          (fun fn_name ->
            match Kosu.Util.Builtin.of_string_opt fn_name.value with
            | Some f ->
                f
            | None ->
                raise @@ unbound_builtin_function fn_name
          )
          fn_name
      in
      let expected_arity = Kosu.Util.Builtin.arity builtin_function.value in
      let args_count = List.length parameters in
      let () =
        if expected_arity <> args_count then
          raise
          @@ KosuError.Exn.callable_wrong_arity fn_name expected_arity
               args_count
      in
      typeof_builin_functions ~tyfresh kosu_env builtin_function parameters
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
                     ~cfound:(Kosu.Util.Ty.of_tyloc' ty)
                     ~cexpected:fresh_ty ty kosu_env
                 )
                 kosu_env
      in
      let schema =
        Kosu.Util.Ty.ty_substitution_schema assoc_poly_fresh schema
      in

      let parameters = List.combine schema.parameters_type parameters in
      let kosu_env =
        List.fold_left
          (fun kosu_env (sig_ty, expr) ->
            let env, ty = typeof ~tyfresh kosu_env expr in
            let ty = Kosu.Util.Ty.ty_instanciate ~fresh ty in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            KosuEnv.add_typing_constraint ~cexpected:sig_ty ~cfound:ty expr
              kosu_env
          )
          kosu_env parameters
      in

      (kosu_env, schema.return_type)
  | EWhile { condition_expr; body } ->
      let env, ty = typeof ~tyfresh kosu_env condition_expr in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:TyBool
          condition_expr kosu_env
      in
      let env, body_ty = typeof_block ~tyfresh kosu_env body in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cfound:body_ty ~cexpected:TyUnit
          body.kosu_expr kosu_env
      in
      (kosu_env, TyUnit)
  | ECases { cases; else_body } ->
      let fresh_type = tyfresh () in
      let env, ty = typeof_block ~tyfresh kosu_env else_body in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint ~cexpected:fresh_type ~cfound:ty
          else_body.kosu_expr kosu_env
      in
      let kosu_env =
        List.fold_left
          (fun kosu_env (condition_expr, body) ->
            let env, ty = typeof ~tyfresh kosu_env condition_expr in
            let kosu_env = KosuEnv.merge_constraint env kosu_env in
            let kosu_env =
              KosuEnv.add_typing_constraint ~cfound:ty ~cexpected:TyBool
                condition_expr kosu_env
            in
            let env, body_ty = typeof_block ~tyfresh kosu_env body in
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
      let env, scrutinee_type = typeof ~tyfresh kosu_env expression in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let fresh_variable_ty = tyfresh () in
      let kosu_env =
        List.fold_left
          (fun kosu_env (kosu_pattern, kosu_block) ->
            let bound, (env, pattern_ty) =
              typeof_pattern ~tyfresh scrutinee_type kosu_env kosu_pattern
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
            let block_env, block_ty =
              typeof_block ~tyfresh kosu_block_env kosu_block
            in
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
      let variables, ty_variables =
        List.split
        @@ List.map
             (fun { aname = identifier; akosu_type; ais_var = is_var } ->
               let kosu_type =
                 match akosu_type with
                 | Some t ->
                     Kosu.Util.Ty.of_tyloc' t
                 | None ->
                     tyfresh ()
               in
               ((identifier, not is_var, kosu_type), kosu_type)
             )
             parameters
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
      let clo_env, ty_clo_ret = typeof ~tyfresh closure_kosu_env body in

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

      (kosu_env, ty)

and typeof_block ~tyfresh kosu_env block =
  let kosu_env =
    List.fold_left (typeof_statement ~tyfresh) kosu_env block.kosu_stmts
  in
  typeof ~tyfresh kosu_env block.kosu_expr

and typeof_statement ~tyfresh kosu_env
    (statement : KosuAst.kosu_statement location) =
  match statement.value with
  | SDeclaration { is_const; pattern; explicit_type; expression } ->
      let env, ety = typeof ~tyfresh kosu_env expression in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        match explicit_type with
        | Some tly ->
            let ty = Kosu.Util.Ty.of_tyloc' tly in
            KosuEnv.add_typing_constraint ~cexpected:ty ~cfound:ety expression
              kosu_env
        | None ->
            kosu_env
      in
      let bounds, (env, ty) = typeof_pattern ~tyfresh ety kosu_env pattern in
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
      let env, ty = typeof ~tyfresh kosu_env expression in
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
      kosu_env
  | SDiscard expression ->
      let env, _ = typeof ~tyfresh kosu_env expression in
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
and typeof_pattern ~tyfresh scrutinee_type kosu_env
    (pattern : KosuAst.kosu_pattern location) =
  (*Dont forget to raise if we try to bind an identifier to an existing variable in the*)
  let open KosuType.Ty in
  match pattern.value with
  | PEmpty ->
      let ty = Ty.TyUnit in
      ([], (kosu_env, ty))
  | PTrue | PFalse ->
      let ty = Ty.TyBool in
      ([], (kosu_env, ty))
  | PCmpLess | PCmpEqual | PCmpGreater ->
      let ty = TyOrdered in
      ([], (kosu_env, ty))
  | PNullptr ->
      let fresh_ty = tyfresh () in
      let ty = TyPointer { pointer_state = Const; pointee_type = fresh_ty } in
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
            let fresh_ty = tyfresh () in
            let bound, (env, ty) =
              typeof_pattern ~tyfresh fresh_ty kosu_env pattern
            in
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
            raise @@ unbound_enum module_resolver enum_name variant
      in
      let enum_decl, enum_ty =
        Kosu.Util.Enum.substitution_fresh ~fresh:tyfresh module_loc enum_decl
      in
      let enum_associated_type =
        match Kosu.Util.Enum.assoc_types variant.value enum_decl with
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
            let bound, (env, ty) =
              typeof_pattern ~tyfresh fresh_ty kosu_env enum_pattern
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
            raise @@ unbound_struct module_resolver struct_name
      in
      let struct_decl, struct_ty =
        Kosu.Util.Struct.substitution_fresh ~fresh:tyfresh module_resolver
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
      let env, bound_variable =
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
            let bound, (env, ty) =
              typeof_pattern ~tyfresh fresh_ty kosu_env pattern_pattern
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
      let fresh_ty = tyfresh () in
      let env, bound_typed =
        List.fold_left_map
          (fun kosu_env pattern ->
            let bound, (env, ty) =
              typeof_pattern ~tyfresh fresh_ty kosu_env pattern
            in
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
        typeof_pattern ~tyfresh scrutinee_type kosu_env pas_pattern
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

and typeof_builin_functions_args ~tyfresh kosu_env builtin expected args return
    =
  let fn_name = Position.map Kosu.Util.Builtin.to_string builtin in
  let arity = Kosu.Util.Builtin.arity builtin.value in
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
        let env, ty = typeof ~tyfresh kosu_env expr in
        let kosu_env = KosuEnv.merge_constraint env kosu_env in
        let cexpected =
          Position.map_use (fun expr -> expected expr.position ty) expr
        in
        let kosu_env =
          KosuEnv.add_typing_constraint ~cexpected:cexpected.value ~cfound:ty
            expr kosu_env
        in
        (kosu_env, cexpected)
      )
      kosu_env args
  in

  let return_type = return targs in

  (kosu_env, return_type)

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
        let expected0 _ _ = Kosu.Util.Ty.stringl in
        let expected = [ expected0 ] in
        let return_type _ = Kosu.Util.Ty.usize in
        (expected, return_type)
    | StringlPtr ->
        let expected0 _ _ = Kosu.Util.Ty.stringl in
        let expected = [ expected0 ] in
        let return_type _ = Kosu.Util.Ty.(ptr_const s8) in
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
              Kosu.Util.Ty.ptr_mut ktype
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
        let return_type _ = Kosu.Util.Ty.usize in
        (expect, return_type)
    | Exit ->
        let expected0 _ _ = Kosu.Util.Ty.s32 in
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
        let expected1 _ _ = Kosu.Util.Ty.usize in
        let expected = [ expected0; expected1 ] in
        let return_type args =
          let ty = List.hd args in
          match ty.value with
          | Ty.TyPointer { pointer_state = _; pointee_type } ->
              Kosu.Util.Ty.ptr_mut pointee_type
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
  match expression.value with
  | EIdentifier { module_resolver; id } ->
      let c =
        match Kosu.Util.ModuleResolver.is_empty module_resolver with
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
                     Kosu.Util.Ty.of_tyloc' t
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
          let bound, (_, _) =
            typeof_pattern ~tyfresh scrutine closure_env pattern
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
        @@ Option.map Kosu.Util.Ty.of_tyloc' explicit_type
      in
      (* Maybe merge closure_env and scope_env *)
      let pvariables, _ = typeof_pattern ~tyfresh ty closure_env pattern in
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
    (expr : KosuAst.kosu_expression location) =
  let counter = ref 0 in
  let fresh () =
    let n = !counter in
    let () = incr counter in
    let s = Printf.sprintf "'t%u" n in
    Ty.TyPolymorphic (CompilerPolymorphicVar s)
  in
  typeof ~tyfresh:fresh kosu_env expr

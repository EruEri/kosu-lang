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

open KosuTysuBase.Tysu

let rec of_kosu_expression kosu_env solutions expression =
  let Kosu.Ast.{ kosu_expression; expression_associated } =
    expression.Util.Position.value
  in
  let tysu_expression = of_expression kosu_env solutions kosu_expression in
  let tysu_type =
    KosuTysuBase.Tysu.of_kosu_type_solved solutions expression.position
      expression_associated
  in
  (TysuUtil.Type.typed tysu_expression tysu_type, kosu_env)

and of_expression kosu_env solutions :
    (Kosu.Type.Ty.kosu_type, _, _) Kosu.Ast.expression ->
    TysuAst.tysu_expression = function
  | EEmpty ->
      EEmpty
  | ETrue ->
      ETrue
  | EFalse ->
      EFalse
  | ECmpLess ->
      ECmpLess
  | ECmpEqual ->
      ECmpEqual
  | ECmpGreater ->
      ECmpGreater
  | ENullptr { is_const } ->
      ENullptr { is_const }
  | EStringl string ->
      EStringl string
  | EChar char ->
      EChar char
  | EInteger { integer_info = _; ivalue } ->
      EInteger ivalue
  | EFloat { fsize = _; fvalue } ->
      EFloat fvalue
  | ESizeof either ->
      let kosu_type, position =
        match either with
        | Either.Left kosu_type ->
            (Kosu.Util.Ty.of_tyloc' kosu_type, kosu_type.position)
        | Either.Right rhs ->
            (rhs.value.expression_associated, rhs.position)
      in
      let tysu_type =
        KosuTysuBase.Tysu.of_kosu_type_solved solutions position kosu_type
      in
      ESizeof tysu_type
  | EFieldAccess { first_expr; field } ->
      let first_expr, _ = of_kosu_expression kosu_env solutions first_expr in
      let field = field.value in
      EFieldAccess { first_expr; field }
  | EArrayAccess { array_expr; index_expr } ->
      let array_expr, _ = of_kosu_expression kosu_env solutions array_expr in
      let index_expr, _ = of_kosu_expression kosu_env solutions index_expr in
      EArrayAccess { array_expr; index_expr }
  | ETupleAccess { first_expr; index } ->
      let first_expr, _ = of_kosu_expression kosu_env solutions first_expr in
      let index = index.value in
      ETupleAccess { first_expr; index }
  | EConstIdentifier { module_resolver; identifier } ->
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver
        @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver
      in
      let identifier = identifier.value in
      EConstIdentifier { module_resolver; identifier }
  | EIdentifier { module_resolver; id } ->
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver
        @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver
      in
      let id = id.value in
      EIdentifier { module_resolver; id }
  | EStruct { module_resolver; struct_name; fields } ->
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver
        @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver
      in
      let struct_name = struct_name.value in
      let fields =
        List.map
          (fun (name, expr) ->
            let tysu_expr, _ = of_kosu_expression kosu_env solutions expr in
            (name.Util.Position.value, tysu_expr)
          )
          fields
      in
      EStruct { module_resolver; struct_name; fields }
  | EEnum { module_resolver; enum_name; variant; assoc_exprs } ->
      let module_resolver, enum_decl =
        Option.get
        @@ Kosu.Env.find_enum_declaration module_resolver
             (Option.map Util.Position.value enum_name)
             variant kosu_env
      in
      let assoc_exprs =
        List.map
          (fun expr -> fst @@ of_kosu_expression kosu_env solutions expr)
          assoc_exprs
      in
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver module_resolver
      in
      let variant = variant.value in
      let enum_name = enum_decl.enum_name.value in
      EEnum { module_resolver; enum_name; variant; assoc_exprs }
  | EBlock block ->
      let _, tysu_block = of_kosu_block kosu_env solutions block in
      EBlock tysu_block
  | EDeref expr ->
      let expression, _ = of_kosu_expression kosu_env solutions expr in
      EDeref expression
  | ETuple exprs ->
      let ( $ ) = Util.Operator.( $ ) in
      let exprs =
        List.map (fst $ of_kosu_expression kosu_env solutions) exprs
      in
      ETuple exprs
  | EArray exprs ->
      let ( $ ) = Util.Operator.( $ ) in
      let exprs =
        List.map (fst $ of_kosu_expression kosu_env solutions) exprs
      in
      EArray exprs
  | EBuiltinFunctionCall { fn_name; parameters } ->
      let ( $ ) = Util.Operator.( $ ) in
      let parameters =
        List.map (fst $ of_kosu_expression kosu_env solutions) parameters
      in
      let builtin =
        Option.get @@ Kosu.Util.Builtin.of_string_opt fn_name.value
      in
      EBuiltinFunctionCall { builtin; parameters }
  | EFunctionCall { module_resolver; generics_resolver; fn_name; parameters } ->
      let ( $ ) = Util.Operator.( $ ) in
      let parameters =
        List.map (fst $ of_kosu_expression kosu_env solutions) parameters
      in
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver
        @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver
      in
      let generics_resolver =
        Option.map
          (List.map (fun ty ->
               KosuTysuBase.Tysu.of_kosu_type_solved solutions
                 ty.Util.Position.position
               @@ Kosu.Util.Ty.of_tyloc' ty
           )
          )
          generics_resolver
      in
      let fn_name = fn_name.value in
      EFunctionCall { module_resolver; generics_resolver; fn_name; parameters }
  | EWhile { condition_expr; body } ->
      let condition_expr, _ =
        of_kosu_expression kosu_env solutions condition_expr
      in
      let _, tysu_block = of_kosu_block kosu_env solutions body in
      EWhile { condition_expr; body = tysu_block }
  | ECases { cases; else_body } ->
      let _, else_body = of_kosu_block kosu_env solutions else_body in
      let cases =
        List.map
          (fun (condition, block) ->
            let tysu_condition, _ =
              of_kosu_expression kosu_env solutions condition
            in
            let _, tysu_block = of_kosu_block kosu_env solutions block in
            (tysu_condition, tysu_block)
          )
          cases
      in
      ECases { cases; else_body }
  | EMatch { expression; patterns } ->
      let expression, _ = of_kosu_expression kosu_env solutions expression in
      let patterns =
        List.map
          (fun (pattern, body) ->
            let bound, tysu_pattern =
              of_kosu_pattern kosu_env solutions pattern
            in
            let kosu_block_env =
              List.fold_left
                (fun env (id, bound_ty) ->
                  Kosu.Env.add_variable true id bound_ty env
                )
                kosu_env bound
            in
            let _, tysu_block = of_kosu_block kosu_block_env solutions body in
            (tysu_pattern, tysu_block)
          )
          patterns
      in
      EMatch { expression; patterns }
  | EAnonFunction { kind; parameters; body; captured } ->
      let module Captured = Kosu.Typechecking.CapturedIdentifier in
      let tysu_function_parameters, kosu_variables =
        List.split
        @@ List.map
             (fun Kosu.Ast.{ ais_var; akosu_type; aname } ->
               let tysu_type =
                 of_kosu_type_solved solutions aname.position akosu_type
               in
               let kosu_type = KosuTysuBase.Kosu.of_tysu_type tysu_type in
               let name = aname.value in
               ( TysuAst.{ is_var = ais_var; name; tysu_type },
                 (aname, not ais_var, kosu_type)
               )
             )
             parameters
      in
      let captured =
        List.map (fun (Util.Position.{ value = name; position }, kosu_type) ->
            (* Attention : Maybe thanks to the implementation of captured variable in *)
            (* Kosu.Typechecking.variables_expression *)
            (* Some compilerVariable may have been wasted *)
            let tysu_type = of_kosu_type_solved solutions position kosu_type in
            (name, tysu_type)
        )
        @@ Captured.elements captured
      in
      let closure_kosu_env =
        Kosu.Env.rebind_env_variables kosu_variables kosu_env
      in
      let tysu_body, _ = of_kosu_expression closure_kosu_env solutions body in
      EAnonFunction
        {
          kind;
          parameters = tysu_function_parameters;
          captured;
          body = tysu_body;
        }

and of_kosu_block kosu_env solutions block =
  let kosu_env, tysu_stmts =
    List.fold_left_map (of_kosu_statement solutions) kosu_env block.kosu_stmts
  in
  let tysu_expr, kosu_env =
    of_kosu_expression kosu_env solutions block.kosu_expr
  in
  (kosu_env, TysuAst.{ tysu_stmts; tysu_expr })

and of_kosu_statement solutions kosu_env statement =
  match statement.value with
  | SDeclaration { is_const; pattern; explicit_type = _; expression } ->
      let tysu_expression, kosu_env =
        of_kosu_expression kosu_env solutions expression
      in
      let bound, tysu_pattern = of_kosu_pattern kosu_env solutions pattern in
      let kosu_env =
        List.fold_left
          (fun kosu_env (id, ty) ->
            Kosu.Env.add_variable is_const id ty kosu_env
          )
          kosu_env bound
      in
      ( kosu_env,
        SDeclaration
          { is_const; pattern = tysu_pattern; expression = tysu_expression }
      )
  | SAffection { lvalue; expression; is_deref } ->
      let (KosuLvalue { variable; fields }) = lvalue in
      let tysu_expression, _ =
        of_kosu_expression kosu_env solutions expression
      in
      let variable_info =
        Option.get @@ Kosu.Env.assoc_type_opt variable.value kosu_env
      in
      let variable_tysu_type =
        of_kosu_type_solved solutions variable_info.identifier.position
          variable_info.kosu_type
      in
      let variable = TysuUtil.Type.typed variable.value variable_tysu_type in
      let fields = Util.Position.values fields in
      let lvalue = TysuAst.TysuLvalue { variable; fields } in
      (kosu_env, SAffection { is_deref; lvalue; expression = tysu_expression })
  | SDiscard expression ->
      let tysu_expression, _ =
        of_kosu_expression kosu_env solutions expression
      in
      (kosu_env, SDiscard tysu_expression)
  | SOpen { module_resolver } ->
      let kosu_module =
        Option.get @@ Kosu.Env.find_module_opt module_resolver kosu_env
      in
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver
        @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver
      in
      let kosu_env = Kosu.Env.add_module kosu_module kosu_env in
      (kosu_env, SOpen { module_resolver })

and of_kosu_pattern kosu_env solutions kosu_pattern =
  let Kosu.Ast.{ kosu_pattern = pattern; pattern_associated = kosu_type } =
    kosu_pattern.Util.Position.value
  in
  let tysu_type =
    KosuTysuBase.Tysu.of_kosu_type_solved solutions kosu_pattern.position
      kosu_type
  in
  let kosu_type = KosuTysuBase.Kosu.of_tysu_type tysu_type in
  let bound, tysu_pattern = of_pattern kosu_type kosu_env solutions pattern in
  let tysu_pattern_typed = TysuUtil.Type.typed tysu_pattern tysu_type in
  (bound, tysu_pattern_typed)

and of_pattern scrutine_type kosu_env solutions = function
  | PEmpty ->
      ([], TysuAst.PEmpty)
  | PTrue ->
      ([], PTrue)
  | PFalse ->
      ([], PFalse)
  | PCmpLess ->
      ([], PCmpLess)
  | PCmpEqual ->
      ([], PCmpEqual)
  | PCmpGreater ->
      ([], PCmpGreater)
  | PNullptr ->
      ([], PNullptr)
  | PWildcard ->
      ([], PWildcard)
  | PFloat f ->
      ([], PFloat f.value)
  | PChar c ->
      ([], PChar c.value)
  | PInteger { value } ->
      ([], PInteger value.value)
  | PIdentifier identifier ->
      let bound = [ (identifier, scrutine_type) ] in
      (bound, PIdentifier identifier.value)
  | PTuple patterns ->
      let module PIB = Kosu.Typechecking.PatternIdentifierBound in
      let tysu_patterns =
        List.map (of_kosu_pattern kosu_env solutions) patterns
      in
      let bounds, kosu_patterns = List.split tysu_patterns in
      let bounds =
        PIB.elements
        @@ List.fold_left
             (fun set bounds -> PIB.add_seq (List.to_seq bounds) set)
             PIB.empty bounds
      in
      (bounds, PTuple kosu_patterns)
  | PCase { module_resolver; enum_name; variant; assoc_patterns } ->
      let module PIB = Kosu.Typechecking.PatternIdentifierBound in
      let name = Option.map Util.Position.value enum_name in
      let module_loc, enum_decl =
        Option.get
        @@ Kosu.Env.find_enum_declaration module_resolver name variant kosu_env
      in
      let assoc_patterns =
        List.map (of_kosu_pattern kosu_env solutions) assoc_patterns
      in
      let bounds, tysu_patterns = List.split assoc_patterns in
      let bounds =
        PIB.elements
        @@ List.fold_left
             (fun set bounds -> PIB.add_seq (List.to_seq bounds) set)
             PIB.empty bounds
      in
      let module_resolver = KosuTysuBase.Tysu.of_module_resolver module_loc in
      let enum_name = enum_decl.enum_name.value in
      let variant = variant.value in
      ( bounds,
        PCase
          {
            module_resolver;
            enum_name;
            variant;
            assoc_patterns = tysu_patterns;
          }
      )
  | PRecord { module_resolver; struct_name; pfields } ->
      let module PIB = Kosu.Typechecking.PatternIdentifierBound in
      let module_resolver, struct_decl =
        Option.get
        @@ Kosu.Env.find_struct_declaration
             (module_resolver, struct_name)
             kosu_env
      in
      let bound_patterns =
        List.map
          (fun (field_name, kosu_pattern) ->
            let open Util.Position in
            let bound, tysu_pattern =
              of_kosu_pattern kosu_env solutions kosu_pattern
            in
            (bound, (field_name.value, tysu_pattern))
          )
          pfields
      in
      let bounds, tysu_pfields = List.split bound_patterns in
      let bounds =
        PIB.elements
        @@ List.fold_left
             (fun set bounds -> PIB.add_seq (List.to_seq bounds) set)
             PIB.empty bounds
      in
      let module_resolver =
        KosuTysuBase.Tysu.of_module_resolver module_resolver
      in
      let struct_name = struct_decl.struct_name.value in
      (bounds, PRecord { module_resolver; struct_name; pfields = tysu_pfields })
  | POr patterns ->
      let module PIB = Kosu.Typechecking.PatternIdentifierBound in
      let tysu_patterns =
        List.map (of_kosu_pattern kosu_env solutions) patterns
      in
      let bounds, kosu_patterns = List.split tysu_patterns in
      let bounds =
        PIB.elements
        @@ List.fold_left
             (fun set bounds -> PIB.add_seq (List.to_seq bounds) set)
             PIB.empty bounds
      in
      (bounds, POr kosu_patterns)
  | PAs { pas_pattern; pas_bound } ->
      let module PIB = Kosu.Typechecking.PatternIdentifierBound in
      let bounds, tysu_pattern =
        of_kosu_pattern kosu_env solutions pas_pattern
      in
      let bounds = (pas_bound, scrutine_type) :: bounds in
      let bounds = PIB.elements @@ PIB.of_list bounds in
      (bounds, PAs { pas_pattern = tysu_pattern; pas_bound = pas_bound.value })

let of_external_decl _kosu_program _current_module external_func_decl =
  let ( $ ) = Util.Operator.( $ ) in
  let Kosu.Ast.
        {
          sig_name = { value = sig_name; position = _ };
          parameters;
          return_type;
          c_name;
        } =
    external_func_decl
  in
  let return_type = of_kosu_type (Kosu.Util.Ty.of_tyloc' return_type) in
  let parameters =
    List.map (of_kosu_type $ Kosu.Util.Ty.of_tyloc') parameters
  in
  TysuAst.{ sig_name; parameters; return_type; c_name }

let of_opaque_decl _kosu_program _current_module opaque_decl =
  let Kosu.Ast.{ name } = opaque_decl in
  TysuAst.{ name = name.value }

let of_enum_decl _kosu_program _current_module enum_decl =
  let ( $ ) = Util.Operator.( $ ) in
  let Kosu.Ast.{ enum_name; poly_vars; tag_type; variants } = enum_decl in
  let raw_tag_type = Kosu.Util.Ty.of_tyloc' tag_type in
  let poly_vars =
    List.map (of_polymorphic $ Kosu.Util.Ty.of_tyloc_polymorphic) poly_vars
  in
  let enum_name = enum_name.value in
  let tag_type =
    match raw_tag_type with
    | Kosu.Type.Ty.TyInteger integer ->
        integer
    | _ ->
        raise @@ TysuError.enum_not_integer_tag_size tag_type
  in
  let variants =
    List.map
      (fun (variant, assoc_types) ->
        let variant = variant.Util.Position.value in
        let assoc_types =
          List.map (of_kosu_type $ Kosu.Util.Ty.of_tyloc') assoc_types
        in
        (variant, assoc_types)
      )
      variants
  in
  TysuAst.{ enum_name; poly_vars; tag_type; variants }

let of_struct_decl _kosu_program _current_module struct_decl =
  let ( $ ) = Util.Operator.( $ ) in
  let Kosu.Ast.{ struct_name; poly_vars; fields } = struct_decl in
  let struct_name = struct_name.value in
  let poly_vars =
    List.map (of_polymorphic $ Kosu.Util.Ty.of_tyloc_polymorphic) poly_vars
  in
  let fields =
    List.map
      (fun (name, kosu_type) ->
        let tysu_type = of_kosu_type @@ Kosu.Util.Ty.of_tyloc' kosu_type in
        let name = name.Util.Position.value in
        (name, tysu_type)
      )
      fields
  in
  TysuAst.{ struct_name; poly_vars; fields }

let of_const_decl kosu_program current_module const_decl =
  let Kosu.Ast.{ const_name; explicit_type; c_value } = const_decl in
  let empty = Kosu.Env.create current_module kosu_program in
  let (env, ty), kosu_expression = Kosu.Typechecking.typeof empty c_value in
  let kosu_env = Kosu.Env.merge_constraint env empty in
  let kosu_env =
    Kosu.Env.add_typing_constraint
      ~cexpected:(Kosu.Util.Ty.of_tyloc' explicit_type)
      ~cfound:ty explicit_type kosu_env
  in
  let solutions = Kosu.Env.solve kosu_env in
  let kosu_env = Kosu.Env.create current_module kosu_program in
  let c_expression, _ = of_kosu_expression kosu_env solutions kosu_expression in
  let const_name = const_name.value in
  let explicit_type =
    KosuTysuBase.Tysu.of_kosu_type_solved solutions explicit_type.position
    @@ Kosu.Util.Ty.of_tyloc' explicit_type
  in
  TysuAst.{ const_name; explicit_type; c_expression }

let of_function_decl kosu_program current_module fonction_decl =
  let ( $ ) = Util.Operator.( $ ) in
  let Kosu.Ast.{ fn_name; poly_vars; parameters; return_type; body } =
    fonction_decl
  in
  let kosu_env = Kosu.Env.create current_module kosu_program in
  let kosu_env =
    List.fold_left
      (fun kosu_env variable_info ->
        let open Kosu.Ast in
        Kosu.Env.add_variable (not variable_info.is_var) variable_info.name
          (Kosu.Util.Ty.of_tyloc' variable_info.kosu_type)
          kosu_env
      )
      kosu_env parameters
  in
  let kosu_env =
    Kosu.Env.add_bound_poly_vars
      (List.map Kosu.Util.Ty.of_tyloc_polymorphic poly_vars)
      kosu_env
  in
  let (env, ty), kosu_expression = Kosu.Typechecking.typeof kosu_env body in
  let kosu_env = Kosu.Env.merge_constraint env kosu_env in
  let kosu_env =
    Kosu.Env.add_typing_constraint
      ~cexpected:(Kosu.Util.Ty.of_tyloc' return_type)
      ~cfound:ty return_type kosu_env
  in
  let solutions = Kosu.Env.solve kosu_env in
  let kosu_env = Kosu.Env.create current_module kosu_program in
  let tysu_expression, _ =
    of_kosu_expression kosu_env solutions kosu_expression
  in
  let poly_vars =
    List.map (of_polymorphic $ Kosu.Util.Ty.of_tyloc_polymorphic) poly_vars
  in
  let fn_name = fn_name.value in
  let of_kosu_ty ty =
    of_kosu_type_solved solutions ty.Util.Position.position
    @@ Kosu.Util.Ty.of_tyloc' ty
  in
  let return_type = of_kosu_ty return_type in
  let parameters =
    List.map
      (fun Kosu.Ast.{ is_var; name; kosu_type } ->
        let tysu_type = of_kosu_ty kosu_type in
        TysuAst.{ is_var; name = name.value; tysu_type }
      )
      parameters
  in
  TysuAst.
    { fn_name; poly_vars; parameters; return_type; body = tysu_expression }

let of_module_node kosu_program current_module = function
  | Kosu.Ast.NExternFunc e ->
      TysuAst.NExternFunc (of_external_decl kosu_program current_module e)
  | NOpaque opaque ->
      TysuAst.NOpaque (of_opaque_decl kosu_program current_module opaque)
  | NFunction function_decl ->
      let tysu_function =
        of_function_decl kosu_program current_module function_decl
      in
      TysuAst.NFunction tysu_function
  | NStruct struct_decl ->
      let tysu_struct_decl =
        of_struct_decl kosu_program current_module struct_decl
      in
      TysuAst.NStruct tysu_struct_decl
  | NEnum enum ->
      let tysu_enum_decl = of_enum_decl kosu_program current_module enum in
      TysuAst.NEnum tysu_enum_decl
  | NConst const ->
      let tysu_const_decl = of_const_decl kosu_program current_module const in
      TysuAst.NConst tysu_const_decl

let of_module kosu_program kosu_module =
  List.map (of_module_node kosu_program kosu_module) kosu_module

let of_named_module kosu_program kosu_named_module =
  let Kosu.Ast.{ kosu_module; filename } = kosu_named_module in
  try
    let tysu_module = of_module kosu_program kosu_module in
    TysuAst.{ tysu_module; filename }
  with Kosu.Error.KosuRawErr kosu_error ->
    let () =
      Kosu.Reporter.emit ~underline:true
      @@ Kosu.Reporter.file filename kosu_error
    in
    exit KosuMisc.ExitCode.validation_error

let of_program kosu_program =
  List.map (of_named_module kosu_program) kosu_program

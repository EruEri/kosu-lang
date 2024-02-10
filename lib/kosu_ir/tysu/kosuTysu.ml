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
    Kosu.Type.Ty.kosu_type Kosu.Ast.expression -> TysuAst.tysu_expression =
  function
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
      failwith ""
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
      let body = failwith "TODO_BLOCK" in
      EWhile { condition_expr; body }
  (* If expression will be a syntaxique sugar of ecases *)
  | ECases { cases; else_body } ->
      failwith ""
  | EMatch { expression; patterns } ->
      let expression, _ = of_kosu_expression kosu_env solutions expression in
      failwith ""
  | EAnonFunction { kind; parameters; body } ->
      failwith ""

and of_kosu_block kosu_env solutions blocks = failwith ""

and of_kosu_statement kosu_env solutions statement =
  match statement.Util.Position.value with
  | Kosu.Ast.SDeclaration { is_const; pattern; explicit_type; expression } ->
      failwith ""
  | SAffection { lvalue; expression } ->
      failwith ""
  | SDiscard expression ->
      failwith ""
  | SOpen { module_resolver } ->
      failwith ""

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
        let default =
          Kosu.Util.(IntegerInfo.sized @@ TyLoc.(signed, isize_32))
        in
        Option.value ~default integer
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
  let Kosu.Ast.{ const_name = _; explicit_type; c_value } = const_decl in
  let empty = Kosu.Env.create current_module kosu_program in
  let (env, ty), kosu_expression = Kosu.Typechecking.typeof empty c_value in
  let kosu_env = Kosu.Env.merge_constraint env empty in
  let kosu_env =
    Kosu.Env.add_typing_constraint
      ~cexpected:(Kosu.Util.Ty.of_tyloc' explicit_type)
      ~cfound:ty explicit_type kosu_env
  in
  let _solutions = Kosu.Env.solve kosu_env in
  failwith "TODO: "

let of_function_decl _kosu_program _current_module _fonction_decl =
  failwith "TODO: "

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
  let tysu_module = of_module kosu_program kosu_module in
  TysuAst.{ tysu_module; filename }

let of_program kosu_program =
  List.map (of_named_module kosu_program) kosu_program

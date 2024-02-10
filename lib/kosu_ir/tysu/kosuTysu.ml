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

let rec of_kosu_expression solutions expression = 
  let Kosu.Ast.{kosu_expression; expression_associated} = expression.Util.Position.value in
  let tysu_expression = of_expression solutions kosu_expression in
  let tysu_type = KosuTysuBase.Tysu.of_kosu_type_solved solutions expression.position expression_associated in
  TysuUtil.Type.typed tysu_expression tysu_type

and of_expression solutions : Kosu.Type.Ty.kosu_type Kosu.Ast.expression -> TysuAst.tysu_expression = function
  | EEmpty -> EEmpty
  | ETrue -> ETrue
  | EFalse -> EFalse
  | ECmpLess -> ECmpLess
  | ECmpEqual -> ECmpEqual
  | ECmpGreater -> ECmpGreater
  | ENullptr { is_const  } -> ENullptr {is_const}
  | EStringl string -> EStringl string
  | EChar char -> EChar char
  | EInteger { integer_info = _; ivalue } -> 
    EInteger ivalue
  | EFloat  { fsize = _; fvalue } -> EFloat fvalue
  | ESizeof either -> 
    let kosu_type, position = match either with
      | Either.Left kosu_type ->
          Kosu.Util.Ty.of_tyloc' kosu_type, kosu_type.position
      | Either.Right rhs ->
        rhs.value.expression_associated, rhs.position
    in
    let tysu_type = KosuTysuBase.Tysu.of_kosu_type_solved solutions position kosu_type in
    ESizeof tysu_type
  | EFieldAccess {
      first_expr;
      field;
    } -> 
      let first_expr = of_kosu_expression solutions first_expr in
      let field = field.value in
     EFieldAccess {first_expr; field}
  | EArrayAccess {
      array_expr ;
      index_expr;
    } -> 
      let array_expr = of_kosu_expression solutions array_expr in
      let index_expr = of_kosu_expression solutions index_expr in
      EArrayAccess {array_expr; index_expr}
  | ETupleAccess {
      first_expr;
      index;
    } -> 
      let first_expr = of_kosu_expression solutions first_expr in
      let index = index.value in
      ETupleAccess {first_expr; index}

  | EConstIdentifier {
      module_resolver;
      identifier;
    } -> 
      let module_resolver = KosuTysuBase.Tysu.of_module_resolver @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver in
      let identifier = identifier.value in
      EConstIdentifier {module_resolver; identifier}
  | EIdentifier {
      module_resolver;
      id;
    } -> 
      let module_resolver = KosuTysuBase.Tysu.of_module_resolver @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver in
      let id = id.value in
      EIdentifier {module_resolver; id}
  | EStruct {
      module_resolver;
      struct_name;
      fields;
    } ->
    let module_resolver = KosuTysuBase.Tysu.of_module_resolver @@ Kosu.Util.ModuleResolver.to_unlocated module_resolver in
    let struct_name = struct_name.value in
    let fields = List.map (fun (name, expr) -> 
      let tysu_expr = of_kosu_expression solutions expr in
      name.Util.Position.value, tysu_expr
    ) fields in
    EStruct {module_resolver; struct_name; fields}
  | EEnum {
      module_resolver;
      enum_name;
      variant;
      assoc_exprs;
    } -> 
      failwith ""
  | EBlock  'a kosu_block -> failwith ""
  | EDeref 'a kosu_expression location
  | ETuple 'a kosu_expression location list
  | EArray  'a kosu_expression location list
  | EBuiltinFunctionCall of {
      fn_name : string location;
      parameters : 'a kosu_expression location list;
    }
  | EFunctionCall of {
      module_resolver : module_resolver_loc;
      generics_resolver : TyLoc.kosu_loctype location list option;
      fn_name : string location;
      parameters : 'a kosu_expression location list;
    }
  | EWhile of {
      condition_expr : 'a kosu_expression location;
      body : 'a kosu_block;
    }
  (* If expression will be a syntaxique sugar of ecases *)
  | ECases of {
      cases : ('a kosu_expression location * 'a kosu_block) list;
      else_body : 'a kosu_block;
    }
  | EMatch of {
      expression : 'a kosu_expression location;
      patterns : ('a kosu_pattern location * 'a kosu_block) list;
    }
  | EAnonFunction of {
      kind : kosu_anon_function_kind;
      parameters : kosu_anon_parameters list;
      body : 'a kosu_expression location;
    }

(* let of_expression solution kosu_env expr =
   match expr.Util.Position.value with
   | Kosu.Ast.EEmpty ->
     let tysu_element = TysuUtil.Type. *)

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

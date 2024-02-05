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

let of_module_resolver : Kosu.Base.module_resolver -> TysuBase.module_resolver =
  function
  | ModuleResolver_ modules ->
      ModuleResolver modules

let of_polymorphic :
    Kosu.Type.Ty.kosu_type_polymorphic -> TysuType.tysu_variable_polymorphic =
  function
  | PolymorphicVar s ->
      TysuType.ForAllVar s
  | CompilerPolymorphicVar s ->
      raise @@ TysuError.kosu_compiler_variable_found s

let rec of_schema :
    Kosu.Type.Ty.kosu_function_schema -> TysuType.tysu_function_schema =
 fun { poly_vars; parameters_type; return_type } ->
  let poly_vars = List.map of_polymorphic poly_vars in
  let parameters_type = List.map of_kosu_type parameters_type in
  let return_type = of_kosu_type return_type in
  { poly_vars; parameters_type; return_type }

and of_kosu_type : Kosu.Type.Ty.kosu_type -> TysuType.tysu_type = function
  | Kosu.Type.Ty.TyIdentifier { module_resolver; parametrics_type; name } ->
      let parametrics_type = List.map of_kosu_type parametrics_type in
      let module_resolver = of_module_resolver module_resolver in
      TysuIdentifier { module_resolver; parametrics_type; name }
  | TyPolymorphic variable ->
      let variable = of_polymorphic variable in
      TysuPolymorphic variable
  | TyPointer { pointer_state; pointee_type } ->
      let pointee_type = of_kosu_type pointee_type in
      TysuPointer { pointer_state; pointee_type }
  | TyInteger integer ->
      let default = Kosu.Util.(IntegerInfo.sized @@ TyLoc.(signed, isize_32)) in
      let integer = Option.value ~default integer in
      TysuInteger integer
  | TyFloat float_info ->
      let default = Kosu.Base.F32 in
      let float_info = Option.value ~default float_info in
      TysuFloat float_info
  | TyFunctionPtr schema ->
      let schema = of_schema schema in
      TysuFunctionPtr schema
  | TyClosure schema ->
      let schema = of_schema schema in
      TysuClosure schema
  | TyArray { ktype; size } ->
      let tysu_type = of_kosu_type ktype in
      TysuArray { tysu_type; size }
  | TyTuple types ->
      let types = List.map of_kosu_type types in
      TysuTuple types
  | TyOpaque { module_resolver; name } ->
      let module_resolver = of_module_resolver module_resolver in
      TysuOpaque { module_resolver; name }
  | TyOrdered ->
      TysuOrdered
  | TyStringLit ->
      TysuStringLit
  | TyChar ->
      TysuChar
  | TyBool ->
      TysuBool
  | TyUnit ->
      TysuUnit

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
  let env, ty = Kosu.Typechecking.typeof empty c_value in
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

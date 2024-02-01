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

let of_module_resolver :
    KosuFrontendAlt.Base.module_resolver -> TysuBase.module_resolver = function
  | ModuleResolver_ modules ->
      ModuleResolver modules

let of_polymorphic :
    KosuFrontendAlt.Type.Ty.kosu_type_polymorphic ->
    TysuType.tysu_variable_polymorphic = function
  | PolymorphicVar s ->
      TysuType.ForAllVar s
  | CompilerPolymorphicVar s ->
      failwith
      @@ Printf.sprintf
           "[Kosu to Tyzu]: CompilerPolymorphic Variable found : \"%s\"" s

let rec of_schema :
    KosuFrontendAlt.Type.Ty.kosu_function_schema ->
    TysuType.tysu_function_schema =
 fun { poly_vars; parameters_type; return_type } ->
  let poly_vars = List.map of_polymorphic poly_vars in
  let parameters_type = List.map of_kosu_type parameters_type in
  let return_type = of_kosu_type return_type in
  { poly_vars; parameters_type; return_type }

and of_kosu_type : KosuFrontendAlt.Type.Ty.kosu_type -> TysuType.tysu_type =
  function
  | KosuFrontendAlt.Type.Ty.TyIdentifier
      { module_resolver; parametrics_type; name } ->
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
      let default =
        KosuFrontendAlt.Util.(IntegerInfo.sized @@ TyLoc.(signed, isize_32))
      in
      let integer = Option.value ~default integer in
      TysuInteger integer
  | TyFloat float_info ->
      let default = KosuFrontendAlt.Base.F32 in
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

let of_external_decl _kosu_program _current_module external_func_decl =
  let ( $ ) = Util.Operator.( $ ) in
  let KosuFrontendAlt.Ast.
        {
          sig_name = { value = sig_name; position = _ };
          parameters;
          return_type;
          c_name;
        } =
    external_func_decl
  in
  let return_type =
    of_kosu_type (KosuFrontendAlt.Util.Ty.of_tyloc' return_type)
  in
  let parameters =
    List.map (of_kosu_type $ KosuFrontendAlt.Util.Ty.of_tyloc') parameters
  in
  TysuAst.{ sig_name; parameters; return_type; c_name }

let of_opaque_decl _kosu_program _current_module opaque_decl =
  let KosuFrontendAlt.Ast.{ name } = opaque_decl in
  TysuAst.{ name = name.value }

let of_module_node kosu_program current_module = function
  | KosuFrontendAlt.Ast.NExternFunc e ->
      TysuAst.NExternFunc (of_external_decl kosu_program current_module e)
  | NOpaque opaque ->
      TysuAst.NOpaque (of_opaque_decl kosu_program current_module opaque)
  | NFunction _ ->
      failwith ""
  | NStruct _ ->
      failwith ""
  | NEnum _ ->
      failwith ""
  | NConst _ ->
      failwith ""

let of_module kosu_program kosu_module =
  List.map (of_module_node kosu_program kosu_module) kosu_module

let of_named_module kosu_program kosu_named_module =
  let KosuFrontendAlt.Ast.{ kosu_module; filename } = kosu_named_module in
  let tysu_module = of_module kosu_program kosu_module in
  TysuAst.{ tysu_module; filename }

let of_program kosu_program =
  List.map (of_named_module kosu_program) kosu_program

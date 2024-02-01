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

let rec of_kosu_type : KosuFrontendAlt.Type.Ty.kosu_type -> TysuType.tysu_type =
  function
  | KosuFrontendAlt.Type.Ty.TyIdentifier
      { module_resolver; parametrics_type; name } ->
      let parametrics_type = List.map of_kosu_type parametrics_type in
      let module_resolver = of_module_resolver module_resolver in
      TysuIdentifier { module_resolver; parametrics_type; name }
  | TyPolymorphic _ ->
      failwith ""
  | TyPointer _ ->
      failwith ""
  | TyInteger integer ->
      let integer = Option.value ~default:(failwith "") integer in
      TysuInteger integer
  | TyFloat _ ->
      failwith ""
  | TyFunctionPtr _ ->
      failwith ""
  | TyClosure _ ->
      failwith ""
  | TyArray _ ->
      failwith ""
  | TyTuple _ ->
      failwith ""
  | TyOpaque _ ->
      failwith ""
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

let of_module_node kosu_program current_module = function
  | KosuFrontendAlt.Ast.NExternFunc e ->
      TysuAst.NExternFunc (of_external_decl kosu_program current_module e)
  | NFunction _ ->
      failwith ""
  | NStruct _ ->
      failwith ""
  | NEnum _ ->
      failwith ""
  | NConst _ ->
      failwith ""
  | NOpaque _ ->
      failwith ""

let of_module kosu_program kosu_module =
  List.map (of_module_node kosu_program kosu_module) kosu_module

let of_named_module kosu_program kosu_named_module =
  let KosuFrontendAlt.Ast.{ kosu_module; filename } = kosu_named_module in
  let tysu_module = of_module kosu_program kosu_module in
  TysuAst.{ tysu_module; filename }

let of_program kosu_program =
  List.map (of_named_module kosu_program) kosu_program

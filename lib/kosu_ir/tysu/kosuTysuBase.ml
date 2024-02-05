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

let of_kosu_type_solved :
    Kosu.Type.Ty.kosu_type Kosu.Env.KosuTypingSolution.t ->
    Util.Position.position ->
    Kosu.Type.Ty.kosu_type ->
    TysuType.tysu_type =
 fun solutions position kosu_type ->
  match of_kosu_type kosu_type with
  | t ->
      t
  | exception TysuError.(TysuError (KosuCompilerVariableFound s)) -> (
      let compiler_var = Kosu.Type.Ty.CompilerPolymorphicVar s in
      match Kosu.Env.KosuTypingSolution.find_opt compiler_var solutions with
      | Some t ->
          of_kosu_type t
      | None ->
          raise @@ Kosu.Error.Exn.cannot_infer_type position
    )

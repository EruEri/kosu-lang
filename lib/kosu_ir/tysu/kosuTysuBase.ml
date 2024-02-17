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

module Tysu = struct
  let of_module_resolver : Kosu.Base.module_resolver -> TysuBase.module_resolver
      = function
    | ModuleResolver_ modules ->
        ModuleResolver modules

  let of_polymorphic :
      Kosu.Type.Ty.kosu_type_polymorphic -> TysuType.tysu_variable_polymorphic =
    function
    | PolymorphicVar s ->
        TysuType.ForAllVar s
    | CompilerPolymorphicVar { name; hint = _ } ->
        raise @@ TysuError.kosu_compiler_variable_found name

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
    | TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintFloat }) ->
        TysuType.TysuFloat F32
    | TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintInteger }) ->
        TysuType.(TysuInteger (Sized (Signed, I32)))
    | TyPolymorphic variable ->
        let variable = of_polymorphic variable in
        TysuPolymorphic variable
    | TyPointer { pointer_state; pointee_type } ->
        let pointee_type = of_kosu_type pointee_type in
        TysuPointer { pointer_state; pointee_type }
    | TyInteger integer ->
        TysuInteger integer
    | TyFloat float_info ->
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
    | exception TysuError.(TysuError (KosuCompilerVariableFound name)) -> (
        let compiler_var =
          Kosu.Type.Ty.CompilerPolymorphicVar { name; hint = None }
        in
        match Kosu.Env.KosuTypingSolution.find_opt compiler_var solutions with
        | Some t ->
            of_kosu_type t
        | None ->
            raise @@ Kosu.Error.Exn.cannot_infer_type position
      )
end

module Kosu = struct
  let of_module_resolver : TysuBase.module_resolver -> Kosu.Base.module_resolver
      = function
    | ModuleResolver modules ->
        ModuleResolver_ modules

  let of_polymorphic :
      TysuType.tysu_variable_polymorphic -> Kosu.Type.Ty.kosu_type_polymorphic =
    function
    | ForAllVar s ->
        PolymorphicVar s

  let rec of_schema :
      TysuType.tysu_function_schema -> Kosu.Type.Ty.kosu_function_schema =
   fun { poly_vars; parameters_type; return_type } ->
    let poly_vars = List.map of_polymorphic poly_vars in
    let parameters_type = List.map of_tysu_type parameters_type in
    let return_type = of_tysu_type return_type in
    { poly_vars; parameters_type; return_type }

  and of_tysu_type : TysuType.tysu_type -> Kosu.Type.Ty.kosu_type = function
    | TysuIdentifier { module_resolver; parametrics_type; name } ->
        let parametrics_type = List.map of_tysu_type parametrics_type in
        let module_resolver = of_module_resolver module_resolver in
        TyIdentifier { module_resolver; parametrics_type; name }
    | TysuPolymorphic variable ->
        let variable = of_polymorphic variable in
        TyPolymorphic variable
    | TysuPointer { pointer_state; pointee_type } ->
        let pointee_type = of_tysu_type pointee_type in
        TyPointer { pointer_state; pointee_type }
    | TysuInteger integer ->
        TyInteger integer
    | TysuFloat float_info ->
        TyFloat float_info
    | TysuFunctionPtr schema ->
        let schema = of_schema schema in
        TyFunctionPtr schema
    | TysuClosure schema ->
        let schema = of_schema schema in
        TyClosure schema
    | TysuArray { tysu_type; size } ->
        let ktype = of_tysu_type tysu_type in
        TyArray { ktype; size }
    | TysuTuple types ->
        let types = List.map of_tysu_type types in
        TyTuple types
    | TysuOpaque { module_resolver; name } ->
        let module_resolver = of_module_resolver module_resolver in
        TyOpaque { module_resolver; name }
    | TysuOrdered ->
        TyOrdered
    | TysuStringLit ->
        TyStringLit
    | TysuChar ->
        TyChar
    | TysuBool ->
        TyBool
    | TysuUnit ->
        TyUnit
end

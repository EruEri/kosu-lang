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

module ModuleResolver = struct
  open KosuAst

  let is_empty = function
    | ModuleResolverLoc [] ->
        true
    | ModuleResolverLoc (_ :: _) ->
        false

  let empty_module = ModuleResolverLoc []

  let to_unlocated = function
    | ModuleResolverLoc l ->
        ModuleResolver_ (List.map Position.value l)
end

module IntegerInfo = struct
  let sized (sign, size) = KosuBaseAst.Sized (sign, size)
  let worded sign = KosuBaseAst.Worded sign
  let sworded sign = worded (Some sign)
end

module LocType = struct
  open KosuType.TyLoc
  open IntegerInfo

  let isize_8 = Some KosuAst.I8
  let isize_16 = Some KosuAst.I16
  let isize_32 = Some KosuAst.I32
  let isize_64 = Some KosuAst.I64
  let fsize_32 = Some KosuAst.F32
  let fsize_64 = Some KosuAst.F64
  let signed = Some KosuAst.Signed
  let unsigned = Some KosuAst.Unsigned
  let s8 = TyLocInteger (Some (sized @@ (signed, isize_8)))
  let u8 = TyLocInteger (Some (sized @@ (unsigned, isize_8)))
  let s16 = TyLocInteger (Some (sized @@ (signed, isize_16)))
  let u16 = TyLocInteger (Some (sized @@ (unsigned, isize_16)))
  let s32 = TyLocInteger (Some (sized @@ (signed, isize_32)))
  let u32 = TyLocInteger (Some (sized @@ (unsigned, isize_32)))
  let s64 = TyLocInteger (Some (sized @@ (signed, isize_64)))
  let u64 = TyLocInteger (Some (sized @@ (unsigned, isize_64)))
  let f32 = TyLocFloat fsize_32
  let f64 = TyLocFloat fsize_64
  let usize = TyLocInteger (Some (sworded KosuAst.Unsigned))
  let ssize = TyLocInteger (Some (sworded KosuAst.Unsigned))
end

module Ty = struct
  open Position
  open KosuType

  let is_integer : KosuType.Ty.kosu_type -> bool = function
    | Ty.TyInteger _ ->
        true
    | TyParametricIdentifier _
    | TyIdentifier _
    | TyPolymorphic _
    | TyFunctionPtr _
    | TyClosure _
    | TyOpaque _
    | TyFloat _
    | TyOrdered
    | TyChar
    | TyStringLit
    | TyUnit
    | TyPointer _
    | TyInnerClosureId _
    | TyArray _
    | TyTuple _
    | TyBool ->
        false

  let is_polymorphic : KosuType.Ty.kosu_type -> bool = function
    | TyPolymorphic _ ->
        true
    | TyParametricIdentifier _
    | TyIdentifier _
    | TyInteger _
    | TyFunctionPtr _
    | TyClosure _
    | TyOpaque _
    | TyFloat _
    | TyOrdered
    | TyChar
    | TyStringLit
    | TyUnit
    | TyPointer _
    | TyInnerClosureId _
    | TyArray _
    | TyTuple _
    | TyBool ->
        false

  let rec contains_polymorphic : KosuType.Ty.kosu_type -> bool = function
    | TyPolymorphic _ ->
        true
    | TyFunctionPtr (parameters, return_type)
    | TyClosure (parameters, return_type)
    (* Should captured variabe be in the compuation ? *)
    | TyInnerClosureId (ClosureType { id = _; env = _; parameters; return_type })
      ->
        let lhs = List.exists contains_polymorphic parameters in
        let rhs = contains_polymorphic return_type in
        lhs || rhs
    | TyParametricIdentifier
        { parametrics_type = tys; module_resolver = _; name = _ }
    | TyTuple tys ->
        List.exists contains_polymorphic tys
    | TyPointer { pointee_type = ktype; pointer_state = _ }
    | TyArray { ktype; size = _ } ->
        contains_polymorphic ktype
    | TyIdentifier _
    | TyInteger _
    | TyOpaque _
    | TyFloat _
    | TyOrdered
    | TyChar
    | TyStringLit
    | TyUnit
    | TyBool ->
        false

  let rec of_tyloc' tyloc = of_tyloc @@ value tyloc

  and of_tyloc : KosuType.TyLoc.kosu_loctype -> KosuType.Ty.kosu_type = function
    | TyLoc.TyLocParametricIdentifier
        { module_resolver; parametrics_type; name } ->
        let module_resolver = ModuleResolver.to_unlocated module_resolver in
        let parametrics_type = List.map of_tyloc' parametrics_type in
        let name = Position.value name in
        Ty.TyParametricIdentifier { module_resolver; parametrics_type; name }
    | TyLocIdentifier { module_resolver; name } ->
        let module_resolver = ModuleResolver.to_unlocated module_resolver in
        let name = Position.value name in
        Ty.TyIdentifier { module_resolver; name }
    | TyLocPolymorphic poly ->
        TyPolymorphic (of_tyloc_polymorphic poly)
    | TyLocFunctionPtr (parameters, return) ->
        TyFunctionPtr (List.map of_tyloc' parameters, of_tyloc' return)
    | TyLocClosure (parameters, return) ->
        TyClosure (List.map of_tyloc' parameters, of_tyloc' return)
    | TyLocOpaque { module_resolver; name } ->
        let module_resolver = ModuleResolver.to_unlocated module_resolver in
        let name = value name in
        TyOpaque { module_resolver; name }
    | TyLocPointer { pointer_state; pointee_type } ->
        let pointee_type = of_tyloc' pointee_type in
        TyPointer { pointer_state; pointee_type }
    | TyLocInteger info ->
        TyInteger info
    | TyLocFloat size ->
        TyFloat size
    | TyLocTuple elts ->
        TyTuple (List.map of_tyloc' elts)
    | TyLocArray { ktype; size } ->
        let ktype = of_tyloc' ktype in
        let size = value size in
        TyArray { ktype; size }
    | TyLocStringLit ->
        TyStringLit
    | TyLocChar ->
        TyChar
    | TyLocBool ->
        TyBool
    | TyLocUnit ->
        TyUnit
    | TyLocOrdered ->
        TyOrdered

  and of_tyloc_polymorphic = function
    | TyLoc.PolymorphicVarLoc s ->
        Ty.PolymorphicVar s.value
end

module Pattern = struct
  open KosuAst

  let rec flatten_por (pattern : kosu_pattern Position.location) =
    match pattern.value with
    | POr patterns ->
        patterns |> List.map flatten_por |> List.flatten
    | _ ->
        pattern :: []
end

module Expression = struct end

module Module = struct
  let constant_decls kosu_module =
    let open KosuAst in
    kosu_module
    |> List.filter_map (function
         | NConst kosu_const_decl ->
             Some kosu_const_decl
         | NExternFunc _
         | NEnum _
         | NFunction _
         | NSyscall _
         | NStruct _
         | NOpaque _ ->
             None
         )

  let enum_decls kosu_module =
    let open KosuAst in
    kosu_module
    |> List.filter_map (function
         | NEnum kosu_enum_decl ->
             Some kosu_enum_decl
         | NExternFunc _
         | NConst _
         | NFunction _
         | NSyscall _
         | NStruct _
         | NOpaque _ ->
             None
         )
end

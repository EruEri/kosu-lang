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

(* module IntegerInfo = struct
     let sized (sign, size) = KosuBaseAst.Sized (sign, size)

     let worded sign = KosuBaseAst.Worded (sign)
     let sworded sign = worded (Some sign)


   end *)

module LocType = struct
  open KosuType.TyLoc

  let isize_8 = Some KosuAst.I8
  let isize_16 = Some KosuAst.I16
  let isize_32 = Some KosuAst.I32
  let isize_64 = Some KosuAst.I64
  let fsize_32 = Some KosuAst.F32
  let fsize_64 = Some KosuAst.F64
  let signed = Some KosuAst.Signed
  let unsigned = Some KosuAst.Unsigned
  let s8 = TyLocInteger (signed, isize_8)
  let u8 = TyLocInteger (unsigned, isize_8)
  let s16 = TyLocInteger (signed, isize_16)
  let u16 = TyLocInteger (unsigned, isize_16)
  let s32 = TyLocInteger (signed, isize_32)
  let u32 = TyLocInteger (unsigned, isize_32)
  let s64 = TyLocInteger (signed, isize_64)
  let u64 = TyLocInteger (unsigned, isize_64)
  let f32 = TyLocFloat fsize_32
  let f64 = TyLocFloat fsize_64
  let usize = KosuAst.(TyLocPointerSize Unsigned)
  let ssize = KosuAst.(TyLocPointerSize Signed)
end

module Ty = struct
  open Position
  open KosuType

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
    | TyLocInteger (sign, size) ->
        TyInteger (sign, size)
    | TyLocPointerSize sign ->
        TyPointerSize sign
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

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

  let of_filename name =
    (* Should be reversed since file are stacked in reverse order*)
    let rec split s =
      let root_dir = "." in
      let basename =
        s |> Filename.basename |> Filename.remove_extension
        |> String.capitalize_ascii
      in
      let dirname = Filename.dirname s in
      match dirname = root_dir with
      | true ->
          basename :: []
      | false ->
          basename :: split dirname
    in
    let modules = List.rev @@ split name in
    ModuleResolver_ modules

  let to_unlocated = function
    | ModuleResolverLoc l ->
        ModuleResolver_ (List.map Position.value l)

  let dummy_located = function
    | ModuleResolver_ modules ->
        ModuleResolverLoc (List.map Position.dummy_located modules)
end

module IntegerInfo = struct
  let sized (sign, size) = KosuBaseAst.Sized (sign, size)
  let worded sign = KosuBaseAst.Worded sign
end

module TyLoc = struct
  open KosuType.TyLoc
  open IntegerInfo

  let isize_8 = KosuAst.I8
  let isize_16 = KosuAst.I16
  let isize_32 = KosuAst.I32
  let isize_64 = KosuAst.I64
  let fsize_32 = Some KosuAst.F32
  let fsize_64 = Some KosuAst.F64
  let signed = KosuAst.Signed
  let unsigned = KosuAst.Unsigned
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
  let usize = TyLocInteger (Some (worded unsigned))
  let ssize = TyLocInteger (Some (worded signed))

  (**
    [tyloc_substitution bound assoc_types ty] replace the type variable occurences in [ty] 
    by there value associated in [assoc_types] and not in [bound]

    [bound] is useful for function signature where type variable can be bound to the function signature
    (.ie for all 'a . ..)
  *)
  let rec tyloc_substitution bound assoc_types ty =
    match ty.Position.value with
    | TyLocPolymorphic (PolymorphicVarLoc variable) as t ->
        let assoc_type =
          List.find_map
            (fun (PolymorphicVarLoc s, ty) ->
              match s.value = variable.value with
              | true ->
                  Some ty
              | false ->
                  None
            )
            assoc_types
        in
        let is_bound =
          List.exists
            (fun (PolymorphicVarLoc s) -> s.value = variable.value)
            bound
        in
        let ty =
          match (assoc_type, is_bound) with
          | Some ty, false ->
              ty
          | Some _, true | None, (true | false) ->
              t
        in
        ty
    | TyLocIdentifier { module_resolver; parametrics_type; name } ->
        TyLocIdentifier
          {
            module_resolver;
            parametrics_type =
              List.map
                (tyloc_substitution_map bound assoc_types)
                parametrics_type;
            name;
          }
    | TyLocFunctionPtr schema ->
        let schema = tyloc_substitution_schema bound assoc_types schema in
        TyLocFunctionPtr schema
    | TyLocClosure schema ->
        let schema = tyloc_substitution_schema bound assoc_types schema in
        TyLocClosure schema
    | TyLocPointer { pointer_state; pointee_type } ->
        let pointee_type =
          tyloc_substitution_map bound assoc_types pointee_type
        in
        TyLocPointer { pointer_state; pointee_type }
    | TyLocArray { ktype; size } ->
        let ktype = tyloc_substitution_map bound assoc_types ktype in
        TyLocArray { ktype; size }
    | TyLocTuple ttes ->
        let ttes = List.map (tyloc_substitution_map bound assoc_types) ttes in
        TyLocTuple ttes
    | ( TyLocBool
      | TyLocUnit
      | TyLocFloat _
      | TyLocOrdered
      | TyLocChar
      | TyLocStringLit
      | TyLocOpaque { module_resolver = _; name = _ }
      | TyLocInteger (None | Some (Worded _ | Sized (_, _))) ) as ty ->
        ty

  and tyloc_substitution_schema bound assoc_type = function
    | { poly_vars; parameters_type; return_type } as e ->
        let bound = poly_vars @ bound in
        let parameters_type =
          List.map (tyloc_substitution_map bound assoc_type) parameters_type
        in
        let return_type = tyloc_substitution_map bound assoc_type return_type in
        { e with parameters_type; return_type }

  and tyloc_substitution_map bound assoc_types =
    Position.map_use @@ fun ty -> tyloc_substitution bound assoc_types ty
end

module Ty = struct
  open Position
  open KosuType

  let is_integer : KosuType.Ty.kosu_type -> bool = function
    | Ty.TyInteger _ ->
        true
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
    | TyFunctionPtr schema
    | TyClosure schema
    (* Should captured variabe be in the compuation ? *)
    | TyInnerClosureId (ClosureType { id = _; env = _; schema }) ->
        let lhs = List.exists contains_polymorphic schema.parameters_type in
        let rhs = contains_polymorphic schema.return_type in
        lhs || rhs
    | TyIdentifier { parametrics_type = tys; module_resolver = _; name = _ }
    | TyTuple tys ->
        List.exists contains_polymorphic tys
    | TyPointer { pointee_type = ktype; pointer_state = _ }
    | TyArray { ktype; size = _ } ->
        contains_polymorphic ktype
    | TyInteger _
    | TyOpaque _
    | TyFloat _
    | TyOrdered
    | TyChar
    | TyStringLit
    | TyUnit
    | TyBool ->
        false

  let parametrics_type = function
    | Ty.TyIdentifier { parametrics_type; module_resolver = _; name = _ } ->
        parametrics_type
    | TyPolymorphic _
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
        []

  let rec of_tyloc' tyloc = of_tyloc @@ value tyloc

  and of_tyloc : KosuType.TyLoc.kosu_loctype -> KosuType.Ty.kosu_type = function
    | TyLoc.TyLocIdentifier { module_resolver; parametrics_type; name } ->
        let module_resolver = ModuleResolver.to_unlocated module_resolver in
        let parametrics_type = List.map of_tyloc' parametrics_type in
        let name = Position.value name in
        Ty.TyIdentifier { module_resolver; parametrics_type; name }
    | TyLocPolymorphic poly ->
        TyPolymorphic (of_tyloc_polymorphic poly)
    | TyLocFunctionPtr schema ->
        TyFunctionPtr (of_schema schema)
    | TyLocClosure schema ->
        TyClosure (of_schema schema)
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

  and of_schema = function
    | { poly_vars; parameters_type; return_type } ->
        let poly_vars = List.map of_tyloc_polymorphic poly_vars in
        let parameters_type = List.map of_tyloc' parameters_type in
        let return_type = of_tyloc' return_type in
        { poly_vars; parameters_type; return_type }

  (**
    [tyloc_substitution bound assoc_types ty] replace the type variable occurences in [ty] 
    by there value associated in [assoc_types] and not in [bound]

    [bound] is useful for function signature where type variable can be bound to the function signature
    (.ie for all 'a . ..)
  *)
  let rec ty_substitution bound assoc_types ty =
    match ty with
    | Ty.TyPolymorphic variable as t ->
        let assoc_type =
          List.find_map
            (fun (s, ty) ->
              match s = variable with true -> Some ty | false -> None
            )
            assoc_types
        in
        let is_bound = List.exists (( = ) variable) bound in
        let ty =
          match (assoc_type, is_bound) with
          | Some ty, false ->
              ty
          | Some _, true | None, (true | false) ->
              t
        in
        ty
    | TyIdentifier { module_resolver; parametrics_type; name } ->
        TyIdentifier
          {
            module_resolver;
            parametrics_type =
              List.map (ty_substitution bound assoc_types) parametrics_type;
            name;
          }
    | TyFunctionPtr schema ->
        let schema = ty_substitution_schema bound assoc_types schema in
        TyFunctionPtr schema
    | TyClosure schema ->
        let schema = ty_substitution_schema bound assoc_types schema in
        TyClosure schema
    | TyPointer { pointer_state; pointee_type } ->
        let pointee_type = ty_substitution bound assoc_types pointee_type in
        TyPointer { pointer_state; pointee_type }
    | TyInnerClosureId (ClosureType { id = _; schema = _; env = _ } as ct) ->
        let closure_type = ty_substitution_closure_type bound assoc_types ct in
        TyInnerClosureId closure_type
    | TyArray { ktype; size } ->
        let ktype = ty_substitution bound assoc_types ktype in
        TyArray { ktype; size }
    | TyTuple ttes ->
        let ttes = List.map (ty_substitution bound assoc_types) ttes in
        TyTuple ttes
    | ( TyBool
      | TyUnit
      | TyFloat _
      | TyOrdered
      | TyChar
      | TyStringLit
      | TyOpaque { module_resolver = _; name = _ }
      | TyInteger (None | Some (Worded _ | Sized (_, _))) ) as ty ->
        ty

  and ty_substitution_schema bound assoc_type = function
    | { poly_vars; parameters_type; return_type } as e ->
        let bound = poly_vars @ bound in
        let parameters_type =
          List.map (ty_substitution bound assoc_type) parameters_type
        in
        let return_type = ty_substitution bound assoc_type return_type in
        { e with parameters_type; return_type }

  and ty_substitution_closure_type bound assoc_type = function
    | ClosureType
        ( {
            id = _;
            schema = { poly_vars; parameters_type; return_type } as aschema;
            env;
          } as e
        ) ->
        let bound = poly_vars @ bound in
        let parameters_type =
          List.map (ty_substitution bound assoc_type) parameters_type
        in
        let return_type = ty_substitution bound assoc_type return_type in
        let schema = { aschema with parameters_type; return_type } in
        let env =
          List.map
            (fun (name, ty) ->
              let ty = ty_substitution bound assoc_type ty in
              (name, ty)
            )
            env
        in
        ClosureType { e with schema; env }
end

module Struct = struct
  (**
      [substitution module_resolver types kosu_struct_decl] replaces the type variable in 
      [kosu_struct_decl] by there value in [types].
      Returns [None] if [types] hasn't the same length as [kosu_struct_decl.poly_vars]
  *)
  let substitution module_resolver types
      (kosu_struct_decl : KosuAst.kosu_struct_decl) =
    let ( let* ) = Option.bind in
    let* assoc =
      match List.combine kosu_struct_decl.poly_vars types with
      | assoc ->
          Some assoc
      | exception _ ->
          None
    in
    let fields =
      List.map
        (fun (name, ty) ->
          let ty = TyLoc.tyloc_substitution_map [] assoc ty in
          (name, ty)
        )
        kosu_struct_decl.fields
    in
    let ty =
      KosuType.TyLoc.TyLocIdentifier
        {
          module_resolver = ModuleResolver.dummy_located module_resolver;
          parametrics_type = List.map Position.dummy_located types;
          name = kosu_struct_decl.struct_name;
        }
    in
    Option.some @@ ({ kosu_struct_decl with fields }, ty)

  (**
    [substitution_fresh ~fresh module_resolver kosu_struct_decl] substitutes the type variable
    in [kosu_struct_decl] by the values provided by [fresh]
  *)
  let substitution_fresh ~fresh module_resolver
      (kosu_struct_decl : KosuAst.kosu_struct_decl) =
    let fresh_variable =
      List.map (fun _ -> fresh ()) kosu_struct_decl.poly_vars
    in
    Option.get @@ substitution module_resolver fresh_variable kosu_struct_decl
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

  let struct_decls kosu_module =
    let open KosuAst in
    kosu_module
    |> List.filter_map (function
         | NStruct kosu_struct_decl ->
             Some kosu_struct_decl
         | NExternFunc _
         | NConst _
         | NFunction _
         | NSyscall _
         | NEnum _
         | NOpaque _ ->
             None
         )

  let type_decl_from_name name =
    let open KosuAst in
    List.filter_map (function
      | NEnum ({ enum_name; _ } as enum_decl) when enum_name.value = name ->
          Option.some @@ DEnum enum_decl
      | NStruct ({ struct_name; _ } as strucT_decl)
        when struct_name.value = name ->
          Option.some @@ DStruct strucT_decl
      | NEnum _
      | NStruct _
      | NConst _
      | NOpaque _
      | NFunction _
      | NExternFunc _
      | NSyscall _ ->
          None
      )
end

module Program = struct
  let find_module_opt (KosuBaseAst.ModuleResolverLoc modules) kosu_program =
    let open KosuAst in
    let module_name = Position.filename_of_module modules in
    kosu_program
    |> List.find_map (fun { filename; kosu_module } ->
           match filename = module_name with
           | true ->
               Some kosu_module
           | false ->
               None
       )

  (**
    [type_decl kosu_type ~curent_module kosu_program] retuns all type declarations
    [`enum`] or [`struct`] for the type [kosu_type] in [kosu_program]
    return [None] if [kosu_type] is not a [TyIdentifier]
  *)
  let type_decl kosu_type ~curent_module kosu_program =
    let open KosuType in
    match kosu_type with
    | Ty.TyIdentifier { module_resolver; parametrics_type = _; name } ->
        let kosu_module =
          find_module_opt
            (ModuleResolver.dummy_located module_resolver)
            kosu_program
        in
        let kosu_module = Option.value ~default:curent_module kosu_module in
        let decls = Module.type_decl_from_name name kosu_module in
        Some decls
    | TyPolymorphic _
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
        None
end

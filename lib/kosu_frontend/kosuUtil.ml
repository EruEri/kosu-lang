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

module KosuTypeVariableSet = Set.Make (struct
  type t = KosuType.Ty.kosu_type_polymorphic

  let compare = Stdlib.compare
end)

module KosuTypeVariableLocSet = Set.Make (struct
  type t = KosuType.TyLoc.kosu_loctype_polymorphic

  let compare (KosuType.TyLoc.PolymorphicVarLoc lhs)
      (KosuType.TyLoc.PolymorphicVarLoc rhs) =
    String.compare lhs.value rhs.value
end)

module ModuleResolver = struct
  open KosuAst

  let is_empty = function
    | ModuleResolverLoc [] ->
        true
    | ModuleResolverLoc (_ :: _) ->
        false

  let empty_module = ModuleResolverLoc []

  let of_filename_raw name =
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
    modules

  let of_filename name = ModuleResolver_ (of_filename_raw name)

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

  (**
    [polymorphic_vars acc kosuloc_type] accumules all polymorphics variables present within [kosuloc_type],
    in [acc]
  *)
  let rec polymorphic_vars bound acc kosuloc_type =
    let open KosuType.TyLoc in
    match kosuloc_type with
    | TyLocPolymorphic poly ->
        let r =
          match KosuTypeVariableLocSet.mem poly bound with
          | true ->
              acc
          | false ->
              KosuTypeVariableLocSet.add poly acc
        in
        r
    | TyLocIdentifier { module_resolver = _; parametrics_type = ttes; name = _ }
    | TyLocTuple ttes ->
        let set = List.map (polymorphic_vars' bound acc) ttes in
        List.fold_left KosuTypeVariableLocSet.union KosuTypeVariableLocSet.empty
          set
    | TyLocFunctionPtr schema | TyLocClosure schema ->
        polymorphic_vars_of_schema bound acc schema
    | TyLocOpaque _ ->
        acc
    | TyLocPointer { pointer_state = _; pointee_type = kt }
    | TyLocArray { ktype = kt; size = _ } ->
        polymorphic_vars' bound acc kt
    | TyLocInteger _ ->
        acc
    | TyLocFloat _ ->
        acc
    | TyLocStringLit ->
        acc
    | TyLocChar ->
        acc
    | TyLocBool ->
        acc
    | TyLocUnit ->
        acc
    | TyLocOrdered ->
        acc

  and polymorphic_vars' bound acc loc_type =
    polymorphic_vars bound acc @@ Position.value loc_type

  and polymorphic_vars_of_schema bound acc = function
    | { poly_vars; parameters_type; return_type } ->
        let locale_bound = KosuTypeVariableLocSet.of_list poly_vars in
        let bound = KosuTypeVariableLocSet.union bound locale_bound in

        let set = List.map (polymorphic_vars' bound acc) parameters_type in
        let singleton = polymorphic_vars' bound acc return_type in
        List.fold_left KosuTypeVariableLocSet.union singleton set

  let rec explicit_module_type' current_module tyloc =
    Position.map (explicit_module_type current_module) tyloc

  and explicit_module_type current_module : kosu_loctype -> kosu_loctype =
    function
    | TyLocIdentifier { module_resolver; parametrics_type; name } ->
        let module_resolver =
          match module_resolver with
          | KosuAst.ModuleResolverLoc [] ->
              current_module
          | KosuAst.ModuleResolverLoc (_ :: _) as m ->
              m
        in
        let parametrics_type =
          List.map (explicit_module_type' current_module) parametrics_type
        in
        TyLocIdentifier { module_resolver; parametrics_type; name }
    | TyLocPolymorphic _ as e ->
        e
    | TyLocPointer { pointer_state; pointee_type } ->
        let pointee_type = explicit_module_type' current_module pointee_type in
        TyLocPointer { pointer_state; pointee_type }
    | TyLocInteger _ as ti ->
        ti
    | TyLocFloat _ as fi ->
        fi
    | TyLocFunctionPtr schema ->
        let schema = explicit_module_type_schema current_module schema in
        TyLocFunctionPtr schema
    | TyLocClosure schema ->
        let schema = explicit_module_type_schema current_module schema in
        TyLocClosure schema
    | TyLocArray { ktype; size } ->
        let ktype = explicit_module_type' current_module ktype in
        TyLocArray { ktype; size }
    | TyLocTuple ttes ->
        let ttes = List.map (explicit_module_type' current_module) ttes in
        TyLocTuple ttes
    | TyLocOpaque { module_resolver; name } ->
        let module_resolver =
          match module_resolver with
          | KosuAst.ModuleResolverLoc [] ->
              current_module
          | KosuAst.ModuleResolverLoc (_ :: _) as m ->
              m
        in
        TyLocOpaque { module_resolver; name }
    | (TyLocOrdered | TyLocStringLit | TyLocChar | TyLocBool | TyLocUnit) as e
      ->
        e

  and explicit_module_type_schema current_module = function
    | { poly_vars; parameters_type; return_type } ->
        let parameters_type =
          List.map (explicit_module_type' current_module) parameters_type
        in
        let return_type = explicit_module_type' current_module return_type in
        { poly_vars; parameters_type; return_type }
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
    | TyArray _
    | TyTuple _
    | TyBool ->
        false

  let rec contains_polymorphic : KosuType.Ty.kosu_type -> bool = function
    | TyPolymorphic _ ->
        true
    | TyFunctionPtr schema | TyClosure schema ->
        let lhs () = List.exists contains_polymorphic schema.parameters_type in
        let rhs = contains_polymorphic schema.return_type in
        rhs || lhs ()
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
    | TyArray _
    | TyTuple _
    | TyBool ->
        []

  (**
      [is_number_unknwon_size ty] returns if the size of the numerical type [integer] or [float]
      aren't fully infered
  *)
  let is_number_unknwon_size = function
    | Ty.TyFloat None | Ty.TyInteger None ->
        true
    | Ty.TyIdentifier _
    | TyPolymorphic _
    | TyInteger (Some _)
    | TyFloat (Some _)
    | TyFunctionPtr _
    | TyClosure _
    | TyOpaque _
    | TyOrdered
    | TyChar
    | TyStringLit
    | TyUnit
    | TyPointer _
    | TyArray _
    | TyTuple _
    | TyBool ->
        false

  (**
      [are_number_compatible lhs rhs] compares if number information is compatible.
        - [are_number_compatible Some None] returns [true]
        - [are_number_compatible None Some] returns [true]
        - [are_number_compatible None None] returns [true]
        - [are_number_compatible (Some l) (Some r)] returns [l = r]
  *)
  let are_number_compatible lhs rhs =
    match (lhs, rhs) with
    | None, (Some _ | None) | Some _, None ->
        true
    | Some lhs, Some rhs ->
        lhs = rhs

  let to_quantified_ty_vars = function
    | Ty.CompilerPolymorphicVar s ->
        Ty.PolymorphicVar s
    | Ty.PolymorphicVar _ as e ->
        e

  let rec generalize : Ty.kosu_type -> Ty.kosu_type = function
    | TyPolymorphic p ->
        TyPolymorphic (to_quantified_ty_vars p)
    | TyFunctionPtr schema ->
        let return_type = generalize schema.return_type in
        let parameters_type = List.map generalize schema.parameters_type in
        let poly_vars = schema.poly_vars in
        TyFunctionPtr { return_type; poly_vars; parameters_type }
    | TyClosure schema ->
        let return_type = generalize schema.return_type in
        let parameters_type = List.map generalize schema.parameters_type in
        let poly_vars = schema.poly_vars in
        TyClosure { return_type; poly_vars; parameters_type }
    | TyPointer { pointee_type; pointer_state } ->
        let pointee_type = generalize pointee_type in
        TyPointer { pointee_type; pointer_state }
    | TyArray { size; ktype } ->
        let ktype = generalize ktype in
        TyArray { size; ktype }
    | TyTuple ttys ->
        TyTuple (List.map generalize ttys)
    | ( TyIdentifier _
      | TyInteger _
      | TyOpaque _
      | TyFloat _
      | TyOrdered
      | TyChar
      | TyStringLit
      | TyUnit
      | TyBool ) as t ->
        t

  let rec quantified_ty_vars bound acc ty =
    let open Ty in
    match ty with
    | Ty.TyPolymorphic (PolymorphicVar _ as variable) ->
        if List.mem variable bound then
          acc
        else
          KosuTypeVariableSet.add variable acc
    | Ty.TyPolymorphic (CompilerPolymorphicVar _) ->
        acc
    | TyTuple ttes
    | TyIdentifier { module_resolver = _; parametrics_type = ttes; name = _ } ->
        List.fold_left (quantified_ty_vars bound) acc ttes
    | TyFunctionPtr schema | TyClosure schema ->
        let bound = schema.poly_vars @ bound in
        let acc =
          List.fold_left (quantified_ty_vars bound) acc schema.parameters_type
        in
        quantified_ty_vars bound acc schema.return_type
    | TyPointer { pointer_state = _; pointee_type = ty }
    | TyArray { ktype = ty; size = _ } ->
        quantified_ty_vars bound acc ty
    | TyBool
    | TyUnit
    | TyFloat _
    | TyOrdered
    | TyChar
    | TyStringLit
    | TyOpaque { module_resolver = _; name = _ }
    | TyInteger _ ->
        acc

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
    [tyloc_substitution bound assoc_types ty] replaces the type variable occurences in [ty] 
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
        Option.value ~default:t assoc_type
        (* The variable needs to be bound in order to be substitutate *)
        (* let is_bound = List.exists (( = ) variable) bound in
           let ty =
             match (assoc_type, is_bound) with
             | Some ty, true ->
                 ty
             | Some _, false | None, (true | false) ->
                 t
           in
           ty *)
    | TyIdentifier { module_resolver; parametrics_type; name } ->
        TyIdentifier
          {
            module_resolver;
            parametrics_type =
              List.map (ty_substitution bound assoc_types) parametrics_type;
            name;
          }
    | TyFunctionPtr schema ->
        let schema = ty_substitution_schema assoc_types schema in
        TyFunctionPtr schema
    | TyClosure schema ->
        let schema = ty_substitution_schema assoc_types schema in
        TyClosure schema
    | TyPointer { pointer_state; pointee_type } ->
        let pointee_type = ty_substitution bound assoc_types pointee_type in
        TyPointer { pointer_state; pointee_type }
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

  and ty_substitution_schema assoc_type = function
    | { poly_vars; parameters_type; return_type } as e ->
        let bound = poly_vars in
        let parameters_type =
          List.map (ty_substitution bound assoc_type) parameters_type
        in
        let return_type = ty_substitution bound assoc_type return_type in
        { e with parameters_type; return_type }

  let schema_instanciate ~fresh (schema : Ty.kosu_function_schema) =
    let fresh_variables = List.map (fun _ -> fresh ()) schema.poly_vars in
    let assoc_poly_fresh = List.combine schema.poly_vars fresh_variables in
    let schema = ty_substitution_schema assoc_poly_fresh schema in
    { schema with poly_vars = [] }

  (**
    [ty_instanciate ~fresh ty] instanciates callable type schema [Ty.TyFunctionPtr] 
    and [Ty.TyClosure] by substituing quantified types by types created by [fresh]
*)
  let ty_instanciate ~fresh = function
    | Ty.TyFunctionPtr schema ->
        let schema = schema_instanciate ~fresh schema in
        Ty.TyFunctionPtr schema
    | TyClosure schema ->
        let schema = schema_instanciate ~fresh schema in
        Ty.TyClosure schema
    | ( TyOrdered
      | TyStringLit
      | TyChar
      | TyBool
      | TyUnit
      | TyIdentifier { module_resolver = ModuleResolver_ _; _ }
      | TyPolymorphic (PolymorphicVar _ | CompilerPolymorphicVar _)
      | TyPointer _
      | TyInteger _
      | TyFloat _
      | TyArray _
      | TyTuple _
      | TyOpaque { module_resolver = ModuleResolver_ _; _ } ) as t ->
        t

  let return_type : KosuType.Ty.kosu_type -> KosuType.Ty.kosu_type option =
    function
    | TyFunctionPtr schema | TyClosure schema ->
        Some schema.return_type
    | TyOrdered
    | TyStringLit
    | TyChar
    | TyBool
    | TyUnit
    | TyIdentifier { module_resolver = ModuleResolver_ _; _ }
    | TyPolymorphic (PolymorphicVar _ | CompilerPolymorphicVar _)
    | TyPointer _
    | TyInteger _
    | TyFloat _
    | TyArray _
    | TyTuple _
    | TyOpaque { module_resolver = ModuleResolver_ _; _ } ->
        None

  let ty_of_callable : KosuAst.kosu_callable_decl -> Ty.kosu_type = function
    | CdExternalFunction { parameters; return_type; _ } ->
        let poly_vars = [] in
        let parameters_type = List.map of_tyloc' parameters in
        let return_type = of_tyloc' return_type in
        TyFunctionPtr { poly_vars; parameters_type; return_type }
    | CdKosuFuntion { poly_vars; parameters; return_type; _ } ->
        let poly_vars = List.map of_tyloc_polymorphic poly_vars in
        let parameters_type =
          List.map
            (fun KosuAst.{ kosu_type; _ } -> of_tyloc' kosu_type)
            parameters
        in
        let return_type = of_tyloc' return_type in
        TyFunctionPtr { poly_vars; parameters_type; return_type }
end

module Enum = struct
  let to_unlocated (enum_decl : KosuAst.kosu_enum_decl) :
      KosuAst.kosu_raw_enum_decl =
    let enum_name = enum_decl.enum_name.value in
    let poly_vars = List.map Ty.of_tyloc_polymorphic enum_decl.poly_vars in
    let tag_type = Ty.of_tyloc' enum_decl.tag_type in
    let variants =
      List.map
        (fun (variant, assoc_type) ->
          let tytes = List.map Ty.of_tyloc' assoc_type in
          let variant = Position.value variant in
          (variant, tytes)
        )
        enum_decl.variants
    in
    { enum_name; poly_vars; tag_type; variants }

  (**
      [substitution module_resolver types kosu_enum_decl] replaces the type variable in 
      [kosu_enum_decl] by there value in [types].
      Returns [None] if [types] hasn't the same length as [kosu_enum_decl.poly_vars]
  *)
  let substitution module_resolver types
      (kosu_enum_decl : KosuAst.kosu_enum_decl) =
    let kosu_enum_decl = to_unlocated kosu_enum_decl in
    let ( let* ) = Option.bind in
    let* assoc =
      match List.combine kosu_enum_decl.poly_vars types with
      | assoc ->
          Some assoc
      | exception _ ->
          None
    in
    let variants =
      List.map
        (fun (name, assoc_types) ->
          let assoc_types =
            List.map
              (Ty.ty_substitution kosu_enum_decl.poly_vars assoc)
              assoc_types
          in
          (name, assoc_types)
        )
        kosu_enum_decl.variants
    in

    let ty =
      KosuType.Ty.TyIdentifier
        {
          module_resolver;
          parametrics_type = types;
          name = kosu_enum_decl.enum_name;
        }
    in
    Option.some @@ ({ kosu_enum_decl with variants }, ty)

  (**
    [substitution_fresh ~fresh module_resolver kosu_enum_decl] substitutes the type variable
    in [kosu_enum_decl] by the values provided by [fresh]
  *)
  let substitution_fresh ~fresh module_resolver
      (kosu_enum_decl : KosuAst.kosu_enum_decl) =
    let fresh_variable =
      List.map (fun _ -> fresh ()) kosu_enum_decl.poly_vars
    in
    Option.get @@ substitution module_resolver fresh_variable kosu_enum_decl

  (**
      [assoc_types variant kosu_enum_decl] returns the associated types for the variant [variant] in
      [kosu_enum_decl]

      Return [None], if [variant] isn't a variant of [kosu_enum_decl]
  *)
  let assoc_types variant (kosu_enum_decl : KosuAst.kosu_raw_enum_decl) =
    List.assoc_opt variant kosu_enum_decl.variants
end

module Struct = struct
  let to_unlocated (struct_decl : KosuAst.kosu_struct_decl) :
      KosuAst.kosu_raw_struct_decl =
    let struct_name = struct_decl.struct_name.value in
    let poly_vars = List.map Ty.of_tyloc_polymorphic struct_decl.poly_vars in
    let fields =
      List.map
        (fun (s, ty) ->
          let s = s.Position.value in
          let ty = Ty.of_tyloc' ty in
          (s, ty)
        )
        struct_decl.fields
    in
    { struct_name; poly_vars; fields }

  (**
      [substitution module_resolver types kosu_struct_decl] replaces the type variable in 
      [kosu_struct_decl] by there value in [types].
      Returns [None] if [types] hasn't the same length as [kosu_struct_decl.poly_vars]
  *)
  let substitution module_resolver types
      (kosu_struct_decl : KosuAst.kosu_struct_decl) =
    let kosu_struct_decl = to_unlocated kosu_struct_decl in
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
          let ty = Ty.ty_substitution kosu_struct_decl.poly_vars assoc ty in
          (name, ty)
        )
        kosu_struct_decl.fields
    in
    let ty =
      KosuType.Ty.TyIdentifier
        {
          module_resolver;
          parametrics_type = types;
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

  let field name (struct_decl : KosuAst.kosu_raw_struct_decl) =
    List.assoc_opt name struct_decl.fields
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

module Expression = struct
  let explicit_module current_module = function
    | KosuAst.ModuleResolverLoc [] ->
        current_module
    | KosuAst.ModuleResolverLoc (_ :: _) as m ->
        m

  let rec explicit_module_type' current_module exprloc =
    Position.map (explicit_module_type current_module) exprloc

  and explicit_module_type current_module :
      KosuAst.kosu_expression -> KosuAst.kosu_expression = function
    | ( EEmpty
      | ETrue
      | EFalse
      | ENullptr _
      | ECmpLess
      | ECmpEqual
      | ECmpGreater
      | EStringl _
      | EChar _
      | EInteger _
      | EFloat _ ) as e ->
        e
    | ESizeof either ->
        let either =
          match either with
          | Either.Left ty ->
              Either.left @@ TyLoc.explicit_module_type' current_module ty
          | Either.Right expr ->
              Either.right @@ explicit_module_type' current_module expr
        in
        ESizeof either
    | EFieldAccess { first_expr; field } ->
        let first_expr = explicit_module_type' current_module first_expr in
        EFieldAccess { first_expr; field }
    | EArrayAccess { array_expr; index_expr } ->
        let array_expr = explicit_module_type' current_module array_expr in
        let index_expr = explicit_module_type' current_module index_expr in
        EArrayAccess { array_expr; index_expr }
    | ETupleAccess { first_expr; index } ->
        let first_expr = explicit_module_type' current_module first_expr in
        ETupleAccess { first_expr; index }
    | EConstIdentifier { module_resolver; identifier } ->
        let module_resolver = explicit_module current_module module_resolver in
        EConstIdentifier { module_resolver; identifier }
    (* Idenfier doesn need to remap module *)
    | EIdentifier { module_resolver = _; id = _ } as e ->
        e
    | EStruct { module_resolver; struct_name; fields } ->
        let module_resolver = explicit_module current_module module_resolver in
        let fields =
          List.map
            (fun (s, expr) ->
              let expr = explicit_module_type' current_module expr in
              (s, expr)
            )
            fields
        in
        EStruct { module_resolver; struct_name; fields }
    | EEnum { module_resolver; enum_name; variant; assoc_exprs } ->
        let module_resolver = explicit_module current_module module_resolver in
        let assoc_exprs =
          List.map (explicit_module_type' current_module) assoc_exprs
        in
        EEnum { module_resolver; enum_name; variant; assoc_exprs }
    | EBlock block ->
        EBlock (explicit_module_type_block current_module block)
    | EDeref expr ->
        EDeref (explicit_module_type' current_module expr)
    | ETuple exprs ->
        let exprs = List.map (explicit_module_type' current_module) exprs in
        ETuple exprs
    | EArray exprs ->
        let exprs = List.map (explicit_module_type' current_module) exprs in
        EArray exprs
    | EBuiltinFunctionCall { fn_name; parameters } ->
        let parameters =
          List.map (explicit_module_type' current_module) parameters
        in
        EBuiltinFunctionCall { fn_name; parameters }
    (* Kosuenv already resolve function module*)
    | EFunctionCall { module_resolver; generics_resolver; fn_name; parameters }
      ->
        let parameters =
          List.map (explicit_module_type' current_module) parameters
        in
        EFunctionCall
          { module_resolver; generics_resolver; fn_name; parameters }
    | EWhile { condition_expr; body } ->
        let condition_expr =
          explicit_module_type' current_module condition_expr
        in
        let body = explicit_module_type_block current_module body in
        EWhile { condition_expr; body }
    | ECases { cases; else_body } ->
        let cases =
          List.map
            (fun (expr, body) ->
              let expr = explicit_module_type' current_module expr in
              let body = explicit_module_type_block current_module body in
              (expr, body)
            )
            cases
        in
        let else_body = explicit_module_type_block current_module else_body in
        ECases { cases; else_body }
    | EAnonFunction { kind; parameters; body } ->
        let parameters =
          List.map
            (fun (p : KosuAst.kosu_anon_parameters) ->
              let akosu_type =
                Option.map
                  (TyLoc.explicit_module_type' current_module)
                  p.akosu_type
              in
              { p with akosu_type }
            )
            parameters
        in
        let body = explicit_module_type' current_module body in
        EAnonFunction { kind; parameters; body }
    | EMatch { expression; patterns } ->
        let expression = explicit_module_type' current_module expression in
        let patterns =
          List.map
            (fun (pattern, body) ->
              let pattern =
                explicit_module_type_pattern' current_module pattern
              in
              let body = explicit_module_type_block current_module body in
              (pattern, body)
            )
            patterns
        in
        EMatch { expression; patterns }

  and explicit_module_type_block current_module = function
    | { kosu_stmts; kosu_expr } ->
        let kosu_stmts =
          List.map (explicit_module_type_statement' current_module) kosu_stmts
        in
        let kosu_expr = explicit_module_type' current_module kosu_expr in
        { kosu_stmts; kosu_expr }

  and explicit_module_type_pattern' current_module pattern_loc =
    Position.map (explicit_module_type_pattern current_module) pattern_loc

  and explicit_module_type_pattern current_module = function
    | ( PTrue
      | PFalse
      | PEmpty
      | PCmpLess
      | PCmpEqual
      | PCmpGreater
      | PNullptr
      | PWildcard
      | PFloat _
      | PChar _
      | PInteger _
      | PIdentifier _ ) as e ->
        e
    | PTuple patterns ->
        let patterns =
          List.map (explicit_module_type_pattern' current_module) patterns
        in
        PTuple patterns
    | POr patterns ->
        let patterns =
          List.map (explicit_module_type_pattern' current_module) patterns
        in
        POr patterns
    | PCase { module_resolver; enum_name; variant; assoc_patterns } ->
        let module_resolver = explicit_module current_module module_resolver in
        let assoc_patterns =
          List.map (explicit_module_type_pattern' current_module) assoc_patterns
        in
        PCase { module_resolver; enum_name; variant; assoc_patterns }
    | PRecord { module_resolver; struct_name; pfields } ->
        let module_resolver = explicit_module current_module module_resolver in
        let pfields =
          List.map
            (fun (n, pattern) ->
              let pattern =
                explicit_module_type_pattern' current_module pattern
              in
              (n, pattern)
            )
            pfields
        in
        PRecord { module_resolver; struct_name; pfields }
    | PAs { pas_pattern; pas_bound } ->
        let pas_pattern =
          explicit_module_type_pattern' current_module pas_pattern
        in
        PAs { pas_pattern; pas_bound }

  and explicit_module_type_statement' current_module =
    Position.map @@ explicit_module_type_statement current_module

  and explicit_module_type_statement current_module = function
    | SDeclaration { is_const; pattern; explicit_type; expression } ->
        let pattern = explicit_module_type_pattern' current_module pattern in
        let explicit_type =
          Option.map (TyLoc.explicit_module_type' current_module) explicit_type
        in
        let expression = explicit_module_type' current_module expression in
        SDeclaration { is_const; pattern; explicit_type; expression }
    | SAffection { is_deref; lvalue; expression } ->
        let expression = explicit_module_type' current_module expression in
        SAffection { is_deref; lvalue; expression }
    | SDiscard expresssion ->
        let expression = explicit_module_type' current_module expresssion in
        SDiscard expression
    | SOpen _ as so ->
        so
end

module Builtin = struct
  type kosu_builtin_function = KosuAst.kosu_builtin_function

  let of_string_opt =
    let open KosuAst in
    function
    | "tos8" ->
        Option.some @@ Tos8
    | "tou8" ->
        Option.some @@ Tou8
    | "tos16" ->
        Option.some @@ Tos16
    | "tou16" ->
        Option.some @@ Tou16
    | "tof32" ->
        Option.some @@ Tos32
    | "tos32" ->
        Option.some @@ Tos32
    | "tou32" ->
        Option.some @@ Tou32
    | "tos64" ->
        Option.some @@ Tos64
    | "tou64" ->
        Option.some @@ Tou64
    | "tof64" ->
        Option.some @@ Tof64
    | "tagof" ->
        Option.some @@ Tagof
    | "arraylen" ->
        Option.some @@ ArrayLen
    | "arrayptr" ->
        Option.some @@ ArrayPtr
    | "stringlptr" ->
        Option.some @@ StringlPtr
    | "stringlen" ->
        Option.some @@ StringLen
    | "alloc" ->
        Option.some @@ Alloc { const = true }
    | "allocmut" ->
        Option.some @@ Alloc { const = false }
    | "ralloc" ->
        Option.some @@ Ralloc
    | "exit" ->
        Option.some @@ Exit
    | _ ->
        None

  let arity =
    let open KosuAst in
    function
    | Tos8
    | Tou8
    | Tos16
    | Tou16
    | Tos32
    | Tou32
    | Tof32
    | Tos64
    | Tou64
    | Tof64
    | StringLen
    | StringlPtr
    | ArrayPtr
    | ArrayLen
    | Tagof
    | Exit
    | Alloc _ ->
        1
    | Ralloc ->
        2
end

module Node = struct
  let explicit_module_type current_module _program :
      KosuAst.kosu_module_node -> KosuAst.kosu_module_node = function
    | KosuAst.NExternFunc external_decl ->
        let parameters =
          List.map
            (TyLoc.explicit_module_type' current_module)
            external_decl.parameters
        in
        let return_type =
          TyLoc.explicit_module_type' current_module external_decl.return_type
        in
        NExternFunc { external_decl with parameters; return_type }
    | NFunction kosu_function ->
        let parameters =
          List.map
            (fun (p : KosuAst.kosu_function_parameters) ->
              let kosu_type =
                TyLoc.explicit_module_type' current_module p.kosu_type
              in
              { p with kosu_type }
            )
            kosu_function.parameters
        in
        let return_type =
          TyLoc.explicit_module_type' current_module kosu_function.return_type
        in
        let body =
          Expression.explicit_module_type' current_module kosu_function.body
        in
        NFunction { kosu_function with parameters; return_type; body }
    | NStruct kosu_struct_decl ->
        let fields =
          List.map
            (fun (s, types) ->
              let types = TyLoc.explicit_module_type' current_module types in
              (s, types)
            )
            kosu_struct_decl.fields
        in
        NStruct { kosu_struct_decl with fields }
    | NConst const_decl ->
        let explicit_type =
          TyLoc.explicit_module_type' current_module const_decl.explicit_type
        in
        let c_value =
          Expression.explicit_module_type' current_module const_decl.c_value
        in
        NConst { const_decl with explicit_type; c_value }
    | NEnum enum_decl ->
        let tag_type =
          TyLoc.explicit_module_type' current_module enum_decl.tag_type
        in
        let variants =
          List.map
            (fun (s, types) ->
              let types =
                List.map (TyLoc.explicit_module_type' current_module) types
              in
              (s, types)
            )
            enum_decl.variants
        in
        NEnum { enum_decl with tag_type; variants }
    | NOpaque _ as no ->
        no
end

module Module = struct
  let filename_of_module s =
    s |> List.map Position.value |> String.concat Filename.dir_sep

  let constant_decls kosu_module =
    let open KosuAst in
    kosu_module
    |> List.filter_map (function
         | NConst kosu_const_decl ->
             Some kosu_const_decl
         | NExternFunc _ | NEnum _ | NFunction _ | NStruct _ | NOpaque _ ->
             None
         )

  let enum_decls kosu_module =
    let open KosuAst in
    kosu_module
    |> List.filter_map (function
         | NEnum kosu_enum_decl ->
             Some kosu_enum_decl
         | NExternFunc _ | NConst _ | NFunction _ | NStruct _ | NOpaque _ ->
             None
         )

  let struct_decls kosu_module =
    let open KosuAst in
    kosu_module
    |> List.filter_map (function
         | NStruct kosu_struct_decl ->
             Some kosu_struct_decl
         | NExternFunc _ | NConst _ | NFunction _ | NEnum _ | NOpaque _ ->
             None
         )

  let callable_decls_name name =
    let open KosuAst in
    List.filter_map (function
      | NFunction ({ fn_name; _ } as fn_decl) when fn_name.value = name ->
          Option.some @@ CdKosuFuntion fn_decl
      | NExternFunc ({ sig_name; _ } as external_decl)
        when sig_name.value = name ->
          Option.some @@ CdExternalFunction external_decl
      | NEnum _ | NConst _ | NOpaque _ | NStruct _ | NExternFunc _ | NFunction _
        ->
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
      | NEnum _ | NStruct _ | NConst _ | NOpaque _ | NFunction _ | NExternFunc _
        ->
          None
      )

  (**
      [enum_decls_from_variant_name name kosu_module] finds all the enum_declaration within [kosu_module] which have a variant named [name]
  *)
  let enum_decls_from_variant_name name =
    let open KosuAst in
    List.filter_map (function
      | NEnum ({ variants; _ } as enum_decl) ->
          let ( let* ) = Option.bind in
          let* enum_decl =
            match
              List.exists
                (fun (variant, _) -> variant.Position.value = name)
                variants
            with
            | true ->
                Some enum_decl
            | false ->
                None
          in
          Some enum_decl
      | NStruct _ | NConst _ | NOpaque _ | NFunction _ | NExternFunc _ ->
          None
      )

  let explicit_module_type current_module program kosu_module =
    List.map (Node.explicit_module_type current_module program) kosu_module
end

module Program = struct
  let find_module_opt (KosuBaseAst.ModuleResolverLoc modules) kosu_program =
    let open KosuAst in
    let module_name = Module.filename_of_module modules in
    kosu_program
    |> List.find_map (fun { filename; kosu_module } ->
           let module_filename =
             Module.filename_of_module
             @@ List.map Position.dummy_located
             @@ ModuleResolver.of_filename_raw filename
           in
           match module_filename = module_name with
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
  let type_decl ~current_module kosu_type kosu_program =
    let open KosuType in
    match kosu_type with
    | Ty.TyIdentifier { module_resolver; parametrics_type = _; name } ->
        let kosu_module =
          find_module_opt
            (ModuleResolver.dummy_located module_resolver)
            kosu_program
        in
        let kosu_module = Option.value ~default:current_module kosu_module in
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
    | TyArray _
    | TyTuple _
    | TyBool ->
        None

  let module_resolver_of_module kosu_module kosu_program =
    let filename =
      Option.get
      @@ List.find_map
           (fun KosuAst.{ filename; kosu_module = module_target } ->
             (* Can use == since module are unique *)
             if module_target == kosu_module then
               Some filename
             else
               None
           )
           kosu_program
    in
    ModuleResolver.of_filename filename

  (**

  *)
  let explicit_module_type program =
    List.map
      (fun KosuAst.{ filename; kosu_module } ->
        let current_module =
          ModuleResolver.dummy_located @@ ModuleResolver.of_filename filename
        in
        let kosu_module =
          Module.explicit_module_type current_module program kosu_module
        in
        KosuAst.{ filename; kosu_module }
      )
      program
end

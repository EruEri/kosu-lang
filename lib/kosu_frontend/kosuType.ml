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

open Position
open KosuBaseAst

module TyLoc = struct
  type kosu_loctype_polymorphic = PolymorphicVarLoc of string location

  type kosu_locfunction_schema = {
    poly_vars : kosu_loctype_polymorphic list;
    parameters_type : kosu_loctype location list;
    return_type : kosu_loctype location;
  }

  and kosu_loctype =
    | TyLocIdentifier of {
        module_resolver : module_resolver_loc;
        parametrics_type : kosu_loctype location list;
        name : string location;
      }
    | TyLocPolymorphic of kosu_loctype_polymorphic
    | TyLocPointer of {
        pointer_state : pointer_state;
        pointee_type : kosu_loctype location;
      }
    | TyLocInteger of integer_info
    | TyLocFloat of fsize
    | TyLocFunctionPtr of kosu_locfunction_schema
    (* This closure type is used by the user in function signature*)
    | TyLocClosure of kosu_locfunction_schema
    (* Used by the typecker to give an unique id to the closure *)
    | TyLocArray of { ktype : kosu_loctype location; size : int64 location }
    | TyLocTuple of kosu_loctype location list
    | TyLocOpaque of {
        module_resolver : module_resolver_loc;
        name : string location;
      }
    | TyLocOrdered
    | TyLocStringLit
    | TyLocChar
    | TyLocBool
    | TyLocUnit
end

module Ty = struct
  type kosu_type_polymorphic_hint = KTyHintInteger | KTyHintFloat

  type kosu_type_polymorphic =
    | PolymorphicVar of string
    | CompilerPolymorphicVar of {
        name : string;
        hint : kosu_type_polymorphic_hint option;
      }

  type kosu_function_schema = {
    poly_vars : kosu_type_polymorphic list;
    parameters_type : kosu_type list;
    return_type : kosu_type;
  }

  and kosu_type =
    | TyIdentifier of {
        module_resolver : module_resolver;
        parametrics_type : kosu_type list;
        name : string;
      }
    | TyPolymorphic of kosu_type_polymorphic
    | TyPointer of { pointer_state : pointer_state; pointee_type : kosu_type }
    | TyInteger of integer_info
    | TyFloat of fsize
    | TyFunctionPtr of kosu_function_schema
    (* This closure type is used by the user in function signature*)
    | TyClosure of kosu_function_schema
    (* Used by the typecker to give an unique id to the closure *)
    (* | TyInnerClosureId of kosu_inner_closure_type *)
    | TyArray of { ktype : kosu_type; size : int64 }
    | TyTuple of kosu_type list
    | TyOpaque of { module_resolver : module_resolver; name : string }
    | TyOrdered
    | TyStringLit
    | TyChar
    | TyBool
    | TyUnit

  and kosu_type_constraint = {
    cexpected : kosu_type;
    cfound : kosu_type;
    position : Position.position;
  }

  (**
    [tyloc_substitution bound assoc_types ty] replaces the type variable occurences in [ty] 
    by there value associated in [assoc_types] and not in [bound]

    [bound] is useful for function signature where type variable can be bound to the function signature
    (.ie for all 'a . ..)
  *)
  let rec ty_substitution bound assoc_types ty =
    match ty with
    | TyPolymorphic variable as t ->
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
      | TyInteger (Worded _ | Sized (_, _)) ) as ty ->
        ty

  and ty_substitution_schema assoc_type = function
    | { poly_vars; parameters_type; return_type } as e ->
        let bound = poly_vars in
        let parameters_type =
          List.map (ty_substitution bound assoc_type) parameters_type
        in
        let return_type = ty_substitution bound assoc_type return_type in
        { e with parameters_type; return_type }
end

module PatternIdentifierBound = Set.Make (struct
  type t = string location * Ty.kosu_type

  let compare (lhs : t) (rhs : t) =
    String.compare (Position.value @@ fst lhs) (Position.value @@ fst rhs)
end)

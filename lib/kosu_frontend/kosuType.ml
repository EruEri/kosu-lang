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
    | TyLocInteger of integer_info option
    | TyLocFloat of fsize option
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
  type kosu_type_polymorphic = PolymorphicVar of string

  type kosu_function_schema = {
    poly_vars : kosu_type_polymorphic list;
    parameters_type : kosu_type list;
    return_type : kosu_type;
  }

  and kosu_inner_closure_type =
    | ClosureType of {
        id : string;
        schema : kosu_function_schema;
        env : (string * kosu_type) list;
      }

  and kosu_type =
    | TyIdentifier of {
        module_resolver : module_resolver;
        parametrics_type : kosu_type list;
        name : string;
      }
    | TyPolymorphic of kosu_type_polymorphic
    | TyPointer of { pointer_state : pointer_state; pointee_type : kosu_type }
    | TyInteger of integer_info option
    | TyFloat of fsize option
    | TyFunctionPtr of kosu_function_schema
    (* This closure type is used by the user in function signature*)
    | TyClosure of kosu_function_schema
    (* Used by the typecker to give an unique id to the closure *)
    | TyInnerClosureId of kosu_inner_closure_type
    | TyArray of { ktype : kosu_type; size : int64 }
    | TyTuple of kosu_type list
    | TyOpaque of { module_resolver : module_resolver; name : string }
    | TyOrdered
    | TyStringLit
    | TyChar
    | TyBool
    | TyUnit

  type kosu_type_constraint = {
    clhs : kosu_type;
    crhs : kosu_type;
    position : Position.position;
  }

  type _ kosu_type_kind =
    | KosuTy : kosu_type kosu_type_kind
    | KosuTyLoc : TyLoc.kosu_loctype kosu_type_kind

  let counter = ref 0

  let fresh_variable reset () =
    let () = match reset with false -> () | true -> counter := 0 in
    let n = !counter in
    let () = incr counter in
    Printf.sprintf "'t%u" n

  let fresh_variable_type_gadt :
      type a. kind:a kosu_type_kind -> ?reset:bool -> unit -> a =
   fun ~kind ?(reset = false) () ->
    let n = fresh_variable reset () in
    match kind with
    | KosuTy ->
        TyPolymorphic (PolymorphicVar n)
    | KosuTyLoc ->
        TyLoc.(TyLocPolymorphic (PolymorphicVarLoc (Position.dummy_located n)))

  let fresh_variable_type = fresh_variable_type_gadt ~kind:KosuTy
  let fresh_variable_typeloc = fresh_variable_type_gadt ~kind:KosuTyLoc
end

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

type t = KosuType.Ty.kosu_type_constraint

let compare (lhs : t) (rhs : t) =
  match compare lhs.clhs rhs.clhs with 0 -> compare lhs.clhs rhs.crhs | n -> n

(**
    [other ty equation] returns the other side of [ty] in the equation [equation].
    Returns [None] if [ty] isn't part of [equation] 
*)
let other ty (equation : t) =
  match ty = equation.clhs with
  | true ->
      Some equation.crhs
  | false -> (
      match ty = equation.crhs with true -> Some equation.clhs | false -> None
    )

let rec restrict ~(with_ty : KosuType.Ty.kosu_type) (ty : KosuType.Ty.kosu_type)
    =
  let open KosuType.Ty in
  let ( let* ) = Option.bind in
  let return = Option.some in
  match (with_ty, ty) with
  | TyPolymorphic (PolymorphicVar _), ty | ty, TyPolymorphic (PolymorphicVar _)
    ->
      Some ty
  | ( TyIdentifier
        { module_resolver = lmr; parametrics_type = lpt; name = lname },
      TyIdentifier
        { module_resolver = rmr; parametrics_type = rpt; name = rname } ) ->
      let* module_resolver =
        match lmr = rmr with true -> Some lmr | false -> None
      in
      let* name =
        match lname = rname with true -> Some lname | false -> None
      in
      let* _ =
        match Util.Ulist.are_same_length lpt rpt with
        | true ->
            Some ()
        | false ->
            None
      in
      let parametrics_type = List.combine lpt rpt in
      let* parametrics_type =
        Util.Ulist.map_some
          (fun (lhs, rhs) -> restrict ~with_ty:lhs rhs)
          parametrics_type
      in
      return @@ TyIdentifier { module_resolver; parametrics_type; name }
  | ( TyIdentifier _,
      ( TyPointer _
      | TyInteger _
      | TyFloat _
      | TyFunctionPtr _
      | TyClosure _
      | TyInnerClosureId _
      | TyArray _
      | TyTuple _
      | TyOpaque _
      | TyOrdered
      | TyStringLit
      | TyChar
      | TyBool
      | TyUnit ) )
  | ( ( TyPointer _
      | TyInteger _
      | TyFloat _
      | TyFunctionPtr _
      | TyClosure _
      | TyInnerClosureId _
      | TyArray _
      | TyTuple _
      | TyOpaque _
      | TyOrdered
      | TyStringLit
      | TyChar
      | TyBool
      | TyUnit ),
      TyIdentifier _ ) ->
      None
  | ( TyPointer { pointer_state = lstate; pointee_type = ltype },
      TyPointer { pointer_state = rstate; pointee_type = rtype } ) ->
      let* pointer_state =
        match lstate = rstate with true -> Some lstate | false -> None
      in
      let* pointee_type = restrict ~with_ty:ltype rtype in
      return @@ TyPointer { pointer_state; pointee_type }
  | ( TyPointer _,
      ( TyInteger _
      | TyFloat _
      | TyFunctionPtr _
      | TyClosure _
      | TyInnerClosureId _
      | TyArray _
      | TyTuple _
      | TyOpaque _
      | TyOrdered
      | TyStringLit
      | TyChar
      | TyBool
      | TyUnit ) )
  | ( ( TyInteger _
      | TyFloat _
      | TyFunctionPtr _
      | TyClosure _
      | TyInnerClosureId _
      | TyArray _
      | TyTuple _
      | TyOpaque _
      | TyOrdered
      | TyStringLit
      | TyChar
      | TyBool
      | TyUnit ),
      TyPointer _ ) ->
      None
  | TyInteger lhs, TyInteger rhs ->
      let* integer_info =
        match (lhs, rhs) with
        | None, info | info, None ->
            info
        | (Some (Worded lhs) as info), Some (Worded rhs) ->
            let* info =
              match lhs = rhs with true -> Some info | false -> None
            in
            info
        | (Some (Sized (lsign, lsize)) as info), Some (Sized (rsign, rsize)) ->
            let* info =
              match lsign = rsign && lsize = rsize with
              | true ->
                  Some info
              | false ->
                  None
            in
            info
        | Some (Worded _), Some (Sized _) | Some (Sized _), Some (Worded _) ->
            None
      in
      return @@ TyInteger (Some integer_info)
  | _ ->
      failwith ""

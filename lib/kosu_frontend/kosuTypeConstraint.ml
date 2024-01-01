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
  match compare lhs.cexpected rhs.cexpected with
  | 0 ->
      compare lhs.cfound rhs.cfound
  | n ->
      n

module KosuTypingSolution = Map.Make (struct
  type t = KosuType.Ty.kosu_type_polymorphic

  let compare = Stdlib.compare
end)

(**
    [other ty equation] returns the other side of [ty] in the equation [equation].
    Returns [None] if [ty] isn't part of [equation] 
*)
let other ty (equation : t) =
  match ty = equation.cexpected with
  | true ->
      Some equation.cfound
  | false -> (
      match ty = equation.cfound with
      | true ->
          Some equation.cexpected
      | false ->
          None
    )

let is_compiler_var_only_and_dif (equation : t) =
  match (equation.cexpected, equation.cfound) with
  | ( KosuType.Ty.TyPolymorphic (CompilerPolymorphicVar lhs),
      KosuType.Ty.TyPolymorphic (CompilerPolymorphicVar rhs) ) ->
      lhs <> rhs
  | _, _ ->
      false

let substitute ty_var by (equation : t) =
  {
    equation with
    cexpected =
      KosuUtil.Ty.ty_substitution [] [ (ty_var, by) ] equation.cexpected;
    cfound = KosuUtil.Ty.ty_substitution [] [ (ty_var, by) ] equation.cfound;
  }

let rec restrict ~(with_ty : KosuType.Ty.kosu_type) (ty : KosuType.Ty.kosu_type)
    =
  let open KosuType.Ty in
  let ( let* ) = Option.bind in
  let return = Option.some in
  match (with_ty, ty) with
  | TyPolymorphic (PolymorphicVar _ | CompilerPolymorphicVar _), ty
  | ty, TyPolymorphic (PolymorphicVar _ | CompilerPolymorphicVar _) ->
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
  | TyIdentifier _, _ ->
      None
  | ( TyPointer { pointer_state = lstate; pointee_type = ltype },
      TyPointer { pointer_state = rstate; pointee_type = rtype } ) ->
      let* pointer_state =
        match lstate = rstate with true -> Some lstate | false -> None
      in
      let* pointee_type = restrict ~with_ty:ltype rtype in
      return @@ TyPointer { pointer_state; pointee_type }
  | TyPointer _, _ ->
      None
  | (TyInteger lhs as tmp), TyInteger rhs ->
      let res =
        match (lhs, rhs) with
        | None, info | info, None ->
            Option.some @@ TyInteger info
        | _, _ -> (
            match KosuUtil.Ty.are_number_compatible lhs rhs with
            | true ->
                Some tmp
            | false ->
                None
          )
      in
      res
  | TyInteger _, _ ->
      None
  | (TyFloat lhs as tmp), TyFloat rhs ->
      let res =
        match (lhs, rhs) with
        | None, info | info, None ->
            Option.some @@ TyFloat info
        | _, _ -> (
            match KosuUtil.Ty.are_number_compatible lhs rhs with
            | true ->
                Some tmp
            | false ->
                None
          )
      in
      res
  | TyFloat _, _ ->
      None
  | ( TyFunctionPtr { poly_vars = _; parameters_type = lpt; return_type = lrt },
      TyFunctionPtr { poly_vars; parameters_type = rpt; return_type = rrt } ) ->
      let* () =
        match Util.Ulist.are_same_length lpt rpt with
        | true ->
            Some ()
        | false ->
            None
      in
      let parameters_type = List.combine lpt rpt in
      let* parameters_type =
        Util.Ulist.map_some
          (fun (lhs, rhs) -> restrict ~with_ty:lhs rhs)
          parameters_type
      in
      let* return_type = restrict ~with_ty:lrt rrt in

      return @@ TyFunctionPtr { poly_vars; parameters_type; return_type }
  | TyFunctionPtr _, _ ->
      None
  | ( TyClosure { poly_vars = _; parameters_type = lpt; return_type = lrt },
      TyClosure { poly_vars; parameters_type = rpt; return_type = rrt } ) ->
      let* () =
        match Util.Ulist.are_same_length lpt rpt with
        | true ->
            Some ()
        | false ->
            None
      in
      let parameters_type = List.combine lpt rpt in
      let* parameters_type =
        Util.Ulist.map_some
          (fun (lhs, rhs) -> restrict ~with_ty:lhs rhs)
          parameters_type
      in
      let* return_type = restrict ~with_ty:lrt rrt in

      return @@ TyClosure { poly_vars; parameters_type; return_type }
  | TyClosure _, _ ->
      None
  | TyInnerClosureId linner, TyInnerClosureId rinner ->
      (* How to handle id ... *)
      let () = ignore (linner, rinner) in
      failwith ""
  | TyInnerClosureId _, _ ->
      None
  | ( TyArray { ktype = ltype; size = lsize },
      TyArray { ktype = rtype; size = rsize } ) ->
      let* size =
        match lsize = rsize with true -> Some lsize | false -> None
      in
      let* ktype = restrict ~with_ty:ltype rtype in
      return @@ TyArray { ktype; size }
  | TyArray _, _ ->
      None
  | TyTuple lttes, TyTuple rttes ->
      let* () =
        match Util.Ulist.are_same_length lttes rttes with
        | true ->
            Some ()
        | false ->
            None
      in
      let ttes = List.combine lttes rttes in
      let* ttes =
        Util.Ulist.map_some (fun (lhs, rhs) -> restrict ~with_ty:lhs rhs) ttes
      in
      return @@ TyTuple ttes
  | TyTuple _, _ ->
      None
  | ( TyOpaque { module_resolver = lmr; name = lname },
      TyOpaque { module_resolver = rmr; name = rname } ) ->
      let* module_resolver =
        match lmr = rmr with true -> Some lmr | false -> None
      in
      let* name =
        match lname = rname with true -> Some lname | false -> None
      in
      return @@ TyOpaque { module_resolver; name }
  | TyOpaque _, _ ->
      None
  | (TyOrdered as ty), TyOrdered ->
      return ty
  | TyOrdered, _ ->
      None
  | (TyStringLit as ty), TyStringLit ->
      return ty
  | TyStringLit, _ ->
      None
  | (TyChar as ty), TyChar ->
      return ty
  | TyChar, _ ->
      None
  | (TyBool as ty), TyBool ->
      return ty
  | TyBool, _ ->
      None
  | (TyUnit as ty), TyUnit ->
      Some ty
  | TyUnit, _ ->
      None

let reduce lhs rhs =
  let open KosuType.Ty in
  let ( let* ) = Option.bind in
  let some = Option.some in
  let left = Either.left in
  let right = Either.right in
  if lhs = rhs then
    some @@ right @@ []
  else
    match (lhs, rhs) with
    | TyPolymorphic ((PolymorphicVar _ | CompilerPolymorphicVar _) as p), ty
    | ty, TyPolymorphic ((PolymorphicVar _ | CompilerPolymorphicVar _) as p) ->
        (* Should check if p appears in ty*)
        some @@ left (p, ty)
    | ( TyIdentifier
          { module_resolver = lmr; parametrics_type = lpt; name = lname },
        TyIdentifier
          { module_resolver = rmr; parametrics_type = rpt; name = rname } ) ->
        let* _ = match lmr = rmr with true -> Some lmr | false -> None in
        let* _ =
          match lname = rname with true -> Some lname | false -> None
        in
        let* _ =
          match Util.Ulist.are_same_length lpt rpt with
          | true ->
              Some ()
          | false ->
              None
        in
        some @@ right @@ List.combine lpt rpt
    | TyIdentifier _, _ ->
        None
    | ( TyPointer { pointer_state = lstate; pointee_type = ltype },
        TyPointer { pointer_state = rstate; pointee_type = rtype } ) ->
        let* _ =
          match lstate = rstate with true -> Some lstate | false -> None
        in
        some @@ right @@ ((ltype, rtype) :: [])
    | TyPointer _, _ ->
        failwith ""
    | TyInteger linfo, TyInteger rinfo ->
        let res =
          match KosuUtil.Ty.are_number_compatible linfo rinfo with
          | false ->
              None
          | true ->
              rhs |> restrict ~with_ty:lhs |> Option.map @@ fun _ -> right []
        in
        res
    | TyInteger _, _ ->
        None
    | TyFloat linfo, TyFloat rinfo ->
        let res =
          match KosuUtil.Ty.are_number_compatible linfo rinfo with
          | false ->
              None
          | true ->
              rhs |> restrict ~with_ty:lhs |> Option.map @@ fun _ -> right []
        in
        res
    | TyFloat _, _ ->
        None
    | ( TyFunctionPtr { poly_vars = _; parameters_type = lpt; return_type = lrt },
        TyFunctionPtr
          { poly_vars = _; parameters_type = rpt; return_type = rrt } )
    | ( TyClosure { poly_vars = _; parameters_type = lpt; return_type = lrt },
        TyClosure { poly_vars = _; parameters_type = rpt; return_type = rrt } )
      ->
        let* () =
          match Util.Ulist.are_same_length lpt rpt with
          | true ->
              Some ()
          | false ->
              None
        in
        let param_constraints = List.combine lpt rpt in
        let fn_constrains = (lrt, rrt) :: param_constraints in
        some @@ right fn_constrains
    | (TyFunctionPtr _ | TyClosure _), _ ->
        None
    | TyInnerClosureId linner, TyInnerClosureId rinner ->
        (* How to handle id ... *)
        let () = ignore (linner, rinner) in
        failwith ""
    | TyInnerClosureId _, _ ->
        None
    | ( TyArray { ktype = ltype; size = lsize },
        TyArray { ktype = rtype; size = rsize } ) ->
        let* () = match lsize = rsize with true -> Some () | false -> None in
        some @@ right @@ ((ltype, rtype) :: [])
    | TyArray _, _ ->
        None
    | TyTuple lttes, TyTuple rttes ->
        let* () =
          match Util.Ulist.are_same_length lttes rttes with
          | true ->
              Some ()
          | false ->
              None
        in
        some @@ right @@ List.combine lttes rttes
    | TyTuple _, _ ->
        None
    | ( TyOpaque { module_resolver = lmr; name = lname },
        TyOpaque { module_resolver = rmr; name = rname } ) ->
        let* () = match lmr = rmr with true -> Some () | false -> None in
        let* () = match lname = rname with true -> Some () | false -> None in
        some @@ right []
    | TyOpaque _, _ ->
        None
    (* Remains type than can be check trivially *)
    | ((TyStringLit | TyOrdered | TyChar | TyBool | TyUnit) as l), r ->
        let res = match l = r with true -> some @@ right [] | false -> None in
        res

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

let compare (lhs : t) (rhs : t) = Stdlib.compare lhs rhs

module KosuTypingSolution = Map.Make (struct
  type t = KosuType.Ty.kosu_type_polymorphic

  let compare = Stdlib.compare
end)

let mem ty (constr : KosuType.Ty.kosu_type_constraint) =
  constr.cfound = ty || constr.cexpected = ty

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
  (* Restrict the infered type is an integer *)
  | ( TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintInteger }),
      (TyInteger _ as tyinteger) )
  | ( (TyInteger _ as tyinteger),
      TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintInteger }) ) ->
      Some tyinteger
  | ( ( TyPolymorphic
          (CompilerPolymorphicVar { name = _; hint = Some KTyHintInteger }) as
        lhs
      ),
      TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintInteger }) ) ->
      Some lhs
  | ( TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintInteger }),
      _ )
  (* Reject if not an integer *)
  | ( _,
      TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintInteger }) ) ->
      None
  | ( TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintFloat }),
      (TyFloat _ as tyinteger) )
  (* Restrict the infered type is an float *)
  | ( (TyFloat _ as tyinteger),
      TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintFloat }) ) ->
      Some tyinteger
  | ( ( TyPolymorphic
          (CompilerPolymorphicVar { name = _; hint = Some KTyHintFloat }) as lhs
      ),
      TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintFloat }) ) ->
      Some lhs
  (* Reject if not a float *)
  | ( TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintFloat }),
      _ )
  | ( _,
      TyPolymorphic
        (CompilerPolymorphicVar { name = _; hint = Some KTyHintFloat }) ) ->
      None
  | ( TyPolymorphic
        (PolymorphicVar _ | CompilerPolymorphicVar { name = _; hint = None }),
      ty )
  | ( ty,
      TyPolymorphic
        (PolymorphicVar _ | CompilerPolymorphicVar { name = _; hint = None }) )
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
      if lhs = rhs then
        Some tmp
      else
        None
  | TyInteger _, _ ->
      None
  | (TyFloat lhs as tmp), TyFloat rhs ->
      if lhs = rhs then
        Some tmp
      else
        None
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
    | TyPolymorphic (CompilerPolymorphicVar _ as p), ty
    | ty, TyPolymorphic (CompilerPolymorphicVar _ as p) ->
        some @@ left (p, ty)
    | (TyPolymorphic (PolymorphicVar _) as p), ty
    | ty, (TyPolymorphic (PolymorphicVar _) as p) ->
        (* If it 'a . 'a : *)
        (* It can only equal to self *)
        if p = ty then
          some @@ right @@ []
        else
          None
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
        None
    | TyInteger linfo, TyInteger rinfo when linfo = rinfo ->
        some @@ right []
    | TyInteger _, _ ->
        None
    | TyFloat linfo, TyFloat rinfo when linfo = rinfo ->
        some @@ right []
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

let to_schema equations solutions parameters return_type =
  let module KTS = KosuTypingSolution in
  let equations : KosuType.Ty.kosu_type_constraint list = equations in
  let map_type t =
    match t with
    | KosuType.Ty.TyPolymorphic p ->
        let t = match KTS.find_opt p solutions with Some t -> t | None -> t in
        let t =
          if List.exists (mem t) equations then
            t
          else
            KosuUtil.Ty.generalize t
        in
        t
    | t ->
        t
  in
  let parameters_type = List.map map_type parameters in
  let return_type = map_type return_type in
  let poly_vars =
    KosuUtil.Ty.quantified_ty_vars [] KosuUtil.KosuTypeVariableSet.empty
      return_type
  in
  let poly_vars =
    List.fold_left (KosuUtil.Ty.quantified_ty_vars []) poly_vars parameters_type
  in
  let poly_vars = KosuUtil.KosuTypeVariableSet.elements poly_vars in
  KosuType.Ty.{ poly_vars; return_type; parameters_type }

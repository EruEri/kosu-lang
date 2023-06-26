(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of kosu-register-allocator                                               *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
(*                                                                                            *)
(* kosu-register-allocator is free software: you can redistribute it and/or modify            *)
(*  it under the terms either:                                                                *)
(*                                                                                            *)
(*    - the GNU General Public License as published by the Free Software Foundation,          *)
(*        either version 3 of the License, or (at your option) any later version              *)
(*   or                                                                                       *)
(*                                                                                            *)
(*     - the GNU Lesser General Public License as published by the Free Software Foundation,  *)
(*        either version 3 of the License, or (at your option) any later version              *)
(*                                                                                            *)
(* kosu-register-allocator is distributed in the hope that it will be useful,                 *)
(*   but WITHOUT ANY WARRANTY;                                                                *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along                    *)
(* with kosu-register-allocator. If not, see <http://www.gnu.org/licenses/>.                  *)
(*                                                                                            *)
(**********************************************************************************************)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module Make (O : OrderedType) = struct
  module IndexedO = struct
    type t = O.t * int

    let compare (lt, li) (rt, ri) =
      let elt_cmp = O.compare lt rt in
      if elt_cmp <> 0 then
        elt_cmp
      else
        Int.compare li ri
  end

  module ReturnSet = Set.Make (O)
  module ParameterSet = Set.Make (IndexedO)
  module ParameterSetSet = Set.Make (ParameterSet)

  type constraints = {
    self_parameters : ParameterSet.t;
    inner_call_parameters : ParameterSetSet.t;
    return : ReturnSet.t;
  }

  let empty =
    {
      self_parameters = ParameterSet.empty;
      inner_call_parameters = ParameterSetSet.empty;
      return = ReturnSet.empty;
    }

  let add_self_parameters_of_list self_params constraints =
    let self_parameters_set = ParameterSet.of_list self_params in
    {
      constraints with
      self_parameters =
        ParameterSet.union constraints.self_parameters self_parameters_set;
    }

  let add_return_constraint return constr =
    { constr with return = ReturnSet.add return constr.return }

  let add_returns_constraints returns constr =
    let returns_set = ReturnSet.of_list returns in
    { constr with return = ReturnSet.union constr.return returns_set }

  let add_function_constraint parameters constr =
    if parameters = [] then
      constr
    else
      let parameters_set = parameters |> ParameterSet.of_list in
      {
        constr with
        inner_call_parameters =
          ParameterSetSet.add parameters_set constr.inner_call_parameters;
      }

  let return_constraints constraints = constraints.return |> ReturnSet.elements

  let parameters_constraints constraints =
    constraints.inner_call_parameters |> ParameterSetSet.elements
    |> List.map ParameterSet.elements

  let union lhs rhs =
    {
      self_parameters =
        ParameterSet.union lhs.self_parameters rhs.self_parameters;
      inner_call_parameters =
        ParameterSetSet.union lhs.inner_call_parameters
          rhs.inner_call_parameters;
      return = ReturnSet.union lhs.return rhs.return;
    }
end

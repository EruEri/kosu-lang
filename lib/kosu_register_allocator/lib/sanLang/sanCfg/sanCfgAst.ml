(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module SanRegisterAllocator =
  KosuRegisterAllocator.MakePprint (SanCommon.Cfg_Sig) (SanCommon.CfgPprint)

<<<<<<< HEAD
<<<<<<< HEAD
module Cfg_Sig = struct
  type variable = string * san_type
  type t = variable
  type atom = typed_atom
  type rvalue = typed_san_rvalue

  let repr (v, st) = Printf.sprintf "%s : %s" v (string_of_san_type st)
  let compare = Stdlib.compare
  let lvalue_variable vname tysr : variable = (vname, tysr.san_type)
  let lvalue_deref_variable _ _ = failwith "San doesnt have deref"

  let tte_idenfier_used tea =
    match tea.atom with Variable s -> (s, tea.atom_type) :: [] | _ -> []

  let is_affectation tysr =
    match tysr.san_rvalue with
    | TyRVDiscard _ | TYRVLater _ -> false
    | _ -> true

  let ttrv_identifiers_used tysr =
    match tysr.san_rvalue with
    | TyRVExpr ty_atom | TYRVUnary { ty_atom; _ } -> tte_idenfier_used ty_atom
    | TYRVBinary { tylhs; tyrhs; _ } ->
        let lgerenated = tte_idenfier_used tylhs in
        let rgenerated = tte_idenfier_used tyrhs in
        lgerenated @ rgenerated
    | TyRVFunctionCall { parameters; _ } ->
        parameters |> List.map tte_idenfier_used |> List.flatten
    | TyRVDiscard _ | TYRVLater _ -> []

  let variables_as_parameter tysr =
    match tysr.san_rvalue with
    | TyRVFunctionCall { parameters; _ } ->
        parameters
        |> List.fold_left
             (fun (index, acc) tte ->
               let next_index = index + 1 in
               match tte_idenfier_used tte with
               | t :: [] -> (next_index, (t, index) :: acc)
               | [] | _ :: _ -> (next_index, acc))
             (0, [])
        |> snd |> List.rev |> Option.some
    | _ -> None
end

module CfgPprint = struct
  type variable = Cfg_Sig.variable
  type atom = Cfg_Sig.atom
  type rvalue = Cfg_Sig.rvalue

  let string_of_variable = Cfg_Sig.repr
  let string_of_atom = SanTyped.SanTyPprint.string_of_typed_atom
  let string_of_rvalue = SanTyped.SanTyPprint.string_of_typed_san_rvalue
end
<<<<<<< HEAD

module SanRegisterAllocator =
  KosuRegisterAllocator.MakePprint (Cfg_Sig) (CfgPprint)

module GreedyColoring = SanRegisterAllocator.GreedyColoring (SanCfgRegister)
=======
module SanRegisterAllocator = KosuRegisterAllocator.MakePprint( Cfg_Sig)(CfgPprint)
module GreedyColoring = SanRegisterAllocator.GreedyColoring(SanCfgRegister)
>>>>>>> fd18d3e ([San]: Mov Sancfg to SanCommon)
=======
module SanRegisterAllocator = KosuRegisterAllocator.MakePprint(SanCommon.Cfg_Sig)(SanCommon.CfgPprint)
module GreedyColoring = SanRegisterAllocator.GreedyColoring(SanBackend.Aarch64.Register)
>>>>>>> 4dfa4e5 ([San]: fix compilation)
=======
module GreedyColoring =
  SanRegisterAllocator.GreedyColoring (SanBackend.Aarch64.Register)
>>>>>>> ce71891 ([Fmt + Folder cli struct])

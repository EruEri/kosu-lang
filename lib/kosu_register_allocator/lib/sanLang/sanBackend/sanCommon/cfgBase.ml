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


open SanTyped.SanTyAst
open SanFrontend.SanPprint

module Cfg_Sig = struct
  type variable = (string * san_type)
  type t = variable
  type atom = typed_atom
  type rvalue = typed_san_rvalue

  let repr (v, st) = Printf.sprintf "%s : %s" v (string_of_san_type st)
  let compare = Stdlib.compare

  let lvalue_variable vname tysr: variable = 
    vname, tysr.san_type

  let lvalue_deref_variable _ _ = failwith "San doesnt have deref"
  
  let tte_idenfier_used tea = match tea.atom with
  | Variable s -> (s, tea.atom_type)::[]
  | _ -> []  

  let is_affectation tysr = match tysr.san_rvalue with
  | TyRVDiscard _ | TYRVLater _ -> false
  | _ -> true
  let ttrv_identifiers_used tysr = match tysr.san_rvalue with 
    | TyRVExpr ty_atom | TYRVUnary {ty_atom; _}  -> tte_idenfier_used ty_atom
    | TYRVBinary {tylhs; tyrhs; _} ->
      let lgerenated = tte_idenfier_used tylhs in
      let rgenerated = tte_idenfier_used tyrhs in
      lgerenated @ rgenerated
    | TyRVFunctionCall {parameters; _} ->
      parameters |> List.map tte_idenfier_used |> List.flatten
    | TyRVDiscard _ | TYRVLater _ -> []

  let variables_as_parameter tysr = match tysr.san_rvalue with 
  | TyRVFunctionCall {parameters; _} ->
      parameters |> List.fold_left (fun (index, acc) tte ->
        let next_index = index + 1 in
        match tte_idenfier_used tte with
        | t::[] -> (next_index, (t, index)::acc) 
        | [] | _::_ -> (next_index, acc)
      ) (0, []) |> snd |> List.rev |> Option.some
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

module SanVariableMap = Map.Make(Cfg_Sig)
module SanRegisterAllocator = KosuRegisterAllocator.MakePprint(Cfg_Sig)(CfgPprint)


module Conv = struct
  open SanTyped
  open SanRegisterAllocator
  let cfg_statement_of_san_statement ty_san_stmt =
    let open Basic in
    match ty_san_stmt with
    | TySSDeclaration (s, ty_rvalue) -> CFG_STacDeclaration {identifier = s; trvalue = ty_rvalue}
  
  let cfg_ending_of_san_ending = 
    let open Basic in
    function
  | TySE_return atom -> Bbe_return atom
  | TYSE_If {expr; if_label; else_label} -> BBe_if {condition = expr; if_label; else_label}
   
  
  let cfg_block_of_san_block ~(next_block: ty_san_basic_block option) {label; statements; ending} =
    let open Basic in
    let cfg_statements = statements |> List.map cfg_statement_of_san_statement in
    let cfg_ending = ending |> Option.map cfg_ending_of_san_ending in
    let followed_by = match next_block with
      | None -> []
      | Some san_block -> san_block.label::[] in
    let followed_by = match ending with
      | Some TySE_return _ -> []
      | Some TYSE_If {if_label; else_label; _} -> if_label::else_label::followed_by
      | None -> followed_by
    in 
    create_basic_block ~label ~cfg_statements ~followed_by ~ending:cfg_ending
  
  
  let basic_of_san_tyfunction san_tyfunction = 
    let rec make_blocks blocks = match blocks with
    | [] -> []
    | t::[] -> [cfg_block_of_san_block ~next_block:None t]
    | h1::(h2::q as xs) ->
      (cfg_block_of_san_block ~next_block:(Some h2) h1)::make_blocks xs
    in
    let open Basic in
    create_cfg ~entry_block:san_tyfunction.fn_name 
      ~parameters:san_tyfunction.parameters
      ~locals_vars:san_tyfunction.locals
      (make_blocks san_tyfunction.san_basic_blocks)
  
  let detail_of_san_tyfunction san_tyfunction = 
    let cfg = basic_of_san_tyfunction san_tyfunction in
    Detail.of_cfg cfg
  
  let liveness_of_san_tyfunction san_tyfunction = 
    let details = detail_of_san_tyfunction san_tyfunction in
    Liveness.of_cfg_details details ~delete_useless_stmt:false
  
  let inference_graph_san_tyfunction san_tyfunction = 
    let liveness = liveness_of_san_tyfunction san_tyfunction in
    Interference_Graph.interfere liveness
end
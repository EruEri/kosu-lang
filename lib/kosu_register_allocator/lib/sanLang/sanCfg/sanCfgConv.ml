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

open SanCfgAst.SanRegisterAllocator
open SanTyped.SanTyAst

let cfg_statement_of_san_statement ty_san_stmt =
  let open Basic in
  match ty_san_stmt with
  | TySSDeclaration (s, ty_rvalue) ->
      CFG_STacDeclaration { identifier = s; trvalue = ty_rvalue }

let cfg_ending_of_san_ending =
  let open Basic in
  function
  | TySE_return atom -> Bbe_return atom
  | TYSE_If { expr; if_label; else_label } ->
      BBe_if { condition = expr; if_label; else_label }

let cfg_block_of_san_block ~(next_block : ty_san_basic_block option)
    { label; statements; ending } =
  let open Basic in
  let cfg_statements = statements |> List.map cfg_statement_of_san_statement in
  let cfg_ending = ending |> Option.map cfg_ending_of_san_ending in
  let followed_by =
    match next_block with None -> [] | Some san_block -> san_block.label :: []
  in
  let followed_by =
    match ending with
    | Some (TySE_return _) -> []
    | Some (TYSE_If { if_label; else_label; _ }) ->
        if_label :: else_label :: followed_by
    | None -> followed_by
  in
  create_basic_block ~label ~cfg_statements ~followed_by ~ending:cfg_ending

let basic_of_san_tyfunction san_tyfunction =
  let rec make_blocks blocks =
    match blocks with
    | [] -> []
    | t :: [] -> [ cfg_block_of_san_block ~next_block:None t ]
    | h1 :: (h2 :: q as xs) ->
        cfg_block_of_san_block ~next_block:(Some h2) h1 :: make_blocks xs
  in
  let open Basic in
  create_cfg ~entry_block:san_tyfunction.fn_name
    ~parameters:san_tyfunction.parameters ~locals_vars:san_tyfunction.locals
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

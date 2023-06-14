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

open SanTyAst
open SanFrontend.SanAst
open SanFrontend.SanPosition
open SanFrontend.SanTypechecker
open SanFrontend

let typed_atom t atom = { atom_type = t; atom }
let typed_rvalue t san_rvalue = { san_rvalue; san_type = t }

let rec of_san_basic_block san_module ~acc env
    ({ label; statements; ending } as block) : SanEnv.env * ty_san_basic_block =
  match statements with
  | decl :: q -> (
      match decl with
      | SSDeclaration (s, rvlaue) ->
          let san_rtype = typeof_rvalue san_module env rvlaue in
          let san_rvalue =
            match rvlaue.value with
            | RVExpr atom ->
                let atom_type = typeof_atom san_module env atom in
                TyRVExpr (typed_atom atom_type atom.value)
            | RVUnary { unop; atom } ->
                let atom_type = typeof_atom san_module env atom in
                TYRVUnary { unop; ty_atom = typed_atom atom_type atom.value }
            | RVBinary { binop; blhs; brhs } ->
                let latom_type = typeof_atom san_module env blhs in
                let ratom_type = typeof_atom san_module env brhs in
                TYRVBinary
                  {
                    binop;
                    tylhs = typed_atom latom_type blhs.value;
                    tyrhs = typed_atom ratom_type brhs.value;
                  }
            | RVFunctionCall { fn_name; parameters } ->
                TyRVFunctionCall
                  {
                    fn_name = fn_name.value;
                    parameters =
                      parameters
                      |> List.map (fun atom_loc ->
                             let atom_type =
                               typeof_atom san_module env atom_loc
                             in
                             typed_atom atom_type atom_loc.value);
                  }
            | RVLater san_type -> TYRVLater san_type.value
            | RVDiscard san_type -> TyRVDiscard san_type.value
          in
          let exented =
            TySSDeclaration (s.value, typed_rvalue san_rtype san_rvalue) :: acc
          in
          let extented_env = SanEnv.add (s.value, san_rtype) env in
          of_san_basic_block san_module ~acc:exented extented_env
            { block with statements = q })
  | [] ->
      let ending =
        ending
        |> Option.map (function
             | SE_return atom ->
                 let atom_type = typeof_atom san_module env atom in
                 TySE_return (typed_atom atom_type atom.value)
             | SE_If { expr; if_label; else_label } ->
                 let atom_type = typeof_atom san_module env expr in
                 TYSE_If
                   {
                     expr = typed_atom atom_type expr.value;
                     if_label = if_label.value;
                     else_label = else_label.value;
                   })
      in
      let statements = List.rev acc in
      (env, { label = label.value; statements; ending })

let of_san_basic_blocks san_module env san_basic_blocks =
  san_basic_blocks
  |> List.fold_left
       (fun (acc_env, acc_ty_bbl) basic_block ->
         let extended_env, ty_bbl =
           of_san_basic_block san_module ~acc:[] acc_env basic_block
         in
         (extended_env, ty_bbl :: acc_ty_bbl))
       (env, [])
  |> fun (env, list) -> (env, List.rev list)

let of_san_node san_module = function
  | External { fn_name; signature = params, return; cname } ->
      TyExternal
        {
          fn_name = fn_name.value;
          signature = (params |> List.map value, return.value);
          cname = cname |> Option.map value;
        }
  | Declaration { fn_name; parameters; return_type; san_basic_blocks } ->
      let parameters = parameters |> List.map assoc_value in
      let env = SanEnv.of_list parameters in
      let locals, san_basic_blocks =
        of_san_basic_blocks san_module env san_basic_blocks
      in
      TyDeclaration
        {
          fn_name = fn_name.value;
          parameters;
          return_type = return_type.value;
          san_basic_blocks;
          locals;
        }

let of_san_module san_module = san_module |> List.map (of_san_node san_module)

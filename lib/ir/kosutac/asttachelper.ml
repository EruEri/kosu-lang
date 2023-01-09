(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
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

open KosuIrTyped.Asttyped
open Asttac
open Util

module Operator = struct
  let parser_unary_op_of_tac_unary_op =
    let open KosuFrontend.Ast in
    function TacNot -> PNot | TacUminus -> PUMinus

  let parser_binary_op_of_tac_binary_op =
    let open KosuFrontend.Ast in
    function
    | TacSelf TacAdd -> Add
    | TacSelf TacMinus -> Minus
    | TacSelf TacMult -> Mult
    | TacSelf TacDiv -> Div
    | TacSelf TacModulo -> Modulo
    | TacSelf TacBitwiseOr -> BitwiseOr
    | TacSelf TacBitwiseAnd -> BitwiseAnd
    | TacSelf TacBitwiseXor -> BitwiseXor
    | TacSelf TacShiftLeft -> ShiftLeft
    | TacSelf TacShiftRight -> ShiftRight
    | TacBool TacSup | TacBool TacSupEq -> Sup
    | TacBool TacInf | TacBool TacInfEq -> Inf
    | TacBool TacEqual | TacBool TacDiff -> Equal
    | _ -> failwith "Operator not Overridable"

  let bin_operantor = function
    | RBAdd _ -> TacSelf TacAdd
    | RBMinus _ -> TacSelf TacMinus
    | RBMult _ -> TacSelf TacMult
    | RBDiv _ -> TacSelf TacDiv
    | RBMod _ -> TacSelf TacModulo
    | RBBitwiseOr _ -> TacSelf TacBitwiseOr
    | RBBitwiseAnd _ -> TacSelf TacBitwiseAnd
    | RBBitwiseXor _ -> TacSelf TacBitwiseXor
    | RBShiftLeft _ -> TacSelf TacShiftLeft
    | RBShiftRight _ -> TacSelf TacShiftRight
    | RBAnd _ -> TacBool TacAnd
    | RBOr _ -> TacBool TacOr
    | RBSup _ -> TacBool TacSup
    | RBSupEq _ -> TacBool TacSupEq
    | RBInf _ -> TacBool TacInf
    | RBInfEq _ -> TacBool TacInfEq
    | RBEqual _ -> TacBool TacEqual
    | RBDif _ -> TacBool TacDiff

  let unary_operator = function RUMinus _ -> TacUminus | RUNot _ -> TacNot
  let typed_operand = function RUMinus e | RUNot e -> e
  let typed_operandes = KosuIrTyped.Asttyhelper.Binop.operands
end

module OperatorDeclaration = struct
  type t = Asttac.tac_operator_decl

  let tac_body = function
    | TacUnary { tac_body; _ } | TacBinary { tac_body; _ } -> tac_body
end

module Function = struct
  let label_of_fn_name current_module name typed_list =
    Printf.sprintf "_%s.%s_%s" current_module name
      (typed_list
      |> List.map KosuIrTyped.Asttypprint.string_of_label_rktype
      |> String.concat "_")

  let label_of_fn_name fn_module rfunction_decl =
    if rfunction_decl.rfn_name = "main" then "_main"
    else
      label_of_fn_name fn_module rfunction_decl.rfn_name
        (rfunction_decl.rparameters |> List.map snd)
end

module StringLitteral = struct
  let make_string_litteral_label count =
    let lab =
      Printf.sprintf "l_.str%s"
        (if count > 0 then Printf.sprintf ".%u" count else "")
    in
    SLit lab

  let rec map_fill_string_lit_of_tac_expression map expression () =
    match expression with
    | TEString s -> (
        match Hashtbl.find_opt map s with
        | None ->
            Hashtbl.add map s
              (make_string_litteral_label (map |> Hashtbl.length))
        | Some _ -> ())
    | _ -> ()

  and map_fill_string_lit_of_trvalue map trvalue () =
    match trvalue with
    | RVUminus e | RVNot e -> map_fill_string_lit_of_trvalue map e.rvalue ()
    | RVExpression e ->
        map_fill_string_lit_of_tac_expression map e.tac_expression ()
    | RVFunction { tac_parameters; _ } ->
        tac_parameters
        |> List.iter (fun typed_expr ->
               map_fill_string_lit_of_tac_expression map
                 typed_expr.tac_expression ())
    | RVStruct { fields; _ } ->
        fields
        |> List.iter (fun (_, typed_expr) ->
               map_fill_string_lit_of_tac_expression map
                 typed_expr.tac_expression ())
    | RVEnum { assoc_tac_exprs; _ } ->
        assoc_tac_exprs
        |> List.iter (fun typed_expr ->
               map_fill_string_lit_of_tac_expression map
                 typed_expr.tac_expression ())
    | RVBuiltinCall { parameters; _ } | RVTuple parameters ->
        parameters
        |> List.iter (fun typed_expr ->
               map_fill_string_lit_of_tac_expression map
                 typed_expr.tac_expression ())
    | RVCustomBinop bin | RVBuiltinBinop bin ->
        let () =
          map_fill_string_lit_of_tac_expression map bin.blhs.tac_expression ()
        in
        map_fill_string_lit_of_tac_expression map bin.brhs.tac_expression ()
    | RVCustomUnop un | RVBuiltinUnop un ->
        map_fill_string_lit_of_tac_expression map un.expr.tac_expression ()
    | RVFieldAcess { first_expr; _ } ->
        map_fill_string_lit_of_tac_expression map first_expr.tac_expression ()
    | RVAdress _ | RVDefer _ | RVDiscard | RVLater -> ()

  and map_fill_string_lit_of_tac_case map
      { statement_for_condition; condition; tac_body; _ } () =
    let () =
      statement_for_condition
      |> List.iter (fun stmt ->
             map_fill_string_lit_of_tac_statement map stmt ())
    in
    let () =
      map_fill_string_lit_of_tac_expression map condition.tac_expression ()
    in
    let () = map_fill_string_lit_of_tac_body map tac_body () in
    ()

  and map_fill_string_lit_of_tac_switch map { switch_tac_body; _ } () =
    map_fill_string_lit_of_tac_body map switch_tac_body ()

  and map_fill_string_lit_of_tac_statement map statement () =
    match statement with
    | STacDeclaration { trvalue; _ }
    | STacModification { trvalue; _ }
    | STDerefAffectation { trvalue; _ } ->
        map_fill_string_lit_of_trvalue map trvalue.rvalue ()
    | STIf
        { statement_for_bool; condition_rvalue; if_tac_body; else_tac_body; _ }
      ->
        let () =
          statement_for_bool
          |> List.iter (fun lstmt ->
                 map_fill_string_lit_of_tac_statement map lstmt ())
        in
        let () =
          map_fill_string_lit_of_tac_expression map
            condition_rvalue.tac_expression ()
        in
        let () = map_fill_string_lit_of_tac_body map if_tac_body () in
        let () = map_fill_string_lit_of_tac_body map else_tac_body () in
        ()
    | STSwitch
        { statemenets_for_case; condition_switch; sw_cases; wildcard_body; _ }
      ->
        let () =
          statemenets_for_case
          |> List.iter (fun smt ->
                 map_fill_string_lit_of_tac_statement map smt ())
        in
        let () =
          map_fill_string_lit_of_tac_expression map
            condition_switch.tac_expression ()
        in
        let () =
          sw_cases
          |> List.iter (fun tac_switch ->
                 map_fill_string_lit_of_tac_switch map tac_switch ())
        in
        let () =
          wildcard_body
          |> Option.iter (fun tb -> map_fill_string_lit_of_tac_body map tb ())
        in
        ()
    | SCases { cases; else_tac_body; _ } ->
        let () =
          cases
          |> List.iter (fun case -> map_fill_string_lit_of_tac_case map case ())
        in
        map_fill_string_lit_of_tac_body map else_tac_body ()

  and map_fill_string_lit_of_tac_body map { label = _; body = statements, last }
      () =
    let () =
      statements
      |> List.iter (fun stmt ->
             map_fill_string_lit_of_tac_statement map stmt ())
    in
    last
    |> Option.iter (fun expr ->
           map_fill_string_lit_of_tac_expression map expr.tac_expression ())

  and map_fill_string_lit_of_module_node map node () =
    match node with
    | TNFunction { tac_body; _ } ->
        map_fill_string_lit_of_tac_body map tac_body ()
    | TNOperator op ->
        let body = OperatorDeclaration.tac_body op in
        map_fill_string_lit_of_tac_body map body ()
    (* | TNConst {rconst_name; value = {rktype = RTString_lit; rexpression = REstring s}} ->
       Hashtbl.add map s (SLit rconst_name) *)
    | _ -> ()

  let map_of_string_litteral_in_module (TacModule rmodule) () =
    let map = Hashtbl.create 10 in
    let () =
      rmodule
      |> List.iter (fun modul ->
             map_fill_string_lit_of_module_node map modul ())
    in
    map

  let map_string_litteral_of_named_rmodule_path
      { filename = _; tac_module_path = { path = _; tac_module }; rprogram = _ }
      =
    map_of_string_litteral_in_module tac_module

  let maps_of_prgram program =
    program
    |> List.map (fun ({ filename; _ } as named_module) ->
           (filename, map_string_litteral_of_named_rmodule_path named_module ()))

  let string_of_binding hashmap =
    hashmap |> Hashtbl.to_seq
    |> Seq.map (fun (s, SLit label) -> Printf.sprintf "%s : \"%s\"" label s)
    |> List.of_seq |> String.concat "\n\t"
end

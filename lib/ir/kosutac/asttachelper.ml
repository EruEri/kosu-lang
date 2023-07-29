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
    let open KosuIrTyped.Asttyhelper.Binop in
    function
    | TacSelf TacAdd ->
        pbadd
    | TacSelf TacMinus ->
        pbminus
    | TacSelf TacMult ->
        pbmult
    | TacSelf TacDiv ->
        pbdiv
    | TacSelf TacModulo ->
        pbmodulo
    | TacSelf TacBitwiseOr ->
        pbbitwiseor
    | TacSelf TacBitwiseAnd ->
        pbbitwiseoand
    | TacSelf TacBitwiseXor ->
        pbbitwisexor
    | TacSelf TacShiftLeft ->
        pbshiftleft
    | TacSelf TacShiftRight ->
        pbshiftright
    | TacBool TacSup ->
        pbsup
    | TacBool TacSupEq ->
        pbsupeq
    | TacBool TacInf ->
        pbinf
    | TacBool TacInfEq ->
        pbinfeq
    | TacBool TacEqual ->
        pbequal
    | TacBool TacDiff ->
        pbdiff
    | TacCmp TacOrdered ->
        pbordered
    | TacBool (TacOr | TacAnd) ->
        failwith "Operator not overloadable"

  let bin_operantor = function
    | RBAdd _ ->
        TacSelf TacAdd
    | RBMinus _ ->
        TacSelf TacMinus
    | RBMult _ ->
        TacSelf TacMult
    | RBDiv _ ->
        TacSelf TacDiv
    | RBMod _ ->
        TacSelf TacModulo
    | RBBitwiseOr _ ->
        TacSelf TacBitwiseOr
    | RBBitwiseAnd _ ->
        TacSelf TacBitwiseAnd
    | RBBitwiseXor _ ->
        TacSelf TacBitwiseXor
    | RBShiftLeft _ ->
        TacSelf TacShiftLeft
    | RBShiftRight _ ->
        TacSelf TacShiftRight
    | RBAnd _ ->
        TacBool TacAnd
    | RBOr _ ->
        TacBool TacOr
    | RBSup _ ->
        TacBool TacSup
    | RBSupEq _ ->
        TacBool TacSupEq
    | RBInf _ ->
        TacBool TacInf
    | RBInfEq _ ->
        TacBool TacInfEq
    | RBEqual _ ->
        TacBool TacEqual
    | RBDif _ ->
        TacBool TacDiff
    | RBCmp _ ->
        TacCmp TacOrdered

  let unary_operator = function RUMinus _ -> TacUminus | RUNot _ -> TacNot
  let typed_operand = function RUMinus e | RUNot e -> e
  let typed_operandes = KosuIrTyped.Asttyhelper.Binop.operands
end

module OperatorDeclaration = struct
  type t = Asttac.tac_operator_decl

  let tac_body = function
    | TacUnary { tac_body; _ } | TacBinary { tac_body; _ } ->
        tac_body

  let tac_function_of_operator : tac_operator_decl -> tac_function_decl =
    function
    | TacUnary
        {
          op = _;
          asm_name;
          rfield;
          return_type;
          tac_body;
          fn_call_infos;
          locale_var;
          discarded_values;
        } ->
        {
          rfn_name = asm_name;
          generics = [];
          rparameters = [ rfield ];
          return_type;
          tac_body;
          fn_call_infos;
          locale_var;
          discarded_values;
        }
    | TacBinary
        {
          op = _;
          asm_name;
          rfields = lhs, rhs;
          return_type;
          tac_body;
          fn_call_infos;
          locale_var;
          discarded_values;
        } ->
        {
          rfn_name = asm_name;
          generics = [];
          rparameters = [ lhs; rhs ];
          return_type;
          tac_body;
          fn_call_infos;
          locale_var;
          discarded_values;
        }
end

module Function = struct
  let label_of_fn_name current_module name typed_list =
    Printf.sprintf "_%s.%s_%s" current_module name
      (typed_list
      |> List.map KosuIrTyped.Asttypprint.string_of_label_rktype
      |> String.concat "_"
      )

  let label_of_fn_name fn_module rfunction_decl =
    if rfunction_decl.rfn_name = "main" then
      "_main"
    else
      label_of_fn_name fn_module rfunction_decl.rfn_name
        (rfunction_decl.rparameters |> List.map snd)
end

module StringLitteral = struct
  let make_string_litteral_label count =
    let lab =
      Printf.sprintf "l_.str%s"
        ( if count > 0 then
            Printf.sprintf ".%u" count
          else
            ""
        )
    in
    SLit lab

  let rec map_fill_string_lit_of_tac_expression ~null_terminated_string map
      expression () =
    match expression with
    | TEString s -> (
        let s =
          match null_terminated_string with
          | false ->
              s
          | true ->
              Printf.sprintf "%s\000" s
        in
        match Hashtbl.find_opt map s with
        | None ->
            Hashtbl.add map s @@ make_string_litteral_label
            @@ Hashtbl.length map
        | Some _ ->
            ()
      )
    | _ ->
        ()

  and map_fill_string_lit_of_trvalue ~null_terminated_string map trvalue () =
    match trvalue with
    | RVExpression e ->
        map_fill_string_lit_of_tac_expression ~null_terminated_string map
          e.tac_expression ()
    | RVFunction { tac_parameters; _ } ->
        tac_parameters
        |> List.iter (fun typed_expr ->
               map_fill_string_lit_of_tac_expression ~null_terminated_string map
                 typed_expr.tac_expression ()
           )
    | RVStruct { fields; _ } ->
        fields
        |> List.iter (fun (_, typed_expr) ->
               map_fill_string_lit_of_tac_expression ~null_terminated_string map
                 typed_expr.tac_expression ()
           )
    | RVArrayAccess { array_expr; index_expr } ->
        let () =
          map_fill_string_lit_of_tac_expression ~null_terminated_string map
            array_expr.tac_expression ()
        in
        let () =
          map_fill_string_lit_of_tac_expression ~null_terminated_string map
            index_expr.tac_expression ()
        in
        ()
    | RVEnum { assoc_tac_exprs; _ } ->
        assoc_tac_exprs
        |> List.iter (fun typed_expr ->
               map_fill_string_lit_of_tac_expression ~null_terminated_string map
                 typed_expr.tac_expression ()
           )
    | RVBuiltinCall { parameters; _ } | RVTuple parameters | RVArray parameters
      ->
        parameters
        |> List.iter (fun typed_expr ->
               map_fill_string_lit_of_tac_expression ~null_terminated_string map
                 typed_expr.tac_expression ()
           )
    | RVCustomBinop bin | RVBuiltinBinop bin ->
        let () =
          map_fill_string_lit_of_tac_expression ~null_terminated_string map
            bin.blhs.tac_expression ()
        in
        map_fill_string_lit_of_tac_expression ~null_terminated_string map
          bin.brhs.tac_expression ()
    | RVCustomUnop un | RVBuiltinUnop un ->
        map_fill_string_lit_of_tac_expression ~null_terminated_string map
          un.expr.tac_expression ()
    | RVFieldAcess { first_expr; _ } | RVTupleAccess { first_expr; _ } ->
        map_fill_string_lit_of_tac_expression ~null_terminated_string map
          first_expr.tac_expression ()
    | RVAdress _ | RVAdressof _ | RVDefer _ | RVDiscard | RVLater ->
        ()

  and map_fill_string_lit_of_tac_case ~null_terminated_string map
      { statement_for_condition; condition; tac_body; _ } () =
    let () =
      statement_for_condition
      |> List.iter (fun stmt ->
             map_fill_string_lit_of_tac_statement ~null_terminated_string map
               stmt ()
         )
    in
    let () =
      map_fill_string_lit_of_tac_expression ~null_terminated_string map
        condition.tac_expression ()
    in
    let () =
      map_fill_string_lit_of_tac_body ~null_terminated_string map tac_body ()
    in
    ()

  and map_fill_string_lit_of_tac_switch map { switch_tac_body; _ } () =
    map_fill_string_lit_of_tac_body map switch_tac_body ()

  and map_fill_string_lit_of_tac_statement ~null_terminated_string map statement
      () =
    match statement with
    | STacDeclaration { trvalue; _ }
    | STacModification { trvalue; _ }
    | STDerefAffectationField { trvalue; _ }
    | STacModificationField { trvalue; _ }
    | STDerefAffectation { trvalue; _ } ->
        map_fill_string_lit_of_trvalue ~null_terminated_string map
          trvalue.rvalue ()
    | STWhile
        {
          statements_condition;
          condition;
          loop_body;
          inner_body_label = _;
          self_label = _;
          exit_label = _;
        } ->
        let () =
          statements_condition
          |> List.iter (fun lstmt ->
                 map_fill_string_lit_of_tac_statement ~null_terminated_string
                   map lstmt ()
             )
        in
        let () =
          map_fill_string_lit_of_tac_expression ~null_terminated_string map
            condition.tac_expression ()
        in
        let () =
          map_fill_string_lit_of_tac_body ~null_terminated_string map loop_body
            ()
        in
        ()
    | STIf
        { statement_for_bool; condition_rvalue; if_tac_body; else_tac_body; _ }
      ->
        let () =
          statement_for_bool
          |> List.iter (fun lstmt ->
                 map_fill_string_lit_of_tac_statement ~null_terminated_string
                   map lstmt ()
             )
        in
        let () =
          map_fill_string_lit_of_tac_expression ~null_terminated_string map
            condition_rvalue.tac_expression ()
        in
        let () =
          map_fill_string_lit_of_tac_body ~null_terminated_string map
            if_tac_body ()
        in
        let () =
          map_fill_string_lit_of_tac_body ~null_terminated_string map
            else_tac_body ()
        in
        ()
    | STSwitch
        { statemenets_for_case; condition_switch; sw_cases; wildcard_body; _ }
      ->
        let () =
          statemenets_for_case
          |> List.iter (fun smt ->
                 map_fill_string_lit_of_tac_statement ~null_terminated_string
                   map smt ()
             )
        in
        let () =
          map_fill_string_lit_of_tac_expression ~null_terminated_string map
            condition_switch.tac_expression ()
        in
        let () =
          sw_cases
          |> List.iter (fun tac_switch ->
                 map_fill_string_lit_of_tac_switch ~null_terminated_string map
                   tac_switch ()
             )
        in
        let () =
          wildcard_body
          |> Option.iter (fun tb ->
                 map_fill_string_lit_of_tac_body ~null_terminated_string map tb
                   ()
             )
        in
        ()
    | STSwitchTmp
        {
          tmp_statemenets_for_case;
          tag_atom;
          tmp_switch_list;
          tmp_wildcard_body;
          _;
        } ->
        let () =
          tmp_statemenets_for_case
          |> List.iter (fun smt ->
                 map_fill_string_lit_of_tac_statement ~null_terminated_string
                   map smt ()
             )
        in
        let () =
          map_fill_string_lit_of_tac_expression ~null_terminated_string map
            tag_atom.tac_expression ()
        in

        let () =
          tmp_switch_list
          |> List.iter (fun sw ->
                 let () =
                   sw.variants
                   |> List.iter (fun { cmp_statement; _ } ->
                          map_fill_string_lit_of_tac_statement
                            ~null_terminated_string map cmp_statement ()
                      )
                 in
                 let () =
                   map_fill_string_lit_of_tac_body ~null_terminated_string map
                     sw.tmp_switch_tac_body ()
                 in
                 ()
             )
        in
        let () =
          tmp_wildcard_body
          |> Option.iter (fun tb ->
                 map_fill_string_lit_of_tac_body ~null_terminated_string map tb
                   ()
             )
        in
        ()
    | SCases { cases; else_tac_body; _ } ->
        let () =
          cases
          |> List.iter (fun case ->
                 map_fill_string_lit_of_tac_case map ~null_terminated_string
                   case ()
             )
        in
        map_fill_string_lit_of_tac_body ~null_terminated_string map
          else_tac_body ()

  and map_fill_string_lit_of_tac_body ~null_terminated_string map
      { label = _; body = statements, last } () =
    let () =
      statements
      |> List.iter (fun stmt ->
             map_fill_string_lit_of_tac_statement ~null_terminated_string map
               stmt ()
         )
    in
    last
    |> Option.iter (fun expr ->
           map_fill_string_lit_of_tac_expression ~null_terminated_string map
             expr.tac_expression ()
       )

  and map_fill_string_lit_of_module_node ~null_terminated_string map node () =
    match node with
    | TNFunction { tac_body; _ } ->
        map_fill_string_lit_of_tac_body ~null_terminated_string map tac_body ()
    | TNOperator op ->
        let body = OperatorDeclaration.tac_body op in
        map_fill_string_lit_of_tac_body ~null_terminated_string map body ()
    (* | TNConst {rconst_name; value = {rktype = RTString_lit; rexpression = REstring s}} ->
       Hashtbl.add map s (SLit rconst_name) *)
    | _ ->
        ()

  let map_of_string_litteral_in_module ~null_terminated_string
      (TacModule rmodule) () =
    let map = Hashtbl.create 10 in
    let () =
      rmodule
      |> List.iter (fun modul ->
             map_fill_string_lit_of_module_node ~null_terminated_string map
               modul ()
         )
    in
    map

  let map_string_litteral_of_named_rmodule_path ~null_terminated_string
      { filename = _; tac_module_path = { path = _; tac_module }; rprogram = _ }
      =
    map_of_string_litteral_in_module ~null_terminated_string tac_module

  let maps_of_prgram program =
    program
    |> List.map (fun ({ filename; _ } as named_module) ->
           (filename, map_string_litteral_of_named_rmodule_path named_module ())
       )

  let string_of_binding hashmap =
    hashmap |> Hashtbl.to_seq
    |> Seq.map (fun (s, SLit label) -> Printf.sprintf "%s : \"%s\"" label s)
    |> List.of_seq |> String.concat "\n\t"
end

module LocaleVariable = struct
  let variable_of_tac_locale_variable { locale_ty; locale } =
    match locale with
    | Locale s ->
        (s, locale_ty)
    | Enum_Assoc_id { name; _ } ->
        (name, locale_ty)
end

module FloatLitteral = struct
  let make_float_litteral_label count =
    let lab =
      Printf.sprintf "l_.float%s"
        ( if count > 0 then
            Printf.sprintf ".%u" count
          else
            ""
        )
    in
    FLit lab

  let rec map_fill_float_lit_of_tac_expression map expression () =
    match expression with
    | TEFloat float -> (
        (* let () = Printf.printf "%s\n" (match fst float with KosuFrontend.Ast.F32 -> "f32" | _ -> "f64") in *)
        match Hashtbl.find_opt map float with
        | None ->
            Hashtbl.add map float
              (make_float_litteral_label (map |> Hashtbl.length))
        | Some _ ->
            ()
      )
    | _ ->
        ()

  and map_fill_float_lit_of_trvalue map trvalue () =
    match trvalue with
    | RVExpression e ->
        map_fill_float_lit_of_tac_expression map e.tac_expression ()
    | RVFunction { tac_parameters; _ } ->
        tac_parameters
        |> List.iter (fun typed_expr ->
               map_fill_float_lit_of_tac_expression map
                 typed_expr.tac_expression ()
           )
    | RVArrayAccess { array_expr; index_expr } ->
        let () =
          map_fill_float_lit_of_tac_expression map array_expr.tac_expression ()
        in
        let () =
          map_fill_float_lit_of_tac_expression map index_expr.tac_expression ()
        in
        ()
    | RVStruct { fields; _ } ->
        fields
        |> List.iter (fun (_, typed_expr) ->
               map_fill_float_lit_of_tac_expression map
                 typed_expr.tac_expression ()
           )
    | RVEnum { assoc_tac_exprs; _ } ->
        assoc_tac_exprs
        |> List.iter (fun typed_expr ->
               map_fill_float_lit_of_tac_expression map
                 typed_expr.tac_expression ()
           )
    | RVBuiltinCall { parameters; _ } | RVTuple parameters | RVArray parameters
      ->
        parameters
        |> List.iter (fun typed_expr ->
               map_fill_float_lit_of_tac_expression map
                 typed_expr.tac_expression ()
           )
    | RVCustomBinop bin | RVBuiltinBinop bin ->
        let () =
          map_fill_float_lit_of_tac_expression map bin.blhs.tac_expression ()
        in
        map_fill_float_lit_of_tac_expression map bin.brhs.tac_expression ()
    | RVCustomUnop un | RVBuiltinUnop un ->
        map_fill_float_lit_of_tac_expression map un.expr.tac_expression ()
    | RVFieldAcess { first_expr; _ } | RVTupleAccess { first_expr; _ } ->
        map_fill_float_lit_of_tac_expression map first_expr.tac_expression ()
    | RVAdress _ | RVAdressof _ | RVDefer _ | RVDiscard | RVLater ->
        ()

  and map_fill_float_lit_of_tac_case map
      { statement_for_condition; condition; tac_body; _ } () =
    let () =
      statement_for_condition
      |> List.iter (fun stmt -> map_fill_float_lit_of_tac_statement map stmt ())
    in
    let () =
      map_fill_float_lit_of_tac_expression map condition.tac_expression ()
    in
    let () = map_fill_float_lit_of_tac_body map tac_body () in
    ()

  and map_fill_float_lit_of_tac_switch map { switch_tac_body; _ } () =
    map_fill_float_lit_of_tac_body map switch_tac_body ()

  and map_fill_float_lit_of_tac_statement map statement () =
    match statement with
    | STacDeclaration { trvalue; _ }
    | STacModification { trvalue; _ }
    | STDerefAffectationField { trvalue; _ }
    | STacModificationField { trvalue; _ }
    | STDerefAffectation { trvalue; _ } ->
        map_fill_float_lit_of_trvalue map trvalue.rvalue ()
    | STWhile
        {
          statements_condition;
          condition;
          loop_body;
          inner_body_label = _;
          self_label = _;
          exit_label = _;
        } ->
        let () =
          statements_condition
          |> List.iter (fun lstmt ->
                 map_fill_float_lit_of_tac_statement map lstmt ()
             )
        in
        let () =
          map_fill_float_lit_of_tac_expression map condition.tac_expression ()
        in
        let () = map_fill_float_lit_of_tac_body map loop_body () in
        ()
    | STIf
        { statement_for_bool; condition_rvalue; if_tac_body; else_tac_body; _ }
      ->
        let () =
          statement_for_bool
          |> List.iter (fun lstmt ->
                 map_fill_float_lit_of_tac_statement map lstmt ()
             )
        in
        let () =
          map_fill_float_lit_of_tac_expression map
            condition_rvalue.tac_expression ()
        in
        let () = map_fill_float_lit_of_tac_body map if_tac_body () in
        let () = map_fill_float_lit_of_tac_body map else_tac_body () in
        ()
    | STSwitch
        { statemenets_for_case; condition_switch; sw_cases; wildcard_body; _ }
      ->
        let () =
          statemenets_for_case
          |> List.iter (fun smt ->
                 map_fill_float_lit_of_tac_statement map smt ()
             )
        in
        let () =
          map_fill_float_lit_of_tac_expression map
            condition_switch.tac_expression ()
        in
        let () =
          sw_cases
          |> List.iter (fun tac_switch ->
                 map_fill_float_lit_of_tac_switch map tac_switch ()
             )
        in
        let () =
          wildcard_body
          |> Option.iter (fun tb -> map_fill_float_lit_of_tac_body map tb ())
        in
        ()
    | SCases { cases; else_tac_body; _ } ->
        let () =
          cases
          |> List.iter (fun case -> map_fill_float_lit_of_tac_case map case ())
        in
        map_fill_float_lit_of_tac_body map else_tac_body ()
    | STSwitchTmp
        {
          tmp_statemenets_for_case;
          tag_atom;
          tmp_switch_list;
          tmp_wildcard_body;
          _;
        } ->
        let () =
          tmp_statemenets_for_case
          |> List.iter (fun smt ->
                 map_fill_float_lit_of_tac_statement map smt ()
             )
        in
        let () =
          map_fill_float_lit_of_tac_expression map tag_atom.tac_expression ()
        in

        let () =
          tmp_switch_list
          |> List.iter (fun sw ->
                 let () =
                   sw.variants
                   |> List.iter (fun { cmp_statement; _ } ->
                          map_fill_float_lit_of_tac_statement map cmp_statement
                            ()
                      )
                 in
                 let () =
                   map_fill_float_lit_of_tac_body map sw.tmp_switch_tac_body ()
                 in
                 ()
             )
        in
        let () =
          tmp_wildcard_body
          |> Option.iter (fun tb -> map_fill_float_lit_of_tac_body map tb ())
        in
        ()

  and map_fill_float_lit_of_tac_body map { label = _; body = statements, last }
      () =
    let () =
      statements
      |> List.iter (fun stmt -> map_fill_float_lit_of_tac_statement map stmt ())
    in
    last
    |> Option.iter (fun expr ->
           map_fill_float_lit_of_tac_expression map expr.tac_expression ()
       )

  and map_fill_float_lit_of_module_node map node () =
    match node with
    | TNFunction { tac_body; _ } ->
        map_fill_float_lit_of_tac_body map tac_body ()
    | TNOperator op ->
        let body = OperatorDeclaration.tac_body op in
        map_fill_float_lit_of_tac_body map body ()
    (* | TNConst {rconst_name; value = {rktype = RTfloat_lit; rexpression = REfloat s}} ->
       Hashtbl.add map s (SLit rconst_name) *)
    | _ ->
        ()

  let map_of_float_litteral_in_module (TacModule rmodule) () =
    let map = Hashtbl.create 10 in
    let () =
      rmodule
      |> List.iter (fun modul -> map_fill_float_lit_of_module_node map modul ())
    in
    map

  let map_float_litteral_of_named_rmodule_path
      { filename = _; tac_module_path = { path = _; tac_module }; rprogram = _ }
      =
    map_of_float_litteral_in_module tac_module

  let maps_of_prgram program =
    program
    |> List.map (fun ({ filename; _ } as named_module) ->
           (filename, map_float_litteral_of_named_rmodule_path named_module ())
       )

  let float_of_binding hashmap =
    hashmap |> Hashtbl.to_seq
    |> Seq.map (fun (s, SLit label) -> Printf.sprintf "%s : \"%s\"" label s)
    |> List.of_seq |> String.concat "\n\t"
end

module Convertion = struct

  let null_terminate = 
    Printf.sprintf "%s\000"

  let rec null_terminate_typed_expression ty_ex = 
    {
      ty_ex with rexpression = null_terminate_expression ty_ex.rexpression
    }

  and null_terminate_expression = function
  | REstring s ->
    REstring (null_terminate s)
  | RETupleAccess { first_expr; index } 
  -> 
    let first_expr = null_terminate_typed_expression first_expr in
    RETupleAccess {first_expr; index}
  | REFieldAcces { first_expr; field }
  ->
    let first_expr = null_terminate_typed_expression first_expr in
    REFieldAcces {first_expr; field}
  | REArrayAccess {
      array_expr;
      index_expr;
    } ->
      let array_expr = null_terminate_typed_expression array_expr in
      let index_expr = null_terminate_typed_expression index_expr in
      REArrayAccess {array_expr; index_expr}
  | REStruct {
    modules_path;
    struct_name;
    fields : (string * typed_expression) list;
  } -> 
    let fields = List.map (fun (s, ty) -> s, null_terminate_typed_expression ty) fields in
    REStruct {modules_path; struct_name; fields}
    | REEnum {
      modules_path;
      enum_name;
      variant;
      assoc_exprs;
    }
    ->
      let assoc_exprs = List.map null_terminate_typed_expression assoc_exprs in
      REEnum {modules_path; enum_name; variant; assoc_exprs}
  | REArray tes ->
    REArray (List.map null_terminate_typed_expression tes)
  | RETuple tes ->
    RETuple (List.map null_terminate_typed_expression tes)
  | REBuiltin_Function_call {fn_name; parameters} ->
    let parameters = List.map null_terminate_typed_expression parameters in
    REBuiltin_Function_call {fn_name; parameters}
  | REFunction_call {modules_path; generics_resolver; fn_name; parameters} ->
    let parameters = List.map null_terminate_typed_expression parameters in
    REFunction_call {modules_path; generics_resolver; fn_name; parameters}
  | REBinOperator_Function_call rkbin_op -> 
    REBinOperator_Function_call (null_terminate_rkbin_op rkbin_op)
  | REUnOperator_Function_call  rkunary_op ->
    REUnOperator_Function_call (null_terminate_rkunary_op rkunary_op)
  | REWhile (ty_ex, body) ->
    let ty_ex = null_terminate_typed_expression ty_ex in
    let body = null_terminate_kbody body in
    REWhile (ty_ex, body)
  | REIf (condition, if_true, else_false) ->
    let condition = null_terminate_typed_expression condition in
    let if_true = null_terminate_kbody if_true in
    let else_false = null_terminate_kbody else_false in
    REIf (condition, if_true, else_false)
  | RECases {cases; else_case} ->
    let cases = cases |> List.map (fun (ty, body) ->
      let ty = null_terminate_typed_expression ty in
      let body = null_terminate_kbody body in
      ty, body
    ) in 
    let else_case = null_terminate_kbody else_case in
    RECases {cases; else_case}
  | RESwitch {rexpression; cases; wildcard_case} ->
    let rexpression = null_terminate_typed_expression rexpression in
    let cases = cases |> List.map (fun (rswi, bounds, body) -> 
      let body = null_terminate_kbody body in
      rswi, bounds, body
    )
    in
    let wildcard_case = Option.map null_terminate_kbody wildcard_case in
    RESwitch {rexpression; cases; wildcard_case}
  | REBin_op rkbin_op -> 
    REBin_op (null_terminate_rkbin_op rkbin_op)
  | REUn_op rkunary_op ->
    REUn_op (null_terminate_rkunary_op rkunary_op)
  | REmpty
  | RTrue
  | RFalse
  | RENullptr
  | RECmpLess
  | RECmpEqual
  | RECmpGreater
  | REInteger _
  | REFloat _
  | REChar _
  | RESizeof _
  | REAdress _
  | REAdressof _
  | REDeference _
  | REIdentifier _ 
  | REConst_Identifier _ as e 
    -> e
  and null_terminate_rkunary_op = function
  | RUMinus te -> RUMinus (null_terminate_typed_expression te)
  | RUNot te -> RUNot (null_terminate_typed_expression te)
  and null_terminate_rkbin_op = function
  | RBAdd (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBAdd (lhs, rhs)
  | RBMinus  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBMinus (lhs, rhs)
  | RBMult  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBMult (lhs, rhs)
  | RBDiv (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBDiv (lhs, rhs)
  | RBMod  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBMod (lhs, rhs)
  | RBBitwiseOr  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBBitwiseOr (lhs, rhs)
  | RBBitwiseAnd  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBBitwiseAnd (lhs, rhs)
  | RBBitwiseXor  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBBitwiseXor (lhs, rhs)
  | RBShiftLeft  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBShiftLeft (lhs, rhs)
  | RBShiftRight  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBShiftRight (lhs, rhs)
  | RBAnd  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBAnd  (lhs, rhs)
  | RBOr  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBOr (lhs, rhs)
  | RBSup  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBSup (lhs, rhs)
  | RBSupEq  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBSupEq (lhs, rhs)
  | RBInf  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBInf (lhs, rhs)
  | RBInfEq  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBInfEq (lhs, rhs)
  | RBEqual  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBEqual  (lhs, rhs)
  | RBDif  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBDif (lhs, rhs)
  | RBCmp  (lhs, rhs) ->
    let lhs = null_terminate_typed_expression lhs in
    let rhs = null_terminate_typed_expression rhs in
    RBCmp (lhs, rhs)
  and null_terminate_statement = function
  | RSDeclaration {is_const; variable_name; typed_expression} -> 
    let typed_expression = null_terminate_typed_expression typed_expression in
    RSDeclaration {is_const; variable_name; typed_expression}
  | RSAffection (ra_val, ty) ->
    let ty = null_terminate_typed_expression ty in
    RSAffection (ra_val, ty)
  | RSDiscard ty -> 
    let ty = null_terminate_typed_expression ty in
    RSDiscard ty
  | RSDerefAffectation (raffection, ty) ->
    let ty = null_terminate_typed_expression ty in
    RSDerefAffectation (raffection, ty)
  and null_terminate_kbody (stmts, final_expr) =
    let stmts = List.map null_terminate_statement stmts in
    let final_expr = null_terminate_typed_expression final_expr in
    stmts, final_expr 

  let null_terminate_rconst_decl rconst_decl = 
    {
      rconst_decl with value = null_terminate_typed_expression rconst_decl.value
    }


  let null_terminate_rmodule = function
  | TNConst rconst_decl -> TNConst (null_terminate_rconst_decl rconst_decl)
  | e -> e

  let null_terminated_tac_module (TacModule rmodule) = 
    let rmodules = List.map null_terminate_rmodule rmodule in
    TacModule rmodules

  let null_terminated_named_rmodule_path rmodule = 
    let { filename = _; tac_module_path = { path = _; tac_module } as t; rprogram = _ } as m = rmodule in
    {
      m with tac_module_path = { t with tac_module = null_terminated_tac_module tac_module}
    }


  let null_terminated_program program =
    program
    |> List.map (fun named_module -> 
      null_terminated_named_rmodule_path named_module
    )
end
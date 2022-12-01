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

module Operator = struct
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
  let typed_operandes = KosuIrTyped.Asttyped.Binop.operands
end

let if_count = ref 0
let cases_count = ref 0
let switch_count = ref 0


let make_tmp = Printf.sprintf "$tmp%u"
let make_goto_label ~count_if = Printf.sprintf "if.%u.%u" count_if
let make_end_label ~count_if = Printf.sprintf "if.%u.end" count_if
let make_case_goto_label ~cases_count = Printf.sprintf "case.%u.%u" cases_count

let make_case_goto_cond_label ~cases_count =
  Printf.sprintf "case.%u.%u.cond" cases_count

(**
  [case.%u.end]    
*)
let make_case_end_label ~cases_count = Printf.sprintf "case.%u.end" cases_count

(**
    [case.%u.else]
*)
let make_case_else ~cases_count = Printf.sprintf "case.%u.else" cases_count

let make_switch_goto_label ~switch_count =
  Printf.sprintf "switch.%u.%u" switch_count

let make_switch_wild_label ~switch_count =
  Printf.sprintf "switch.%u.wildcard" switch_count

let make_switch_end_label ~switch_count =
  Printf.sprintf "switch.%u.end" switch_count

(**
@returns: the value of [n] before the incrementation    
*)
let post_inc n =
  let x = !n in
  let () = incr n in
  x

let make_inc_tmp n = make_tmp (post_inc n)

let typed_locale_assoc ~name ~from ~assoc_index_bound ~rktype = {
  locale_ty = rktype;
  locale = Enum_Assoc_id {
    name;
    from;
    assoc_index_bound;
  }
}

let typed_locale_locale id ~rktype = {
  locale_ty = rktype;
  locale = Locale id
}

let add_statements_to_tac_body stmts tac_body =
  let { label; body = future_stmts, future_result } = tac_body in
  { label; body = (stmts @ future_stmts, future_result) }

let make_typed_tac_expression expr_rktype tac_expression = {
  expr_rktype;
  tac_expression
}

let make_typed_tac_rvalue rval_rktype rvalue = {
  rval_rktype;
  rvalue
}
let convert_if_allocated ~expr_rktype ~allocated tac_expression =
  match allocated with
  | None -> ([], make_typed_tac_expression expr_rktype tac_expression)
  | Some (identifier, rktype) ->
    let typed_expr = make_typed_tac_expression expr_rktype (tac_expression) in
      ( STacModification { identifier; trvalue = make_typed_tac_rvalue rktype (RVExpression typed_expr) }
        :: [],
        make_typed_tac_expression rktype
        (TEIdentifier identifier) )

let create_forward_init ?(rvalue = RVLater) ~count_var typed_expression = 
  if Expression.is_typed_expresion_branch typed_expression then
    let new_tmp = make_inc_tmp count_var in
    Some (new_tmp, typed_expression.rktype),
    STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue typed_expression.rktype rvalue}::[]
  else None, []

let rec convert_from_typed_expression ~allocated ~map ~count_var ~if_count
    ~cases_count ~switch_count typed_expression =
  let trktype = typed_expression.rktype in
  let expr = typed_expression.rexpression in
  match (expr, allocated) with
  | REIf (if_typed_expres, if_body, else_body), Some (identifier, id_rktype)  ->
      let incremented = post_inc if_count in
      let next_allocated, stmt = create_forward_init ~count_var if_typed_expres in

      let statement_for_bool, condition_rvalue =
        convert_from_typed_expression ~allocated:(next_allocated) ~switch_count
          ~map ~count_var ~if_count ~cases_count if_typed_expres
      in

      let goto_label1 = make_goto_label ~count_if:incremented 0 in
      let goto_label2 = make_goto_label ~count_if:incremented 1 in
      let exit_label = make_end_label ~count_if:incremented in
      let if_tac_body =
        convert_from_rkbody ~switch_count ~cases_count ~previous_alloc:allocated
          ~label_name:goto_label1 ~map ~count_var ~if_count if_body
      in
      let else_tac_body =
        convert_from_rkbody ~cases_count ~switch_count ~previous_alloc:allocated
          ~label_name:goto_label2 ~map ~count_var ~if_count else_body
      in
      ( STIf
          {
            statement_for_bool = statement_for_bool @ stmt;
            condition_rvalue;
            goto1 = goto_label1;
            goto2 = goto_label2;
            exit_label;
            if_tac_body;
            else_tac_body;
          }
        :: [],
        make_typed_tac_expression id_rktype (TEIdentifier identifier) 
      )
  | RECases { cases; else_case }, Some (identifier, id_rktype) ->
      let incremented = post_inc cases_count in

      let make_locale_label = make_case_goto_label ~cases_count:incremented in
      let lambda_make_locale_condition_label =
        make_case_goto_cond_label ~cases_count:incremented
      in
      let else_label = make_case_else ~cases_count:incremented in
      let end_label = make_case_end_label ~cases_count:incremented in
      let cases_len = cases |> List.length in
      let cases =
        cases
        |> List.mapi (fun i (case_condition, rkbody) ->
               let label = make_locale_label i in
               let self_condition_label =
                 if i = 0 then None
                 else Some (lambda_make_locale_condition_label i)
               in
               let jmp_next_condition =
                 if i < cases_len - 1 then
                   lambda_make_locale_condition_label (i + 1)
                 else else_label
               in
               let next_allocated, stmt = create_forward_init ~count_var case_condition in

               let statement_for_condition, tac_condition =
                 convert_from_typed_expression ~allocated:next_allocated
                   ~switch_count ~map ~count_var ~if_count ~cases_count
                   case_condition
               in
               let tac_body =
                 convert_from_rkbody ~cases_count ~switch_count
                   ~previous_alloc:allocated ~label_name:label ~map ~count_var
                   ~if_count rkbody
               in
               {
                 condition_label = self_condition_label;
                 statement_for_condition = stmt @ statement_for_condition;
                 condition = tac_condition;
                 end_label;
                 goto = label;
                 jmp_false = jmp_next_condition;
                 tac_body;
               })
      in
      let else_tac_body =
        convert_from_rkbody ~switch_count ~cases_count ~previous_alloc:allocated
          ~label_name:else_label ~map ~count_var ~if_count else_case
      in
      ( 
        SCases { cases; exit_label = end_label; else_tac_body } :: [],
      make_typed_tac_expression id_rktype (TEIdentifier identifier) 
      )
  | RESwitch { rexpression; cases; wildcard_case }, Some (identifier, id_rktype) ->
      let incremented = post_inc switch_count in

      let fn_local_switch_label =
        make_switch_goto_label ~switch_count:incremented
      in
      let sw_exit_label = make_switch_end_label ~switch_count:incremented in
      let next_allocated, forward_push = create_forward_init ~count_var rexpression in
      let statemenets_for_case, condition_switch =
        convert_from_typed_expression ~allocated:next_allocated ~switch_count
          ~cases_count ~if_count ~count_var ~map rexpression
      in
      let sw_cases =
        cases
        |> List.mapi (fun i (variants, bounds, kbody) ->
               let sw_goto = fn_local_switch_label i in
               let variants_to_match =
                 variants |> List.map RSwitch_Case.variant
               in
               let () =
               bounds
                 |> List.iteri (fun _ (index, name, rktype) ->
                        Hashtbl.add map name ( 
                          typed_locale_assoc ~name ~from:condition_switch
                          ~assoc_index_bound:index ~rktype
                        ))
               in
               let assoc_bound =
                 bounds
                 |> List.map (fun (index, id, rtype) -> (id, (index, rtype)))
                 |> List.split |> snd
               in
               let switch_tac_body =
                 convert_from_rkbody ~previous_alloc:allocated
                   ~label_name:sw_goto ~map ~count_var ~if_count ~cases_count
                   ~switch_count kbody
               in
               {
                 variants_to_match;
                 assoc_bound;
                 sw_goto;
                 sw_exit_label;
                 switch_tac_body;
               })
      in

      let wildcard_label =
        wildcard_case
        |> Option.map (fun _ ->
               make_switch_wild_label ~switch_count:incremented)
      in
      let wildcard_body =
        wildcard_case
        |> Option.map (fun wild_body ->
               let label_name =
                 make_switch_wild_label ~switch_count:incremented
               in
               convert_from_rkbody ~previous_alloc:allocated ~cases_count
                 ~switch_count ~label_name ~map ~count_var ~if_count wild_body)
      in
      ( STSwitch
          {
            statemenets_for_case = forward_push @ statemenets_for_case;
            condition_switch;
            sw_cases;
            wildcard_label;
            wildcard_body;
            sw_exit_label;
          }
        :: [],
        make_typed_tac_expression id_rktype
        (TEIdentifier identifier) )
  | REmpty, _ -> convert_if_allocated ~expr_rktype:(trktype) ~allocated TEmpty
  | RFalse, _ -> convert_if_allocated ~expr_rktype:(trktype) ~allocated TEFalse
  | RTrue, _ -> convert_if_allocated ~expr_rktype:(trktype) ~allocated TETrue
  | RENullptr, _ -> convert_if_allocated ~expr_rktype:(trktype) ~allocated TENullptr
  | REInteger (sign, size, int), _ ->
      convert_if_allocated  ~expr_rktype:(trktype) ~allocated (TEInt (sign, size, int))
  | REFloat float, _ -> convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEFloat float)
  | RESizeof rktype, _ -> convert_if_allocated ~expr_rktype:(trktype) ~allocated (TESizeof rktype)
  | REstring s, _ -> convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEString s)
  | REIdentifier { identifier; _ }, _ ->
      convert_if_allocated ~expr_rktype:(trktype) ~allocated
        (TEIdentifier identifier)
  | REConst_Identifier { modules_path; identifier }, _ ->
      convert_if_allocated ~expr_rktype:(trktype) ~allocated
        (TEConst { module_path = modules_path; name = identifier })
  | RETuple typed_expressions, _ ->
      let stmts_needed, tac_expression =
        typed_expressions
        |> List.map (fun ty_ex ->
               let next_allocated, stmt = create_forward_init ~count_var ty_ex in
               let stmt_needed, tac_expression =
                 convert_from_typed_expression ~allocated:next_allocated
                   ~switch_count ~cases_count ~map ~count_var ~if_count ty_ex
               in
               (stmt @ stmt_needed, tac_expression))
        |> List.fold_left_map
             (fun acc (stmts, value) -> (acc @ stmts, value))
             []
      in
      let new_tmp = make_inc_tmp count_var in
      let tuple = RVTuple tac_expression in
      let stt = STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype tuple } in
      let last_stmt, return =
        convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
      in
      (stmts_needed @ (last_stmt |> List.cons stt), return)
  | REFunction_call { modules_path; generics_resolver; fn_name; parameters }, _
    ->
      let stmts_needed, tac_parameters =
        parameters
        |> List.map (fun ty_ex ->
               let next_allocated, stmt = create_forward_init ~count_var ty_ex in
               let stmt_needed, tac_expression =
                 convert_from_typed_expression ~allocated:next_allocated
                   ~switch_count ~cases_count ~map ~count_var ~if_count ty_ex
               in
               (stmt @ stmt_needed, tac_expression))
        |> List.fold_left_map
             (fun acc (stmts, value) -> (acc @ stmts, value))
             []
      in
      let new_tmp = make_inc_tmp count_var in
      let call_rvalue =
        RVFunction
          {
            module_path = modules_path;
            fn_name;
            generics_resolver;
            tac_parameters;
          }
      in
      let stt =
        STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype call_rvalue }
      in
      let last_stmt, return =
        convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
      in
      (stmts_needed @ (last_stmt |> List.cons stt), return)
  | REStruct { modules_path; struct_name; fields }, _ ->
      let stmts_needed, tac_fields =
        fields
        |> List.map (fun (field, ty_ex) ->
               let next_allocated, stmt = create_forward_init ~count_var ty_ex in
               let stmt_needed, tac_expression =
                 convert_from_typed_expression ~allocated:next_allocated
                   ~switch_count ~cases_count ~map ~count_var ~if_count ty_ex
               in

               (field, (stmt @ stmt_needed, tac_expression)))
        |> List.fold_left_map
             (fun acc (field, (stmts, tac_expr)) ->
               (acc @ stmts, (field, tac_expr)))
             []
      in
      let new_tmp = make_inc_tmp count_var in
      let struct_rvalue =
        RVStruct
          { module_path = modules_path; struct_name; fields = tac_fields }
      in
      let statament =
        STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype struct_rvalue }
      in
      let last_stmt, return =
        convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
      in
      (stmts_needed @ (statament :: last_stmt), return)
  | REEnum { modules_path; enum_name; variant; assoc_exprs }, _ ->
      let stmts_needed, assoc_tac_exprs =
        assoc_exprs
        |> List.map (fun ty_ex ->
               let next_allocated, stmt = create_forward_init ~count_var ty_ex in
               let stmt_needed, tac_expression =
                 convert_from_typed_expression ~allocated:next_allocated
                   ~switch_count ~cases_count ~map ~count_var ~if_count ty_ex
               in
               (stmt @ stmt_needed, tac_expression))
        |> List.fold_left_map (fun acc (smts, value) -> (acc @ smts, value)) []
      in
      let new_tmp = make_inc_tmp count_var in
      let enum_rvalue =
        RVEnum
          { module_path = modules_path; enum_name; variant; assoc_tac_exprs }
      in
      let statement =
        STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype enum_rvalue }
      in
      let last_stmt, return =
        convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
      in
      (stmts_needed @ (statement :: last_stmt), return)
  | REFieldAcces { first_expr; field }, _ ->
      let next_allocated, stmt = create_forward_init ~count_var first_expr in
      let needed_statement, tac_expr =
        convert_from_typed_expression ~allocated:next_allocated ~switch_count
          ~cases_count ~map ~if_count ~count_var first_expr
      in
      let new_tmp = make_inc_tmp count_var in
      let field_acces = RVFieldAcess { first_expr = tac_expr; field } in
      let statement =
        STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype field_acces }
      in
      let last_stmt, return =
        convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
      in
      (needed_statement @ stmt @ (statement :: last_stmt), return)
  | REAdress identifier, _ ->
      let new_tmp = make_inc_tmp count_var in
      let adress = RVAdress identifier in
      let statement =
        STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype adress }
      in
      let last_stmt, return =
        convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
      in
      (statement :: last_stmt, return)
  | REBin_op bin, _ ->
      let operator = Operator.bin_operantor bin in
      let ltyped, rtyped = Operator.typed_operandes bin in
      let lnext_allocated, lstmt = create_forward_init ~count_var ltyped in

      let rnext_allocated, rstmt = create_forward_init ~count_var rtyped in
      let lstamements_needed, lhs_value =
        convert_from_typed_expression ~allocated:lnext_allocated ~switch_count
          ~cases_count ~map ~if_count ~count_var ltyped
      in
      let rstamements_needed, rhs_value =
        convert_from_typed_expression ~allocated:rnext_allocated ~switch_count
          ~cases_count ~map ~if_count ~count_var rtyped
      in
      let new_tmp = make_inc_tmp count_var in
      let binary_op =
        RVBuiltinBinop { binop = operator; blhs = lhs_value; brhs = rhs_value }
      in
      let stamement =
        STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype binary_op }
      in
      let last_stmt, return =
        convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
      in
      ( lstamements_needed @ rstamements_needed @ lstmt @ rstmt
        @ (stamement :: last_stmt),
        return )
  | REUn_op unary, _ ->
      let operator = Operator.unary_operator unary in
      let operand = Operator.typed_operand unary in
      let next_allocated, stmt = create_forward_init ~count_var operand in
      let need_stmts, lvalue =
        convert_from_typed_expression ~allocated:next_allocated ~switch_count
          ~cases_count ~map ~if_count ~count_var operand
      in
      let new_tmp = make_inc_tmp count_var in
      let unary_op = RVBuiltinUnop { unop = operator; expr = lvalue } in
      let statement =
        STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype unary_op }
      in
      let last_stmt, return =
        convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
      in
      (stmt @ need_stmts @ (statement :: last_stmt), return)
  | REDeference (n, id), _ ->
    
      let rec loop ~origin_type ~from i =
        match i with
        | 0 -> failwith "Never I hope: deferencement without start ??"
        | 1 ->
            let new_tmp = make_inc_tmp count_var in
            let rtpointee = KosuIrTyped.Asttyped.RType.rtpointee origin_type in  
            ( new_tmp,
              STacDeclaration
                { identifier = new_tmp; trvalue = make_typed_tac_rvalue rtpointee (RVDefer from) }
              :: [] )
        | _ ->
            let new_tmp = make_inc_tmp count_var in
            let rtpointee = KosuIrTyped.Asttyped.RType.rtpointee origin_type in 
            let result, future_stmt = loop ~origin_type:rtpointee ~from:new_tmp (i - 1) in
            ( result,
              STacDeclaration
                { identifier = new_tmp; trvalue = make_typed_tac_rvalue (rtpointee) (RVDefer from) }
              :: future_stmt )
      in
      let origin_type = KosuIrTyped.Asttyped.RType.npointer n trktype in
      let restult, stmt = loop ~origin_type ~from:id n in
      (stmt, 
      make_typed_tac_expression trktype
      (TEIdentifier restult))
  | REBinOperator_Function_call rkbin_op, _ -> 
    let operator = Operator.bin_operantor rkbin_op in
    let ltyped, rtyped = Operator.typed_operandes rkbin_op in
    let lnext_allocated, lstmt = create_forward_init ~count_var ltyped in

    let rnext_allocated, rstmt = create_forward_init ~count_var rtyped in
    let lstamements_needed, lhs_value =
      convert_from_typed_expression ~allocated:lnext_allocated ~switch_count
        ~cases_count ~map ~if_count ~count_var ltyped
    in
    let rstamements_needed, rhs_value =
      convert_from_typed_expression ~allocated:rnext_allocated ~switch_count
        ~cases_count ~map ~if_count ~count_var rtyped
    in
    let new_tmp = make_inc_tmp count_var in
    let binary_op =
      RVCustomBinop { binop = operator; blhs = lhs_value; brhs = rhs_value }
    in
    let stamement =
      STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype binary_op }
    in
    let last_stmt, return =
      convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
    in
    ( lstamements_needed @ rstamements_needed @ lstmt @ rstmt
      @ (stamement :: last_stmt),
      return )

  | REUnOperator_Function_call rkunary_op, _ -> 
    let operator = Operator.unary_operator rkunary_op in
    let operand = Operator.typed_operand rkunary_op in
    let next_allocated, stmt = create_forward_init ~count_var operand in
    let need_stmts, lvalue =
      convert_from_typed_expression ~allocated:next_allocated ~switch_count
        ~cases_count ~map ~if_count ~count_var operand
    in
    let new_tmp = make_inc_tmp count_var in
    let unary_op = RVCustomUnop { unop = operator; expr = lvalue } in
    let statement =
      STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype unary_op }
    in
    let last_stmt, return =
      convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
    in
    (stmt @ need_stmts @ (statement :: last_stmt), return)
  | REBuiltin_Function_call {fn_name; parameters}, _ ->
    let stmts_needed, tac_parameters =
    parameters
    |> List.map (fun ty_ex ->
           let next_allocated, stmt = create_forward_init ~count_var ty_ex in
           let stmt_needed, tac_expression =
             convert_from_typed_expression ~allocated:next_allocated
               ~switch_count ~cases_count ~map ~count_var ~if_count ty_ex
           in
           (stmt @ stmt_needed, tac_expression))
    |> List.fold_left_map
         (fun acc (stmts, value) -> (acc @ stmts, value))
         []
  in
  let new_tmp = make_inc_tmp count_var in
  let call_rvalue =
    RVBuiltinCall
      {
        fn_name;
        parameters = tac_parameters;
      }
  in
  let stt =
    STacDeclaration { identifier = new_tmp; trvalue = make_typed_tac_rvalue trktype call_rvalue }
  in
  let last_stmt, return =
    convert_if_allocated ~expr_rktype:(trktype) ~allocated (TEIdentifier new_tmp)
  in
  (stmts_needed @ (last_stmt |> List.cons stt), return)
  | (RESwitch _ | RECases _ | REIf _), _ ->
      failwith
        "Compiler code Error: Cannot create branch without previous allocation"

and convert_from_rkbody ?(previous_alloc = None) ~label_name ~map ~count_var
    ~if_count ~cases_count ~switch_count ?(function_return = false) (rkbody : rkbody) =
  let stmts, types_return = rkbody in
  match stmts with
  | stmt :: q -> (
      match stmt with
      | RSDeclaration { is_const = _; variable_name; typed_expression } ->
          let () = Hashtbl.add map variable_name (typed_locale_locale variable_name ~rktype: typed_expression.rktype) in
          let allocated, stmt_opt = create_forward_init ~count_var typed_expression in
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~cases_count ~allocated ~map
              ~count_var ~if_count ~switch_count typed_expression
          in

          let body =
            convert_from_rkbody ~switch_count ~cases_count ~previous_alloc
              ~label_name ~map ~count_var ~if_count ~function_return (q, types_return)
          in
          add_statements_to_tac_body
            (stmt_opt
            @ tac_stmts
            @ STacDeclaration
                {
                  identifier = variable_name;
                  trvalue = make_typed_tac_rvalue typed_expression.rktype (RVExpression tac_expression);
                }
              :: [])
            body
      | RSAffection (identifier, aff_typed_expr) ->
          (* let find_tmp = Hashtbl.find map identifier in *)
          let allocated, forward_push = create_forward_init ~count_var aff_typed_expr in
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~cases_count ~allocated ~map
              ~count_var ~if_count ~switch_count aff_typed_expr
          in
          let body =
            convert_from_rkbody ~switch_count ~cases_count ~previous_alloc
              ~label_name ~map ~count_var ~if_count ~function_return (q, types_return)
          in
          body
          |> add_statements_to_tac_body
               (
                forward_push
               @ tac_stmts
               @ STacModification
                   {
                     identifier = identifier;
                     trvalue = make_typed_tac_rvalue aff_typed_expr.rktype (RVExpression tac_expression);
                   }
                 :: [])
      | RSDiscard discard_typed_expression ->
          let allocated, push_forward = create_forward_init ~count_var ~rvalue:(RVDiscard) discard_typed_expression in
          let tac_stmts, _tac_rvalue =
            convert_from_typed_expression ~cases_count ~allocated ~map
              ~count_var ~if_count ~switch_count discard_typed_expression
          in
          add_statements_to_tac_body (push_forward @ tac_stmts)
            (convert_from_rkbody ~cases_count ~previous_alloc ~label_name ~map
               ~count_var ~if_count ~switch_count ~function_return (q, types_return))
      | RSDerefAffectation (identifier, deref_typed_expr) ->
          let allocated, forward_declaration = create_forward_init ~count_var deref_typed_expr in
          (* let find_tmp = Hashtbl.find map identifier in *)
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~cases_count ~allocated ~map
              ~count_var ~if_count ~switch_count deref_typed_expr
          in
          let body =
            convert_from_rkbody ~switch_count ~previous_alloc ~label_name ~map
              ~count_var ~if_count ~cases_count ~function_return (q, types_return)
          in
          add_statements_to_tac_body
            (
            forward_declaration
            @ tac_stmts
            @ STDerefAffectation
                {
                  identifier = identifier;
                  trvalue = make_typed_tac_rvalue deref_typed_expr.rktype (RVExpression tac_expression);
                }
              :: [])
            body)
  | [] ->
      let allocated, forward_push = create_forward_init ~count_var types_return in
      let stmts, expr =
        convert_from_typed_expression ~cases_count ~allocated ~map ~count_var
          ~if_count ~switch_count types_return
      in
      let penultimate_stmt =
        match previous_alloc with
        | None -> []
        | Some identifier ->
            STacModification { identifier = identifier |> fst; trvalue = make_typed_tac_rvalue types_return.rktype (RVExpression expr) }
            :: []
      in

      {
        label = label_name;
        body =
          (forward_push @ stmts @ penultimate_stmt, if function_return then Some expr else None);
      }

let tac_function_decl_of_rfunction (rfunction_decl : rfunction_decl) =

  let map = Hashtbl.create (rfunction_decl.rbody |> fst |> List.length) in

  let tac_body = convert_from_rkbody ~switch_count ~cases_count
  ~label_name:rfunction_decl.rfn_name ~map ~count_var:(ref 0)
  ~function_return:true
  ~if_count rfunction_decl.rbody in
  {
    rfn_name = rfunction_decl.rfn_name;
    generics = rfunction_decl.generics;
    rparameters = rfunction_decl.rparameters;
    return_type = rfunction_decl.return_type;
    tac_body;
    locale_var = map |> Hashtbl.to_seq_values |> List.of_seq
  }

let tac_operator_decl_of_roperator_decl = function
  | (RUnary { op; rfield; return_type; kbody } as self) ->
    let label_name = 
    (Printf.sprintf "%s_%s" (KosuIrTyped.Asttpprint.name_of_roperator self ) (KosuIrTyped.Asttpprint.string_of_rktype return_type))
    in
      let map = Hashtbl.create (kbody |> fst |> List.length) in
      let tac_body = 
        convert_from_rkbody ~switch_count ~cases_count
          ~label_name ~map ~count_var:(ref 0) ~if_count
          ~function_return:true kbody in
      TacUnary
        {
          op;
          rfield;
          return_type;
          tac_body;
          locale_var = map |> Hashtbl.to_seq_values |> List.of_seq
        }
  | (RBinary { op; rfields = ((_f1, _), (_f2, _)) as rfields; return_type; kbody } as self) 
    ->
      let label_name = 
        (Printf.sprintf "%s_%s" (KosuIrTyped.Asttpprint.name_of_roperator self ) (KosuIrTyped.Asttpprint.string_of_rktype return_type))
        in
        let map = Hashtbl.create (kbody |> fst |> List.length) in
        let tac_body = 
          convert_from_rkbody ~switch_count ~cases_count
            ~label_name ~map ~count_var:(ref 0) ~if_count
            ~function_return:true kbody in
      TacBinary
        {
          op;
          rfields;
          return_type;
          tac_body;
          locale_var = map |> Hashtbl.to_seq_values |> List.of_seq
        }

let rec tac_module_node_from_rmodule_node = function
  | RNExternFunc f -> TNExternFunc f
  | RNSyscall f -> TNSyscall f
  | RNStruct s -> TNStruct s
  | RNEnum s -> TNEnum s
  | RNConst s -> TNConst s
  | RNFunction f ->
      let tmp = tac_function_decl_of_rfunction f in
      let () =
        Printf.printf "Locales = %s\nBody:\n%s\n"
          (tmp.locale_var |> List.map Asttacpprint.string_of_typed_locale |> String.concat ", ") 
          (Asttacpprint.string_of_label_tac_body tmp.tac_body)
      in
      TNFunction tmp
  | RNOperator s -> TNOperator (tac_operator_decl_of_roperator_decl s)

and tac_module_path_of_rmodule_path { path; rmodule = RModule module_nodes } =
  {
    path;
    tac_module =
      TacModule
        (module_nodes
        |> List.map (fun node -> tac_module_node_from_rmodule_node node));
  }

and tac_program_of_rprogram (rprogram : rprogram) : tac_program =
  rprogram
  |> List.map (fun { filename; rmodule_path } ->
         {
           filename;
           tac_module_path = tac_module_path_of_rmodule_path rmodule_path;
         })

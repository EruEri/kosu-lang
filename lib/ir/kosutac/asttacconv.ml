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
  
  let unary_operator = function
  | RUMinus _ -> TacUminus
  | RUNot _ -> TacNot

  let typed_operand = function
  | RUMinus e | RUNot e -> e


  let typed_operandes = KosuIrTyped.Asttyped.Binop.operands
end
let make_tmp = Printf.sprintf "r%u"
let make_goto_label ~count_if = Printf.sprintf "if.%u.%u" count_if
let make_end_label ~count_if = Printf.sprintf "if.%u.end" count_if
let make_case_goto_label ~cases_count = Printf.sprintf "case.%u.%u" cases_count
let make_case_goto_cond_label ~cases_count = Printf.sprintf "case.%u.%u.cond" cases_count

(**
  [case.%u.end]    
*)
let make_case_end_label ~cases_count = Printf.sprintf "case.%u.end" cases_count

(**
    [case.%u.else]
*)
let make_case_else ~cases_count = Printf.sprintf "case.%u.else" cases_count

let make_switch_goto_label ~switch_count = Printf.sprintf "switch.%u.%u" switch_count

let make_switch_wild_label ~switch_count = Printf.sprintf "switch.%u.wildcard" switch_count

let make_switch_end_label ~switch_count = Printf.sprintf "switch.%u.end" switch_count

(**
@returns: the value of [n] before the incrementation    
*)
let post_inc n =
  let x = !n in
  let () = incr n in
  x

let make_inc_tmp n = make_tmp (post_inc n)

let add_statements_to_tac_body stmts tac_body =
  let { label; body = future_stmts, future_result } = tac_body in
  { label; body = (stmts @ future_stmts, future_result) }


let convert_if_allocated ~allocated tac_expression = 
  match allocated with
  | None -> [], tac_expression
  | Some identifier -> STacModification {identifier; expression = RVExpression tac_expression}::[], TEIdentifier identifier
let rec convert_from_typed_expression ?(allocated = None) ~map ~count_var
    ~if_count ~cases_count ~switch_count typed_expression =
  let _rktype = typed_expression.rktype in
  let expr = typed_expression.rexpression in
  match (allocated, expr) with
  | Some identifier, REIf (typed_expression, if_body, else_body) ->
    let incremented = (post_inc if_count) in
      let next_allocated, stmt = if typed_expression |> Expression.is_typed_expresion_branch then 
        let new_tmp = (make_inc_tmp count_var) in Some new_tmp, STacDeclaration {identifier = new_tmp; expression = RVLater}::[]
    else None, [] in 
 
    let statement_for_bool, condition_rvalue =
        convert_from_typed_expression ~allocated:(next_allocated) ~switch_count ~map ~count_var ~if_count ~cases_count typed_expression
      in
      
      let goto_label1 = make_goto_label ~count_if:incremented 0 in
      let goto_label2 = (make_goto_label ~count_if:incremented 1) in
      let exit_label = make_end_label ~count_if:incremented in
      let if_tac_body =
        convert_from_rkbody ~switch_count ~cases_count ~previous_alloc:(allocated) ~label_name:goto_label1 ~map ~count_var ~if_count
          if_body
      in
      let else_tac_body =
        convert_from_rkbody
          ~cases_count
          ~switch_count
          ~previous_alloc:(allocated)
          ~label_name:goto_label2
          ~map ~count_var ~if_count else_body
      in
      STIf {
        statement_for_bool = statement_for_bool @ stmt;
        condition_rvalue;
        goto1 = goto_label1;
        goto2 = goto_label2;
        exit_label;
        if_tac_body;
        else_tac_body
      } :: [], (TEIdentifier identifier)

  | Some identifier, RECases {cases; else_case} -> 
    let incremented = post_inc cases_count in

    let make_locale_label = make_case_goto_label ~cases_count:(incremented) in
    let lambda_make_locale_condition_label = make_case_goto_cond_label ~cases_count:(incremented) in
    let else_label = make_case_else ~cases_count:(incremented) in
    let end_label = make_case_end_label ~cases_count:(incremented) in
    let cases_len = cases |> List.length in
    let cases = cases |> List.mapi (fun i (case_condition, rkbody) ->
      let label = make_locale_label (i) in
      let self_condition_label =  if i = 0 then None else Some (lambda_make_locale_condition_label i) in
      let jmp_next_condition = if i < cases_len - 1 then  
        lambda_make_locale_condition_label (i + 1)
      else  else_label in
      
      let (statement_for_condition, tac_condition) = convert_from_typed_expression ~switch_count ~map ~count_var ~if_count ~cases_count case_condition in
      let tac_body = convert_from_rkbody ~cases_count ~switch_count ~previous_alloc:(allocated) ~label_name:label ~map ~count_var ~if_count rkbody in
         {
          condition_label = self_condition_label;
          statement_for_condition;
          condition = tac_condition;
          end_label;
          goto = label;
          jmp_false = jmp_next_condition;
          tac_body
         }
  )
    in 
    let else_tac_body = convert_from_rkbody  ~switch_count ~cases_count ~previous_alloc:(allocated) ~label_name:else_label ~map ~count_var ~if_count else_case in
    SCases {
      cases;
      exit_label = end_label;
      else_tac_body
    }::[], TEIdentifier identifier
  | Some identifier, RESwitch {rexpression; cases; wildcard_case} -> 
    let incremented = post_inc switch_count in

    let fn_local_switch_label = make_switch_goto_label ~switch_count:incremented in
    let sw_exit_label = make_switch_end_label ~switch_count:(incremented) in
      let next_allocated, forward_push = 
    if typed_expression |> Expression.is_typed_expresion_branch then 
      let new_tmp = (make_inc_tmp count_var) in Some new_tmp,  STacDeclaration {identifier = new_tmp; expression = RVLater}::[]
  else None, [] in 
    let statemenets_for_case, condition_switch = convert_from_typed_expression ~allocated:next_allocated  ~switch_count ~cases_count ~if_count ~count_var ~map rexpression in
    let sw_cases = cases |> List.mapi (fun i (variants, bounds, kbody) ->
      let sw_goto = fn_local_switch_label i in 
        let variants_to_match = variants |> List.map RSwitch_Case.variant in
        let bounds_id, assoc_bound = bounds |> List.map (fun (index, id, rtype) -> id, (index, rtype)) |> List.split in
        let () = bounds_id |> List.iteri (fun i s -> 
          Hashtbl.add map s (Printf.sprintf "tmp.%u" i)
          
          ) in
        let switch_tac_body = convert_from_rkbody ~previous_alloc:(allocated) ~label_name:sw_goto ~map ~count_var ~if_count ~cases_count ~switch_count kbody in
        {
          variants_to_match;
          assoc_bound;
          sw_goto;
          sw_exit_label;
          switch_tac_body
        }
      )
  in

  let wildcard_label = wildcard_case |> Option.map (fun _ -> make_switch_wild_label ~switch_count:incremented) in
  let wildcard_body = wildcard_case |> Option.map (fun wild_body ->
    let label_name = make_switch_wild_label ~switch_count:incremented in
      convert_from_rkbody ~previous_alloc:(allocated) ~cases_count ~switch_count ~label_name ~map ~count_var ~if_count wild_body) 
    in
    STSwitch {
    statemenets_for_case = forward_push @ statemenets_for_case;
    condition_switch;
    sw_cases;
    wildcard_label;
    wildcard_body;
    sw_exit_label;
  }:: [], TEIdentifier identifier
  | _, REmpty -> convert_if_allocated ~allocated TEmpty
  | _, RFalse -> convert_if_allocated ~allocated TEFalse
  | _, RTrue -> convert_if_allocated ~allocated TETrue
  | _, RENullptr -> convert_if_allocated ~allocated TENullptr
  | _, REInteger (sign, size, int) -> convert_if_allocated ~allocated (TEInt (sign, size, int))
  | _, REFloat float -> convert_if_allocated ~allocated (TEFloat float)
  | _, RESizeof rktype -> convert_if_allocated ~allocated (TESizeof rktype)
  | _, REstring s ->  convert_if_allocated ~allocated (TEString s)
  | _, REIdentifier { identifier; _ } -> convert_if_allocated ~allocated (TEIdentifier (Hashtbl.find map identifier))
  | _, REConst_Identifier {modules_path; identifier} -> convert_if_allocated ~allocated (TEConst {module_path = modules_path; name = identifier})
  | _, RETuple (typed_expressions) -> 
    let stmts_needed, tac_expression =
    typed_expressions
    |> List.map (convert_from_typed_expression ~switch_count ~cases_count ~map ~count_var ~if_count)
    |> List.fold_left_map
         (fun acc (stmts, value) -> (acc @ stmts, value))
         []
  in
  let new_tmp = make_inc_tmp count_var in
  let tuple =
    RVTuple tac_expression
  in
  let stt =
    STacDeclaration { identifier = new_tmp; expression = tuple }
  in
  let (last_stmt, return) = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
  (stmts_needed @ (last_stmt |> List.cons stt ), return )
  | _, REFunction_call { modules_path; generics_resolver; fn_name; parameters }
    ->
      let stmts_needed, tac_parameters =
        parameters
        |> List.map (convert_from_typed_expression ~switch_count ~cases_count ~map ~count_var ~if_count)
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
        STacDeclaration { identifier = new_tmp; expression = call_rvalue }
      in
      let (last_stmt, return) = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
      (stmts_needed @ (last_stmt |> List.cons stt ), return )
  | _, REStruct {modules_path; struct_name; fields } -> 
    let stmts_needed, tac_fields = 
    fields
    |> List.map (fun (field, typed_expression) ->
      field, convert_from_typed_expression ~switch_count ~cases_count ~map ~count_var ~if_count typed_expression
    )
    |> List.fold_left_map (fun acc (field, (stmts, tac_expr)) -> 
      acc @ stmts, (field, tac_expr)
      ) []
    in
    let new_tmp = make_inc_tmp count_var in
    let struct_rvalue = RVStruct {
      module_path = modules_path;
      struct_name;
      fields = tac_fields
    } in
    let statament = STacDeclaration { identifier = new_tmp; expression = struct_rvalue} in
    let (last_stmt, return) = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
    stmts_needed @ (statament::last_stmt), return
  | _ , REEnum {modules_path; enum_name; variant; assoc_exprs} -> 
    let stmts_needed, assoc_tac_exprs = 
    assoc_exprs
    |> List.map (convert_from_typed_expression ~switch_count ~cases_count ~map ~count_var ~if_count)
    |> List.fold_left_map (fun acc (smts, value) -> acc @ smts, value) []
  in
  let new_tmp = make_inc_tmp count_var in
  let enum_rvalue = RVEnum {
    module_path = modules_path;
    enum_name;
    variant;
    assoc_tac_exprs
  } in
  let statement = STacDeclaration {identifier = new_tmp; expression = enum_rvalue} in
  let (last_stmt, return) = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
  stmts_needed @ (statement::last_stmt), return
  | _, REFieldAcces {first_expr; field} -> 
    let needed_statement, tac_expr = convert_from_typed_expression ~switch_count ~cases_count ~map ~if_count ~count_var first_expr in
    let new_tmp = make_inc_tmp count_var in
    let field_acces = RVFieldAcess {
      first_expr = tac_expr;
      field
    } in
    let statement = STacDeclaration { identifier = new_tmp; expression = field_acces} in
    let last_stmt, return = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
    needed_statement @ (statement::last_stmt), return
  | _, REAdress identifier ->
    let new_tmp = make_inc_tmp count_var in
    let adress = RVAdress (Hashtbl.find map identifier) in
    let statement = STacDeclaration { identifier = new_tmp; expression = adress} in
    let last_stmt, return = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
    (statement::last_stmt), return 
  | _, REBin_op bin -> 
    let operator = Operator.bin_operantor bin in
    let (ltyped, rtyped) = Operator.typed_operandes bin in
    let (lstamements_needed, lhs_value) = convert_from_typed_expression ~switch_count ~cases_count ~map ~if_count ~count_var ltyped in
    let (rstamements_needed, rhs_value) = convert_from_typed_expression ~switch_count ~cases_count ~map ~if_count ~count_var rtyped in
    let new_tmp = make_inc_tmp count_var in
    let binary_op = RVBinop {
      binop = operator;
      blhs = lhs_value;
      brhs = rhs_value
    } in
    let stamement = STacDeclaration { identifier = new_tmp; expression = binary_op} in
    let last_stmt, return = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
    lstamements_needed @ rstamements_needed @ (stamement::last_stmt), return
  | _, REUn_op unary -> 
    let operator = Operator.unary_operator unary in
    let operand = Operator.typed_operand unary in
    let (need_stmts, lvalue) = convert_from_typed_expression ~switch_count ~cases_count ~map ~if_count ~count_var operand in
    let new_tmp = make_inc_tmp count_var in
    let unary_op = RVUnop {
      unop = operator;
      expr = lvalue;
    } in
    let statement = STacDeclaration {identifier = new_tmp; expression = unary_op } in
    let last_stmt, return = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
    (need_stmts @ statement::last_stmt ), return
  | _, REDeference (n, id) -> 
    let rec loop i = 
      match i with
      | 0 -> failwith ""
      | 1 -> 
        let defer = if 1 = n then Hashtbl.find map id else make_inc_tmp count_var in
        let new_tmp = make_inc_tmp count_var in
        new_tmp, STacDeclaration {identifier = new_tmp; expression = RVDefer defer}::[]
      | count -> 
        let defer = if count = n then Hashtbl.find map id else make_inc_tmp count_var in
        let new_tmp = make_inc_tmp count_var in
        let result, future_stmt = (loop (i - 1)) in
        result, STacDeclaration {identifier = new_tmp; expression = RVDefer defer}::future_stmt
    in
  let restult, stmt = loop n in
  stmt, TEIdentifier restult
  | _, REBinOperator_Function_call _ -> failwith "TODO: Custom binary operator"
  | _, REUnOperator_Function_call _ -> failwith "TODO: Custom unary operator"
  | _, REBuiltin_Function_call _ -> failwith "TODO: builtitn function"
  | _, (RESwitch _ | RECases _ | REIf _) -> failwith "Compiler code Error: Cannot create branch without previous allocation" 

and convert_from_rkbody ?(previous_alloc = None) ~label_name ~map ~count_var ~if_count ~cases_count ~switch_count (rkbody : rkbody)
    =
  let stmts, types_return = rkbody in
  match stmts with
  | stmt :: q -> (
      match stmt with
      | RSDeclaration { is_const = _; variable_name; typed_expression } ->
          let new_tmp = make_inc_tmp count_var in
          let () = Hashtbl.add map variable_name new_tmp in
          let allocated, stmt_opt =
            if
              KosuIrTyped.Asttyped.Expression.is_typed_expresion_branch
                typed_expression
            then Some new_tmp, Some (STacDeclaration { identifier = new_tmp; expression = RVLater})
            else None, None
          in
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~cases_count ~allocated ~map ~count_var ~if_count ~switch_count
              typed_expression
          in

          let body =
            convert_from_rkbody ~switch_count ~cases_count ~previous_alloc ~label_name ~map ~count_var ~if_count
              (q, types_return)
          in
          add_statements_to_tac_body
            (
            (stmt_opt |> Option.to_list)
            @ tac_stmts
            @ STacDeclaration { identifier = new_tmp; expression = RVExpression tac_expression }
              :: [])
            body
      | RSAffection (identifier, typed_expression) ->
          let find_tmp = Hashtbl.find map identifier in
          let allocated, forward_push = 
            if typed_expression |> Expression.is_typed_expresion_branch then 
              let new_tmp = (make_inc_tmp count_var) in Some new_tmp, Some (STacDeclaration {identifier = new_tmp; expression = RVLater})
          else None, None in 
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~cases_count ~allocated ~map ~count_var ~if_count ~switch_count
              typed_expression
          in
          let body =
            convert_from_rkbody ~switch_count ~cases_count ~previous_alloc ~label_name ~map ~count_var ~if_count
              (q, types_return)
          in
          body
          |> add_statements_to_tac_body
               (
                (
                  forward_push |> Option.to_list)
                @ tac_stmts
                @ STacModification { identifier = find_tmp; expression = RVExpression tac_expression }
                 :: [])
      | RSDiscard typed_expression ->
        let allocated, push_forward = 
          if typed_expression |> Expression.is_typed_expresion_branch then 
            let new_tmp = (make_inc_tmp count_var) in
            Some new_tmp, STacDeclaration {identifier = new_tmp ; expression = RVDiscard}::[]  else None, [] in 
          let tac_stmts, _tac_rvalue =
            convert_from_typed_expression ~cases_count ~allocated ~map ~count_var ~if_count ~switch_count
              typed_expression
          in
          add_statements_to_tac_body (push_forward @ tac_stmts)
            (convert_from_rkbody ~cases_count ~previous_alloc ~label_name ~map ~count_var ~if_count ~switch_count
               (q, types_return))
      | RSDerefAffectation (identifier, typed_expression) ->
        let allocated = if typed_expression |> Expression.is_typed_expresion_branch then Some (make_inc_tmp count_var) else None in 
          let find_tmp = Hashtbl.find map identifier in
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~cases_count ~allocated ~map ~count_var ~if_count ~switch_count
              typed_expression
          in
          let body =
            convert_from_rkbody ~switch_count ~previous_alloc ~label_name ~map ~count_var ~if_count ~cases_count
              (q, types_return)
          in
          add_statements_to_tac_body
            (tac_stmts
            @ STDerefAffectation { identifier = find_tmp; expression = RVExpression tac_expression }
              :: [])
            body)
  | [] ->
    let allocated, forward_push = 
      if types_return |> Expression.is_typed_expresion_branch 
        then let new_tmp = (make_inc_tmp count_var) in  Some new_tmp, Some (STacDeclaration {identifier = new_tmp; expression = RVLater})
    else None, None in 
    let stmts, expr = convert_from_typed_expression ~cases_count ~allocated ~map ~count_var ~if_count ~switch_count types_return in
    let penultimate_stmt = match previous_alloc with
    | None -> []
    | Some identifier -> STacModification { identifier; expression = RVExpression expr}::[] in 

      {
        label = label_name;
        body = ((forward_push |> Option.to_list) @ stmts @ penultimate_stmt), expr
          
      }



let tac_function_decl_of_rfunction (rfunction_decl: rfunction_decl) = 
  let map = rfunction_decl.rparameters
  |> List.mapi (fun i (n, _kt) -> n, Printf.sprintf "p%d" i )
  |> List.to_seq
  |> Hashtbl.of_seq
 in
  {
    rfn_name = rfunction_decl.rfn_name;
    generics = rfunction_decl.generics;
    rparameters = rfunction_decl.rparameters;
    return_type = rfunction_decl.return_type;
    tac_body = convert_from_rkbody ~switch_count:(ref 0) ~cases_count:(ref 0) ~label_name:(rfunction_decl.rfn_name) ~map ~count_var:(ref 0) ~if_count:(ref 0) rfunction_decl.rbody
  }

let tac_operator_decl_of_roperator_decl = function
| RUnary {op; rfield; return_type; kbody} ->
  let map = [rfield |> fst |> fun n -> n, "p0"] |> List.to_seq |> Hashtbl.of_seq in
  TacUnary {
    op;
    rfield;
    return_type;
    tac_body = convert_from_rkbody ~switch_count:(ref 0) ~cases_count:(ref 0) ~label_name:("operator") ~map ~count_var:(ref 0) ~if_count:(ref 0) kbody
  }
| RBinary {op; rfields = (((f1, _), (f2, _)) as rfields); return_type; kbody} -> 
  let map = f1::f2::[] |> List.mapi (fun i s -> s, Printf.sprintf "p%u" i) |> List.to_seq |> Hashtbl.of_seq in
  TacBinary {
    op;
    rfields;
    return_type;
    tac_body = convert_from_rkbody ~switch_count:(ref 0) ~cases_count:(ref 0) ~label_name:("binary operator") ~map ~count_var:(ref 0) ~if_count:(ref 0) kbody
  }

let rec tac_module_node_from_rmodule_node = function
  | RNExternFunc f -> TNExternFunc f
  | RNSyscall f -> TNSyscall f
  | RNStruct s -> TNStruct s
  | RNEnum s -> TNEnum s
  | RNConst s -> TNConst s
  | RNFunction f -> let tmp = (tac_function_decl_of_rfunction f) in 
  let () = Printf.printf "%s\n" (Asttacpprint.string_of_label_tac_body tmp.tac_body) in
  TNFunction tmp
  | RNOperator s -> TNOperator (tac_operator_decl_of_roperator_decl s)


and tac_module_path_of_rmodule_path { path; rmodule = RModule module_nodes } = 
{
  path;
  tac_module = TacModule (
    module_nodes |> List.map (fun node -> tac_module_node_from_rmodule_node node) )
}

and tac_program_of_rprogram (rprogram: rprogram): tac_program = 
  rprogram
  |> List.map (fun {filename; rmodule_path} -> 
    {
      filename;
      tac_module_path = tac_module_path_of_rmodule_path rmodule_path
    }
    )
(* and tac_program_of_rprogram (program: rprogram): tac_program = 
  program
  |> List.map (
    fun {filename, rmodule_path} ->
      {
        filename;
        tac_module_path = tac_module_path_of_rmodule_path md
      }
  ) *)
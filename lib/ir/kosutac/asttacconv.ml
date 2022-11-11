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
    | RBAnd _ -> TacSelf TacAnd
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
    ~if_count typed_expression =
  let _rktype = typed_expression.rktype in
  let expr = typed_expression.rexpression in
  match (allocated, expr) with
  | Some identifier, REIf (typed_expression, if_body, else_body) ->
      let statement_for_bool, condition_rvalue =
        convert_from_typed_expression ~map ~count_var ~if_count typed_expression
      in
      let goto_label = make_goto_label ~count_if:(post_inc if_count) 0 in
      let if_tac_body =
        convert_from_rkbody ~label_name:goto_label ~map ~count_var ~if_count
          if_body
      in
      let else_tac_body =
        convert_from_rkbody
          ~label_name:(make_goto_label ~count_if:(post_inc if_count) 1)
          ~map ~count_var ~if_count else_body
      in
      STIf {
        statement_for_bool;
        condition_rvalue;
        if_tac_body;
        else_tac_body
      } :: [], (TEIdentifier identifier)

  | Some identifier, RECases {cases; else_case} -> 
    let make_locale_label = make_goto_label ~count_if:(post_inc if_count) in
    let cases = cases |> List.mapi (fun i (case_condition, rkbody) -> 
      let label =  make_locale_label (i + 1) in
      let (statement_for_condition, tac_condition) = convert_from_typed_expression ~map ~count_var ~if_count case_condition in
      let tac_body = convert_from_rkbody ~label_name:label ~map ~count_var ~if_count rkbody in
         {
          statement_for_condition;
          condition = tac_condition;
          tac_body
         }
  )
    in 
    let else_tac_body = convert_from_rkbody ~label_name:(make_locale_label 0) ~map ~count_var ~if_count else_case in
    SCases {
      cases;
      else_tac_body
    }::[], TEIdentifier identifier
  | Some _identifier, RESwitch _ -> failwith "RESwitch to do"
  | _, REmpty -> convert_if_allocated ~allocated TEmpty
  | _, RFalse -> convert_if_allocated ~allocated TEFalse
  | _, RTrue -> convert_if_allocated ~allocated TETrue
  | _, RENullptr -> convert_if_allocated ~allocated TENullptr
  | _, REInteger (sign, size, int) -> convert_if_allocated ~allocated (TEInt (sign, size, int))
  | _, REFloat float -> convert_if_allocated ~allocated (TEFloat float)
  | _, RESizeof rktype -> convert_if_allocated ~allocated (TESizeof rktype)
  | _, REstring s ->  convert_if_allocated ~allocated (TEString s)
  | _, REIdentifier { identifier; _ } -> convert_if_allocated ~allocated (TEIdentifier (Hashtbl.find map identifier))
  | _, RETuple (typed_expressions) -> 
    let stmts_needed, tac_expression =
    typed_expressions
    |> List.map (convert_from_typed_expression ~map ~count_var ~if_count)
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
        |> List.map (convert_from_typed_expression ~map ~count_var ~if_count)
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
      field, convert_from_typed_expression ~map ~count_var ~if_count typed_expression
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
    |> List.map (convert_from_typed_expression ~map ~count_var ~if_count)
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
    let needed_statement, tac_expr = convert_from_typed_expression ~map ~if_count ~count_var first_expr in
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
    let (lstamements_needed, lhs_value) = convert_from_typed_expression ~map ~if_count ~count_var ltyped in
    let (rstamements_needed, rhs_value) = convert_from_typed_expression ~map ~if_count ~count_var rtyped in
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
    let (need_stmts, lvalue) = convert_from_typed_expression ~map ~if_count ~count_var operand in
    let new_tmp = make_inc_tmp count_var in
    let unary_op = RVUnop {
      unop = operator;
      expr = lvalue;
    } in
    let statement = STacDeclaration {identifier = new_tmp; expression = unary_op } in
    let last_stmt, return = convert_if_allocated ~allocated (TEIdentifier new_tmp) in
    (need_stmts @ statement::last_stmt ), return
  | _, REDeference (_n, _id) -> failwith ""
  | _ -> failwith ""

and convert_from_rkbody ~label_name ~map ~count_var ~if_count (rkbody : rkbody)
    =
  let stmts, types_return = rkbody in
  match stmts with
  | stmt :: q -> (
      match stmt with
      | RSDeclaration { is_const = _; variable_name; typed_expression } ->
          let new_tmp = make_inc_tmp count_var in
          let () = Hashtbl.add map variable_name new_tmp in
          let allocated =
            if
              KosuIrTyped.Asttyped.Expression.is_typed_expreesion_branch
                typed_expression
            then Some new_tmp
            else None
          in
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~allocated ~map ~count_var ~if_count
              typed_expression
          in

          let body =
            convert_from_rkbody ~label_name ~map ~count_var ~if_count
              (q, types_return)
          in
          add_statements_to_tac_body
            (tac_stmts
            @ STacDeclaration { identifier = new_tmp; expression = RVExpression tac_expression }
              :: [])
            body
      | RSAffection (identifier, typed_expression) ->
          let find_tmp = Hashtbl.find map identifier in
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~map ~count_var ~if_count
              typed_expression
          in
          let body =
            convert_from_rkbody ~label_name ~map ~count_var ~if_count
              (q, types_return)
          in
          body
          |> add_statements_to_tac_body
               (tac_stmts
               @ STacModification { identifier = find_tmp; expression = RVExpression tac_expression }
                 :: [])
      | RSDiscard typed_expression ->
          let tac_stmts, _tac_rvalue =
            convert_from_typed_expression ~map ~count_var ~if_count
              typed_expression
          in
          add_statements_to_tac_body tac_stmts
            (convert_from_rkbody ~label_name ~map ~count_var ~if_count
               (q, types_return))
      | RSDerefAffectation (identifier, typed_expression) ->
          let find_tmp = Hashtbl.find map identifier in
          let tac_stmts, tac_expression =
            convert_from_typed_expression ~map ~count_var ~if_count
              typed_expression
          in
          let body =
            convert_from_rkbody ~label_name ~map ~count_var ~if_count
              (q, types_return)
          in
          add_statements_to_tac_body
            (tac_stmts
            @ STDerefAffectation { identifier = find_tmp; expression = RVExpression tac_expression }
              :: [])
            body)
  | [] ->
      {
        label = label_name;
        body =
          convert_from_typed_expression ~map ~count_var ~if_count types_return;
      }

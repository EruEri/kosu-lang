open KosuIrTyped.Asttyped
open Asttac

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
  | Some identifier -> STacModification {identifier; expression = RExpression tac_expression}::[], TEIdentifier identifier
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

  | Some _identifier, RECases _ -> failwith "RECases to do"
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
        RFunction
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
            @ STacDeclaration { identifier = new_tmp; expression = RExpression tac_expression }
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
               @ STacModification { identifier = find_tmp; expression = RExpression tac_expression }
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
            @ STDerefAffectation { identifier = find_tmp; expression = RExpression tac_expression }
              :: [])
            body)
  | [] ->
      {
        label = label_name;
        body =
          convert_from_typed_expression ~map ~count_var ~if_count types_return;
      }

open KosuIrTyped.Asttyped
open Asttac
open KosuFrontend.Ast

let make_tmp = Printf.sprintf "r%u"

let make_goto_label ~count_if = Printf.sprintf "if.%u.%u" count_if

(**
@returns: the value of [n] before the incrementation    
*)
let post_inc n = let x = !n in let () = incr n in x

let add_statements_to_tac_body stmts tac_body = 
  let {label; body = (future_stmts, future_result)} = tac_body in
  {
    label;
    body = (stmts @ future_stmts), future_result
  }

let rec convert_from_typed_expression ~map ~count_var ~if_count typed_expression = 
  let _rktype = typed_expression.rktype in
  let expr = typed_expression.rexpression in
  match expr with
  | REmpty | RFalse -> [], (tac_rvalue_litteral_int Unsigned I8 0L)
  | RTrue -> [], (tac_rvalue_litteral_int Unsigned I8 1L)
  | RENullptr -> [], (tac_rvalue_litteral_int Unsigned I64 0L)
  | REInteger (sign, size, int) -> [], (tac_rvalue_litteral_int sign size int)
  | REFloat float -> [], (tac_rvalue_litteral_flaot float)
  | RESizeof rktype -> [], ( RSizeof rktype )
  | REstring s -> [], (tac_rvalue_litteral_stringlit s)
  | REIdentifier {identifier; _ } -> [], (tac_rvalue_litteral_identifier (Hashtbl.find map identifier))
  | REFunction_call {modules_path; generics_resolver; fn_name; parameters} -> 
    let (stmts_needed, tac_parameters) = 
      parameters 
      |> List.map (convert_from_typed_expression ~map ~count_var ~if_count)
      |> List.fold_left_map (fun acc (stmts, value) -> acc @ stmts, value) []
 in
    (* stmts_needed, RFunction( Function_call {
      module_path = modules_path;
      fn_name;
      generics_resolver;
      tac_parameters
    }) *)
    failwith ""
  | REIf (typed_expression, if_body, else_body) -> 
    let (statement_for_bool, condition_rvalue) = convert_from_typed_expression ~map ~count_var ~if_count typed_expression in
    let goto_label = (make_goto_label ~count_if:(post_inc if_count) 0) in
    let if_tac_body = convert_from_rkbody ~label_name:goto_label ~map ~count_var ~if_count if_body in
    let else_tac_body = convert_from_rkbody ~label_name:(make_goto_label ~count_if:(post_inc if_count) 1) ~map ~count_var ~if_count else_body in
    [], RIf {
      statement_for_bool;
      condition_rvalue;
      if_tac_body;
        else_tac_body
    }
  | _ -> failwith ""

and convert_from_rkbody ~label_name ~map ~count_var ~if_count (rkbody: rkbody) = 
  let (stmts, types_return) = rkbody in
  match stmts with
  | stmt :: q -> begin
    match stmt with
  | RSDeclaration {is_const = _; variable_name; typed_expression} -> 
    let (tac_stmts, tac_rvalue) = convert_from_typed_expression ~map ~count_var ~if_count typed_expression in
    let new_tmp = make_tmp (post_inc count_var) in
    let () = Hashtbl.add map variable_name new_tmp in
    let body = (convert_from_rkbody ~label_name ~map ~count_var ~if_count (q, types_return)) in
      add_statements_to_tac_body (tac_stmts @ STacDeclaration {identifier = new_tmp; expression = tac_rvalue} :: []) body
  | RSAffection (identifier, typed_expression) -> 
    let find_tmp = Hashtbl.find map identifier in
    let (tac_stmts, result) = convert_from_typed_expression ~map ~count_var ~if_count typed_expression in
    let body = convert_from_rkbody ~label_name ~map ~count_var ~if_count (q, types_return) in
    body |> add_statements_to_tac_body (tac_stmts @ STacModification {identifier = find_tmp; expression = result} :: [])
  | RSDiscard (typed_expression) -> 
    let (tac_stmts, _tac_rvalue) = convert_from_typed_expression ~map ~count_var ~if_count typed_expression in
    add_statements_to_tac_body tac_stmts (convert_from_rkbody ~label_name ~map ~count_var ~if_count (q, types_return))
  | RSDerefAffectation (identifier, typed_expression) -> 
    let find_tmp = Hashtbl.find map identifier in
    let (tac_stmts, result) = convert_from_typed_expression ~map ~count_var ~if_count typed_expression in
    let body = convert_from_rkbody ~label_name ~map ~count_var ~if_count (q, types_return) in
    add_statements_to_tac_body (tac_stmts @ STDerefAffectation {identifier = find_tmp; expression = result} :: []) body
  end
  | [] -> {
    label = label_name;
    body = convert_from_typed_expression ~map ~count_var ~if_count types_return
  }
    

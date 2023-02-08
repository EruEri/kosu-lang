open KosuIrTAC.Asttac
open Asttaccfg.Cfg_Sig_Impl


  
let rec of_tac_statements ~start_label ~end_labels ~ending ~cfg_statements (stmts, return) = match stmts with 
  | [] -> let block =  {
    label = start_label;
    cfg_statements = List.rev cfg_statements;
    followed_by = Asttaccfg.StringSet.of_list end_labels;
    ending = (match return with 
      None -> begin 
        match ending with 
        | None -> None
        | Some ending -> Some (BBe_if ending)

      end 
      | Some tte -> Some (Bbe_return tte) )
  } in
  BasicBlockSet.singleton block
| (stmt::q) as _stmts -> begin match stmt with
  | STacDeclaration {identifier; trvalue} -> begin 
    let declaration =  CFG_STacDeclaration {identifier; trvalue} in
    of_tac_statements ~start_label ~end_labels ~ending ~cfg_statements:(declaration::cfg_statements) (q, return)
  end
  | STacModification {identifier; trvalue} -> begin 
    let modification = CFG_STacModification {identifier; trvalue} in
    of_tac_statements ~start_label ~end_labels ~ending ~cfg_statements:(modification::cfg_statements) (q, return)
end
  | STDerefAffectation {identifier; trvalue} -> begin 
    let derefaffect = CFG_STDerefAffectation {identifier; trvalue} in
    of_tac_statements ~start_label ~end_labels ~ending ~cfg_statements:(derefaffect::cfg_statements) (q, return)
end
  | STIf {statement_for_bool; condition_rvalue; goto1; goto2; if_tac_body; else_tac_body; exit_label} ->
    let continuation = 
      of_tac_statements ~start_label ~end_labels:[goto1; goto2]
      ~ending:(Some {condition = condition_rvalue; if_label = goto1; else_label = goto2}) 
      ~cfg_statements (statement_for_bool, None)
    in
    let if_blocks = of_tac_body ~end_labels:[exit_label] if_tac_body in
    let else_blocks = of_tac_body ~end_labels:[exit_label] else_tac_body in 

    let blocks_continuation = of_tac_statements ~start_label:exit_label ~end_labels ~ending ~cfg_statements:[] (q, return) in
    
    continuation
    |> BasicBlockSet.union if_blocks
    |> BasicBlockSet.union else_blocks
    |> BasicBlockSet.union blocks_continuation
  | SCases {cases = {condition_label; statement_for_condition; condition; goto; jmp_false; end_label; tac_body}::tac_cases; else_tac_body; exit_label} -> 
    let () = match condition_label with
    | None -> ()
    | Some s -> failwith @@ (Printf.sprintf "First cases with a label name ? : %s" s) 
  in
  let continuation = of_tac_statements ~start_label ~end_labels:[goto; jmp_false] 
    ~ending:(Some {condition = condition; if_label = goto; else_label = jmp_false})
    ~cfg_statements (statement_for_condition, None) in
  let first_block_body = of_tac_body ~end_labels:[end_label] tac_body in

  let blocks_continuation = of_tac_statements ~start_label:exit_label ~end_labels ~ending ~cfg_statements:[] (q, return) in

  let cases_basic_block = tac_cases |> List.fold_left (fun acc {condition_label; statement_for_condition; condition; goto; jmp_false; end_label; tac_body} -> 
    let start_label = match condition_label with Some s -> s | None -> "Very wierd start label for cases should be None" in
    let block_condition = of_tac_statements ~start_label:start_label ~end_labels:[goto; jmp_false] 
        ~ending:(Some {condition; if_label = goto; else_label = jmp_false})
        ~cfg_statements:[] (statement_for_condition, None) in
      let block = of_tac_body ~end_labels:[end_label] tac_body in
      acc
      |> BasicBlockSet.union block_condition
      |> BasicBlockSet.union block
  ) BasicBlockSet.empty in

  let else_basic_block = of_tac_body ~end_labels:[exit_label] else_tac_body in

    continuation
    |> BasicBlockSet.union first_block_body
    |> BasicBlockSet.union cases_basic_block
    |> BasicBlockSet.union else_basic_block
    |> BasicBlockSet.union blocks_continuation

  | SCases {cases = []; else_tac_body = _; exit_label = _} -> failwith "Unreachable code: Syntax for at least a branch"
  | STSwitch _ -> failwith "switch todo"
end

and of_tac_body ~end_labels ({label; body} : tac_body) = of_tac_statements ~ending:None ~start_label:label ~end_labels ~cfg_statements:[] body

let of_tac_body tac_body = 
  let basic_blocks = of_tac_body ~end_labels:[] tac_body in
  {
    entry_block = tac_body.label;
    blocks = basic_blocks;
  }


let cfgs_of_tac_program named_tacmodules =
  named_tacmodules |> List.map (fun {filename; tac_module_path; _} -> 
    let TacModule tac_nodes = tac_module_path.tac_module in
    (
      filename,
      tac_nodes |> List.filter_map (function
      | TNFunction tacfun -> 
        Some (of_tac_body tacfun.tac_body)
      | _ -> None
      )
    )
  )

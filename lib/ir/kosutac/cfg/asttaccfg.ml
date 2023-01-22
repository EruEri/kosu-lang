open KosuIrTAC.Asttac

type cfg_statement =
| CFG_STacDeclaration of { identifier : string; trvalue : tac_typed_rvalue }
| CFG_STacModification of { identifier : string; trvalue : tac_typed_rvalue }
| CFG_STDerefAffectation of { identifier : string; trvalue : tac_typed_rvalue }

type basic_block_end = 
| Bbe_if of {
  condition: tac_typed_expression;
  if_label: string;
  else_label: string;
}
| Bbe_return of tac_typed_expression

and basic_block = {
  label: string;
  cfg_statements: cfg_statement list;
  followed_by: string list;
  ending: basic_block_end option 

}

and cfg = {
  entry_block: string;
  blocks: basic_block list
}

let fake_label_counter = ref 0

let fake_label () = 
  let n = !fake_label_counter in
  let () = fake_label_counter := n + 1 in
  Printf.sprintf "fake_label.%u" n


module Convert = struct
  let rec of_tac_statements ~end_label stmts = match stmts with 
  | [] -> [], None , []
  | (stmt::q) as stmts -> begin match stmt with
    | STacDeclaration {identifier; trvalue} -> begin match end_label with
      | None -> 
        let future_stmts, basic_block_end, generated_bb = of_tac_statements ~end_label q in
        CFG_STacDeclaration {identifier; trvalue}::future_stmts, basic_block_end, generated_bb @ []
      | Some label -> 
        [], 
        None, 
        of_tac_body ~end_label:None {label; body = (stmts, None)}    
    end
    | STacModification {identifier; trvalue} -> begin match end_label with
      | None -> 
        let future_stmts, basic_block_end, generated_bb = of_tac_statements ~end_label q in
        CFG_STacModification {identifier; trvalue}::future_stmts, basic_block_end, generated_bb @ []
      | Some label -> 
        [], 
        None, 
        of_tac_body ~end_label:None {label; body = (stmts, None)}    
    end
    | STDerefAffectation {identifier; trvalue} -> begin match end_label with
      | None -> 
        let future_stmts, basic_block_end, generated_bb = of_tac_statements ~end_label q in
        CFG_STDerefAffectation {identifier; trvalue}::future_stmts, basic_block_end, generated_bb @ []
      | Some label -> 
        [], 
        None, 
        of_tac_body ~end_label:None {label; body = (stmts, None)}    
    end
    | STIf {statement_for_bool; condition_rvalue; goto1; goto2; if_tac_body; else_tac_body; exit_label} ->
      begin match end_label with
      | None ->       
        let cfg_pre_condtion_stmts, _basic_block_end, generated = of_tac_statements ~end_label statement_for_bool in
        cfg_pre_condtion_stmts, 
        Some (Bbe_if {condition = condition_rvalue; if_label = goto1; else_label = goto2}), 
        (of_tac_body ~end_label:(Some exit_label) if_tac_body) @ (of_tac_body ~end_label:(Some exit_label) else_tac_body ) @ generated
      | Some label -> 
        [],
        None,
        (of_tac_body ~end_label:None {label; body = (stmts, None)})
      
      end

    | _ -> failwith ""
  
  end

  and of_tac_body ~end_label ({label; body} : tac_body) = 
  let cfg_statements, _basic_block_end, genereted = of_tac_statements ~end_label (fst body) in
  {
      label;
      cfg_statements;
      followed_by = end_label |> Option.to_list;
    ending = body |> snd |> Option.map (fun return -> Bbe_return return);
  }::genereted

  let of_tac_body tac_body = 
    let basic_blocks = of_tac_body ~end_label:None tac_body in
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

end


module CfgPprint = struct
  let string_of_cfg_statement = function
| CFG_STacDeclaration {identifier; trvalue} ->
  let tac_decl = STacDeclaration {identifier; trvalue} in
  KosuIrTAC.Asttacpprint.string_of_tac_statement tac_decl
| CFG_STDerefAffectation {identifier; trvalue} ->
    let tac_decl = STDerefAffectation {identifier; trvalue} in
    KosuIrTAC.Asttacpprint.string_of_tac_statement tac_decl 
| CFG_STacModification {identifier; trvalue} ->
  let tac_decl = STacModification {identifier; trvalue} in
  KosuIrTAC.Asttacpprint.string_of_tac_statement tac_decl

let string_of_basic_block_end = function
| Bbe_if {condition; if_label; else_label} -> 
  Printf.sprintf "if %s goto %s\ngoto %s" 
    (KosuIrTAC.Asttacpprint.string_of_typed_tac_expression condition)
    if_label
    else_label
| Bbe_return tte -> Printf.sprintf "return %s" 
  (KosuIrTAC.Asttacpprint.string_of_typed_tac_expression tte)

let string_of_basic_block bb =
  Printf.sprintf "follow : [%s]\n%s:\n\t%s\n\t%s"
  (bb.followed_by |> String.concat ", ")
  (bb.label)
  (bb.cfg_statements |> List.map string_of_cfg_statement |> String.concat "\n\t")
  (bb.ending |> Option.map string_of_basic_block_end |> Option.value ~default:"")

let string_of_cfg cfg = 
  Printf.sprintf "entry: %s\n\n%s"
  cfg.entry_block
  (cfg.blocks |> List.map string_of_basic_block |> String.concat "\n\n")

let string_of_named_cfg named_cfgs =
  named_cfgs |> List.map (fun (filename, cgfs) ->
    Printf.sprintf "========== %s ============\n\n%s"
    filename
    (cgfs |> List.map string_of_cfg |> String.concat "\n\n")
  ) |> String.concat "\n\n"

end

open KosuIrTAC.Asttac
open KosuIrTyped.Asttyped

module StringSet = Set.Make(String)

module Date = struct

  type 'a dated = {
    value: 'a;
    start: int option;
    ending: int option
  }

  let has_started {start; _} = Option.is_some start
  let has_finished {ending; _} = Option.is_some ending
  let value {value; _} = value
  let set_start debut dated = {
    dated with start = Some debut
  }
  let set_end ending dated = {
    dated with ending = Some ending
  }

  let add_date date dated = 
    match dated.start with
    | None -> { dated with start = Some date }
    | Some s -> set_end s dated
  
end

open Date

module TypedIdentifierSet = Set.Make (struct
type t = string * rktype
let compare lhs rhs = 
  let string_compare = compare (fst lhs) (fst rhs) in
  if string_compare = 0 then compare (snd lhs) (snd rhs)
  else string_compare  
end
)

type liveness_info = string dated list

module LabelSet = Set.Make(struct
  type t = string

  let compare = Stdlib.compare
end)

type cfg_statement =
| CFG_STacDeclaration of { identifier : string; trvalue : tac_typed_rvalue }
| CFG_STacModification of { identifier : string; trvalue : tac_typed_rvalue }
| CFG_STDerefAffectation of { identifier : string; trvalue : tac_typed_rvalue }

type cfg_live_statetment = 
  cfg_statement * liveness_info

type bbe_if = {
  condition: tac_typed_expression;
  if_label: string;
  else_label: string;
}

type basic_block_end = 
| BBe_if of bbe_if
| Bbe_return of tac_typed_expression

type 'a basic_block = {
  label: string;
  cfg_statements: 'a list;
  followed_by: StringSet.t;
  ending: basic_block_end option
}

type 'a basic_block_detail = {
  basic_block: 'a basic_block;
  in_vars: TypedIdentifierSet.t;
  out_vars: TypedIdentifierSet.t;

}

module BasicBlockSet = Set.Make(struct
  type t = cfg_statement basic_block
  let compare (lhs: t) (rhs: t) = String.compare lhs.label rhs.label
  end)

  module BasicBlockDetailSet = Set.Make(struct
  type t = cfg_statement basic_block_detail
  let compare (lhs: t) (rhs: t) = String.compare lhs.basic_block.label rhs.basic_block.label
  end)

  type cfg = {
    entry_block: string;
    blocks: BasicBlockSet.t
  }

  type cfg_detail = {
    entry_block: string;
    blocks_details: BasicBlockDetailSet.t
  }
let fake_label_counter = ref 0

let fake_label () = 
  let n = !fake_label_counter in
  let () = fake_label_counter := n + 1 in
  Printf.sprintf "fake_label.%u" n


module Convert = struct
  
  let rec of_tac_statements ~start_label ~end_labels ~ending ~cfg_statements (stmts, return) = match stmts with 
    | [] -> let block =  {
      label = start_label;
      cfg_statements = List.rev cfg_statements;
      followed_by = StringSet.of_list end_labels;
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
      let declaration = CFG_STacDeclaration {identifier; trvalue} in
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
| Bbe_return tte -> Printf.sprintf "return %s" 
  (KosuIrTAC.Asttacpprint.string_of_typed_tac_expression tte)
| BBe_if {condition; if_label; else_label} -> Printf.sprintf "if %s goto %s\n\tgoto %s" 
  (KosuIrTAC.Asttacpprint.string_of_typed_tac_expression condition)
  if_label
  else_label

let string_of_basic_block bb =
  Printf.sprintf "follow : [%s]\n%s:\n\t%s\n\t%s"
  (bb.followed_by |> StringSet.elements |> String.concat ", ")
  (bb.label)
  (bb.cfg_statements |> List.map string_of_cfg_statement |> String.concat "\n\t")
  (bb.ending |> Option.map string_of_basic_block_end |> Option.value ~default:"")

let string_of_typed_indentifier (s, kt) = 
  Printf.sprintf "(%s: %s)" s (KosuIrTyped.Asttypprint.string_of_rktype kt)

let string_of_typed_indentifier_set set = 
  set |> TypedIdentifierSet.elements |> List.map string_of_typed_indentifier |> String.concat ", "
let string_of_basic_block_details bbd = 
  Printf.sprintf "in_vars : {%s}\n%s\nout_vars : {%s}" 
  (string_of_typed_indentifier_set bbd.in_vars) 
  (string_of_basic_block bbd.basic_block)
  (string_of_typed_indentifier_set bbd.out_vars)

let string_of_cfg (cfg: cfg) = 
  Printf.sprintf "entry: %s\n\n%s"
  cfg.entry_block
  (cfg.blocks |> BasicBlockSet.elements |> List.map string_of_basic_block |> String.concat "\n\n")

let string_of_cfg_details cfg = 
  Printf.sprintf "entry: %s\n\n%s"
  cfg.entry_block
  (cfg.blocks_details |> BasicBlockDetailSet.elements |> List.map string_of_basic_block_details |> String.concat "\n\n")

let string_of_named_cfg named_cfgs =
  named_cfgs |> List.map (fun (filename, cgfs) ->
    Printf.sprintf "========== %s ============\n\n%s"
    filename
    (cgfs |> List.map string_of_cfg |> String.concat "\n\n")
  ) |> String.concat "\n\n"

let string_of_named_cfg_details named_cfgs =
  named_cfgs |> List.map (fun (filename, cgfs) ->
    Printf.sprintf "========== %s ============\n\n%s"
    filename
    (cgfs |> List.map string_of_cfg_details |> String.concat "\n\n")
  ) |> String.concat "\n\n"

end

open KosuIrTAC.Asttac
open Asttaccfg.Cfg
open Asttaccfg.Cfg.Detail
open Asttaccfg.Cfg.Basic


let string_of_typed_indentifier (s, kt) = 
  Printf.sprintf "(%s: %s)" s (KosuIrTyped.Asttypprint.string_of_rktype kt)

let string_of_typed_indentifier_set set = 
  set |> TypedIdentifierSet.elements |> List.map string_of_typed_indentifier |> String.concat ", "

  
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

  let string_of_cfg_liveness_statement (cfgl_statement: Asttaccfg.Cfg.Liveness.cfg_liveness_statement) = 
    Printf.sprintf "%s [%s]" 
    (string_of_cfg_statement cfgl_statement.cfg_statement)
    (cfgl_statement.liveness_info 
      |> Asttaccfg.Cfg.Liveness.LivenessInfo.to_list 
      |> List.map (fun (typed_id, bool) -> Printf.sprintf "<%s => %s>" (string_of_typed_indentifier typed_id) (if bool then "alive" else "dead"))
      |> String.concat ", "
    )

let string_of_basic_block_end = function
| Bbe_return tte -> Printf.sprintf "return %s" 
  (KosuIrTAC.Asttacpprint.string_of_typed_tac_expression tte)
| BBe_if {condition; if_label; else_label} -> Printf.sprintf "if %s goto %s\n\tgoto %s" 
  (KosuIrTAC.Asttacpprint.string_of_typed_tac_expression condition)
  if_label
  else_label

let string_of_basic_block bb =
  Printf.sprintf "follow : [%s]\n%s:\n\t%s\n\t%s"
  (bb.followed_by |> Asttaccfg.StringSet.elements |> String.concat ", ")
  (bb.label)
  (bb.cfg_statements |> List.map string_of_cfg_statement |> String.concat "\n\t")
  (bb.ending |> Option.map string_of_basic_block_end |> Option.value ~default:"")

let string_of_liveness_basic_block bb =
  Printf.sprintf "follow : [%s]\n%s:\n\t%s\n\t%s"
  (bb.followed_by |> Asttaccfg.StringSet.elements |> String.concat ", ")
  (bb.label)
  (bb.cfg_statements |> List.map string_of_cfg_liveness_statement |> String.concat "\n\t")
  (bb.ending |> Option.map string_of_basic_block_end |> Option.value ~default:"")


let string_of_basic_block_details bbd = 
  Printf.sprintf "in_vars : {%s}\n%s\nout_vars : {%s}" 
  (string_of_typed_indentifier_set bbd.in_vars) 
  (string_of_basic_block bbd.basic_block)
  (string_of_typed_indentifier_set bbd.out_vars)

let string_of_basic_block_liveness_details (bbld: Asttaccfg.Cfg.Liveness.cfg_liveness_statement basic_block_detail) = 
  Printf.sprintf "in_vars : {%s}\n%s\nout_vars : {%s}"
  (string_of_typed_indentifier_set bbld.in_vars) 
  (string_of_liveness_basic_block bbld.basic_block)
  (string_of_typed_indentifier_set bbld.out_vars)

let string_of_cfg (cfg: cfg) = 
  Printf.sprintf "entry: %s\n\n%s"
  cfg.entry_block
  (cfg.blocks |> BasicBlockMap.bindings |> List.map snd |> List.map string_of_basic_block |> String.concat "\n\n")

let string_of_cfg_details (cfg: Asttaccfg.Cfg.Detail.cfg_detail) = 
  Printf.sprintf "entry: %s\n\n%s"
  cfg.entry_block
  (cfg.blocks_details |> Asttaccfg.BasicBlockMap.bindings |> List.map snd |> List.map string_of_basic_block_details |> String.concat "\n\n")

let string_of_cfg_liveness_details (cfg: Asttaccfg.Cfg.Liveness.cfg_liveness_detail) = 
  Printf.sprintf "entry: %s\n\n%s"
  cfg.entry_block
  (cfg.blocks_liveness_details |> Asttaccfg.BasicBlockMap.bindings |> List.map snd |> List.map string_of_basic_block_liveness_details |> String.concat "\n\n")


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

let string_of_named_cfg_liveness_details named_cfgs =
  named_cfgs |> List.map (fun (filename, cgfs) ->
    Printf.sprintf "========== %s ============\n\n%s"
    filename
    (cgfs |> List.map string_of_cfg_liveness_details |> String.concat "\n\n")
  ) |> String.concat "\n\n"


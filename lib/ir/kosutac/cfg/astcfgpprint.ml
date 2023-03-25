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
  (bb.ending |> fst |> Option.map ( string_of_basic_block_end) |> Option.value ~default:"")


let string_of_basic_block_details bbd = 
  Printf.sprintf "in_vars : {%s}\n%s\nout_vars : {%s}" 
  (string_of_typed_indentifier_set bbd.in_vars) 
  (string_of_basic_block bbd.basic_block)
  (string_of_typed_indentifier_set bbd.out_vars)

let string_of_basic_block_liveness_details (bbld: (Asttaccfg.Cfg.Liveness.cfg_liveness_statement, 'b) basic_block_detail) = 
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

type dot_digraph_node = {
  name: string;
  elements: string list;
  ending: string option;
  link_to: string list;
  din_vars: TypedIdentifierSet.t;
  dout_vars: TypedIdentifierSet.t;
}

type dot_digrah = {
  entry: string;
  nodes: dot_digraph_node list
  }


let convert = String.map (fun c -> if c = ':' || c = '.' then '_' else c)

let diagraph_node_of_basic_block ~(func : 'a -> string) ?(in_vars = TypedIdentifierSet.empty) ?(out_vars = TypedIdentifierSet.empty) (bb: ('a, 'b) Basic.basic_block) = 
  {
    name = bb.label;
    elements = List.map func bb.cfg_statements;
    ending = bb.ending |> Option.map (string_of_basic_block_end);
    link_to = Asttaccfg.StringSet.elements bb.followed_by;
    din_vars = in_vars;
    dout_vars = out_vars;
  }

let dot_digrah_of_cfg (cfg: Asttaccfg.Cfg.Detail.cfg_detail) = 
  {
    entry = cfg.entry_block;
    nodes = cfg.blocks_details |> Asttaccfg.BasicBlockMap.bindings |> List.map (fun (_, bb) -> diagraph_node_of_basic_block ~in_vars:bb.in_vars ~out_vars:bb.out_vars ~func:string_of_cfg_statement bb.basic_block)
  }

let dot_diagrah_of_cfg_liveness (cfg: Asttaccfg.Cfg.Liveness.cfg_liveness_detail) = 
  {
    entry = cfg.entry_block;
    nodes = cfg.blocks_liveness_details |> Asttaccfg.BasicBlockMap.bindings |> List.map (fun (_, bbl) ->
      diagraph_node_of_basic_block ~in_vars:bbl.in_vars ~out_vars:bbl.out_vars ~func:string_of_cfg_liveness_statement {bbl.basic_block with ending = fst bbl.basic_block.ending}
    )
  }

let quoted = Printf.sprintf "\"%s\""

let export_infer_graph_of_cfg ~outchan (cfg: Asttaccfg.Cfg.Liveness.cfg_liveness_detail) () = 
  let graph = Inference_Graph.infer cfg in
  let bindings =  Inference_Graph.IG.bindings graph in
  let () = Printf.fprintf outchan "graph %s {\n" (Printf.sprintf "infered_%s" cfg.entry_block) in
  let () = bindings |> List.iter (fun (node, edges) ->   
    Printf.fprintf outchan "\t%s -- {%s}\n" 
    (node |> string_of_typed_indentifier |> quoted)  
    (edges |> List.map (fun node -> node |> string_of_typed_indentifier |> quoted) |> String.concat " ")
  ) in 
  let () = Printf.fprintf outchan "}" in
  ()
let dot_chars_to_escape = ['<'; '>']

let escape ~chars s =
  let buffer = Buffer.create (String.length s) in
  let () = s |> String.iter (fun c -> 
    match List.mem c chars with
    | false -> Buffer.add_char buffer c
    | true -> Printf.bprintf buffer "\\%c" c
  ) in
  buffer |> Buffer.to_bytes |> Bytes.to_string

let string_of_dot_graph ?(out = stdout) graph = 
  let open Printf in
  let links = graph.nodes |> List.map (fun {name; link_to; _} -> (name, link_to)) in
  let () = Printf.fprintf out "digraph %s {\n" graph.entry in
  let () = Printf.fprintf out "\tnode [shape=record fontname=Arial];\n\n" in
  let () = Printf.fprintf out "%s" (graph.nodes 
  |> List.map (fun {name; elements; ending; din_vars; dout_vars; _} -> 
    Printf.sprintf "\t\"%s\" [label=\"invars \\{%s\\}\\n%s:\\l%s%s\\l\\loutvars \\{%s\\}\"];"
     name 
     (string_of_typed_indentifier_set din_vars)
     name
     ( elements |> String.concat "\n" |>  String.escaped |> escape ~chars:dot_chars_to_escape)
     (ending |> Option.map ( ( ^ ) "\\n") |> Option.value ~default:"")
     (string_of_typed_indentifier_set dout_vars)
    ) |> String.concat "\n"
  ) in
  let () = Printf.fprintf out "\n" in
  let () = Printf.fprintf out "\n\t%s" (
    links |> List.map (fun (name, link_to) -> 
      link_to |> List.map (fun link -> 
        sprintf "\"%s\" -> \"%s\"" name link
    ) |> String.concat ";\n\t"
  ) |> String.concat "\n\t"
  ) in
  let () = Printf.fprintf out "\n\n}" in
  ()

let fetch_function (fun_name: string) (named_cfg) = 
  named_cfg |> List.find_map (fun (_, functions) ->
    functions |> List.find_map (fun (cfg: Asttaccfg.Cfg.Detail.cfg_detail) -> if cfg.entry_block = fun_name then Some cfg else None )
  )

let fetch_function_liveness (fun_name: string) liveness_cfg = 
  liveness_cfg |> List.find_map (fun (_, functions) -> 
    functions |> List.find_map (fun (cfg: Asttaccfg.Cfg.Liveness.cfg_liveness_detail) -> 
      if cfg.entry_block = fun_name then Some cfg else None
    )
  )
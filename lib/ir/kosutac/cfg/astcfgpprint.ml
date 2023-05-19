open Asttaccfg.KosuRegisterAllocator
open Asttaccfg.KosuRegisterAllocator.Basic
open Asttaccfg.KosuRegisterAllocator.Detail
open Asttaccfg.KosuRegisterAllocator.Pprint

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

let diagraph_node_of_basic_block ~(func : 'a -> string) ?(in_vars = TypedIdentifierSet.empty) ?(out_vars = TypedIdentifierSet.empty) (bb: ('a, 'b) Basic.basic_block) = 
  {
    name = bb.label;
    elements = List.map func bb.cfg_statements;
    ending = bb.ending |> Option.map (string_of_basic_block_end);
    link_to = StringSet.elements bb.followed_by;
    din_vars = in_vars;
    dout_vars = out_vars;
  }

let dot_diagrah_of_cfg_basic (cfg: Basic.cfg) = 
  {
    entry = cfg.entry_block;
    nodes = cfg.blocks |> BasicBlockMap.bindings |> List.map (fun (_, bbl) ->
      diagraph_node_of_basic_block ~in_vars:TypedIdentifierSet.empty ~out_vars:TypedIdentifierSet.empty ~func:string_of_cfg_statement bbl
    )
  }

let dot_diagrah_of_cfg_detail (cfg:  Detail.cfg_detail) = 
  { 
    entry = cfg.entry_block;
    nodes = cfg.blocks_details |> BasicBlockMap.bindings |> List.map (fun (_, bbl) ->
      diagraph_node_of_basic_block ~in_vars:bbl.in_vars ~out_vars:bbl.out_vars ~func:string_of_cfg_statement bbl.basic_block
    )
  }
let dot_diagrah_of_cfg_liveness (cfg:  Liveness.cfg_liveness_detail) = 
  {
    entry = cfg.entry_block;
    nodes = cfg.blocks_liveness_details |> BasicBlockMap.bindings |> List.map (fun (_, bbl) ->
      diagraph_node_of_basic_block ~in_vars:bbl.in_vars ~out_vars:bbl.out_vars ~func:string_of_cfg_liveness_statement {bbl.basic_block with ending = fst bbl.basic_block.ending}
    )
  }

let quoted = Printf.sprintf "\"%s\""

let dot_chars_to_escape = ['<'; '>']

let escape ~chars s =
  let buffer = Buffer.create (String.length s) in
  let () = s |> String.iter (fun c -> 
    match List.mem c chars with
    | false -> Buffer.add_char buffer c
    | true -> Printf.bprintf buffer "\\%c" c
  ) in
  buffer |> Buffer.to_bytes |> Bytes.to_string

  let rec combine_safe lhs rhs =
    match (lhs, rhs) with
    | [], _ | _, [] -> []
    | t1 :: q1, t2 :: q2 -> (t1, t2) :: combine_safe q1 q2

(* let export_colored_graph ~outchan (cfg: Liveness.cfg_liveness_detail) () = 
    let open Asttaccfg in
    let open Asttaccfg.GreedyColoring.ColoredGraph in
    let parameters = combine_safe cfg.parameters SanCfgRegister.arguments_register in
    let graph = GreedyColoring.coloration ~parameters ~available_color:[R0; R9; R10; R3] cfg in
    let bindings = GreedyColoring.ColoredGraph.bindings graph in
    let () = Printf.fprintf outchan "strict graph %s {\n" (Printf.sprintf "infered_%s" cfg.entry_block) in
    let () = bindings |> List.iter (fun (node, _) -> 
      Printf.fprintf outchan "\t%s [color=%s]\n"
      (node.node |> SanCfgAst.Cfg_Sig.repr |> quoted)
      ( match node.color with
        | None -> "black"
        | Some c -> SanCfgRegister.color_map |> List.assoc_opt c |> Option.value ~default:"white" |> quoted)
    ) in
    let () = bindings |> List.iter (fun (node, edges) ->  
      Printf.fprintf outchan "\t%s -- {%s}\n" 
      (node.node |> SanCfgAst.Cfg_Sig.repr |> quoted)  
      (edges |> List.map (fun node -> node.node |> SanCfgAst.Cfg_Sig.repr |> quoted) |> String.concat " ")
    ) in 

    let () = Printf.fprintf outchan "}" in
    () *)

 let export_infer_graph_of_cfg ~outchan (cfg: Liveness.cfg_liveness_detail) () = 
    let graph = Interference_Graph.interfere cfg in
    let bindings = Interference_Graph.IG.bindings graph in
    let () = Printf.fprintf outchan "strict graph %s {\n" (Printf.sprintf "infered_%s" cfg.entry_block) in
    let () = bindings |> List.iter (fun (node, edges) ->   
      Printf.fprintf outchan "\t%s -- {%s}\n" 
      (node |> Asttaccfg.CfgPprint.string_of_variable |> quoted)  
      (edges |> List.map (fun node -> node |> Asttaccfg.CfgPprint.string_of_variable |> quoted) |> String.concat " ")
    ) in 
    let () = Printf.fprintf outchan "}" in
    ()

let string_of_dot_graph ~out graph = 
  let open Printf in
  let links = graph.nodes |> List.map (fun {name; link_to; _} -> (name, link_to)) in
  let () = Printf.fprintf out "graph %s {\n" graph.entry in
  let () = Printf.fprintf out "\tnode [shape=record fontname=Arial];\n\n" in
  let () = Printf.fprintf out "%s" (graph.nodes 
  |> List.map (fun {name; elements; ending; din_vars; dout_vars; _} -> 
    Printf.sprintf "\t\"%s\" [label=\"invars \\{%s\\}\\n%s:\\l%s%s\\l\\loutvars \\{%s\\}\"];"
     name 
     (string_of_typed_indentifier_set din_vars)
     name
     (elements |> String.concat "\n" |>  String.escaped |> escape ~chars:dot_chars_to_escape)
     (ending |> Option.map ( ( ^ ) "\\n") |> Option.value ~default:"")
     (string_of_typed_indentifier_set dout_vars)
    ) |> String.concat "\n"
  ) in
  let () = Printf.fprintf out "\n" in
  let () = Printf.fprintf out "\n\t%s" (
    links |> List.map (fun (name, link_to) -> 
      link_to |> List.map (fun link -> 
        sprintf "\"%s\" -- \"%s\";" name link
    ) |> String.concat "\n\t"
  ) |> String.concat "\n\t"
  ) in
  let () = Printf.fprintf out "\n\n}" in
  ()



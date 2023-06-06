(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of kosu-register-allocator                                               *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
(*                                                                                            *)
(* kosu-register-allocator is free software: you can redistribute it and/or modify            *)
(*  it under the terms either:                                                                *)
(*                                                                                            *)
(*    - the GNU General Public License as published by the Free Software Foundation,          *)
(*        either version 3 of the License, or (at your option) any later version              *)
(*   or                                                                                       *)
(*                                                                                            *)
(*     - the GNU Lesser General Public License as published by the Free Software Foundation,  *)
(*        either version 3 of the License, or (at your option) any later version              *)
(*                                                                                            *)
(* kosu-register-allocator is distributed in the hope that it will be useful,                 *)
(*   but WITHOUT ANY WARRANTY;                                                                *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along                    *)
(* with kosu-register-allocator. If not, see <http://www.gnu.org/licenses/>.                  *)
(*                                                                                            *)
(**********************************************************************************************)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type ColoredType = sig
  type t 
  val compare : t -> t -> int
end

module Make (S : OrderedType) = struct
  module NodeSet = Set.Make (S)

  module EgdesSig = struct
    type t = { root : S.t; along : S.t }

    let compare lhs rhs =
      let tmp = S.compare lhs.root rhs.root in
      if tmp <> 0 then tmp else S.compare lhs.along rhs.along
  end

  module EdgeSet = Set.Make (EgdesSig)

  type graph = { nodes : NodeSet.t; edges : EdgeSet.t }

  let empty : graph = { nodes = NodeSet.empty; edges = EdgeSet.empty }
  let exist_node func graph = NodeSet.exists (fun key -> func key) graph.nodes

  (**
    add a new node to the graph. the graph is physically equal if the node was already in the graph    
  *)
  let add_node (node : S.t) (graph : graph) = NodeSet.add node graph.nodes

  let of_seq (nodes : S.t Seq.t) : graph =
    let node_set = nodes |> Seq.map (fun node -> node) |> NodeSet.of_seq in
    { nodes = node_set; edges = EdgeSet.empty }

  let contains node graph = NodeSet.mem node graph.nodes

  (**
    @raise Not_found if [node] is not in [graph]    
  *)
  let check_contains node graph =
    if not @@ contains node graph then raise Not_found

  (**
      Return all nodes where [node] is the root of the edge
      @raise Not_found if [node] is not in [graph]    
  *)
  let egde_of : S.t -> graph -> NodeSet.t =
   fun node graph ->
    let open EgdesSig in
    EdgeSet.fold
      (fun egde acc ->
        if S.compare egde.root node = 0 then NodeSet.add egde.along acc else acc)
      graph.edges NodeSet.empty

  (** 
    Add an egde beetween [node] and [along] in [graph]. 
    @raise Not_found if [node] or [along] aren't in the graph
  *)
  let link (node : S.t) ~(along : S.t) (graph : graph) : graph =
    let open EgdesSig in
    let () = check_contains node graph in
    let () = check_contains along graph in
    let edge = { root = node; along } in
    { graph with edges = EdgeSet.add edge graph.edges }

  let mutual_link (node1 : S.t) (node2 : S.t) (graph : graph) : graph =
    let graph = link node1 ~along:node2 graph in
    link node2 ~along:node1 graph

  let merge (lhs : graph) (rhs : graph) : graph =
    {
      nodes = NodeSet.union lhs.nodes rhs.nodes;
      edges = EdgeSet.union lhs.edges rhs.edges;
    }

  let bindings (graph : graph) =
    NodeSet.fold
      (fun node acc ->
        let egdes = egde_of node graph in
        (node, NodeSet.elements egdes) :: acc)
      graph.nodes []
end

module ColoredMake (S : OrderedType) (Color : ColoredType) = struct
  type colored_node = { node : S.t; color : Color.t option }
  type egde = { root : S.t; along : S.t }

  let compare_opt lhs rhs =
    match (lhs, rhs) with
    | Some lcolor, Some rcolor -> Color.compare lcolor rcolor
    | _ -> compare lhs rhs

  exception Unexisting_node of S.t
  exception Link_same_color of colored_node * colored_node

  module NodeSig = struct
    type t = colored_node

    let compare lhs rhs = S.compare lhs.node rhs.node
  end

  module NodeSet = Set.Make (NodeSig)
  module ColorSet = Set.Make (Color)

  module EdgeSet = Set.Make (struct
    type t = egde

    let compare lhs rhs =
      let f_comp = S.compare lhs.root rhs.root in
      if f_comp <> 0 then f_comp else S.compare lhs.along rhs.along
  end)

  module G = Make (S)

  type colored_graph = { nodes : NodeSet.t; edges : EdgeSet.t }

  let empty = { nodes = NodeSet.empty; edges = EdgeSet.empty }
  let create_colored color node = { node; color }
  let create_edge root along = { root = root.node; along = along.node }
  let add_node node graph = { graph with nodes = NodeSet.add node graph.nodes }
  let add_edge egde graph = { graph with edges = EdgeSet.add egde graph.edges }

  let replace_color_node node graph =
    {
      graph with
      nodes =
        NodeSet.map
          (fun cnode -> if NodeSig.compare cnode node = 0 then node else cnode)
          graph.nodes;
    }

  let union_edges edges graph =
    { graph with edges = EdgeSet.union edges graph.edges }

  let remove_node_color node cg = 
    {
      cg with nodes = NodeSet.map (fun cn -> if S.compare cn.node node = 0 then { cn with color = None} else cn ) cg.nodes
    }

  let add_uncolored_node ?color node graph =
    add_node (create_colored color node) graph

  let remove_color node = { node with color = None }

  let edges node cgraph =
    cgraph.edges |> EdgeSet.filter (fun edge -> S.compare edge.root node = 0)

  let check_contains node graph =
    if not @@ NodeSet.mem (create_colored None node) @@ graph.nodes then
      raise @@ Unexisting_node node

  let check_color root along =
    if compare_opt root.color along.color = 0 && Option.is_some root.color then
      raise @@ Link_same_color (root, along)

  let link root ~along graph =
    let () = check_contains root.node graph in
    let () = check_contains along.node graph in
    let () = check_color root along in
    let () = check_color along root in
    let edge = create_edge root along in
    add_edge edge graph

  let find node graph = NodeSet.find (create_colored None node) graph.nodes

  let egde_of : colored_node -> colored_graph -> NodeSet.t =
   fun node graph ->
    EdgeSet.fold
      (fun edge acc ->
        if S.compare edge.root node.node = 0 then
          NodeSet.add (find edge.along graph) acc
        else acc)
      graph.edges NodeSet.empty

  let remove node graph =
    let keep, remove =
      EdgeSet.partition
        (fun edge -> S.compare node.node edge.root <> 0)
        graph.edges
    in
    let nodes = NodeSet.remove node graph.nodes in
    ({ nodes; edges = keep }, remove)

  (**
      @raise Not_found if [node] is not in the graph    
    *)
  let node_degre node graph = EdgeSet.cardinal @@ edges node.node graph

  let surrounded_color node graph =
    let edges = graph |> edges node.node in
    EdgeSet.fold
      (fun egde acc ->
        let colored_node = find egde.along graph in
        match colored_node.color with
        | None -> acc
        | Some color -> ColorSet.add color acc)
      edges ColorSet.empty

  let of_graph ?(precolored = ([] : (S.t * Color.t) list)) (graph : G.graph)
      =
    let color_links ~precolored linked =
      linked
      |> List.map (fun node ->
             let color =
               precolored
               |> List.find_map (fun (elt, color) ->
                      if S.compare node elt = 0 then Some color else None)
             in
             create_colored color node)
    in
    graph |> G.bindings
    |> List.fold_left
         (fun graph_acc (node, linked) ->
           let colored_opt =
             precolored
             |> List.find_map (fun (elt, color) ->
                    if S.compare node elt = 0 then Some color else None)
           in
           let cnode = create_colored colored_opt node in
           let clinked = color_links ~precolored linked in
           let graph_acc = add_node cnode graph_acc in
           let graph_acc =
             clinked
             |> List.fold_left
                  (fun inner_graph cl ->
                    let new_graph =
                      inner_graph |> add_node cl |> link cnode ~along:cl
                    in
                    new_graph)
                  graph_acc
           in
           graph_acc)
         empty

  (**
      color [graph] with the [colorlist] without trying to color node [immuable]
  *)
  let color_graph ~immuable colorlist graph =
    let colorset = ColorSet.of_list colorlist in
    let ordered_nodes =
      graph.nodes |> NodeSet.elements
      |> List.sort (fun lhs rhs ->
             let lcardinal = node_degre lhs graph in
             let rcardinal = node_degre rhs graph in
             Int.compare rcardinal lcardinal)
    in

    ordered_nodes
    |> List.fold_left
         (fun acc_graph node ->
           let around_color_set = surrounded_color node acc_graph in
           let available_color_set = ColorSet.diff colorset around_color_set in
           match ColorSet.min_elt_opt available_color_set with
           | None ->
               let node = remove_color node in
               replace_color_node node acc_graph
           | Some color -> (
               match node.color with
               | None -> 
                if List.exists (fun reg -> S.compare reg node.node = 0) immuable
                  then acc_graph 
              else
                   let node = { node with color = Some color } in
                   replace_color_node node acc_graph
               | Some c ->
                   if ColorSet.mem c available_color_set then acc_graph
                   else
                     (* let node = remove_color node in replace_color_node node  *)
                     acc_graph))
         graph

  let bindings (graph : colored_graph) =
    NodeSet.fold
      (fun node acc ->
        let egdes = egde_of node graph in
        (node, NodeSet.elements egdes) :: acc)
      graph.nodes []
end

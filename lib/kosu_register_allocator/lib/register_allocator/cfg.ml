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

module type CfgS = sig
  type variable
  type t = variable
  type rvalue
  type atom

  val repr : variable -> string
  val compare : variable -> variable -> int
  val lvalue_variable : string -> rvalue -> variable
  val lvalue_deref_variable : string -> rvalue -> variable
  val ttrv_identifiers_used : rvalue -> variable list
  val tte_idenfier_used : atom -> variable list
  val is_affectation : rvalue -> bool
  val variables_as_parameter : rvalue -> (variable * int) list option
end

module type CfgPprintSig = sig
  type variable
  type rvalue
  type atom

  val string_of_variable : variable -> string
  val string_of_rvalue : rvalue -> string
  val string_of_atom : atom -> string
end

module type ABI = sig
  type t
  type variable

  type return_strategy =
    | Indirect_return
    | Simple_return of t
    | Splitted_return of t * t

  val compare : t -> t -> int
  val callee_saved_register : t list
  val caller_saved_register : t list
  val syscall_register : t list
  val arguments_register : variable -> t list
  val non_float_argument_registers : t list
  val is_valid_register : variable -> t -> bool
  val does_return_hold_in_register : variable -> bool
  val indirect_return_register : t
  val return_strategy : variable -> return_strategy
end

module type ColoredType = Graph.ColoredType

module type VariableSig = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type variable
  type rvalue
  type atom
  type constraints

  module VariableSig : sig
    include VariableSig with type t = variable
  end

  module StringSet : sig
    include module type of Set.Make (String)
  end

  module TypedIdentifierSet : sig
    include module type of Set.Make (VariableSig)
  end

  module BasicBlockMap : sig
    include Map.S with type key = String.t
  end

  type cfg_statement =
    | CFG_STacDeclaration of { identifier : string; trvalue : rvalue }
    | CFG_STacModification of { identifier : string; trvalue : rvalue }
    | CFG_STDerefAffectation of { identifier : string; trvalue : rvalue }

  type bbe_if = { condition : atom; if_label : string; else_label : string }
  type basic_block_end = BBe_if of bbe_if | Bbe_return of atom

  module Basic : sig
    type ('a, 'b) basic_block = {
      label : string;
      cfg_statements : 'a list;
      followed_by : StringSet.t;
      ending : 'b;
    }

    type cfg = {
      entry_block : string;
      blocks :
        (cfg_statement, basic_block_end option) basic_block BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    val create_basic_block :
      label:string ->
      cfg_statements:'a list ->
      followed_by:string list ->
      ending:'b ->
      ('a, 'b) basic_block

    val create_cfg :
      entry_block:string ->
      parameters:variable list ->
      locals_vars:variable list ->
      (cfg_statement, basic_block_end option) basic_block list ->
      cfg
  end

  module Detail : sig
    type ('a, 'b) basic_block_detail = {
      basic_block : ('a, 'b) Basic.basic_block;
      in_vars : TypedIdentifierSet.t;
      out_vars : TypedIdentifierSet.t;
    }

    type cfg_detail = {
      entry_block : string;
      blocks_details :
        (cfg_statement, basic_block_end option) basic_block_detail
        BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    val of_cfg : Basic.cfg -> cfg_detail
  end

  module Liveness : sig
    module LivenessInfo : sig
      include LivenessInfo.LivenessInfoS with type elt = variable
    end

    type cfg_liveness_statement = {
      cfg_statement : cfg_statement;
      liveness_info : LivenessInfo.liveness_info;
    }

    type liveness_ending = basic_block_end option * LivenessInfo.liveness_info

    type cfg_liveness_detail = {
      entry_block : string;
      blocks_liveness_details :
        (cfg_liveness_statement, liveness_ending) Detail.basic_block_detail
        BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    val of_cfg_details :
      delete_useless_stmt:bool -> Detail.cfg_detail -> cfg_liveness_detail
  end

  module Interference_Graph : sig
    module IG : sig
      include module type of Graph.Make (VariableSig)
    end

    val interfere : Liveness.cfg_liveness_detail -> IG.graph
  end

  module GreedyColoring (ABI : ABI with type variable = VariableSig.t) : sig
    module ColoredGraph : sig
      include module type of Graph.ColoredMake (VariableSig) (ABI)
    end

    val coloration :
      parameters:(variable * ABI.t) list ->
      available_color:ABI.t list ->
      Liveness.cfg_liveness_detail ->
      ColoredGraph.colored_graph
  end
end

module type SP = sig
  include S

  module Pprint : sig
    val string_of_typed_indentifier_set : TypedIdentifierSet.t -> string
    val string_of_cfg_statement : cfg_statement -> string

    val string_of_cfg_liveness_statement :
      Liveness.cfg_liveness_statement -> string

    val string_of_basic_block_end : basic_block_end -> string
  end
end

module Make (CfgS : CfgS) :
  S
    with type variable = CfgS.variable
     and type atom = CfgS.atom
     and type rvalue = CfgS.rvalue = struct
  type variable = CfgS.variable
  type atom = CfgS.atom
  type rvalue = CfgS.rvalue

  module VariableSig = struct
    type t = CfgS.variable

    let compare = CfgS.compare
  end

  module StringSet = Set.Make (String)
  module TypedIdentifierSet = Set.Make (VariableSig)
  module BasicBlockMap = Map.Make (String)
  module Constraint = Constraint.Make (VariableSig)

  type cfg_statement =
    | CFG_STacDeclaration of { identifier : string; trvalue : CfgS.rvalue }
    | CFG_STacModification of { identifier : string; trvalue : CfgS.rvalue }
    | CFG_STDerefAffectation of { identifier : string; trvalue : CfgS.rvalue }

  type bbe_if = {
    condition : CfgS.atom;
    if_label : string;
    else_label : string;
  }

  type basic_block_end = BBe_if of bbe_if | Bbe_return of CfgS.atom

  type constraints = {
    parameters_constr : (variable * int) list list;
    return_constraint : variable list;
  }

  module Basic = struct
    type ('a, 'b) basic_block = {
      label : string;
      cfg_statements : 'a list;
      followed_by : StringSet.t;
      ending : 'b;
    }

    let create_basic_block ~label ~cfg_statements ~followed_by ~ending =
      {
        label;
        cfg_statements;
        followed_by = StringSet.of_list followed_by;
        ending;
      }

    type cfg = {
      entry_block : string;
      blocks :
        (cfg_statement, basic_block_end option) basic_block BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    let create_cfg ~entry_block ~parameters ~locals_vars blocks =
      {
        entry_block;
        blocks =
          blocks
          |> List.map (fun block -> (block.label, block))
          |> List.to_seq |> BasicBlockMap.of_seq;
        parameters;
        locals_vars = TypedIdentifierSet.of_list locals_vars;
      }

    let fetch_basic_block_from_label label_name bbset =
      (* let () = print_endline label_name in *)
      bbset |> BasicBlockMap.find label_name

    let identifier_of_stmt = function
      | CFG_STDerefAffectation { identifier; _ }
      | CFG_STacDeclaration { identifier; _ }
      | CFG_STacModification { identifier; _ } ->
          identifier

    let trvalue_of_stmt = function
      | CFG_STDerefAffectation { trvalue; _ }
      | CFG_STacDeclaration { trvalue; _ }
      | CFG_STacModification { trvalue; _ } ->
          trvalue

    let stmt_identifiers_used = function
      | CFG_STDerefAffectation { trvalue; _ }
      | CFG_STacDeclaration { trvalue; _ }
      | CFG_STacModification { trvalue; _ } ->
          CfgS.ttrv_identifiers_used trvalue

    let basic_block_input_var ~out_vars basic_block =
      let open CfgS in
      let rec basic_block_cfg_statement_list ~killed ~generated = function
        | [] ->
            basic_block.ending
            |> Option.map
                 (fun (Bbe_return tte | BBe_if { condition = tte; _ }) ->
                   let right_value_variables_used_set =
                     tte |> tte_idenfier_used |> TypedIdentifierSet.of_list
                   in
                   let remove_block_create_variable_set =
                     TypedIdentifierSet.diff right_value_variables_used_set
                       killed
                   in
                   TypedIdentifierSet.union generated
                     remove_block_create_variable_set
               )
            |> Option.value ~default:generated
            |> TypedIdentifierSet.union (TypedIdentifierSet.diff out_vars killed)
        | stmt :: q -> (
            match stmt with
            | CFG_STacDeclaration { identifier; trvalue } ->
                let extented_killed_vars, new_geneated =
                  let right_value_set =
                    trvalue |> ttrv_identifiers_used
                    |> TypedIdentifierSet.of_list
                  in
                  let remove_block_create_variable_set =
                    TypedIdentifierSet.diff right_value_set killed
                  in
                  let extented_killed_vars =
                    TypedIdentifierSet.add
                      (lvalue_variable identifier trvalue)
                      killed
                  in
                  let new_geneated =
                    TypedIdentifierSet.union remove_block_create_variable_set
                      generated
                  in
                  (extented_killed_vars, new_geneated)
                in
                basic_block_cfg_statement_list ~killed:extented_killed_vars
                  ~generated:new_geneated q
            | CFG_STacModification { identifier = _; trvalue } ->
                let right_value_set =
                  trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list
                in
                let remove_block_create_variable_set =
                  TypedIdentifierSet.diff right_value_set killed
                in
                let new_geneated =
                  TypedIdentifierSet.union remove_block_create_variable_set
                    generated
                in
                basic_block_cfg_statement_list ~killed ~generated:new_geneated q
            | CFG_STDerefAffectation { trvalue; identifier } ->
                let right_value_set =
                  trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list
                in
                let remove_block_create_variable_set =
                  TypedIdentifierSet.diff right_value_set killed
                in
                let new_geneated =
                  TypedIdentifierSet.union remove_block_create_variable_set
                    generated
                in
                let extented_generated =
                  TypedIdentifierSet.add
                    (lvalue_deref_variable identifier trvalue)
                    new_geneated
                in
                basic_block_cfg_statement_list ~killed
                  ~generated:extented_generated q
          )
      in
      basic_block_cfg_statement_list ~killed:TypedIdentifierSet.empty
        ~generated:TypedIdentifierSet.empty basic_block.cfg_statements

    let rec basic_block_output_var_aux ~visited basic_block_set basic_block =
      match basic_block.ending with
      | Some (Bbe_return _) ->
          TypedIdentifierSet.empty
      | _ ->
          StringSet.fold
            (fun elt acc ->
              if Hashtbl.mem visited elt then
                acc
              else
                let () = Hashtbl.add visited elt () in
                let follow_block =
                  fetch_basic_block_from_label elt basic_block_set
                in
                let out_vars =
                  basic_block_output_var_aux ~visited basic_block_set
                    follow_block
                in
                let follow_basic_block_input =
                  basic_block_input_var ~out_vars follow_block
                in
                TypedIdentifierSet.union acc follow_basic_block_input
            )
            basic_block.followed_by TypedIdentifierSet.empty

    let basic_block_output_var basic_block_set basic_block =
      basic_block_output_var_aux ~visited:(Hashtbl.create 11) basic_block_set
        basic_block
  end

  module Detail = struct
    type ('a, 'b) basic_block_detail = {
      basic_block : ('a, 'b) Basic.basic_block;
      in_vars : TypedIdentifierSet.t;
      out_vars : TypedIdentifierSet.t;
    }

    type cfg_detail = {
      entry_block : string;
      blocks_details :
        (cfg_statement, basic_block_end option) basic_block_detail
        BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    let basic_block_detail_of_basic_block set bb =
      let out_vars = Basic.basic_block_output_var set bb in
      let in_vars = Basic.basic_block_input_var ~out_vars bb in

      { basic_block = bb; in_vars; out_vars }

    let of_cfg (cfg : Basic.cfg) =
      {
        entry_block = cfg.entry_block;
        blocks_details =
          cfg.blocks |> BasicBlockMap.bindings
          |> List.map (fun (label, block) ->
                 (label, basic_block_detail_of_basic_block cfg.blocks block)
             )
          |> List.to_seq |> BasicBlockMap.of_seq;
        parameters = cfg.parameters;
        locals_vars = cfg.locals_vars;
      }
  end

  module Liveness = struct
    module LivenessInfo = LivenessInfo.Make (VariableSig)

    type cfg_liveness_statement = {
      cfg_statement : cfg_statement;
      liveness_info : LivenessInfo.liveness_info;
    }

    type liveness_ending = basic_block_end option * LivenessInfo.liveness_info

    type cfg_liveness_detail = {
      entry_block : string;
      blocks_liveness_details :
        (cfg_liveness_statement, liveness_ending) Detail.basic_block_detail
        BasicBlockMap.t;
      parameters : variable list;
      locals_vars : TypedIdentifierSet.t;
    }

    let is_function_call stmt =
      Option.is_some @@ CfgS.variables_as_parameter
      @@ Basic.trvalue_of_stmt stmt

    let rec does_live_after_function_call variable (liveness_stmts, ending) =
      match liveness_stmts with
      | [] | _ :: [] ->
          false
      | [ t1; t2 ] ->
          LivenessInfo.is_alive variable t1.liveness_info
          && is_function_call t2.cfg_statement
          && ending |> snd |> LivenessInfo.is_alive variable
      | t1 :: (t2 :: t3 :: _ as next) ->
          let pre_condition =
            LivenessInfo.is_alive variable t1.liveness_info
            && is_function_call t2.cfg_statement
          in
          let is_alive_after =
            LivenessInfo.is_alive variable t3.liveness_info
          in
          if not pre_condition then
            does_live_after_function_call variable (next, ending)
          else if pre_condition && is_alive_after then
            true
          else if pre_condition && not is_alive_after then
            false
          else
            failwith "Unreachable"

    let does_live_after_function_call_cfg variable cfg =
      let open Basic in
      let open Detail in
      BasicBlockMap.fold
        (fun _ block acc_live_after ->
          if acc_live_after then
            acc_live_after
          else
            let ending = block.basic_block.ending in
            let stmts = block.basic_block.cfg_statements in
            let does_survive =
              does_live_after_function_call variable (stmts, ending)
            in
            does_survive
        )
        cfg.blocks_liveness_details false

    type liveness_var_block = Die_at of int | Die_in_ending

    (**
      @returns : whenever the variable leaves the block
     *)
    let does_outlives_block (elt : TypedIdentifierSet.elt)
        (bbd : (cfg_statement, basic_block_end option) Detail.basic_block_detail)
        =
      TypedIdentifierSet.mem elt bbd.out_vars

    let dying_in_vars_in_block
        (bbd : (cfg_statement, basic_block_end option) Detail.basic_block_detail)
        =
      TypedIdentifierSet.diff bbd.in_vars bbd.out_vars

    let when_variable_dies ~start_from (elt : variable)
        (bbd : (cfg_statement, basic_block_end option) Detail.basic_block_detail)
        =
      let open CfgS in
      let ( >>= ) = Option.bind in
      let liveness =
        bbd.basic_block.cfg_statements
        |> List.fold_left
             (fun (index, acc) stmt ->
               let used_vars = Basic.stmt_identifiers_used stmt in
               if
                 used_vars
                 |> List.exists (fun var -> elt |> CfgS.compare var |> ( = ) 0)
               then
                 (index + 1, Die_at (max start_from index))
               else
                 (index + 1, acc)
             )
             (0, Die_at start_from)
        |> snd
      in
      bbd.basic_block.ending
      >>= (fun (Bbe_return tte | BBe_if { condition = tte; _ }) ->
            let used_vars = tte_idenfier_used tte in
            if
              used_vars
              |> List.exists (fun var -> elt |> CfgS.compare var |> ( = ) 0)
            then
              Some Die_in_ending
            else
              None
          )
      |> Option.value ~default:liveness

    (**
        @returns in how many statements the variable dies 
        @raise Invalid_argument : if the variable outlive the block
      *)
    let when_variable_dies_unsafe ~start_from elt bdd =
      match when_variable_dies ~start_from elt bdd with
      | Die_at n ->
          Some n
      | Die_in_ending ->
          None

    (**
    Remove the from the hashtable the variable that die at the times of [current]
    @return : List of dying variable at the times [current]    
    *)
    let fetch_dying_variable ~current map () : variable list =
      let removed =
        map |> Hashtbl.to_seq
        |> Seq.filter_map (fun ((time, elt), _) ->
               if time = Some current then
                 Some elt
               else
                 None
           )
        |> List.of_seq
      in
      (* let removed =  Hashtbl.find_all map (Some current) in *)
      let () =
        map
        |> Hashtbl.filter_map_inplace (fun (key, _) value ->
               if key = Some current then
                 None
               else
                 Some value
           )
      in
      removed

    let merge_basic_block_map lmap rmap =
      BasicBlockMap.union
        (fun key m1 m2 ->
          let () = Printf.eprintf "Conficiting label = %s\n" key in
          if m1 <> m2 then
            failwith "Diff for key"
          else
            Some m1
        )
        lmap rmap

    let dated_basic_block_of_basic_block_detail ~delete_useless_stmt
        dated_info_list
        (bbd : (cfg_statement, basic_block_end option) Detail.basic_block_detail)
        =
      let open Detail in
      let open Basic in
      let open CfgS in
      let when_to_die_hashmap =
        bbd |> dying_in_vars_in_block |> TypedIdentifierSet.elements
        |> List.map (fun elt ->
               let when_it, elt =
                 (when_variable_dies_unsafe ~start_from:0 elt bbd, elt)
               in
               ((when_it, elt), elt)
           )
        |> List.to_seq |> Hashtbl.of_seq
      in

      let dated_info_list =
        LivenessInfo.init
          (fun variable ->
            TypedIdentifierSet.mem variable bbd.in_vars
            && LivenessInfo.is_alive variable dated_info_list
          )
          (LivenessInfo.elements dated_info_list)
      in

      (* Be careful new statement are in the reversed order *)
      let stmts, lastest_live_info =
        bbd.basic_block.cfg_statements
        |> List.fold_left
             (fun ( block_line_index,
                    cfg_liveness_statements,
                    last_dated_info_list
                  ) stmt ->
               let dated_info_list = last_dated_info_list in
               let listof_now_dying_var =
                 fetch_dying_variable ~current:block_line_index
                   when_to_die_hashmap ()
               in
               let date_info_updated_dying =
                 LivenessInfo.set_dead_of_list listof_now_dying_var
                   dated_info_list
               in
               let next_line = block_line_index + 1 in
               match stmt with
               | CFG_STacDeclaration { identifier; trvalue }
               | CFG_STacModification { identifier; trvalue } -> (
                   let variable = lvalue_variable identifier trvalue in

                   (* let () = Printf.printf "line : %u, dying = [%s]\n%!" block_line_index (listof_now_dying_var |> List.map CfgS.repr |> String.concat ", ") in *)
                   match does_outlives_block variable bbd with
                   | true ->
                       let updated_alive_info =
                         if CfgS.is_affectation trvalue then
                           LivenessInfo.set_alive variable
                             date_info_updated_dying
                         else
                           date_info_updated_dying
                       in
                       ( next_line,
                         {
                           cfg_statement = stmt;
                           liveness_info = dated_info_list;
                         }
                         :: cfg_liveness_statements,
                         updated_alive_info
                       )
                   | false -> (
                       match CfgS.is_affectation trvalue with
                       | false ->
                           ( next_line,
                             {
                               cfg_statement = stmt;
                               liveness_info = date_info_updated_dying;
                             }
                             :: cfg_liveness_statements,
                             date_info_updated_dying
                           )
                       | true ->
                           let in_how_many_times_it_dies =
                             when_variable_dies_unsafe
                               ~start_from:block_line_index variable bbd
                           in
                           let does_it_dies_now =
                             in_how_many_times_it_dies
                             |> Option.map (( = ) block_line_index)
                             |> Option.value ~default:false
                           in
                           if does_it_dies_now then
                             let updated_dead_liveinfo =
                               LivenessInfo.set_dead variable
                                 date_info_updated_dying
                             in
                             ( next_line,
                               ( if delete_useless_stmt then
                                   cfg_liveness_statements
                                 else
                                   {
                                     cfg_statement = stmt;
                                     liveness_info = dated_info_list;
                                   }
                                   :: cfg_liveness_statements
                               ),
                               updated_dead_liveinfo
                             )
                           else
                             let () =
                               Hashtbl.replace when_to_die_hashmap
                                 (in_how_many_times_it_dies, variable)
                                 variable
                             in
                             let updated_alive_info =
                               LivenessInfo.set_alive variable
                                 date_info_updated_dying
                             in
                             ( next_line,
                               {
                                 cfg_statement = stmt;
                                 liveness_info = dated_info_list;
                               }
                               :: cfg_liveness_statements,
                               updated_alive_info
                             )
                     )
                 )
               | CFG_STDerefAffectation { identifier; trvalue } ->
                   let variable = lvalue_deref_variable identifier trvalue in
                   let updated_alive_info =
                     LivenessInfo.set_alive variable date_info_updated_dying
                   in
                   ( next_line,
                     { cfg_statement = stmt; liveness_info = dated_info_list }
                     :: cfg_liveness_statements,
                     updated_alive_info
                   )
             )
             (0, [], dated_info_list)
        |> fun (_, list, latest_liveness_info) ->
        match list with
        | [] ->
            ([], dated_info_list)
        | _ :: _ as l ->
            (List.rev l, latest_liveness_info)
      in
      (stmts, lastest_live_info)

    (*
      Relie on the fac that a variable create in a branch block cannot be used in a following block
      It's mostlt true for staticlly typed language but false for dynamic not like Python   
    *)
    let rec basic_block_liveness_of_convert ~delete_useless_stmt ~visited
        ~dated_info basic_blocks_map
        (bbd : (cfg_statement, basic_block_end option) Detail.basic_block_detail)
        =
      let open Detail in
      if not @@ Hashtbl.mem visited bbd.basic_block.label then
        let self_to_cfg_liveness, last_dated_info =
          dated_basic_block_of_basic_block_detail ~delete_useless_stmt
            dated_info bbd
        in
        let basic_block_liveness =
          {
            in_vars = bbd.in_vars;
            out_vars = bbd.out_vars;
            basic_block =
              {
                label = bbd.basic_block.label;
                followed_by = bbd.basic_block.followed_by;
                ending = (bbd.basic_block.ending, last_dated_info);
                cfg_statements = self_to_cfg_liveness;
              };
          }
        in
        let () = Hashtbl.replace visited bbd.basic_block.label () in
        let singleton =
          BasicBlockMap.singleton basic_block_liveness.basic_block.label
            basic_block_liveness
        in
        let converted_blocks_map =
          StringSet.fold
            (fun follow acc ->
              let follow_block =
                Basic.fetch_basic_block_from_label follow basic_blocks_map
              in
              let follow_transformed_block =
                basic_block_liveness_of_convert ~delete_useless_stmt ~visited
                  ~dated_info:last_dated_info basic_blocks_map follow_block
              in
              merge_basic_block_map acc follow_transformed_block
            )
            bbd.basic_block.followed_by singleton
        in
        converted_blocks_map
      else
        BasicBlockMap.empty

    let of_cfg_details ~delete_useless_stmt (cfg : Detail.cfg_detail) =
      let parameters_set = TypedIdentifierSet.of_list cfg.parameters in
      let visited = Hashtbl.create 5 in
      let entry_block =
        Basic.fetch_basic_block_from_label cfg.entry_block cfg.blocks_details
      in
      let liveness_info =
        TypedIdentifierSet.fold
          (fun typed_var acc ->
            let is_parameters =
              TypedIdentifierSet.mem typed_var parameters_set
            in
            (typed_var, is_parameters) :: acc
          )
          (TypedIdentifierSet.union cfg.locals_vars parameters_set)
          []
        |> LivenessInfo.of_list
      in
      let open Detail in
      {
        entry_block = cfg.entry_block;
        parameters = cfg.parameters;
        locals_vars = cfg.locals_vars;
        blocks_liveness_details =
          basic_block_liveness_of_convert ~delete_useless_stmt ~visited
            ~dated_info:liveness_info cfg.blocks_details entry_block;
      }
  end

  module Interference_Graph = struct
    module IG = Graph.Make (VariableSig)

    let infer_acc liveness_info graph =
      let alive_elt = Liveness.LivenessInfo.alive_elements liveness_info in
      let combined_alives =
        List.flatten
        @@ Util.combinaison
             (fun lhs rhs ->
               if VariableSig.compare lhs rhs = 0 then
                 None
               else
                 Some (lhs, rhs)
             )
             alive_elt alive_elt
      in
      combined_alives
      |> List.fold_left
           (fun acc_graph (link, along) -> IG.link link ~along acc_graph)
           graph

    let constraints (cfg : Liveness.cfg_liveness_detail) =
      let open Basic in
      let open Detail in
      let open Liveness in
      let ( >== ) = Option.bind in
      let constraints = Constraint.empty in
      BasicBlockMap.fold
        (fun _ block acc_constr ->
          let acc_constr =
            block.basic_block.cfg_statements
            |> List.fold_left
                 (fun inner_acc_str stmt ->
                   match
                     stmt.cfg_statement |> Basic.trvalue_of_stmt
                     |> CfgS.variables_as_parameter
                   with
                   | None ->
                       inner_acc_str
                   | Some parameters ->
                       Constraint.add_function_constraint parameters
                         inner_acc_str
                 )
                 acc_constr
          in
          let return_constraint =
            block.basic_block.ending |> fst
            >== (function
                  | BBe_if _ ->
                      None
                  | Bbe_return tte ->
                      let rc = tte |> CfgS.tte_idenfier_used in
                      Constraint.empty
                      |> Constraint.add_returns_constraints rc
                      |> Option.some
                  )
            |> Option.value ~default:Constraint.empty
          in

          Constraint.union return_constraint acc_constr
        )
        cfg.blocks_liveness_details constraints

    let interfere (cfg : Liveness.cfg_liveness_detail) =
      let open Basic in
      let open Detail in
      let open Liveness in
      let parameters_set = TypedIdentifierSet.of_list cfg.parameters in
      let all_variables_seq =
        TypedIdentifierSet.to_seq
        @@ TypedIdentifierSet.union cfg.locals_vars parameters_set
      in
      let graph = IG.of_seq all_variables_seq in
      BasicBlockMap.fold
        (fun _ block graph_acc ->
          let new_graph =
            block.basic_block.cfg_statements
            |> List.fold_left
                 (fun inner_graph_acc stmt ->
                   infer_acc stmt.liveness_info inner_graph_acc
                 )
                 graph_acc
          in
          let ending, liveness = block.basic_block.ending in
          match ending with
          | None ->
              new_graph
          | Some (BBe_if { condition = _; _ } | Bbe_return _) ->
              infer_acc liveness new_graph
        )
        cfg.blocks_liveness_details graph
  end

  module GreedyColoring (ABI : ABI with type variable = CfgS.variable) = struct
    module ColoredGraph = Graph.ColoredMake (VariableSig) (ABI)

    module VariableReturnStrategySig = struct
      type t = variable * ABI.return_strategy

      let compare lhs rhs =
        let i_compare = CfgS.compare (fst lhs) (fst rhs) in
        if i_compare = 0 then
          compare (snd lhs) (snd rhs)
        else
          i_compare
    end

    module VariableReturnStrategySet = Set.Make (VariableReturnStrategySig)
    module VariableReturnStrategyMap = Map.Make (VariableSig)

    let variable_return_set_aux map stmt =
      let rvalue = Basic.trvalue_of_stmt stmt in
      match CfgS.variables_as_parameter rvalue with
      | None ->
          map
      | Some _ -> (
          let lvalue_var =
            CfgS.lvalue_variable (Basic.identifier_of_stmt stmt) rvalue
          in
          let ret_strat = ABI.return_strategy lvalue_var in
          let singleton =
            VariableReturnStrategySet.singleton (lvalue_var, ret_strat)
          in
          match VariableReturnStrategyMap.find_opt lvalue_var map with
          | None ->
              VariableReturnStrategyMap.add lvalue_var singleton map
          | Some set ->
              let extended_set =
                VariableReturnStrategySet.union singleton set
              in
              VariableReturnStrategyMap.add lvalue_var extended_set map
        )

    let variable_return_set (cfg : Liveness.cfg_liveness_detail) :
        VariableReturnStrategySet.t VariableReturnStrategyMap.t =
      let open Basic in
      let open Detail in
      let open Liveness in
      BasicBlockMap.fold
        (fun _ block map ->
          block.basic_block.cfg_statements
          |> List.fold_left
               (fun acc_map stmt ->
                 variable_return_set_aux acc_map stmt.cfg_statement
               )
               map
        )
        cfg.blocks_liveness_details VariableReturnStrategyMap.empty

    let return_color_variable variable =
      match ABI.return_strategy variable with
      | ABI.Simple_return t ->
          Some (variable, t)
      | _ ->
          None

    module VariableAbiMap = Map.Make (struct
      type t = TypedIdentifierSet.elt

      let compare = CfgS.compare
    end)

    let try_color base_graph variable color map =
      let open Interference_Graph in
      let nodes = IG.egde_of variable base_graph in
      let conflicts =
        TypedIdentifierSet.filter
          (fun iv ->
            match VariableAbiMap.find_opt iv map with
            | None ->
                false
            | Some icolor ->
                ABI.compare icolor color = 0 && CfgS.compare iv variable <> 0
          )
          nodes
      in

      let are_all_parameters =
        conflicts
        |> TypedIdentifierSet.for_all (fun iv -> VariableAbiMap.mem iv map)
      in

      (* let () = Printf.printf "has conflict = %b\n\n%!" are_all_parameters in *)
      match are_all_parameters with
      | false ->
          map
      | true ->
          VariableAbiMap.add variable color
          @@ TypedIdentifierSet.fold
               (fun iv inner_acc_map ->
                 (* let () = Printf.printf "decolor : variable = %s\n%!" (CfgS.repr iv) in *)
                 VariableAbiMap.remove iv inner_acc_map
               )
               nodes map

    let _does_live_in_same_moment v1 v2 infer_graph =
      let open Interference_Graph in
      let edges = IG.egde_of v1 infer_graph in
      TypedIdentifierSet.mem v2 edges

    let base_coloration ~(parameters : (TypedIdentifierSet.elt * ABI.t) list)
        ~available_color (cfg : Liveness.cfg_liveness_detail) =
      let open Interference_Graph in
      let base_graph = interfere cfg in
      let constraints = constraints cfg in

      let map = parameters |> List.to_seq |> VariableAbiMap.of_seq in

      let parameters_functions =
        constraints.inner_call_parameters |> Constraint.ParameterSetSet.elements
        |> List.map Constraint.ParameterSet.elements
        |> List.flatten
        |> List.fold_left
             (fun acc_map (variable, index) ->
               match VariableAbiMap.find_opt variable acc_map with
               (* check if the parameter function variable has already been colored *)
               | None -> (
                   match
                     List.nth_opt (ABI.arguments_register variable) index
                   with
                   | None ->
                       acc_map
                   | Some color ->
                       try_color base_graph variable color acc_map
                 )
               | Some color ->
                   let index_color =
                     variable |> ABI.arguments_register |> List.mapi Util.couple
                     |> List.find_map (fun (index, reg) ->
                            if ABI.compare reg color = 0 then
                              Some index
                            else
                              None
                        )
                     |> Option.get
                   in
                   if index = index_color then
                     acc_map
                   else
                     (* let () = Printf.printf "decolor : variable = %s: reg = r%d\n%!" (CfgS.repr variable) index in *)
                     VariableAbiMap.remove variable acc_map
             )
             map
      in

      let return_functions =
        constraints.return |> TypedIdentifierSet.elements
        |> List.fold_left
             (fun acc_map variable ->
               match ABI.return_strategy variable with
               | Indirect_return | Splitted_return _ ->
                   acc_map
               | Simple_return reg ->
                   try_color base_graph variable reg acc_map
             )
             parameters_functions
      in

      let precolored = VariableAbiMap.bindings return_functions in

      let colored_graph = ColoredGraph.of_graph ~precolored base_graph in
      ColoredGraph.color_graph ~immuable:(List.map fst parameters)
        available_color colored_graph

    let decolaration ~(parameters : (TypedIdentifierSet.elt * ABI.t) list)
        (cfg : Liveness.cfg_liveness_detail) (cg : ColoredGraph.colored_graph) =
      let constraints = Interference_Graph.constraints cfg in
      let return_strategies = variable_return_set cfg in

      (* let parameters_functions = constraints.inner_call_parameters |> Constraint.ParameterSetSet.elements |> List.map Constraint.ParameterSet.elements |> List.flatten  in

         let cg = parameters_functions |> List.fold_left (fun acc_cg (variable, index) ->
           ColoredGraph.remove_node_color variable cg
         ) cg
         in *)
      let cg =
        TypedIdentifierSet.(
          parameters |> List.map fst |> of_list |> union cfg.locals_vars
          |> elements
        )
        |> List.fold_left
             (fun acc_cg variable ->
               match
                 Liveness.does_live_after_function_call_cfg variable cfg
               with
               | true ->
                   ColoredGraph.remove_node_color variable acc_cg
               | false ->
                   acc_cg
             )
             cg
      in

      let extented =
        constraints.return |> TypedIdentifierSet.elements
        |> List.filter_map return_color_variable
      in
      let cg =
        parameters
        |> List.fold_left
             (fun acc_cg (elt, reg) ->
               match
                 extented
                 |> List.find_opt (fun (var, _) -> CfgS.compare elt var = 0)
               with
               | None ->
                   acc_cg
               | Some (variable, color) -> (
                   match ABI.compare color reg = 0 with
                   | true ->
                       acc_cg
                   | false ->
                       ColoredGraph.remove_node_color variable acc_cg
                 )
             )
             cg
      in
      let cg =
        VariableReturnStrategyMap.fold
          (fun variable strategie_set acc_cg ->
            let colored_node = ColoredGraph.find variable acc_cg in
            let final_color =
              colored_node.color
              |> Option.map (fun _ ->
                     VariableReturnStrategySet.fold
                       (fun strategy acc ->
                         match strategy with
                         | _, ABI.Indirect_return ->
                             None
                         | _ ->
                             acc
                       )
                       strategie_set colored_node.color
                 )
              |> Option.value ~default:None
            in
            match final_color with
            | Some _ ->
                acc_cg
            | None ->
                ColoredGraph.remove_node_color variable acc_cg
          )
          return_strategies cg
      in
      cg

    let coloration ~(parameters : (TypedIdentifierSet.elt * ABI.t) list)
        ~available_color (cfg : Liveness.cfg_liveness_detail) =
      let cg =
        base_coloration ~parameters ~available_color
          ~select:ABI.is_valid_register cfg
      in
      let cg = decolaration ~parameters cfg cg in
      cg
  end
end

module MakePprint
    (CfgS : CfgS)
    (Pp : CfgPprintSig
            with type variable = CfgS.variable
             and type atom = CfgS.atom
             and type rvalue = CfgS.rvalue) :
  SP
    with type variable = CfgS.variable
     and type atom = CfgS.atom
     and type rvalue = CfgS.rvalue = struct
  module Cfg = Make (CfgS)
  include Cfg

  module Pprint = struct
    open Printf
    open Pp

    let string_of_typed_indentifier_set set =
      set |> TypedIdentifierSet.elements |> List.map CfgS.repr
      |> String.concat ", "

    let string_of_cfg_statement = function
      | CFG_STacDeclaration { identifier; trvalue } ->
          sprintf "%s = %s" identifier (string_of_rvalue trvalue)
      | CFG_STDerefAffectation { identifier; trvalue } ->
          sprintf "*%s <- %s" identifier (string_of_rvalue trvalue)
      | CFG_STacModification { identifier; trvalue } ->
          sprintf "%s <- %s" identifier (string_of_rvalue trvalue)

    let string_of_cfg_liveness_statement
        (cfgl_statement : Liveness.cfg_liveness_statement) =
      Printf.sprintf "%s [%s]"
        (string_of_cfg_statement cfgl_statement.cfg_statement)
        (cfgl_statement.liveness_info |> Liveness.LivenessInfo.to_list
        |> List.map (fun (typed_id, bool) ->
               Printf.sprintf "<%s => %s>" (CfgS.repr typed_id)
                 ( if bool then
                     "alive"
                   else
                     "dead"
                 )
           )
        |> String.concat ", "
        )

    let string_of_basic_block_end = function
      | Bbe_return tte ->
          Printf.sprintf "return %s" (string_of_atom tte)
      | BBe_if { condition; if_label; else_label } ->
          Printf.sprintf "if %s goto %s\n\tgoto %s" (string_of_atom condition)
            if_label else_label
  end
end

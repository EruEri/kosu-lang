open Asttaccfg
open KosuIrTyped.Asttyped
open KosuIrTAC.Asttac

let compare_type: rktype -> rktype -> int = compare
let declaration_typed: string -> tac_typed_rvalue -> rktype = fun _ -> fun ttrv -> ttrv.rval_rktype

let derefed_typed: string -> tac_typed_rvalue -> rktype = fun _ -> fun ttrv -> RTPointer ttrv.rval_rktype

let tte_idenfier_used: tac_typed_expression -> (string * rktype) list = function
| {tac_expression = TEIdentifier id; expr_rktype} -> (id, expr_rktype)::[]
| _ -> []

let ttrv_identifiers_used: tac_typed_rvalue -> (string * rktype) list = fun ttrv -> match ttrv.rvalue with
| RVExpression tte 
| RVFieldAcess {first_expr = tte; _} 
| RVCustomUnop {expr = tte; _} | RVBuiltinUnop {expr = tte; _}
-> tte |> tte_idenfier_used
| RVFunction {tac_parameters = ttes; _} 
| RVTuple ttes
| RVBuiltinCall {parameters = ttes; _}
| RVEnum {assoc_tac_exprs = ttes; _} -> ttes |> List.fold_left (fun acc tte -> 
  match tte_idenfier_used tte with
  | [] -> acc
  | t::[] -> t::acc
  | _::_ -> failwith "UNreachable only one zero element"
) []
| RVAdress id | RVDefer id -> (id, ttrv.rval_rktype)::[]
| RVCustomBinop {blhs; brhs; _} | RVBuiltinBinop {blhs; brhs; _} -> 
  let lhs_identifier_used = tte_idenfier_used blhs in
  let rhs_identifier_used = tte_idenfier_used brhs in
  lhs_identifier_used @ rhs_identifier_used
| RVStruct {fields; _} -> fields |> List.fold_left (fun acc (_, tte) -> 
  match tte_idenfier_used tte with
  | [] -> acc
  | t::[] -> t::acc
  | _::_ -> failwith "UNreachable only one zero element"
) []
| RVDiscard | RVLater -> []


let fetch_basic_block_from_label label_name bbset = 
  bbset |> BasicBlockSet.elements |> List.find (fun (bb: cfg_statement basic_block) -> 
    bb.label = label_name
  ) 

let basic_block_input_var basic_block = 
  let rec basic_block_cfg_statement_list ~created ~acc = function
  | [] ->
    basic_block.ending 
    |> Option.map (fun (Bbe_return tte | BBe_if {condition = tte; _}) -> 
      let right_value_variables_used_set = tte |> tte_idenfier_used |> TypedIdentifierSet.of_list in
      let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_variables_used_set created in
      TypedIdentifierSet.union acc remove_block_create_variable_set
    ) |> Option.value ~default:acc
  | stmt::q -> 
    begin match stmt with
    | CFG_STacDeclaration {identifier; trvalue}
      -> 
        let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
        let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set created in
        let extented_created_var = TypedIdentifierSet.add (identifier, declaration_typed identifier trvalue) created in
        let new_acc = TypedIdentifierSet.union remove_block_create_variable_set acc in
        basic_block_cfg_statement_list ~created:extented_created_var ~acc:new_acc q

    | CFG_STDerefAffectation {trvalue; _} | CFG_STacModification {identifier = _; trvalue}  -> 
      let right_value_set = trvalue |> ttrv_identifiers_used |> TypedIdentifierSet.of_list in
      let remove_block_create_variable_set = TypedIdentifierSet.diff right_value_set created in
      let new_acc = TypedIdentifierSet.union remove_block_create_variable_set acc in
      basic_block_cfg_statement_list ~created:created ~acc:new_acc q
    end
  in
  basic_block_cfg_statement_list ~created:TypedIdentifierSet.empty ~acc:TypedIdentifierSet.empty basic_block.cfg_statements

let basic_block_output_var basic_block_set basic_block = 
  match basic_block.ending with
  | Some (Bbe_return _) -> TypedIdentifierSet.empty
  | _ -> 
     StringSet.fold (fun elt acc -> 
      let follow_block = fetch_basic_block_from_label elt basic_block_set in 
      let follow_basic_block_input = basic_block_input_var follow_block in
      TypedIdentifierSet.union acc follow_basic_block_input
    ) basic_block.followed_by TypedIdentifierSet.empty


let basic_block_detail_of_basic_block set bb = 
  let in_vars = basic_block_input_var bb in
  let out_vars = basic_block_output_var set bb in
  {
    basic_block = bb;
    in_vars;
    out_vars;
  }

let cfg_to_cfg_details (cfg: cfg) = {
  entry_block = cfg.entry_block;
  blocks_details = cfg.blocks |> BasicBlockSet.elements |> List.map (basic_block_detail_of_basic_block cfg.blocks) |> BasicBlockDetailSet.of_list
}
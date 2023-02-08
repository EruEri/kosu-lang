open KosuIrTAC.Asttac
open KosuIrTyped.Asttyped

module StringSet = Set.Make(String)


module Cfg_Sig_Impl = struct

  type rktype = KosuIrTyped.Asttyped.rktype
  type tac_typed_rvalue = KosuIrTAC.Asttac.tac_typed_rvalue
  type tac_typed_expression = KosuIrTAC.Asttac.tac_typed_expression

  module TypedIdentifierSet = Set.Make (struct
    type t = string * rktype
    let compare lhs rhs = 
      let string_compare = compare (fst lhs) (fst rhs) in
      if string_compare = 0 then compare (snd lhs) (snd rhs)
      else string_compare  
  end
  )

  include TypedIdentifierSet

  type cfg_statement =
  | CFG_STacDeclaration of { identifier : string; trvalue : tac_typed_rvalue }
  | CFG_STacModification of { identifier : string; trvalue : tac_typed_rvalue }
  | CFG_STDerefAffectation of { identifier : string; trvalue : tac_typed_rvalue }

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

  module BasicBlockSet = Set.Make(struct
    type t = cfg_statement basic_block
    let compare (lhs: t) (rhs: t) = String.compare lhs.label rhs.label
  end)

  type cfg = {
    entry_block: string;
    blocks: BasicBlockSet.t
  }

  let compare_type: rktype -> rktype -> int = Stdlib.compare
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

  
end

module Cfg = Register_allocator.Cgf.Make(Cfg_Sig_Impl)
let fake_label_counter = ref 0

let fake_label () = 
  let n = !fake_label_counter in
  let () = fake_label_counter := n + 1 in
  Printf.sprintf "fake_label.%u" n
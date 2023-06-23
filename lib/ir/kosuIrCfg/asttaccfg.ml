open KosuIrTAC.Asttac
open KosuIrTyped.Asttyped
module StringSet = Set.Make (String)

module type ABI_Large = sig
  include KosuRegisterAllocator.ABI

  val float_argument_registers : t list
  val available_register : t list
  val color_map : (t * string) list
end

module Cfg_Sig_Impl = struct
  type variable = string * KosuIrTyped.Asttyped.rktype
  type t = variable
  type rktype = KosuIrTyped.Asttyped.rktype
  type atom = KosuIrTAC.Asttac.tac_typed_expression
  type rvalue = KosuIrTAC.Asttac.tac_typed_rvalue

  let compare lhs rhs =
    let string_compare = String.compare (fst lhs) (fst rhs) in
    if string_compare = 0 then compare (snd lhs) (snd rhs) else string_compare

  let repr (variable, ktype) =
    Printf.sprintf "%s : %s" variable
      (KosuIrTyped.Asttypprint.string_of_rktype ktype)

  let lvalue_variable : string -> rvalue -> variable =
   fun identifier ttrv -> (identifier, ttrv.rval_rktype)

  let lvalue_deref_variable : string -> rvalue -> variable =
   fun identifier ttrv -> (identifier, RTPointer ttrv.rval_rktype)

  let tte_idenfier_used : atom -> (string * rktype) list = function
    | { tac_expression = TEIdentifier id; expr_rktype } ->
        (id, expr_rktype) :: []
    | _ -> []

  let ttrv_identifiers_used : rvalue -> (string * rktype) list =
   fun ttrv ->
    match ttrv.rvalue with
    | RVExpression tte
    | RVTupleAccess { first_expr = tte; _ }
    | RVFieldAcess { first_expr = tte; _ }
    | RVCustomUnop { expr = tte; _ }
    | RVBuiltinUnop { expr = tte; _ } ->
        tte |> tte_idenfier_used
    | RVFunction { tac_parameters = ttes; _ }
    | RVArray ttes
    | RVTuple ttes
    | RVBuiltinCall { parameters = ttes; _ }
    | RVEnum { assoc_tac_exprs = ttes; _ } ->
        ttes
        |> List.fold_left
             (fun acc tte ->
               match tte_idenfier_used tte with
               | [] -> acc
               | t :: [] -> t :: acc
               | _ :: _ -> failwith "UNreachable only one zero element")
             []
    | RVAdress id ->
        (id, KosuIrTyped.Asttyhelper.RType.rtpointee ttrv.rval_rktype) :: []
    | RVDefer id ->
        (id, KosuIrTyped.Asttyhelper.RType.rpointer ttrv.rval_rktype) :: []
    | RVCustomBinop { blhs; brhs; _ } | RVBuiltinBinop { blhs; brhs; _ } ->
        let lhs_identifier_used = tte_idenfier_used blhs in
        let rhs_identifier_used = tte_idenfier_used brhs in
        let captured = lhs_identifier_used @ rhs_identifier_used in
        (* let () = captured |> List.iter (fun elt -> elt |> repr |> Printf.printf "(%s)\n") in *)
        captured
    | RVStruct { fields; _ } ->
        fields
        |> List.fold_left
             (fun acc (_, tte) ->
               match tte_idenfier_used tte with
               | [] -> acc
               | t :: [] -> t :: acc
               | _ :: _ -> failwith "Unreachable only one zero element")
             []
    | RVDiscard | RVLater -> []

  let variables_as_parameter : rvalue -> (variable * int) list option =
   fun ttrv ->
    match ttrv.rvalue with
    | RVFunction { tac_parameters = ttes; _ } ->
        ttes
        |> List.fold_left
             (fun (index, acc) tte ->
               let next_index = index + 1 in
               match tte_idenfier_used tte with
               | t :: [] -> (next_index, (t, index) :: acc)
               | [] | _ :: _ -> (next_index, acc))
             (0, [])
        |> snd |> List.rev |> Option.some
    | _ -> None

  let is_affectation = function
    | { rvalue = RVDiscard | RVLater; _ } -> false
    | _ -> true
end

module CfgPprint = struct
  type variable = Cfg_Sig_Impl.variable
  type atom = Cfg_Sig_Impl.atom
  type rvalue = Cfg_Sig_Impl.rvalue

  let string_of_variable = Cfg_Sig_Impl.repr
  let string_of_atom = KosuIrTAC.Asttacpprint.string_of_typed_tac_expression
  let string_of_rvalue = KosuIrTAC.Asttacpprint.string_of_typed_tac_rvalue
end

module KosuRegisterAllocatorImpl =
  KosuRegisterAllocator.MakePprint (Cfg_Sig_Impl) (CfgPprint)

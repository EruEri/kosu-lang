open KosuIrTyped.Asttyped
open KosuIrTyped.Asttyped.Sizeof


let offset_of_field ~generics field rstruct_decl rprogram = 
  let ( ++ ) = Int64.add in 
  let ( -- ) = Int64.sub in
  rstruct_decl.rfields
  |> List.fold_left ( fun ((acc_size, acc_align, acc_packed_size, found) as acc) (sfield, rktype) ->
    let comming_size =
      rktype |> RType.remap_generic_ktype generics |> size `size rprogram
    in
    let comming_align =
      rktype |> RType.remap_generic_ktype generics |> size `align rprogram
    in
    let quotient = Int64.unsigned_div acc_size comming_align in
    let reminder = Int64.unsigned_rem acc_size comming_align in
    let new_pacced_size = comming_size ++ acc_packed_size in

    let add_size =
      if new_pacced_size < acc_size then 0L
      else if comming_size < acc_align then acc_align
      else comming_size
    in

    let padded_size =
      if reminder = 0L || acc_size = 0L then acc_size
      else Int64.mul comming_align (quotient ++ 1L)
    in

    if found then acc
    else if sfield = field
      then (padded_size ++ add_size -- comming_size, max comming_align acc_align, new_pacced_size, true)
    else
    ( padded_size ++ add_size,
      max comming_align acc_align,
      new_pacced_size, false
    )

    ) (0L, 0L, 0L, false)
  |> (fun (x, _, _, _) -> x)  


type stringlit_label = 
| SLit of string

type label = 
| Label of string


let make_string_litteral_label count = 
  let lab = Printf.sprintf "l_.str%s" (if count > 0 then Printf.sprintf ".%u" count else "") in
  SLit lab


(* let rec map_fill_string_lit_of_typed_expression map typed_expression () = 
  match typed_expression.rexpression with
  | REstring s -> (
    let map_length = Hashtbl.length map in
    match Hashtbl.find_opt map s with
    | None -> Hashtbl.add map s (make_string_litteral_label map_length)
    | Some _ -> ()
  )
  | REmpty | RTrue | RFalse | RENullptr | REInteger _ | REFloat _ | RESizeof _ | REAdress _ | REDeference _ | REIdentifier _ | REConst_Identifier _ -> ()
  | REFieldAcces {first_expr; _ } -> map_fill_string_lit_of_typed_expression map first_expr ()
  | REStruct {fields; _} -> fields |> List.iter (fun (_, typed_expr) -> 
    map_fill_string_lit_of_typed_expression map typed_expr ()
    )
  | REEnum {assoc_exprs = typed_exprs; _ } 
  | RETuple (typed_exprs)
  | REBuiltin_Function_call {parameters = typed_exprs; _} 
  | REFunction_call {parameters = typed_exprs; _} -> typed_exprs |> List.iter (fun typed_expr -> map_fill_string_lit_of_typed_expression map typed_expr ())
  | REIf (condition, if_body, else_body) -> 
    let () = map_fill_string_lit_of_typed_expression map condition () in
    let () = map_fill_string_lit_of_rkbody map if_body () in
    let () = map_fill_string_lit_of_rkbody map else_body () in
    ()
  | RECases {cases; else_case} -> 
    let () = cases |> List.iter (fun (condition, kbody) -> 
      let () = map_fill_string_lit_of_typed_expression map condition () in
      let () = map_fill_string_lit_of_rkbody map kbody () in
      ()
      ) in
      map_fill_string_lit_of_rkbody map else_case ()
  | RESwitch {rexpression; cases; wildcard_case} -> 
    let () = map_fill_string_lit_of_typed_expression map rexpression () in
    let () = cases |> List.iter (fun (_, _, rkbody) -> map_fill_string_lit_of_rkbody map rkbody ()) in
    wildcard_case |> Option.iter (fun rkbody -> map_fill_string_lit_of_rkbody map rkbody () )
  | REUn_op (RUMinus typed_expr | RUNot typed_expr) -> map_fill_string_lit_of_typed_expression map typed_expr ()
  | REBin_op (
    RBAdd (ltyp_expr, rtyp_expr)
    | RBMinus (ltyp_expr, rtyp_expr)
    | RBMult (ltyp_expr, rtyp_expr)
    | RBMod (ltyp_expr, rtyp_expr)
    | RBDiv (ltyp_expr, rtyp_expr)
    | RBBitwiseAnd (ltyp_expr, rtyp_expr)
    | RBBitwiseOr (ltyp_expr, rtyp_expr)
    | RBBitwiseXor (ltyp_expr, rtyp_expr)
    | RBShiftLeft (ltyp_expr, rtyp_expr)
    | RBShiftRight (ltyp_expr, rtyp_expr)
    | RBAnd (ltyp_expr, rtyp_expr)
    | RBOr (ltyp_expr, rtyp_expr)
    | RBSup (ltyp_expr, rtyp_expr)
    | RBSupEq (ltyp_expr, rtyp_expr)
    | RBInf (ltyp_expr, rtyp_expr)
    | RBInfEq (ltyp_expr, rtyp_expr)
    | RBEqual (ltyp_expr, rtyp_expr)
    | RBDif (ltyp_expr, rtyp_expr)
  ) -> 
    let () = map_fill_string_lit_of_typed_expression map ltyp_expr () in
    map_fill_string_lit_of_typed_expression map rtyp_expr ()

and map_fill_string_lit_of_rkbody map (rstmts, last_rexpr) () = 
  match rstmts with
  | t::q -> (
    let () = match t with
    | RSDeclaration {typed_expression; _} | RSAffection (_, typed_expression) | RSDiscard typed_expression | RSDerefAffectation (_, typed_expression)
     -> map_fill_string_lit_of_typed_expression map typed_expression () in
     map_fill_string_lit_of_rkbody map (q,last_rexpr) ()
  )
  | [] -> 
    map_fill_string_lit_of_typed_expression map last_rexpr () *)

let map_fill_string_lit_of_rkbody _a _b () = failwith ""  

let map_fill_string_lit_of_module_node map node  () = 
  match node with
  | RNFunction { rbody; _ } -> map_fill_string_lit_of_rkbody map rbody ()
  | _ -> ()


let map_of_string_litteral_in_module (RModule(rmodule)) () = 
  let map = Hashtbl.create 10 in
  let () = rmodule |> List.iter (fun modul -> 
    map_fill_string_lit_of_module_node map modul  ()
  ) in
  map

let map_string_litteral_of_named_rmodule_path {filename = _;  rmodule_path = {path = _; rmodule = rmodule}} = 
  map_of_string_litteral_in_module rmodule 
  
let maps_of_prgram program = 
  program
  |> List.map (fun ({filename; _} as named_module) -> 
    (filename, map_string_litteral_of_named_rmodule_path named_module ())
  ) 

let string_of_binding hashmap = 
  hashmap
  |> Hashtbl.to_seq
  |> Seq.map (fun (s, SLit(label)) -> Printf.sprintf "%s : \"%s\"" label s)
  |> List.of_seq
  |> String.concat "\n\t"
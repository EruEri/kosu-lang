open KosuIrTyped.Asttyped
open KosuIrTyped.Asttyconvert.Sizeof
open KosuIrTAC.Asttac

type lit = StringL of string | IntL of int64
type scale = [ `One | `Two | `Four | `Eight ]
type imm = Lit of lit | Lab of string
type stringlit_label = SLit of string
type label = Label of string

let offset_of_field ~generics field rstruct_decl rprogram =
  let ( ++ ) = Int64.add in
  let ( -- ) = Int64.sub in
  rstruct_decl.rfields
  |> List.fold_left
       (fun ((acc_size, acc_align, acc_packed_size, found) as acc)
            (sfield, rktype) ->
         let comming_size =
           rktype |> KosuIrTyped.Asttyhelper.RType.remap_generic_ktype generics |> size `size rprogram
         in
         let comming_align =
           rktype |> KosuIrTyped.Asttyhelper.RType.remap_generic_ktype generics |> size `align rprogram
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
         else if sfield = field then
           ( padded_size ++ add_size -- comming_size,
             max comming_align acc_align,
             new_pacced_size,
             true )
         else
           ( padded_size ++ add_size,
             max comming_align acc_align,
             new_pacced_size,
             false ))
       (0L, 0L, 0L, false)
  |> fun (x, _, _, _) -> x

let offset_of_tuple_index ~generics index rktypes rprogram =
  let ( ++ ) = Int64.add in
  let ( -- ) = Int64.sub in
  rktypes
  |> List.mapi (fun i v -> i, v )
  |> List.fold_left
        (fun ((acc_size, acc_align, acc_packed_size, found) as acc)
            (tindex, rktype) ->
          let comming_size =
            rktype |> KosuIrTyped.Asttyhelper.RType.remap_generic_ktype generics |> size `size rprogram
          in
          let comming_align =
            rktype |> KosuIrTyped.Asttyhelper.RType.remap_generic_ktype generics |> size `align rprogram
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
          else if index = tindex then
            ( padded_size ++ add_size -- comming_size,
              max comming_align acc_align,
              new_pacced_size,
              true )
          else
            ( padded_size ++ add_size,
              max comming_align acc_align,
              new_pacced_size,
              false ))
        (0L, 0L, 0L, false)
  |> fun (x, _, _, _) -> x
let make_string_litteral_label count =
  let lab =
    Printf.sprintf "l_.str%s"
      (if count > 0 then Printf.sprintf ".%u" count else "")
  in
  SLit lab

let rec map_fill_string_lit_of_tac_expression map expression () =
  match expression with
  | TEString s -> (
      match Hashtbl.find_opt map s with
      | None ->
          Hashtbl.add map s (make_string_litteral_label (map |> Hashtbl.length))
      | Some _ -> ())
  | _ -> ()

and map_fill_string_lit_of_trvalue map trvalue () =
  match trvalue with
  | RVUminus e | RVNeg e -> map_fill_string_lit_of_trvalue map e.rvalue ()
  | RVExpression e ->
      map_fill_string_lit_of_tac_expression map e.tac_expression ()
  | RVFunction { tac_parameters; _ } ->
      tac_parameters
      |> List.iter (fun typed_expr ->
             map_fill_string_lit_of_tac_expression map typed_expr.tac_expression
               ())
  | RVStruct { fields; _ } ->
      fields
      |> List.iter (fun (_, typed_expr) ->
             map_fill_string_lit_of_tac_expression map typed_expr.tac_expression
               ())
  | RVEnum { assoc_tac_exprs; _ } ->
      assoc_tac_exprs
      |> List.iter (fun typed_expr ->
             map_fill_string_lit_of_tac_expression map typed_expr.tac_expression
               ())
  | RVBuiltinCall { parameters; _ } | RVTuple parameters ->
      parameters
      |> List.iter (fun typed_expr ->
             map_fill_string_lit_of_tac_expression map typed_expr.tac_expression
               ())
  | RVCustomBinop bin | RVBuiltinBinop bin ->
      let () =
        map_fill_string_lit_of_tac_expression map bin.blhs.tac_expression ()
      in
      map_fill_string_lit_of_tac_expression map bin.brhs.tac_expression ()
  | RVCustomUnop un | RVBuiltinUnop un ->
      map_fill_string_lit_of_tac_expression map un.expr.tac_expression ()
  | RVFieldAcess { first_expr; _ } ->
      map_fill_string_lit_of_tac_expression map first_expr.tac_expression ()
  | RVAdress _ | RVDefer _ | RVDiscard | RVLater -> ()

and map_fill_string_lit_of_tac_case map
    { statement_for_condition; condition; tac_body; _ } () =
  let () =
    statement_for_condition
    |> List.iter (fun stmt -> map_fill_string_lit_of_tac_statement map stmt ())
  in
  let () =
    map_fill_string_lit_of_tac_expression map condition.tac_expression ()
  in
  let () = map_fill_string_lit_of_tac_body map tac_body () in
  ()

and map_fill_string_lit_of_tac_switch map { switch_tac_body; _ } () =
  map_fill_string_lit_of_tac_body map switch_tac_body ()

and map_fill_string_lit_of_tac_statement map statement () =
  match statement with
  | STacDeclaration { trvalue; _ }
  | STacModification { trvalue; _ }
  | STDerefAffectation { trvalue; _ } ->
      map_fill_string_lit_of_trvalue map trvalue.rvalue ()
  | STIf { statement_for_bool; condition_rvalue; if_tac_body; else_tac_body; _ }
    ->
      let () =
        statement_for_bool
        |> List.iter (fun lstmt ->
               map_fill_string_lit_of_tac_statement map lstmt ())
      in
      let () =
        map_fill_string_lit_of_tac_expression map
          condition_rvalue.tac_expression ()
      in
      let () = map_fill_string_lit_of_tac_body map if_tac_body () in
      let () = map_fill_string_lit_of_tac_body map else_tac_body () in
      ()
  | STSwitch
      { statemenets_for_case; condition_switch; sw_cases; wildcard_body; _ } ->
      let () =
        statemenets_for_case
        |> List.iter (fun smt ->
               map_fill_string_lit_of_tac_statement map smt ())
      in
      let () =
        map_fill_string_lit_of_tac_expression map
          condition_switch.tac_expression ()
      in
      let () =
        sw_cases
        |> List.iter (fun tac_switch ->
               map_fill_string_lit_of_tac_switch map tac_switch ())
      in
      let () =
        wildcard_body
        |> Option.iter (fun tb -> map_fill_string_lit_of_tac_body map tb ())
      in
      ()
  | SCases { cases; else_tac_body; _ } ->
      let () =
        cases
        |> List.iter (fun case -> map_fill_string_lit_of_tac_case map case ())
      in
      map_fill_string_lit_of_tac_body map else_tac_body ()

and map_fill_string_lit_of_tac_body map { label = _; body = statements, last }
    () =
  let () =
    statements
    |> List.iter (fun stmt -> map_fill_string_lit_of_tac_statement map stmt ())
  in
  last
  |> Option.iter (fun expr ->
         map_fill_string_lit_of_tac_expression map expr.tac_expression ())

and map_fill_string_lit_of_module_node map node () =
  match node with
  | TNFunction { tac_body; _ } ->
      map_fill_string_lit_of_tac_body map tac_body ()
  | TNOperator op ->
      let body = KosuIrTAC.Asttachelper.OperatorDeclaration.tac_body op in
      map_fill_string_lit_of_tac_body map body ()
  | _ -> ()

let map_of_string_litteral_in_module (TacModule rmodule) () =
  let map = Hashtbl.create 10 in
  let () =
    rmodule
    |> List.iter (fun modul -> map_fill_string_lit_of_module_node map modul ())
  in
  map

let map_string_litteral_of_named_rmodule_path
    { filename = _; tac_module_path = { path = _; tac_module }; rprogram = _ } =
  map_of_string_litteral_in_module tac_module

let maps_of_prgram program =
  program
  |> List.map (fun ({ filename; _ } as named_module) ->
         (filename, map_string_litteral_of_named_rmodule_path named_module ()))

let string_of_binding hashmap =
  hashmap |> Hashtbl.to_seq
  |> Seq.map (fun (s, SLit label) -> Printf.sprintf "%s : \"%s\"" label s)
  |> List.of_seq |> String.concat "\n\t"

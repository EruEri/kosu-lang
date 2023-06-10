open KosuIrTAC.Asttac
open Asttaccfg.KosuRegisterAllocator
open Asttaccfg.KosuRegisterAllocator.Basic

let fake_label_counter = ref 0

let tag_variable_conter = ref 0
let tag_variable () = 
  let n = !tag_variable_conter in
  let () = incr tag_variable_conter in
  Printf.sprintf "@tag.%u" n

let fake_label () =
  let n = !fake_label_counter in
  let () = fake_label_counter := n + 1 in
  Printf.sprintf "fake_label.%u" n

let merge_basic_block_map lmap rmap =
  Asttaccfg.KosuRegisterAllocator.BasicBlockMap.union
    (fun key m1 m2 ->
      let () = Printf.eprintf "Conficiting label = %s\n" key in
      if m1 <> m2 then failwith "Diff for key" else Some m1)
    lmap rmap

let typed_set_of_locales_vars locals_vars =
  locals_vars
  |> List.map (fun { locale_ty; locale } ->
         match locale with
         | Locale s -> (s, locale_ty)
         | Enum_Assoc_id { name; _ } -> (name, locale_ty))
  |> Asttaccfg.KosuRegisterAllocator.TypedIdentifierSet.of_list

let rec of_tac_statements ~tag_map ~start_label ~end_labels ~ending ~cfg_statements
    (stmts, return) =
  match stmts with
  | [] ->
      let block =
        {
          label = start_label;
          cfg_statements = List.rev cfg_statements;
          followed_by = Asttaccfg.StringSet.of_list end_labels;
          ending =
            (match return with
            | None -> (
                match ending with
                | None -> None
                | Some ending -> Some (BBe_if ending))
            | Some tte -> Some (Bbe_return tte));
        }
      in
      Asttaccfg.KosuRegisterAllocator.BasicBlockMap.singleton block.label block
  | stmt :: q as _stmts -> begin
      match stmt with
      | STacDeclaration { identifier; trvalue } ->
          let declaration =
            Asttaccfg.KosuRegisterAllocator.CFG_STacDeclaration
              { identifier; trvalue }
          in
          of_tac_statements ~tag_map ~start_label ~end_labels ~ending
            ~cfg_statements:(declaration :: cfg_statements)
            (q, return)
      | STacModification { identifier; trvalue } ->
          let modification =
            Asttaccfg.KosuRegisterAllocator.CFG_STacModification
              { identifier; trvalue }
          in
          of_tac_statements  ~tag_map ~start_label ~end_labels ~ending
            ~cfg_statements:(modification :: cfg_statements)
            (q, return)
      | STDerefAffectation { identifier; trvalue } ->
          let derefaffect =
            Asttaccfg.KosuRegisterAllocator.CFG_STDerefAffectation
              { identifier; trvalue }
          in
          of_tac_statements  ~tag_map ~start_label ~end_labels ~ending
            ~cfg_statements:(derefaffect :: cfg_statements)
            (q, return)
      | STDerefAffectationField _ -> failwith "STDerefAffectationField"
      | STacModificationField _ -> failwith "STacModificationField"
      | STWhile
          {
            statements_condition;
            condition;
            loop_body;
            self_label;
            inner_body_label;
            exit_label;
          } ->
          let pre_jump =
            of_tac_statements  ~tag_map ~start_label:self_label
              ~end_labels:[ exit_label; inner_body_label ]
              ~ending:
                (Some
                   {
                     condition;
                     if_label = inner_body_label;
                     else_label = exit_label;
                   })
              ~cfg_statements
              (statements_condition, None)
          in
          let loop_blocks = of_tac_body ~tag_map ~end_labels:[ self_label ] loop_body in
          let blocks_continuation =
            of_tac_statements  ~tag_map ~start_label:exit_label ~end_labels ~ending
              ~cfg_statements:[] (q, return)
          in

          pre_jump
          |> merge_basic_block_map loop_blocks
          |> merge_basic_block_map blocks_continuation
      | STIf
          {
            statement_for_bool;
            condition_rvalue;
            goto1;
            goto2;
            if_tac_body;
            else_tac_body;
            exit_label;
          } ->
          let continuation =
            of_tac_statements  ~tag_map ~start_label ~end_labels:[ goto1; goto2 ]
              ~ending:
                (Some
                   {
                     condition = condition_rvalue;
                     if_label = goto1;
                     else_label = goto2;
                   })
              ~cfg_statements (statement_for_bool, None)
          in
          let if_blocks = of_tac_body  ~tag_map ~end_labels:[ exit_label ] if_tac_body in
          let else_blocks =
            of_tac_body  ~tag_map ~end_labels:[ exit_label ] else_tac_body
          in

          let blocks_continuation =
            of_tac_statements  ~tag_map ~start_label:exit_label ~end_labels ~ending
              ~cfg_statements:[] (q, return)
          in

          continuation
          |> merge_basic_block_map if_blocks
          |> merge_basic_block_map else_blocks
          |> merge_basic_block_map blocks_continuation
      | SCases
          {
            cases =
              {
                condition_label;
                statement_for_condition;
                condition;
                goto;
                jmp_false;
                end_label;
                tac_body;
              }
              :: tac_cases;
            else_tac_body;
            exit_label;
          } ->
          let () =
            match condition_label with
            | None -> ()
            | Some s ->
                failwith
                @@ Printf.sprintf "First cases with a label name ? : %s" s
          in
          let continuation =
            of_tac_statements  ~tag_map ~start_label ~end_labels:[ goto; jmp_false ]
              ~ending:
                (Some { condition; if_label = goto; else_label = jmp_false })
              ~cfg_statements
              (statement_for_condition, None)
          in
          let first_block_body =
            of_tac_body  ~tag_map ~end_labels:[ end_label ] tac_body
          in

          let blocks_continuation =
            of_tac_statements  ~tag_map ~start_label:exit_label ~end_labels ~ending
              ~cfg_statements:[] (q, return)
          in

          let cases_basic_block =
            tac_cases
            |> List.fold_left
                 (fun acc
                      {
                        condition_label;
                        statement_for_condition;
                        condition;
                        goto;
                        jmp_false;
                        end_label;
                        tac_body;
                      } ->
                   let start_label =
                     match condition_label with
                     | Some s -> s
                     | None ->
                         failwith
                           "Very wierd start label for cases should be None"
                   in
                   let block_condition =
                     of_tac_statements  ~tag_map ~start_label
                       ~end_labels:[ goto; jmp_false ]
                       ~ending:
                         (Some
                            {
                              condition;
                              if_label = goto;
                              else_label = jmp_false;
                            })
                       ~cfg_statements:[]
                       (statement_for_condition, None)
                   in
                   let block = of_tac_body  ~tag_map ~end_labels:[ end_label ] tac_body in
                   acc
                   |> merge_basic_block_map block_condition
                   |> merge_basic_block_map block)
                 Asttaccfg.KosuRegisterAllocator.BasicBlockMap.empty
          in

          let else_basic_block =
            of_tac_body  ~tag_map ~end_labels:[ exit_label ] else_tac_body
          in

          continuation
          |> merge_basic_block_map first_block_body
          |> merge_basic_block_map cases_basic_block
          |> merge_basic_block_map else_basic_block
          |> merge_basic_block_map blocks_continuation

      | STSwitch { statemenets_for_case; condition_switch; 
        sw_cases = {
          variants_to_match;
          assoc_bound;
          sw_goto;
          sw_exit_label;
          switch_tac_body
        }::sw_cases; 
        wildcard_label; 
        wildcard_body; 
      } -> 
        (* let continuation =
          of_tac_statements  ~tag_map ~start_label ~end_labels:[ goto1; goto2 ]
            ~ending:
              (Some
                 {
                   condition = condition_switch;
                   if_label = goto1;
                   else_label = goto2;
                 })
            ~cfg_statements (statemenets_for_case, None)
        in *)
        failwith "switch todo"
      | SCases { cases = []; else_tac_body = _; exit_label = _ } ->
        failwith "Unreachable code: Syntax force at least a branch"
      | STSwitch _ -> failwith ""
    end

and of_tac_body ~tag_map ~end_labels ({ label; body } : tac_body) =
  of_tac_statements ~tag_map ~ending:None ~start_label:label ~end_labels
    ~cfg_statements:[] body

let of_tac_body ~tag_map tac_body ~parameters ~locals_vars =
  let basic_blocks = of_tac_body ~tag_map ~end_labels:[] tac_body in
  {
    entry_block = tac_body.label;
    blocks = basic_blocks;
    parameters;
    locals_vars;
  }

let cfg_of_tac_function tacfun =
  let map = Hashtbl.create 7 in
  of_tac_body ~tag_map:map tacfun.tac_body ~parameters:tacfun.rparameters
    ~locals_vars:(typed_set_of_locales_vars tacfun.locale_var)

let cfg_detail_of_tac_function tacfun =
  tacfun |> cfg_of_tac_function |> Asttaccfg.KosuRegisterAllocator.Detail.of_cfg

let cfg_liveness_of_tac_function tacfun =
  tacfun |> cfg_detail_of_tac_function
  |> Asttaccfg.KosuRegisterAllocator.Liveness.of_cfg_details
       ~delete_useless_stmt:false

let cfgs_of_tac_program named_tacmodules =
  named_tacmodules
  |> List.map (fun { filename; tac_module_path; _ } ->
         let (TacModule tac_nodes) = tac_module_path.tac_module in
         ( filename,
           tac_nodes
           |> List.filter_map (function
                | TNFunction tacfun -> Some (cfg_of_tac_function tacfun)
                | _ -> None) ))

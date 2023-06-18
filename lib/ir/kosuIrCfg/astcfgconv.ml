open KosuIrTAC.Asttac
open Asttaccfg.KosuRegisterAllocatorImpl
open Asttaccfg.KosuRegisterAllocatorImpl.Basic

let fake_label_counter = ref 0
let tag_variable_conter = ref 0

let cmp_variable_counter = ref 0

let tag_variable () =
  let n = !tag_variable_conter in
  let () = incr tag_variable_conter in
  Printf.sprintf "@tag.%u" n

let cmp_variable () =
  let n = !cmp_variable_counter in
  let () = incr cmp_variable_counter in
  Printf.sprintf "@cmp.%u" n

let fake_label () =
  let n = !fake_label_counter in
  let () = fake_label_counter := n + 1 in
  Printf.sprintf "fake_label.%u" n

  let enum_tag_type = KosuIrTyped.Asttyped.(RTInteger (Unsigned, I32))

  let tag_of_variant variant enum_decl = Int32.to_int @@ KosuIrTyped.Asttyhelper.Renum.tag_of_variant variant enum_decl

  let tag_statements enum_tac_expr = 
    let tag = tag_variable () in
    let tag_atom = {expr_rktype = enum_tag_type; tac_expression = TEIdentifier tag} in
    tag_atom, STacDeclaration {
    identifier = tag;
    trvalue = {
      rval_rktype = enum_tag_type;
      rvalue = RVBuiltinCall {
        fn_name = KosuFrontend.Ast.Builtin_Function.Tagof;
        parameters = [enum_tac_expr]
      }
    }
  }

  

  let cmp_statement atom tag_to_match = 
    let cmp = cmp_variable () in
    let cmp_atom = {
      expr_rktype = KosuIrTyped.Asttyped.RTBool; 
      tac_expression = TEIdentifier cmp
    } in
    cmp_atom, STacDeclaration {
      identifier = cmp;
      trvalue = {
        rval_rktype = KosuIrTyped.Asttyped.RTBool;
        rvalue = RVBuiltinBinop {
          binop = TacBool TacEqual;
          blhs = atom;
          brhs = {
            expr_rktype = enum_tag_type;
            tac_expression = TEInt (
              Unsigned,
              I32,
              (Int64.of_int tag_to_match)
            )
          }
        }
      }
    }

let merge_basic_block_map lmap rmap =
  Asttaccfg.KosuRegisterAllocatorImpl.BasicBlockMap.union
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
  |> Asttaccfg.KosuRegisterAllocatorImpl.TypedIdentifierSet.of_list

let rec of_tac_statements ~start_label ~end_labels ~ending
    ~cfg_statements rprogram (stmts, return) =
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
      Asttaccfg.KosuRegisterAllocatorImpl.BasicBlockMap.singleton block.label
        block
  | stmt :: q as _stmts -> (
      match stmt with
      | STacDeclaration { identifier; trvalue } ->
          let declaration =
            Asttaccfg.KosuRegisterAllocatorImpl.CFG_STacDeclaration
              { identifier; trvalue }
          in
          of_tac_statements ~start_label ~end_labels ~ending
            ~cfg_statements:(declaration :: cfg_statements)
            rprogram
            (q, return)
      | STacModification { identifier; trvalue } ->
          let modification =
            Asttaccfg.KosuRegisterAllocatorImpl.CFG_STacModification
              { identifier; trvalue }
          in
          of_tac_statements ~start_label ~end_labels ~ending rprogram
            ~cfg_statements:(modification :: cfg_statements)
            (q, return)
      | STDerefAffectation { identifier; trvalue } ->
          let derefaffect =
            Asttaccfg.KosuRegisterAllocatorImpl.CFG_STDerefAffectation
              { identifier; trvalue }
          in
          of_tac_statements ~start_label ~end_labels ~ending rprogram
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
            of_tac_statements ~start_label:self_label
              ~end_labels:[ exit_label; inner_body_label ]
              ~ending:
                (Some
                   {
                     condition;
                     if_label = inner_body_label;
                     else_label = exit_label;
                   })
              ~cfg_statements
              rprogram
              (statements_condition, None)
          in
          let loop_blocks =
            of_tac_body ~end_labels:[ self_label ] rprogram loop_body
          in
          let blocks_continuation =
            of_tac_statements ~start_label:exit_label ~end_labels
              ~ending ~cfg_statements:[] rprogram (q, return)
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
            of_tac_statements ~start_label ~end_labels:[ goto1; goto2 ]
              ~ending:
                (Some
                   {
                     condition = condition_rvalue;
                     if_label = goto1;
                     else_label = goto2;
                   })
              ~cfg_statements rprogram (statement_for_bool, None)
          in
          let if_blocks =
            of_tac_body ~end_labels:[ exit_label ] rprogram if_tac_body
          in
          let else_blocks =
            of_tac_body ~end_labels:[ exit_label ] rprogram else_tac_body
          in

          let blocks_continuation =
            of_tac_statements ~start_label:exit_label ~end_labels
              ~ending ~cfg_statements:[] rprogram (q, return)
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
            of_tac_statements ~start_label
              ~end_labels:[ goto; jmp_false ]
              ~ending:
                (Some { condition; if_label = goto; else_label = jmp_false })
              ~cfg_statements
              rprogram
              (statement_for_condition, None)
          in
          let first_block_body =
            of_tac_body ~end_labels:[ end_label ] rprogram tac_body
          in

          let blocks_continuation =
            of_tac_statements ~start_label:exit_label ~end_labels
              ~ending ~cfg_statements:[] rprogram (q, return)
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
                     of_tac_statements ~start_label
                       ~end_labels:[ goto; jmp_false ]
                       ~ending:
                         (Some
                            {
                              condition;
                              if_label = goto;
                              else_label = jmp_false;
                            })
                       ~cfg_statements:[]
                       rprogram
                       (statement_for_condition, None)
                   in
                   let block =
                     of_tac_body ~end_labels:[ end_label ] rprogram tac_body
                   in
                   acc
                   |> merge_basic_block_map block_condition
                   |> merge_basic_block_map block)
                 Asttaccfg.KosuRegisterAllocatorImpl.BasicBlockMap.empty
          in

          let else_basic_block =
            of_tac_body ~end_labels:[ exit_label ] rprogram else_tac_body
          in

          continuation
          |> merge_basic_block_map first_block_body
          |> merge_basic_block_map cases_basic_block
          |> merge_basic_block_map else_basic_block
          |> merge_basic_block_map blocks_continuation
      | STSwitch
          {
            statemenets_for_case;
            condition_switch;
            sw_exit_label;
            sw_cases;
            wildcard_label;
            wildcard_body;
          } ->
            let enum_decl =
              match
                KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
                  condition_switch.expr_rktype rprogram
              with
              | Some (RDecl_Struct _) ->
                  failwith "Expected to find an enum get an struct"
              | Some (RDecl_Enum e) -> e
              | None -> failwith "Non type decl ??? my validation is very weak"
            in
            let enum_decl =
              let generics =
                condition_switch.expr_rktype
                |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
                |> List.combine enum_decl.generics
              in
              KosuIrTyped.Asttyhelper.Renum.instanciate_enum_decl generics enum_decl
            in

          let tag_atom, tag_stmt = tag_statements condition_switch in
          begin match sw_cases with
          | [] -> failwith ""
          | sw_case::sw_switch_remains -> begin 
            match sw_case.variants_to_match with
            | [] -> failwith "Impossible"
            | variant::else_variants -> 
              let ivmatch = tag_of_variant variant enum_decl in
              let cmp_atom, cmp_stmt = cmp_statement tag_atom ivmatch in

              let next_label = fake_label () in
              let continuation = of_tac_statements 
               
                ~start_label 
                ~end_labels:[sw_case.sw_goto; next_label]
                ~ending:(
                  Some 
                  {
                    condition = cmp_atom;
                    if_label = sw_case.sw_goto;
                    else_label = next_label;
                  }
                )
              ~cfg_statements rprogram (statemenets_for_case @ tag_stmt::cmp_stmt::[] , None)
            in
            let sw_case_without_first_match = {
              sw_case with variants_to_match = else_variants
            }
            in
            let branch_count = List.length sw_cases in
            let basic_blocks, next_block_label = sw_case_without_first_match::sw_switch_remains |> List.mapi Util.couple |> List.fold_left (fun (acc, block_next_label) (index, sw_switch) -> 
              let is_last = branch_count - 1 = index in
              let blocks, next_label = _of_switch_case 
              ~wildcard_label
              ~enum_decl 
              ~tag_atom 
              ~start_label:block_next_label 
              
              ~is_last
              rprogram
              sw_switch 
            in
              let acc = merge_basic_block_map acc blocks in
              acc, next_label
            ) (BasicBlockMap.empty, next_label) in
            let wildcard_basic_block = wildcard_body |> Option.map (fun body -> 
              let block_setup = 
                of_tac_statements 
               
                ~start_label:next_block_label
                ~end_labels:[Option.get wildcard_label]
                ~ending:None
                ~cfg_statements:[]
                rprogram
                ([], None)
              in
              let block_body = 
                of_tac_body 
              
                ~end_labels:[sw_exit_label]
                rprogram
                body
              in
              merge_basic_block_map block_setup block_body
            ) 
            |> Option.value ~default:BasicBlockMap.empty
            in

            let blocks_continuation =
              of_tac_statements ~start_label:sw_exit_label ~end_labels
                ~ending ~cfg_statements:[] rprogram (q, return)
            in

            continuation
            |> merge_basic_block_map basic_blocks
            |> merge_basic_block_map wildcard_basic_block
            |> merge_basic_block_map blocks_continuation
          
          end 

        end 
      | STSwitchTmp {    
          tmp_statemenets_for_case;
          enum_tte = _;
          tag_atom = _;
          tmp_switch_list;
          tmp_wildcard_label;
          tmp_wildcard_body;
          tmp_sw_exit_label
        } -> 
          let first_goto, first_goto_false, first_cmp_stmt, first_cmp_atom, switch_remains = match tmp_switch_list with
          | [] -> failwith ""
          | sw::remains -> 
            let first_variant, remains_variant = match sw.variants with
              | [] -> failwith "Empty ?"
              | t::q -> t, q
            in
            let new_sw = { sw with variants = remains_variant} in
            let remains = new_sw::remains in
            sw.tmp_sw_goto, (List.hd sw.variants).variant_next_label, first_variant.cmp_statement, first_variant.cmp_atom, remains
          in
          let local_endlabel = 
            match first_goto_false with
            | Some s -> [first_goto; s]
            | None -> [first_goto]
          in
          let local_ending = first_goto_false 
          |> Option.map (fun label -> 
              {
                condition = first_cmp_atom; 
                if_label = first_goto; 
                else_label = label
              }
            )
          in
          let basic_blocks, final_label = switch_remains |> List.fold_left (fun (acc, _) sw -> 
            let block, label = of_switch_case rprogram sw in
            (merge_basic_block_map acc block), label
          ) (BasicBlockMap.empty, "") in
          let wildcard_basic_block = tmp_wildcard_body |> Option.map (fun body -> 
            let block_setup = 
              of_tac_statements 
             
              ~start_label:final_label
              ~end_labels:[Option.get tmp_wildcard_label]
              ~ending:None
              ~cfg_statements:[]
              rprogram
              ([], None)
            in
            let block_body = 
              of_tac_body 
            
              ~end_labels:[tmp_sw_exit_label]
              rprogram
              body
            in
            merge_basic_block_map block_setup block_body
          ) 
          |> Option.value ~default:BasicBlockMap.empty
          in

          let continuation = of_tac_statements
        
          ~start_label
          ~end_labels:local_endlabel
          ~ending:local_ending
          ~cfg_statements rprogram (tmp_statemenets_for_case @ [first_cmp_stmt], None)
          in


          
          let blocks_continuation =
            of_tac_statements ~start_label:tmp_sw_exit_label ~end_labels
              ~ending ~cfg_statements:[] rprogram (q, return)
          in

          continuation
          |> merge_basic_block_map basic_blocks
          |> merge_basic_block_map wildcard_basic_block
          |> merge_basic_block_map blocks_continuation
      | SCases { cases = []; else_tac_body = _; exit_label = _ } ->
          failwith "Unreachable code: Syntax force at least a branch"
      )

and of_switch_case rprogram (tac_switch: tac_switch_tmp) = 
  let blocks, final_label =  tac_switch.variants |> List.fold_left (fun (acc, _) variant ->
    let ending = variant.variant_next_label |> Option.map (fun label -> {condition = variant.cmp_atom; if_label = tac_switch.tmp_sw_goto; else_label = label}) in
    let end_labels = match variant.variant_next_label with 
      | Some label -> tac_switch.tmp_sw_goto::label::[] 
      | None -> tac_switch.tmp_sw_goto::[]
    in
    let block = of_tac_statements 
      ~start_label:variant.variant_label 
      ~end_labels:end_labels
      ~ending
      ~cfg_statements:[]
      rprogram
      ([ variant.cmp_statement], None)
    in
      merge_basic_block_map acc block, variant.variant_label
  ) (BasicBlockMap.empty, "")
  in
  let body_block = of_tac_body ~end_labels:[tac_switch.tmp_sw_exit_label] rprogram tac_switch.tmp_switch_tac_body in
  merge_basic_block_map blocks body_block, final_label
        
and _of_switch_case ~is_last ~wildcard_label ~enum_decl ~tag_atom ~start_label rprogram (sw_switch : tac_switch) = 
  match sw_switch.variants_to_match with
  | [] -> failwith "Masaka"
  | variants -> 
    let variants_count = List.length variants in            
    let other_variants, next_block_label = variants |> List.mapi Util.couple |> List.fold_left (fun (acc, block_start_label) (index, variant) -> 
      let block_next_label = fake_label () in
      let variant_tag = tag_of_variant variant enum_decl in
      let cmp_atom, cmp_stmt = cmp_statement tag_atom variant_tag in
      let is_local_last = index = variants_count - 1 in

      let else_label = 
        let is_full_last = is_last && is_local_last in
        (* let () = Printf.printf "is last = %b\n%!" is_full_last in *)
        if is_full_last then 
          match wildcard_label with
          | Some label -> Some label
          | None -> None
      else Some block_next_label
      in
      let ending = else_label |> Option.map (fun label -> {condition = cmp_atom; if_label = sw_switch.sw_goto; else_label = label}) in
      let end_labels = match else_label with 
        | Some label -> sw_switch.sw_goto::label::[] 
        | None -> sw_switch.sw_goto::[]
      in
      let block = of_tac_statements 
        ~start_label:block_start_label 
        ~end_labels:end_labels
        ~ending
        ~cfg_statements:[]
        rprogram
        ([cmp_stmt], None)
    in
    let acc = merge_basic_block_map acc block in
    acc, block_next_label
    ) (BasicBlockMap.empty, start_label)
  in
  let body_block = of_tac_body ~end_labels:[sw_switch.sw_exit_label] rprogram sw_switch.switch_tac_body in
  let blocks = merge_basic_block_map other_variants body_block in
  blocks, next_block_label 


and of_tac_body ~end_labels rprogram ({ label; body } : tac_body) =
  of_tac_statements ~ending:None ~start_label:label ~end_labels
    ~cfg_statements:[] rprogram body

let of_tac_body ~parameters ~locals_vars rprogram tac_body =
  let basic_blocks = of_tac_body ~end_labels:[] rprogram tac_body in
  {
    entry_block = tac_body.label;
    blocks = basic_blocks;
    parameters;
    locals_vars;
  }

let cfg_of_tac_function rprogram tacfun =
  of_tac_body ~parameters:tacfun.rparameters rprogram tacfun.tac_body
    ~locals_vars:(typed_set_of_locales_vars tacfun.locale_var)

let cfg_detail_of_tac_function rprogram tacfun =
  tacfun |> cfg_of_tac_function rprogram
  |> Asttaccfg.KosuRegisterAllocatorImpl.Detail.of_cfg

let cfg_liveness_of_tac_function rprogram tacfun =
  tacfun |> cfg_detail_of_tac_function rprogram
  |> Asttaccfg.KosuRegisterAllocatorImpl.Liveness.of_cfg_details
       ~delete_useless_stmt:false

let cfgs_of_tac_program named_tacmodules =
  named_tacmodules
  |> List.map (fun { filename; tac_module_path; rprogram } ->
         let (TacModule tac_nodes) = tac_module_path.tac_module in
         ( filename,
           tac_nodes
           |> List.filter_map (function
                | TNFunction tacfun -> Some (cfg_of_tac_function rprogram tacfun)
                | _ -> None) ))

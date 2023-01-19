open X86_64Core
open X86_64Core.Register
open X86_64Core.Operande
open X86_64Core.Instruction
open KosuIrTyped.Asttyconvert.Sizeof
open KosuIrTAC.Asttac
open Util

let sizeofn = KosuIrTyped.Asttyconvert.Sizeof.sizeof

module Make(Spec: Common.AsmSpecification) = struct
  let translate_tac_expression ~str_lit_map ?(target_dst = (`Register {size = D; reg = R10} : dst))
  rprogram (fd: FrameManager.frame_desc) = function
  | { tac_expression = TEString s; expr_rktype = _ } ->
    let (SLit str_labl) = Hashtbl.find str_lit_map s in
    target_dst, load_label (Spec.label_of_constant str_labl) target_dst
| { tac_expression = TEFalse | TEmpty; expr_rktype = _ } ->
  begin match target_dst with
  | (`Register reg ) as rreg -> target_dst, [
    Instruction (Xor {size = reg.size; source = rreg; destination = rreg })
  ]
  | (`Address _) as addr -> target_dst, [
    Instruction (Mov {size = B; destination = addr; source = `ILitteral 0L})
  ]
end
| { tac_expression = TENullptr; expr_rktype = _ } ->
  begin match target_dst with
   | (`Register reg ) as rreg -> target_dst, [
    Instruction (Xor {size = reg.size; source = rreg; destination = rreg })
  ]
  | (`Address _) as addr -> target_dst, [
    Instruction (Mov {size = Q; destination = addr; source = `ILitteral 0L})
  ]
end
| { tac_expression = TETrue; _ } -> 
  target_dst, [ Instruction ( Mov {size = B; destination = target_dst; source = `ILitteral 1L}) ]
| { tac_expression = TEInt (_, isize, int64); _ } ->
    let size = data_size_of_isize isize in
    target_dst, [Instruction (Mov {size; destination = target_dst; source = `ILitteral int64})]
| { tac_expression = TEFloat _float; _ } ->
  failwith "X86_64: Mov Float todo"
| { tac_expression = TEIdentifier id; expr_rktype } ->
    let adress =
      FrameManager.address_of (id, expr_rktype) fd |> fun adr ->
      match adr with
      | Some a -> a
      | None -> failwith "X86_64: tte identifier setup null address"
    in
    let sizeof = sizeofn rprogram expr_rktype in
    let data_size = Option.value ~default:Q @@ data_size_of_int64 sizeof in
    begin match is_register_size sizeof with
    true -> ( 
      match target_dst with
      | `Register _ as reg -> target_dst, [ Instruction (Mov {size = data_size; destination = reg; source = `Address adress})]
      | `Address addr -> target_dst, [
        Instruction (Mov {size = data_size; destination = `Register raxq; source = `Address addr});
        Instruction (Mov {size = data_size; destination = target_dst; source = `Register raxq})
      ]
    )
    | false -> (
      match target_dst with
      | `Register reg -> 
        `Register (resize_register Q reg ), [
          Instruction (Lea {size = Q; source = adress; destination = reg})
        ]
      | `Address _ ->
        `Register (raxq), [
          Line_Com (Comment "Mov identifier larger than reg");
          Instruction (Lea {size = Q; source = adress; destination = raxq})
        ] 
    )
    end
 
| { tac_expression = TESizeof kt; _ } ->
    let sizeof = sizeofn rprogram kt in
    target_dst, [
      Line_Com (Comment "Sizeof ");
      Instruction (Mov {size = Q; destination = target_dst; source = `ILitteral sizeof})
    ]
| {
    tac_expression = TEConst { name; module_path };
    expr_rktype = RTString_lit;
  } ->   
    target_dst, load_label (Spec.label_of_constant ~module_path name) target_dst
| {
    tac_expression = TEConst { name; module_path };
    expr_rktype = RTInteger (_, size);
  } ->
    let data_size = data_size_of_isize size in
    let const_decl = 
      match rprogram |> KosuIrTyped.Asttyhelper.RProgram.find_const_decl ~name ~module_path with
      | None -> failwith (Printf.sprintf "No const decl for %s::%s" module_path name)
      | Some s -> s in
    let int_value =
      match const_decl.value.rexpression with
      | REInteger (_, _, value) -> value
      | _ -> failwith "Not an Integer" in
    
    resize_dst data_size target_dst, [Instruction (Mov {size = data_size; source = `ILitteral int_value; destination = target_dst})]
| _ -> failwith "X86_64 : Other expression"

let move_tte ~str_lit_map ~where ?(offset = 0L) rprogram fd tte = 
  let where = increment_dst_address offset where in
  let size_of_tte = sizeofn rprogram tte.expr_rktype in
  match is_register_size size_of_tte with
  | true ->
    translate_tac_expression ~str_lit_map ~target_dst:(where) rprogram fd tte
  | false ->     
    let last_dst, tte_instructions = translate_tac_expression ~str_lit_map ~target_dst:(where) rprogram fd tte in
    let last_reg = match last_dst with
    | `Register reg -> reg
    | `Address _ -> failwith "Tte size doesnt hold in reg : need address in reg"
  in
    let copy_if_addre = match where with
    | `Register _ -> []
    | `Address addr -> copy_large ~address_str:addr ~base_address_reg:(create_address_offset last_reg) size_of_tte
in
  last_dst,tte_instructions @ copy_if_addre 
    


   

let translate_tac_rvalue ?(is_deref = None) ~str_lit_map 
  ~(where : address option) current_module rprogram
  (fd : FrameManager.frame_desc) {rvalue; rval_rktype} = 
    match rvalue with
    | RVExpression tte ->
      let size_to_move = sizeofn rprogram tte.expr_rktype in
      let _last_dst, instructions = where |> Option.map (fun waddress -> 
        let last_dst, tte_instructions = 
          translate_tac_expression ~str_lit_map ~target_dst:(`Address waddress) rprogram fd tte in 
          let copy_instructions = match last_dst with
          | `Address _ -> []
          | `Register reg -> copy_large ~address_str:waddress ~base_address_reg:(create_address_offset reg) size_to_move in 
          last_dst, tte_instructions @ copy_instructions
      ) |> Option.value ~default:(dummy_dst, []) in
      instructions
    | RVStruct {module_path = _; struct_name = _; fields} ->
      let struct_decl =
        match
          KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
            rval_rktype rprogram
        with
        | Some (RDecl_Struct s) -> s
        | Some (RDecl_Enum _) ->
            failwith "Expected to find a struct get an enum"
        | None -> failwith "Non type decl ??? my validation is very weak"
      in

      let generics =
        rval_rktype
        |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
        |> List.combine struct_decl.generics
        |> List.to_seq |> Hashtbl.of_seq
      in
      let offset_list =
        fields
        |> List.map (fun (field, _) ->
               offset_of_field ~generics field struct_decl rprogram)
      in
      let instructions = 
        fields 
        |> List.mapi couple
        |> List.fold_left (fun acc (index, (_, tte)) -> 
          let offset = (List.nth offset_list index) in  
          let instructions =  where |> Option.map (fun waddress -> 
            let _, instructions = move_tte ~str_lit_map ~where:(`Address waddress) ~offset rprogram fd tte in
            instructions
          ) |> Option.value ~default:[] in
          acc @ instructions
        ) [] in
      instructions
    | RVFunction { module_path; fn_name; generics_resolver = _; tac_parameters } -> (
      let _typed_parameters =
        tac_parameters |> List.map (fun { expr_rktype; _ } -> expr_rktype)
      in
      let fn_module =
        if module_path = "" then current_module else module_path
      in
      let fn_decl =
        Option.get @@ KosuIrTyped.Asttyhelper.RProgram.find_function_decl_of_name fn_module
          fn_name rprogram
      in
      let available_register_params = if fd.need_result_ptr then List.tl Register.argument_registers else Register.argument_registers in
      let available_register_count = List.length available_register_params in
      let return_size = sizeofn rprogram rval_rktype in
      match fn_decl with
      | RExternal_Decl external_func_decl -> 
        let fn_label = Spec.label_of_external_function external_func_decl in 
        let args_in_reg, _args_on_stack = tac_parameters
        |> List.mapi (fun index value -> (index, value))
        |> List.partition_map (fun (index, value) ->
               if index < available_register_count then Either.left value
               else Either.right value)
      in
      let set_in_reg_instructions = 
        args_in_reg 
        |> Util.ListHelper.combine_safe available_register_params
        |> List.fold_left (fun acc (reg, tte) ->
          let data_size = Option.value ~default:Q @@ data_size_of_int64 @@ sizeofn rprogram tte.expr_rktype in
          let _last_reg, instructions = translate_tac_expression ~str_lit_map ~target_dst:(`Register {size = data_size; reg}) rprogram fd tte in
          acc @ instructions
        ) [] in

      let set_on_stack_instructions = [] (* TODO *) in
      let count_float_parameters = tac_parameters |> List.fold_left (fun acc tte -> 
        acc + if KosuIrTyped.Asttyhelper.RType.is_64bits_float tte.expr_rktype then 1 else 0
      ) 0 |> Int64.of_int in

      let variadic_float_use_instruction = if external_func_decl.is_variadic then 
        [Instruction (Mov {size = B; destination = `Register (sized_register B RAX); source = `ILitteral count_float_parameters})] 
      else 
        [] 
      in  

      let call_instructions = FrameManager.call_instruction ~origin:(`Label fn_label) [] fd in

      let return_reg_data_size = Option.value ~default:Q @@ data_size_of_int64 @@ sizeofn rprogram rval_rktype in
      let return_reg = sized_register return_reg_data_size RAX in
      
      let extern_instructions = 
        match KosuIrTyped.Asttyconvert.Sizeof.discardable_size return_size with
        | true ->
            let copy_instruction =
              where
              |> Option.map (fun waddress ->
                     match is_deref with
                     | Some pointer ->
                         Instruction
                           (Mov
                              {
                                size = Q;
                                destination = `Register rdiq;
                                source = `Address pointer;
                              })
                         :: copy_from_reg return_reg (create_address_offset rdiq)
                              external_func_decl.return_type rprogram
                     | None ->
                         copy_from_reg return_reg waddress
                           external_func_decl.return_type rprogram)
              |> Option.value ~default:[]
            in
            ( set_in_reg_instructions @ set_on_stack_instructions @ variadic_float_use_instruction  @ call_instructions
              @ copy_instruction)
        | false -> 
          let copy_instruction =
            where
            |> Option.map (fun waddress ->
                   match is_deref with
                   | Some pointer ->
                       [
                         Instruction
                           (
                            Mov {
                              size = Q;
                              destination = `Register rdiq;
                              source = `Address pointer
                            }
                           );
                         Instruction
                           (
                            Mov {
                              size = Q;
                              destination = `Register rdiq;
                              source = `Address (create_address_offset rdiq)
                            }
                           );
                       ]
                   | None ->
                       [
                         Instruction (
                          Lea {
                            size = Q;
                            destination = rdiq;
                            source = waddress 
                          }
                         );
                       ])
            |> Option.value ~default:[]
          in
          ( set_in_reg_instructions @ set_on_stack_instructions @ copy_instruction
            @ call_instructions )
    in
        extern_instructions
      | RSyscall_Decl syscall_decl -> 
        let set_on_reg_instructions =
        tac_parameters
        |> Util.ListHelper.combine_safe syscall_arguments_register
        |> List.fold_left
             (fun acc (reg, tte) ->
              let data_size = Option.value ~default:Q @@ data_size_of_int64 @@ sizeofn rprogram tte.expr_rktype in
              let _last_reg, instructions = translate_tac_expression ~str_lit_map ~target_dst:(`Register {size = data_size; reg}) rprogram fd tte in
              acc @ instructions
          ) []
      in
      let move_code_syscall_instructions = [
        Line_Com (Comment ("syscall " ^ syscall_decl.rsyscall_name));
        Instruction (Mov {size = D; source = `ILitteral syscall_decl.opcode; destination = `Register (sized_register D RAX)});
        Instruction Syscall
      ] in
      let return_reg_data_size = Option.value ~default:Q @@ data_size_of_int64 @@ sizeofn rprogram rval_rktype in
      
      let copy_result_instruction = where |> Option.map (fun waddress -> 
        let return_reg = sized_register return_reg_data_size RAX in
        let r9 = (tmp_r9 8L) in
        match is_deref with
        | Some pointer -> 
          Instruction (Mov {size = Q; destination = `Register r9  ; source = `Address pointer})
          :: copy_from_reg return_reg (create_address_offset r9) syscall_decl.return_type rprogram
        | None -> copy_from_reg return_reg waddress syscall_decl.return_type rprogram
      ) |> Option.value ~default:[] in
      set_on_reg_instructions @ move_code_syscall_instructions @ copy_result_instruction
      | RKosufn_Decl _ -> 
        let typed_parameters =
          tac_parameters |> List.map (fun { expr_rktype; _ } -> expr_rktype)
        in
        let function_decl =
          rprogram
          |> KosuIrTyped.Asttyhelper.RProgram
             .find_function_decl_exact_param_types ~module_name:fn_module
               ~fn_name ~ktypes:typed_parameters
          |> Option.get
        in
        let fn_label = Spec.label_of_kosu_function ~module_path function_decl in
        let args_in_reg, _args_on_stack =
        tac_parameters
        |> List.mapi (fun index value -> (index, value))
        |> List.partition_map (fun (index, value) ->
               if index < available_register_count then Either.left value
               else Either.right value)
      in
      let set_in_reg_instructions = 
        args_in_reg 
        |> Util.ListHelper.combine_safe available_register_params
        |> List.fold_left (fun acc (reg, tte) ->
          let data_size = Option.value ~default:Q @@ data_size_of_int64 @@ sizeofn rprogram tte.expr_rktype in
          let _last_reg, instructions = translate_tac_expression ~str_lit_map ~target_dst:(`Register {size = data_size; reg}) rprogram fd tte in
          acc @ instructions
        ) [] in

      let set_on_stack_instructions = [] (* TODO *) in
      let call_instructions = FrameManager.call_instruction ~origin:(`Label fn_label) [] fd in
      let return_reg_data_size = Option.value ~default:Q @@ data_size_of_int64 @@ sizeofn rprogram rval_rktype in
      let return_reg = sized_register return_reg_data_size RAX in
      let extern_instructions = 
        match KosuIrTyped.Asttyconvert.Sizeof.discardable_size return_size with
        | true ->
            let copy_instruction =
              where
              |> Option.map (fun waddress ->
                     match is_deref with
                     | Some pointer ->
                         Instruction
                           (Mov
                              {
                                size = Q;
                                destination = `Register rdiq;
                                source = `Address pointer;
                              })
                         :: copy_from_reg return_reg (create_address_offset rdiq)
                              function_decl.return_type rprogram
                     | None ->
                         copy_from_reg return_reg waddress
                           function_decl.return_type rprogram)
              |> Option.value ~default:[]
            in
            ( set_in_reg_instructions @ set_on_stack_instructions  @ call_instructions
              @ copy_instruction)
        | false -> 
          let copy_instruction =
            where
            |> Option.map (fun waddress ->
                   match is_deref with
                   | Some pointer ->
                       [
                         Instruction
                           (
                            Mov {
                              size = Q;
                              destination = `Register rdiq;
                              source = `Address pointer
                            }
                           );
                         Instruction
                           (
                            Mov {
                              size = Q;
                              destination = `Register rdiq;
                              source = `Address (create_address_offset rdiq)
                            }
                           );
                       ]
                   | None ->
                       [
                         Instruction (
                          Lea {
                            size = Q;
                            destination = rdiq;
                            source = waddress 
                          }
                         );
                       ])
            |> Option.value ~default:[]
          in
          ( set_in_reg_instructions @ set_on_stack_instructions @ copy_instruction
            @ call_instructions )
    in
        extern_instructions
    )

    | _ -> failwith ""
end


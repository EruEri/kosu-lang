open X86_64Core
open X86_64Core.Register
open X86_64Core.Operande
open X86_64Core.Instruction
open KosuIrTyped.Asttyconvert.Sizeof
open KosuIrTAC.Asttac
open Util

let sizeofn = KosuIrTyped.Asttyconvert.Sizeof.sizeof

module X86Program = Common.AsmProgram(X86_64Core.Instruction)
open X86Program

module Make(Spec: Common.AsmSpecification) = struct

  let translate_tac_expression ~str_lit_map ?(target_dst = (`Register {size = L; reg = R10} : dst))
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
    

let translate_tac_binop ~str_lit_map ~cc ~blhs ~brhs ~where rval_rktype rprogram fd  = 
      let r10 = tmp_r10_ktype rprogram brhs.expr_rktype in
      let rax = tmp_rax_ktype rprogram brhs.expr_rktype in
      let data_size = data_size_of_int64_def @@ sizeofn rprogram rval_rktype in
      let _right_reg, rinstructions =
        translate_tac_expression ~str_lit_map ~target_dst:(`Register r10)rprogram fd brhs
      in
      let _left_reg, linstructions =
        translate_tac_expression ~str_lit_map ~target_dst:(`Register rax) rprogram fd blhs
      in
      let copy_instructions =
        where
        |> Option.map (fun waddress ->
                let equal_instruction =
                  [
                    Instruction (
                      Cmp {
                        size = data_size;
                        lhs = `Register (resize_register data_size rax);
                        rhs = `Register (resize_register data_size r10)
                      }
                    )
                    ;
                    Instruction (
                      Set {
                        cc = cc;
                        register = rax;
                      }
                    )
                  ]
                in
                equal_instruction
                @ copy_from_reg rax waddress rval_rktype rprogram)
        |> Option.value ~default:[]
      in
      rinstructions @ linstructions @ copy_instructions

let translate_tac_binop_self ~str_lit_map ~blhs ~brhs ~where fbinop rval_rktype rprogram fd = 
  let r10 = tmp_r10_ktype rprogram blhs.expr_rktype in
  let rax = tmp_rax_ktype rprogram brhs.expr_rktype in
  let right_reg, rinstructions =
    translate_tac_expression ~str_lit_map ~target_dst:(`Register r10) rprogram fd brhs
  in
  let left_reg, linstructions =
    translate_tac_expression ~str_lit_map ~target_dst:(`Register rax) rprogram fd blhs
  in

  let left_reg = Operande.register_of_dst left_reg in
  let right_reg = Operande.register_of_dst right_reg in

  let mult_instruction = fbinop ~size:(left_reg.size) ~destination:(`Register left_reg) ~source:(`Register right_reg) in
  let copy_instruction =
    where
    |> Option.map (fun waddress ->
           copy_from_reg left_reg waddress rval_rktype rprogram)
    |> Option.value ~default:[]
  in
  
  linstructions @ rinstructions @ (mult_instruction @ copy_instruction)
  
   

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
        Instruction (Mov {size = L; source = `ILitteral syscall_decl.opcode; destination = `Register (sized_register L RAX)});
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
      let kosu_fn_instructions = 
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
      kosu_fn_instructions
    )
    | RVTuple ttes ->
      let ktlis = ttes |> List.map (fun { expr_rktype; _ } -> expr_rktype) in
      let offset_list =
        ttes
        |> List.mapi (fun index _value ->
               offset_of_tuple_index index ktlis rprogram)
        |> List.tl
        |> fun l -> l @ [ 0L ]
      in
        where
        |> Option.map (fun waddress ->
               ttes
               |> List.mapi (fun index value -> (index, value))
               |> List.fold_left
                    (fun (accumuled_adre, acc) (index, tte) ->
                      let reg_texp, instructions =
                        translate_tac_expression rprogram ~str_lit_map ~target_dst:(`Address accumuled_adre) fd tte
                      in
                      let increment_adress = increment_adress (List.nth offset_list index) accumuled_adre in
                      let acc_plus = acc @ instructions in
                       match reg_texp with
                      | `Address _ -> increment_adress, acc_plus
                      | `Register reg ->
                      ( increment_adress ,
                        acc_plus @ copy_from_reg reg accumuled_adre tte.expr_rktype rprogram )
                      ) (waddress, [])
                    
               |> snd)
        |> Option.value ~default:[]
        | RVFieldAcess
        {
          first_expr = { expr_rktype; tac_expression = TEIdentifier struct_id };
          field;
        } ->
        let struct_decl =
          match
            KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
              expr_rktype rprogram
          with
          | Some (RDecl_Struct s) -> s
          | Some (RDecl_Enum _) ->
              failwith "Expected to find a struct get an enum"
          | None -> failwith "Non type decl ??? my validation is very weak"
        in

        let generics =
          expr_rktype
          |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
          |> List.combine struct_decl.generics
          |> List.to_seq |> Hashtbl.of_seq
        in

        let offset = offset_of_field ~generics field struct_decl rprogram in
        let struct_address =
          FrameManager.address_of (struct_id, expr_rktype) fd |> fun adr ->
          match adr with
          | Some a -> a
          | None -> failwith "field access null address"
        in
        let field_address = increment_adress offset struct_address in
        let sizeof = sizeofn rprogram rval_rktype in

        let size = data_size_of_int64_def sizeof in
        let tmpreg = tmp_r10 sizeof in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 [
                 Instruction (Mov {
                    size;
                    destination = `Register tmpreg;
                    source = `Address field_address
                 }) 
                 ]
                 @ copy_from_reg tmpreg waddress rval_rktype rprogram)
          |> Option.value ~default:[]
        in
        Line_Com (Comment ("Field access of " ^ field)) :: copy_instructions
        
    | RVFieldAcess _ ->
        failwith "Wierd : Fields access force struct as an identifier"

        | RVAdress id ->
          let pointee_type =
            rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee
          in
          let adress =
            FrameManager.address_of (id, pointee_type) fd |> fun adr ->
            match adr with
            | Some a -> a
            | None -> failwith "address of null address"
          in
          let copy_instruction =
            where
            |> Option.map (fun waddress ->
              [
                Instruction (Lea {
                  size = Q;
                  destination = raxq;
                  source = adress
                })
              ]
                   @ copy_from_reg raxq waddress rval_rktype rprogram)
            |> Option.value ~default:[]
          in
          copy_instruction
    | RVDefer id ->
      let adress =
        FrameManager.address_of
          (id, rval_rktype |> KosuIrTyped.Asttyhelper.RType.rpointer)
          fd
        |> fun adr ->
        match adr with
        | Some a -> a
        | None -> failwith "defer of null address"
      in
      let r10q = (tmp_r10 8L) in
      let load_instruction =
        [ 
          Instruction (Mov {
            size = Q;
            destination = `Register r10q;
            source = `Address adress
          })
        ]
      in
      let last_reg, load_indirect =
        if
          is_register_size
          @@ KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram rval_rktype
        then
          ( raxq,
            [
              Instruction (
                Mov {
                  size = Q;
                  destination = `Register raxq;
                  source = `Address (create_address_offset r10q)
                }
              )
            ] )
        else (r10q, [])
      in

      let copy_instructions =
        where
        |> Option.map (fun waddress ->
                load_instruction @ load_indirect
                @ copy_from_reg last_reg waddress rval_rktype rprogram)
        |> Option.value ~default:[]
      in

      copy_instructions
    | RVEnum { variant; assoc_tac_exprs; _ } ->
      let enum_decl =
        match
          KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
            rval_rktype rprogram
        with
        | Some (RDecl_Struct _) ->
            failwith "Expected to find an enum get an struct"
        | Some (RDecl_Enum e) -> e
        | None -> failwith "Non type decl ??? my validation is very weak"
      in
      let tag =
        KosuIrTyped.Asttyhelper.Renum.tag_of_variant variant enum_decl
      in
      let enum_tte_list =
        assoc_tac_exprs
        |> List.cons
              {
                expr_rktype = RTInteger (Unsigned, I32);
                tac_expression = TEInt (Unsigned, I32, Int64.of_int32 tag);
              }
      in
      let enum_type_list =
        enum_tte_list |> List.map (fun { expr_rktype; _ } -> expr_rktype)
      in
      let offset_list =
        enum_tte_list
        |> List.mapi (fun index { expr_rktype = _; _ } ->
                offset_of_tuple_index index enum_type_list rprogram)
      in
      (* let () = offset_list |> List.map (Printf.sprintf "%Lu") |> String.concat ", " |> Printf.printf "%s::%s off = [%s]\n" enum_decl.renum_name variant in *)
      where
      |> Option.map (fun waddress ->
             enum_tte_list
             |> List.mapi (fun index value -> (index, value))
             |> List.fold_left
                  (fun (accumuled_adre, acc) (index, tte) ->
                    let reg_texp, instructions =
                      translate_tac_expression rprogram ~str_lit_map ~target_dst:(`Address accumuled_adre) fd tte
                    in
                    let increment_adress = increment_adress (List.nth offset_list index) accumuled_adre in
                    let acc_plus = acc @ instructions in
                     match reg_texp with
                    | `Address _ -> increment_adress, acc_plus
                    | `Register reg ->
                    ( increment_adress ,
                      acc_plus @ copy_from_reg reg accumuled_adre tte.expr_rktype rprogram )
                    ) (waddress, [])
                  
             |> snd)
      |> Option.value ~default:[]
    | RVDiscard | RVLater -> []
    | RVBuiltinBinop { binop = TacBool TacOr; blhs; brhs } ->
      let r9 = tmp_r9_ktype rprogram brhs.expr_rktype in
      let rax = tmp_rax_ktype rprogram blhs.expr_rktype in
      let _right_reg, rinstructions =
        translate_tac_expression ~str_lit_map ~target_dst:(`Register r9) rprogram fd brhs
      in
      let left_reg, linstructions =
        translate_tac_expression ~str_lit_map ~target_dst:(`Register rax) rprogram fd blhs
      in
      let copy_instructions =
        where
        |> Option.map (fun waddress ->
               let or_instruction =
                 [
                  Instruction (
                    Or {
                      size = B;
                      destination = left_reg;
                      source = `Register (resize_register B r9)
                    }
                  )
                 ]
               in
               or_instruction @ copy_from_reg rax waddress rval_rktype rprogram)
        |> Option.value ~default:[]
      in
      rinstructions @ linstructions @ copy_instructions
      | RVBuiltinBinop { binop = TacBool TacAnd; blhs; brhs } ->
        let r9 = tmp_r9_ktype rprogram brhs.expr_rktype in
        let rax = tmp_rax_ktype rprogram blhs.expr_rktype in
        let _right_reg, rinstructions =
          translate_tac_expression ~str_lit_map ~target_dst:(`Register r9) rprogram fd brhs
        in
        let left_reg, linstructions =
          translate_tac_expression ~str_lit_map ~target_dst:(`Register rax) rprogram fd blhs
        in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 let or_instruction =
                   [
                    Instruction (
                      And {
                        size = B;
                        destination = left_reg;
                        source = `Register (resize_register B r9)
                      }
                    )
                   ]
                 in
                 or_instruction @ copy_from_reg rax waddress rval_rktype rprogram)
          |> Option.value ~default:[]
        in
        rinstructions @ linstructions @ copy_instructions
    | RVBuiltinBinop { binop = TacBool bool_binop; blhs; brhs } ->
      let is_ptr = KosuIrTyped.Asttyhelper.RType.is_pointer blhs.expr_rktype || KosuIrTyped.Asttyhelper.RType.is_pointer brhs.expr_rktype in
      let cc = Option.get @@ Condition_Code.cc_of_tac_bin ~is_ptr bool_binop in
      translate_tac_binop ~str_lit_map ~cc ~blhs ~brhs ~where rval_rktype rprogram fd

    | RVBuiltinBinop {binop = TacSelf (TacMult | TacBitwiseAnd | TacBitwiseOr | TacBitwiseXor | TacShiftLeft | TacShiftRight as self_binop); blhs; brhs} -> 
      (* let size = data_size_of_int64_def @@ sizeofn rprogram blhs.expr_rktype in *)
      let is_unsigned = KosuIrTyped.Asttyhelper.RType.is_unsigned_integer blhs.expr_rktype in
      let binop_func = binop_instruction_of_tacself ~unsigned:is_unsigned self_binop in
      translate_tac_binop_self ~str_lit_map ~blhs ~brhs ~where binop_func rval_rktype rprogram fd
    | RVBuiltinBinop {binop = TacSelf ( TacAdd | TacMinus as self_binop); blhs; brhs } -> 
      begin  match KosuIrTyped.Asttyhelper.RType.is_pointer rval_rktype with
      | false -> 
        let binop_func = binop_instruction_of_tacself self_binop in
        translate_tac_binop_self ~str_lit_map ~blhs ~brhs ~where binop_func rval_rktype rprogram fd
      | true -> 
        let pointee_size =
          rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee
          |> KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram
        in
        let ptr_reg, linstructions =
        translate_tac_expression ~str_lit_map ~target_dst:(`Register raxq)
          rprogram fd blhs
      in
      let nb_reg, rinstructions =
        translate_tac_expression ~str_lit_map ~target_dst:(`Register (tmp_r10 8L))
          rprogram fd brhs
      in

      let size_reg = register_of_dst nb_reg in

      let operator_function_instruction = binop_instruction_of_tacself self_binop in

      let scale_instruction = 
        if pointee_size = 1L then [] 
        else 
          ins_mult ~size:size_reg.size ~destination:(`Register size_reg) ~source:(`ILitteral pointee_size) 
      in 

      let ptr_true_reg = register_of_dst ptr_reg in
      let copy_instructions = where |> Option.map (fun waddress -> 
        copy_from_reg (resize_register Q ptr_true_reg) waddress rval_rktype rprogram
      ) |> Option.value ~default:[]
      in
        linstructions 
          @ rinstructions 
          @ scale_instruction 
          @ (operator_function_instruction ~size:size_reg.size ~destination:ptr_reg ~source:(`Register size_reg)) 
          @ copy_instructions
    end
    | RVBuiltinBinop {binop = TacSelf TacDiv; blhs = dividende; brhs = divisor} -> 
      let unsigned = KosuIrTyped.Asttyhelper.RType.is_unsigned_integer dividende.expr_rktype in
      let last_reg9, divisor_instructions = translate_tac_expression ~str_lit_map ~target_dst:(`Register (tmp_r10 8L) ) rprogram fd divisor in
      let raw_data_size = data_size_of_int64_def @@ sizeofn rprogram dividende.expr_rktype in
      let scaled_data_size = match raw_data_size with
      | Q -> Q
      | _ -> L 
    in 

      let _, instructions = translate_tac_expression ~str_lit_map ~target_dst:(`Register raxq) rprogram fd dividende in
      let setup_div = Instruction (division_split scaled_data_size) in
      
      let r9 = register_of_dst last_reg9 in
      let divi_instruction = division_instruction ~unsigned scaled_data_size (`Register r9) in

      let copy_instructions =
        where
        |> Option.map (fun waddress ->
          copy_from_reg (resize_register raw_data_size raxq) waddress rval_rktype rprogram
          )
        |> Option.value ~default:[]
      in
    instructions @ divisor_instructions @ setup_div::divi_instruction::copy_instructions

    | RVBuiltinBinop {binop = TacSelf TacModulo; blhs = dividende; brhs = divisor} -> 
      let unsigned = KosuIrTyped.Asttyhelper.RType.is_unsigned_integer dividende.expr_rktype in
      let last_reg9, divisor_instructions = translate_tac_expression ~str_lit_map ~target_dst:(`Register (tmp_r10 8L) ) rprogram fd divisor in
      let raw_data_size = data_size_of_int64_def @@ sizeofn rprogram dividende.expr_rktype in
      let scaled_data_size = match raw_data_size with
      | Q -> Q
      | _ -> L 
    in 

      let _, instructions = translate_tac_expression ~str_lit_map ~target_dst:(`Register raxq) rprogram fd dividende in
      let setup_div = Instruction (division_split scaled_data_size) in
      
      let r9 = register_of_dst last_reg9 in
      let divi_instruction = division_instruction ~unsigned scaled_data_size (`Register r9) in

      let copy_instructions =
        where
        |> Option.map (fun waddress ->
          copy_from_reg (sized_register raw_data_size RDX) waddress rval_rktype rprogram
          )
        |> Option.value ~default:[]
      in
    instructions @ divisor_instructions @ setup_div::divi_instruction::copy_instructions

     | RVBuiltinUnop { unop = TacUminus; expr } ->
        let rax = tmp_rax_ktype rprogram expr.expr_rktype in
        let last_reg, instructions =
          translate_tac_expression ~str_lit_map ~target_dst:(`Register rax) rprogram fd expr
        in
        let last_reg = Operande.register_of_dst last_reg in
        let uminus_instructions =
          Instruction (Neg { size = last_reg.size; source = last_reg })
        in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 copy_from_reg last_reg waddress rval_rktype rprogram)
          |> Option.value ~default:[]
        in
        instructions @ (uminus_instructions :: copy_instructions)

      | RVBuiltinUnop { unop = TacNot; expr } ->
        let rax = tmp_rax_ktype rprogram expr.expr_rktype in
        let last_reg, instructions =
          translate_tac_expression ~str_lit_map ~target_dst:(`Register rax) rprogram fd expr
        in
        let last_reg = Operande.register_of_dst last_reg in
        let uminus_instructions =
          Instruction (Not { size = last_reg.size; source = last_reg })
        in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 copy_from_reg last_reg waddress rval_rktype rprogram)
          |> Option.value ~default:[]
        in
        instructions @ (uminus_instructions :: copy_instructions)

    | RVBuiltinCall { fn_name; parameters } -> (
      let open KosuFrontend.Ast.Builtin_Function in
      match fn_name with
      | Tos8 | Tou8 | Tos16 | Tou16 | Tos32 | Tou32 | Tos64 | Tou64 | Stringl_ptr  ->
        let paramter = List.hd parameters in
        let data_size = data_size_of_isize @@ KosuFrontend.Ast.Builtin_Function.isize_of_functions fn_name in
        let target_reg_rax = sized_register data_size RAX in
        let _, instructions = translate_tac_expression ~str_lit_map ~target_dst:(`Register target_reg_rax) rprogram fd paramter in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 copy_from_reg target_reg_rax waddress rval_rktype
                   rprogram)
          |> Option.value ~default:[]
        in
        instructions @ copy_instructions
    )
    | _ -> failwith ""


    let rec translate_tac_statement ~str_lit_map current_module rprogram
      (fd : FrameManager.frame_desc) = function
      | STacDeclaration { identifier; trvalue }
      | STacModification { identifier; trvalue } ->
          let address =
            FrameManager.address_of (identifier, trvalue.rval_rktype) fd
          in
          let instructions =
            translate_tac_rvalue ~str_lit_map ~where:address current_module
              rprogram fd trvalue
          in
          instructions
      | STDerefAffectation { identifier; trvalue } ->
        let tmpreg =
          tmp_r9_ktype rprogram
            (KosuIrTyped.Asttyhelper.RType.rpointer trvalue.rval_rktype)
        in
        let intermediary_adress =
          FrameManager.address_of
            ( identifier,
              KosuIrTyped.Asttyhelper.RType.rpointer trvalue.rval_rktype )
            fd
        in
        let instructions =
          Instruction
            (Mov
                {
                  size = Q;
                  destination = `Register raxq;
                  source = `Address (Option.get intermediary_adress) ;
                })
        in
        let true_adress = create_address_offset tmpreg in
        let true_instructions =
          translate_tac_rvalue ~str_lit_map ~is_deref:intermediary_adress
            ~where:(Some true_adress) current_module rprogram fd trvalue
        in
          Line_Com (Comment "Defered Start") :: instructions
          :: true_instructions
          @ [ Line_Com (Comment "Defered end") ]

    | STIf
    {
      statement_for_bool;
      condition_rvalue;
      goto1;
      goto2;
      exit_label;
      if_tac_body;
      else_tac_body;
    } ->
    let stmts_bool =
      statement_for_bool
      |> List.fold_left
            (fun acc stmt ->
              acc
              @ (translate_tac_statement ~str_lit_map current_module rprogram
                  fd stmt
                ))
            []
    in
    let last_reg, condition_rvalue_inst =
      translate_tac_expression ~str_lit_map rprogram fd condition_rvalue
    in
    let data_size = match last_reg with | `Address _ -> Q | `Register reg -> reg.size in 
    let lhs = src_of_dst last_reg in
    let cmp =
      Instruction (Cmp { size = data_size; lhs; rhs = `ILitteral 1L })
    in
    let jmp = Instruction (Jmp { cc = Some E; where = `Label goto1 }) in
    let jmp2 = Instruction (Jmp { cc = None; where = `Label goto2 }) in
    let if_block =
      translate_tac_body ~str_lit_map ~end_label:(Some exit_label)
        current_module rprogram fd if_tac_body
    in
    let else_block =
      translate_tac_body ~str_lit_map ~end_label:(Some exit_label)
        current_module rprogram fd else_tac_body
    in
    let exit_label_instr = Label exit_label in
      stmts_bool @ condition_rvalue_inst
      @ (cmp :: jmp :: jmp2 :: if_block)
      @ else_block @ [ exit_label_instr ]

    | SCases { cases; else_tac_body; exit_label } ->
      let cases_body, cases_condition =
        cases
        |> List.map (fun scases ->
                (* furute optimisation : do only si else branch*)
                let setup_next_label_instr =
                  scases.condition_label
                  |> Option.map (fun label -> Label label)
                  |> Option.to_list
                in
                let setup_condition_insts =
                  scases.statement_for_condition
                  |> List.map (fun stmt ->
                     translate_tac_statement ~str_lit_map current_module
                              rprogram fd stmt)
                  |> List.flatten
                in
                let last_reg, condition =
                  translate_tac_expression ~str_lit_map rprogram fd
                    scases.condition
                in
                let lhs = src_of_dst last_reg in
                let cmp =
                  Instruction
                    (Cmp { size = B; lhs; rhs = `ILitteral 1L })
                in
                let if_true_instruction =
                  Instruction (Jmp { cc = Some E; where = `Label scases.goto })
                in
                let if_false_instruction =
                  Instruction (Jmp { cc = None; where = `Label scases.jmp_false })
                in
                let body_instruction =
                  translate_tac_body ~str_lit_map
                    ~end_label:(Some scases.end_label) current_module rprogram
                    fd scases.tac_body
                in
                ( body_instruction,
                  setup_next_label_instr @ setup_condition_insts @ condition
                  @ [ cmp; if_true_instruction; if_false_instruction ] ))
        |> List.split
        |> fun (lhs, rhs) -> (List.flatten lhs, List.flatten rhs)
      in
      let _else_jump =
        Instruction (Jmp { cc = None; where = `Label else_tac_body.label })
      in
      let end_label_instruction = Label exit_label in
      let else_body_instruction =
        translate_tac_body ~str_lit_map ~end_label:(Some exit_label)
          current_module rprogram fd else_tac_body
      in
        cases_condition @ cases_body @ else_body_instruction
        @ [ end_label_instruction ]
    | STSwitch
    {
      statemenets_for_case;
      condition_switch;
      sw_cases;
      wildcard_label;
      wildcard_body;
      sw_exit_label;
    } ->
    let tag_of_variant = KosuIrTyped.Asttyhelper.Renum.tag_of_variant in

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
    let exit_label_instruction = Label sw_exit_label in
    let setup_instructions =
      statemenets_for_case
      |> List.fold_left
            (fun acc_stmts value ->
              let insts =
                translate_tac_statement ~str_lit_map current_module rprogram
                  fd value
              in
              acc_stmts @ insts
            ) []
    in
    let last_dst, condition_switch_instruction =
      translate_tac_expression ~str_lit_map rprogram fd condition_switch
    in
    let copy_tag =
      if is_register_size (sizeofn rprogram condition_switch.expr_rktype)
      then
        Instruction
          (Mov
              {
                size = L;
                destination = `Register (sized_register L R10);
                source = src_of_dst last_dst;
              })
      else
        Instruction
          (Mov
              {
                size = Q;
                destination = `Register (sized_register Q R10);
                source = src_of_dst last_dst;
              })
    in
    let switch_variable_name =
      match condition_switch.tac_expression with
      | TEIdentifier id -> id
      | _ -> failwith "I need to get the id"
    in
    (* Tag fetch proper doing*)
    let cmp_instrution_list, fn_block =
      sw_cases
      |> List.map (fun sw_case ->
              let jump_condition =
                sw_case.variants_to_match
                |> List.map (fun mvariant ->
                      let tag = tag_of_variant mvariant enum_decl in
                      let compare =
                        Instruction
                          (Cmp
                              {
                                size = L;
                                lhs = `Register (sized_register L R10);
                                rhs = `ILitteral (Int64.of_int32 tag);
                              })
                      in
                      let assoc_type_for_variants =
                        KosuIrTyped.Asttyhelper.Renum.assoc_types_of_variant
                          ~tagged:true mvariant enum_decl
                      in
                      let fetch_offset_instruction =
                        sw_case.assoc_bound
                        |> List.map (fun (index, id, ktyte) ->
                                let offset_a =
                                  offset_of_tuple_index (index + 1)
                                    assoc_type_for_variants rprogram
                                in
                                let switch_variable_address =
                                  FrameManager.address_of
                                    ( switch_variable_name,
                                      condition_switch.expr_rktype )
                                    fd
                                  |> Option.get
                                in
                                let destination_address =
                                  FrameManager.address_of (id, ktyte) fd
                                  |> Option.get
                                in
                                let size_of_ktype = sizeofn rprogram ktyte in
                                let data_size = data_size_of_int64_def  size_of_ktype in
                                let copy_instructions =
                                  if is_register_size size_of_ktype then
                                    [
                                      Instruction
                                        (Mov
                                          {
                                            size = data_size;
                                            destination =
                                              `Register (tmp_rax size_of_ktype);
                                            source = `Address (
                                              increment_adress offset_a
                                                switch_variable_address);
                                          });
                                      Instruction
                                        (Mov
                                          {
                                            size = data_size;
                                            source = `Register (tmp_rax size_of_ktype);
                                            destination = `Address destination_address;
                                          });
                                    ]
                                  else
                                    let leaq  = 
                                      Instruction ( Lea {
                                        size = Q;
                                        destination = raxq;
                                        source = increment_adress offset_a switch_variable_address
                                      })
                                    in
                                    leaq:: copy_from_reg (tmp_rax size_of_ktype)
                                        destination_address ktyte rprogram
                                in

                                copy_instructions)
                        |> List.flatten
                      in
                      let jump_true =
                        Instruction
                          (Jmp { cc = Some E; where = `Label sw_case.sw_goto })
                      in
                      fetch_offset_instruction @ [ compare; jump_true ])
                |> List.flatten
              in
              let genete_block =
                translate_tac_body ~str_lit_map
                  ~end_label:(Some sw_case.sw_exit_label) current_module
                  rprogram fd sw_case.switch_tac_body
              in
              (jump_condition, genete_block))
      |> List.split
      |> fun (lhs, rhs) -> (List.flatten lhs, List.flatten rhs)
    in
    let wildcard_case_jmp =
      wildcard_label
      |> Option.map (fun lab -> Instruction (Jmp { cc = None; where = `Label lab }))
      |> Option.to_list
    in
    let wildcard_body_block =
      wildcard_body
      |> Option.map (fun body ->
              translate_tac_body ~str_lit_map ~end_label:(Some sw_exit_label)
                current_module rprogram fd body)
      |> Option.value ~default:[]
    in
      setup_instructions @ condition_switch_instruction
      @ (copy_tag :: cmp_instrution_list)
      @ wildcard_case_jmp @ fn_block @ wildcard_body_block
      @ [ exit_label_instruction ]

  and translate_tac_body ~str_lit_map ?(end_label = None) current_module
      rprogram (fd : FrameManager.frame_desc) { label; body } =
    let label_instr = Label label in
    let stmt_instr =
      body |> fst
      |> List.map (fun stmt ->
         translate_tac_statement ~str_lit_map current_module rprogram fd
                  stmt)
      |> List.flatten
    in
    let end_label_inst =
      end_label
      |> Option.map (fun lab -> Instruction (Jmp { cc = None; where = `Label lab }))
      |> Option.to_list
    in
    let return_instr =
      body |> snd
      |> Option.map (fun tte ->
             let last_reg, instructions =
               translate_tac_expression ~str_lit_map rprogram fd tte
             in
             let sizeof = sizeofn rprogram tte.expr_rktype in
             let return_reg =
               tmp_rax
                 sizeof
             in
             instructions
             @
             if is_register_size sizeof then
               Instruction
                 (Mov
                    {
                      size = return_reg.size;
                      destination = `Register return_reg;
                      source = src_of_dst last_reg;
                    })
               :: []
             else
               let x8_address =
                 Option.get @@ FrameManager.(address_of indirect_return_vt fd)
               in
               let str =
                 Instruction
                   (Mov
                      {
                        size = Q;
                        destination = `Register rdiq;
                        source = `Address x8_address;
                      })
               in

               str :: copy_large ~address_str:(create_address_offset rdiq) ~base_address_reg:(address_of_dst last_reg) sizeof)
      |> Option.value ~default:[]
    in
    (label_instr :: stmt_instr) @ return_instr @ end_label_inst

    let asm_module_of_tac_module ~str_lit_map current_module rprogram =
      let open KosuIrTyped.Asttyped in
      function
      | TacModule tac_nodes ->
          tac_nodes
          |> List.filter_map (fun node ->
                 match node with
                 | TNFunction function_decl ->
                     let register_param_count = List.length argument_registers in
                     let fn_register_params, stack_param =
                       function_decl.rparameters
                       |> List.mapi (fun index value -> (index, value))
                       |> List.partition_map (fun (index, value) ->
                              if index < register_param_count then Either.left value
                              else Either.right value)
                     in
                     let stack_param_count =
                       Int64.of_int (function_decl.stack_params_count * 8)
                     in
                     let locals_var =
                       function_decl.locale_var
                       |> List.map (fun { locale_ty; locale } ->
                              match locale with
                              | Locale s -> (s, locale_ty)
                              | Enum_Assoc_id { name; _ } -> (name, locale_ty))
                     in
                     (* let () = locals_var |> List.map (fun (s, kt) -> Printf.sprintf "%s : %s " (s) (KosuIrTyped.Asttypprint.string_of_rktype kt)) |> String.concat ", " |> Printf.printf "%s : locale variables = [%s]\n" function_decl.rfn_name in *)
                     let asm_name = Spec.label_of_tac_function ~module_path:current_module function_decl
                     in
                     let fd =
                       FrameManager.frame_descriptor
                         ~stack_future_call:stack_param_count ~fn_register_params
                         ~stack_param ~return_type:function_decl.return_type
                         ~locals_var
                         ~discarded_values:function_decl.discarded_values rprogram
                     in
                     let prologue =
                       FrameManager.function_prologue
                         ~fn_register_params:function_decl.rparameters
                         ~stack_params:stack_param rprogram fd
                     in
                     let conversion =
                       translate_tac_body ~str_lit_map current_module
                         rprogram fd function_decl.tac_body
                     in
                     let epilogue = FrameManager.function_epilogue fd in
                     (* let () = Printf.printf "\n\n%s:\n" function_decl.rfn_name in
                        let () = fd.stack_map |> IdVarMap.to_seq |> Seq.iter (fun ((s, kt), adr) ->
                          Printf.printf "%s : %s == [%s, %Ld]\n"
                          (s)
                          (KosuIrTyped.Asttypprint.string_of_rktype kt)
                          (Aarch64Pprint.string_of_register adr.base)
                          (adr.offset)
                          ) in *)
                     Some
                       (Afunction
                          {
                            asm_name;
                            asm_body =
                              [ Directive "cfi_startproc" ]
                              @ prologue @ (conversion |> List.tl) @ epilogue
                              @ [ Directive "cfi_endproc" ];
                          })
                 | TNOperator (TacUnary unary_decl as self) ->
                     let stack_param_count =
                       Int64.of_int (unary_decl.stack_params_count * 8)
                     in
                     let locals_var =
                       unary_decl.locale_var
                       |> List.map (fun { locale_ty; locale } ->
                              match locale with
                              | Locale s -> (s, locale_ty)
                              | Enum_Assoc_id { name; _ } -> (name, locale_ty))
                     in
                     (* let () = locals_var |> List.map (fun (s, kt) -> Printf.sprintf "%s : %s " (s) (KosuIrTyped.Asttypprint.string_of_rktype kt)) |> String.concat ", " |> Printf.printf "%s : locale variables = [%s]\n" function_decl.rfn_name in *)
                     let asm_name = unary_decl.asm_name in
                     let fd =
                       FrameManager.frame_descriptor
                         ~stack_future_call:stack_param_count
                         ~fn_register_params:[ unary_decl.rfield ] ~stack_param:[]
                         ~return_type:unary_decl.return_type ~locals_var
                         ~discarded_values:unary_decl.discarded_values rprogram
                     in
                     let prologue =
                       FrameManager.function_prologue
                         ~fn_register_params:[ unary_decl.rfield ] ~stack_params:[]
                         rprogram fd
                     in
                     let conversion =
                       translate_tac_body ~str_lit_map current_module
                         rprogram fd
                         (KosuIrTAC.Asttachelper.OperatorDeclaration.tac_body self)
                     in
                     let epilogue = FrameManager.function_epilogue fd in
                     Some
                       (Afunction
                          {
                            asm_name;
                            asm_body =
                              [ Directive "cfi_startproc" ]
                              @ prologue @ (conversion |> List.tl) @ epilogue
                              @ [ Directive "cfi_endproc" ];
                          })
                 | TNOperator (TacBinary binary as self) ->
                     let stack_param_count =
                       Int64.of_int (binary.stack_params_count * 8)
                     in
                     let locals_var =
                       binary.locale_var
                       |> List.map (fun { locale_ty; locale } ->
                              match locale with
                              | Locale s -> (s, locale_ty)
                              | Enum_Assoc_id { name; _ } -> (name, locale_ty))
                     in
                     (* let () = locals_var |> List.map (fun (s, kt) -> Printf.sprintf "%s : %s " (s) (KosuIrTyped.Asttypprint.string_of_rktype kt)) |> String.concat ", " |> Printf.printf "%s : locale variables = [%s]\n" function_decl.rfn_name in *)
                     let asm_name = binary.asm_name in
                     let lhs_param, rhs_param = binary.rfields in
                     let fn_register_params = [ lhs_param; rhs_param ] in
                     let fd =
                       FrameManager.frame_descriptor
                         ~stack_future_call:stack_param_count ~fn_register_params
                         ~stack_param:[] ~return_type:binary.return_type ~locals_var
                         ~discarded_values:binary.discarded_values rprogram
                     in
                     let prologue =
                       FrameManager.function_prologue ~fn_register_params
                         ~stack_params:[] rprogram fd
                     in
                     let conversion =
                       translate_tac_body ~str_lit_map current_module
                         rprogram fd
                         (KosuIrTAC.Asttachelper.OperatorDeclaration.tac_body self)
                     in
                     let epilogue = FrameManager.function_epilogue fd in
                     Some
                       (Afunction
                          {
                            asm_name;
                            asm_body =
                              [ Directive "cfi_startproc" ]
                              @ prologue @ (conversion |> List.tl) @ epilogue
                              @ [ Directive "cfi_endproc" ];
                          })
                 | TNConst
                     {
                       rconst_name;
                       value =
                         {
                           rktype = RTInteger _;
                           rexpression = REInteger (_ssign, size, value);
                         };
                     } ->
                     Some
                       (AConst
                          {
                            asm_const_name =
                              asm_const_name current_module rconst_name;
                            value = `IntVal (size, value);
                          })
                 | TNConst
                     {
                       rconst_name;
                       value = { rktype = RTFloat; rexpression = REFloat f };
                     } ->
                     Some
                       (AConst
                          {
                            asm_const_name =
                              asm_const_name current_module rconst_name;
                            value =
                              `IntVal (KosuFrontend.Ast.I64, Int64.bits_of_float f);
                          })
                 | TNConst
                     {
                       rconst_name;
                       value = { rktype = _; rexpression = REstring s };
                     } ->
                     Some
                       (AConst
                          {
                            asm_const_name =
                              asm_const_name current_module rconst_name;
                            value = `StrVal s;
                          })
                 | TNEnum _ | TNStruct _ | TNSyscall _ | TNExternFunc _ | _ -> None)


  let asm_module_path_of_tac_module_path ~str_lit_map rprogram
      { path; tac_module } =
    {
      apath = path;
      asm_module =
        AsmModule (asm_module_of_tac_module ~str_lit_map path rprogram tac_module);
    }

  let asm_program_of_tac_program tac_program =
    tac_program
    |> List.map (fun ({ filename; tac_module_path; rprogram } as named) ->
          let str_lit_map = KosuIrTAC.Asttachelper.StringLitteral.map_string_litteral_of_named_rmodule_path named () in
          {
            filename =
              filename |> Filename.chop_extension |> Printf.sprintf "%s.S";
            asm_module_path =
              asm_module_path_of_tac_module_path ~str_lit_map rprogram
                tac_module_path;
            rprogram;
            str_lit_map;
          })

  let sort_asm_module (AsmModule anodes) =
    AsmModule
      (anodes
      |> List.sort (fun lhs rhs ->
            match (lhs, rhs) with
            | Afunction _, AConst _ -> -1
            | AConst _, Afunction _ -> 1
            | _ -> 0))

end


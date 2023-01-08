open Aarch64Core
open Aarch64Core.Instruction
open Aarch64Core.Register
open KosuIrTyped.Asttyconvert.Sizeof
open KosuIrTAC.Asttachelper.StringLitteral
open KosuIrTAC.Asttac
open Util

module Codegen = struct
  (* let function_prologue ~fn_register_params rprogram fd = 
    let base = Instruction.STP {x1 = R29; x2 = R30; base = SP; offset = Int64.to_int (Int64.sub fd.locals_space 2L); adress_mode = Immediat} in
    let stack_sub = Instruction.isub (SP) ~srcl:(`Register SP) ~srcr:(`Litteral fd.locals_space) in
    let copy_instructions = fn_register_params |> Util.ListHelper.combine_safe Arm64ABI.argument_registers |> List.fold_left (fun acc (register , (name, kt)) -> 
      let whereis = address_of (name, kt) fd in
      acc @ (copy_from_reg (register) whereis kt rprogram)
      ) [] in
    base::stack_sub @ copy_instructions *)

    let mov_integer register n = 
      let open Immediat in
      if is_direct_immediat n then 
        Instruction (Mov {destination = register; flexsec_operand = `ILitteral n})::[]
    else
      let (int64, int48, int32, int16) = split n in
      let base = [Instruction (Mov {destination = register; flexsec_operand = `ILitteral int16})] in
      base
      |> (fun l -> if int32 = 0L then l else l @ [Instruction (Movk {destination = register; operand = `ILitteral int32; shift = Some SH16})])
      |> (fun l -> if int48 = 0L then l else l @ [Instruction (Movk {destination = register; operand = `ILitteral int48; shift = Some SH32})])
      |> (fun l -> if int64 = 0L then l else l @ [Instruction (Movk {destination = register; operand = `ILitteral int32; shift = Some SH16})])

  let sizeofn = KosuIrTyped.Asttyconvert.Sizeof.sizeof

  let translate_tac_expression ~str_lit_map ?(target_reg = Register32 W9) rprogram (fd: FrameManager.frame_desc) = 
    function
    |({tac_expression = TEString s; expr_rktype = _}) -> 
      let reg64 = to_64bits target_reg in
      let SLit str_labl = (Hashtbl.find str_lit_map s) in
      target_reg, load_label str_labl  reg64
    |({tac_expression = TEFalse | TEmpty; expr_rktype = _}) -> 
       to_32bits target_reg,  Instruction ( Mov {destination = to_32bits target_reg; flexsec_operand = `Register (Register32 WZR) })::[]
    |({tac_expression = TENullptr; expr_rktype = _}) -> 
      let r64 = to_64bits target_reg in
      r64,  Instruction ( Mov {destination = r64; flexsec_operand = `Register (Register64 XZR) })::[]
    |({tac_expression = TETrue; _}) -> 
      let s32 = to_32bits target_reg in
      s32, Instruction ( Mov { destination = s32; flexsec_operand = `ILitteral 1L})::[]
    |({tac_expression = TEInt (_, isize, int64); _}) -> 
      let rreg = match isize with I64 -> to_64bits target_reg | _ -> to_32bits target_reg in
      rreg, mov_integer rreg int64
    |({tac_expression = TEFloat float; _}) -> 
      let f64reg = to_64fbits target_reg in
      f64reg, Instruction (Mov {destination = f64reg; flexsec_operand = `F64Litteral float})::[]

    |({tac_expression = TEIdentifier id; expr_rktype}) -> 
      let adress = FrameManager.address_of (id, expr_rktype) fd |> (fun adr -> match adr with Some a -> a | None -> failwith "tte identifier setup null address") in
      let sizeof = sizeofn rprogram expr_rktype in
      let rreg = if KosuIrTyped.Asttyhelper.RType.is_64bits_float expr_rktype then FRegister64 D9 else if sizeof > 4L then to_64bits target_reg else to_32bits target_reg in
      if is_register_size sizeof
        then 
          rreg, [
            Instruction (LDR {data_size = compute_data_size expr_rktype sizeof; destination = rreg; adress_src = adress; adress_mode = Immediat})
          ]
      else 
        rreg, [
          Instruction (ADD {destination = rreg; offset = false; operand1 = adress.base; operand2 = `ILitteral adress.offset})
        ]
    | ({tac_expression = TESizeof kt; _}) -> 
      let r64 = to_64bits target_reg in
      let sizeof = sizeofn rprogram kt in
      r64, [ 
        Line_Com (Comment (Printf.sprintf "sizeof %s" (KosuIrTyped.Asttypprint.string_of_rktype kt))); 
        Instruction (Mov {destination = r64; flexsec_operand = `ILitteral sizeof})
      ]
    | ({tac_expression = TEConst {name; module_path}; expr_rktype = RTString_lit}) ->
      let reg64 = to_64bits target_reg in
      target_reg, load_label ~module_path name reg64
    | ({tac_expression = TEConst {name; module_path}; expr_rktype = RTInteger(_, size) as kt}) ->
      let data_size = compute_data_size kt (Int64.of_int @@ KosuFrontend.Ast.Isize.size_of_isize size) in
      let tmp11 = tmp64reg_4 in
      let load_instruction = load_label ~module_path name tmp11 in
      let fetch = (Instruction (LDR {data_size; destination = target_reg; adress_src = create_adress tmp11; adress_mode = Immediat })) in
      target_reg,  load_instruction @ [fetch]

    | _ -> failwith ""

    let rec translate_tac_rvalue ?(is_deref = None) ~str_lit_map ~(where: address option) current_module rprogram (fd: FrameManager.frame_desc) {rval_rktype; rvalue} =
      match rvalue with
      | RVUminus ttr -> 
        let last_reg, insts =  translate_tac_rvalue ~str_lit_map ~where current_module rprogram fd ttr in
        let neg_instruction = [Instruction (Neg {destination = last_reg; source = last_reg})] in
        last_reg, insts @ neg_instruction
      | RVNot ttr -> 
          let last_reg, insts =  translate_tac_rvalue ~str_lit_map ~where current_module rprogram fd ttr in
          let not_instruction = [Instruction (Not {destination = last_reg; source = `Register last_reg})] in
          last_reg, insts @ not_instruction
      
      | RVExpression tac_typed_expression -> 
        let last_reg, instructions =  translate_tac_expression ~str_lit_map rprogram fd tac_typed_expression in
        let copy_instruction = where |> Option.map (fun waddress -> copy_from_reg last_reg waddress tac_typed_expression.expr_rktype rprogram) |> Option.value ~default:[] in
        last_reg, instructions @ copy_instruction
      | RVStruct {module_path = _; struct_name = _s; fields} -> begin 
        let struct_decl = 
          match KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye rval_rktype rprogram with
          | Some (RDecl_Struct s) -> s
          | Some (RDecl_Enum _) -> failwith "Expected to find a struct get an enum"
          | None -> failwith "Non type decl ??? my validation is very weak" in 
        
          let generics = 
            rval_rktype 
            |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
            |> List.combine struct_decl.generics
            |> List.to_seq
            |> Hashtbl.of_seq
          in
        let offset_list = fields 
          |> List.map (fun (field, _) -> offset_of_field ~generics field struct_decl rprogram)
        in
        
        (* let () = offset_list |> List.map (Printf.sprintf "%Lu") |> String.concat ", " |> Printf.printf "%s off = [%s]\n" _s in *)
        let tmpreg = Register64 X9 in
        tmpreg,  fields |> List.mapi (fun index value -> index, value) |> List.fold_left (fun (acc) (index, (_field, tte)) -> 
          let reg_texp, instructions = translate_tac_expression ~str_lit_map rprogram fd tte in
          let copy_instruction = where |> Option.map (fun waddress -> 
            let current_address = increment_adress (List.nth offset_list index) waddress in
            copy_from_reg reg_texp current_address tte.expr_rktype rprogram
          ) |> Option.value ~default:[] in
           acc @ instructions @ copy_instruction
        ) []
  
      end
      | RVFunction {module_path; fn_name; generics_resolver = _; tac_parameters} -> 
        let typed_parameters = tac_parameters |> List.map (fun {expr_rktype; _} -> expr_rktype) in
        let fn_module = (if module_path = "" then current_module else module_path) in
        let fn_decl = KosuIrTyped.Asttyhelper.RProgram.find_function_decl_of_name fn_module fn_name rprogram |> Option.get in
        begin match fn_decl with
        | RExternal_Decl external_func_decl -> 
          let fn_label = Printf.sprintf "_%s" (external_func_decl.c_name |> Option.value ~default:(external_func_decl.rsig_name)) in
          let fn_register_params, _stack_param = tac_parameters |> List.mapi (fun index -> fun value -> index, value) |> List.partition_map (fun (index, value) -> 
            if index < 8 then Either.left value else Either.right value
            ) in
          let register_param_count = min (List.length external_func_decl.fn_parameters) (List.length argument_registers) in
          let args_in_reg, args_on_stack = fn_register_params |> List.mapi (fun index value -> index, value) |> List.partition_map (fun (index, value) -> 
            if index < register_param_count then Either.left value else Either.right value  
            ) in

            (* let stack_params_offset = stack_param |> List.map (fun {expr_rktype; _} -> 
              if KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram expr_rktype > 8L then KosuIrTyped.Asttyhelper.RType.rpointer expr_rktype else expr_rktype
            ) in

          let stack_store = stack_param |> List.map *)
          
          let instructions, regs = args_in_reg |> Util.ListHelper.combine_safe argument_registers |> List.fold_left_map (fun acc (reg, tte) -> 
            let reg, instruction = translate_tac_expression ~str_lit_map ~target_reg:reg rprogram fd tte in 
            acc @ instruction, `Register reg
          ) [] in
          let set_on_stack_instructions = args_on_stack |> List.mapi (fun index tte -> 
            let last_reg, instructions = translate_tac_expression ~str_lit_map rprogram fd tte in
            let address = create_adress ~offset:(Int64.of_int (index * 8)) (Register64 SP) in
            let set = Instruction (STR {data_size = None; source = to_64bits last_reg; adress = address; adress_mode = Immediat}) in
            instructions @ [set]
            ) |> List.flatten in 
          let call_instructions = FrameManager.call_instruction ~origin:fn_label regs fd in
          let return_size = sizeofn rprogram external_func_decl.return_type in
          let return_reg = return_register_ktype ~float:(KosuIrTyped.Asttyhelper.RType.is_64bits_float external_func_decl.return_type) return_size in
          let extern_instructions = begin match is_register_size return_size with
          | true -> 
            let copy_instruction = where |> Option.map (fun waddress -> 
              match is_deref with
              | Some pointer -> (Instruction (LDR {data_size = None; destination = xr; adress_src = pointer; adress_mode = Immediat}))::copy_from_reg return_reg (create_adress xr) external_func_decl.return_type rprogram
              | None  -> copy_from_reg return_reg waddress external_func_decl.return_type rprogram
            ) |> Option.value ~default:[] in
            return_reg, instructions @ set_on_stack_instructions @ call_instructions @ copy_instruction (* Is not the same check the instructions order*)
          | false -> 
            let copy_instruction = where |> Option.map (fun waddress -> 
                match is_deref with
                | Some pointer -> [
                  Instruction (LDR {data_size = None; destination = xr; adress_src = pointer; adress_mode = Immediat});
                  Instruction (LDR {data_size = None; destination = xr; adress_src = create_adress xr; adress_mode = Immediat})
                ] 
                | None  -> [Instruction (ADD {destination = Register.xr; operand1 = waddress.base; operand2 = `ILitteral waddress.offset; offset = false})]
      
              ) |> Option.value ~default:[] in
            return_reg, instructions @ set_on_stack_instructions @ copy_instruction @ call_instructions
            end
        in 
          extern_instructions
        | RSyscall_Decl syscall_decl ->
          let instructions, _regs = tac_parameters |> Util.ListHelper.combine_safe argument_registers |> List.fold_left_map (fun acc (reg, tte) -> 
            let reg, instruction = translate_tac_expression ~str_lit_map ~target_reg:reg rprogram fd tte in 
            acc @ instruction, reg
          ) [] in
          let return_size = sizeofn rprogram syscall_decl.return_type in
          let return_reg = return_register_ktype ~float:(KosuIrTyped.Asttyhelper.RType.is_64bits_float syscall_decl.return_type) return_size in
          return_reg, instructions @ [
            Line_Com (Comment ("syscall " ^ syscall_decl.rsyscall_name));
            Instruction (Mov {destination = Register64 X16; flexsec_operand = `ILitteral syscall_decl.opcode});
            Instruction (SVC)
            ] @  (where |> Option.map (fun waddress -> 
              match is_deref with
              | Some pointer -> (Instruction (LDR {data_size = None; destination = xr; adress_src = pointer; adress_mode = Immediat}))::copy_from_reg return_reg (create_adress xr) syscall_decl.return_type rprogram
              | None  -> copy_from_reg return_reg waddress syscall_decl.return_type rprogram
              ) |> Option.value ~default:[] )
        | RKosufn_Decl _ -> (
        let function_decl = rprogram |> KosuIrTyped.Asttyhelper.RProgram.find_function_decl_exact_param_types 
          ~module_name:fn_module
          ~fn_name
          ~ktypes: typed_parameters
          |> Option.get
      in
      let fn_label = KosuIrTyped.Asttyhelper.Function.label_of_fn_name fn_module function_decl in
      
      let instructions, regs = tac_parameters |> Util.ListHelper.combine_safe argument_registers |> List.fold_left_map (fun acc (reg, tte) -> 
        let reg, instruction = translate_tac_expression ~str_lit_map ~target_reg:reg rprogram fd tte in 
        acc @ instruction, reg
      ) [] in
  
      let call_instructions = FrameManager.call_instruction ~origin:fn_label regs fd in
      let return_size = sizeofn rprogram function_decl.return_type in
      (* let () = Printf.printf "Return size : %s = %Lu" function_decl.rfn_name return_size in *)
      begin match is_register_size return_size with
      | true -> 
        let return_reg = return_register_ktype ~float:(KosuIrTyped.Asttyhelper.RType.is_64bits_float function_decl.return_type) return_size in
        let copy_instruction = where |> Option.map (fun waddress -> 
          match is_deref with
          | Some pointer -> (Instruction (LDR {data_size = None; destination = xr; adress_src = pointer; adress_mode = Immediat}))::copy_from_reg return_reg (create_adress xr) function_decl.return_type rprogram
          | None  -> copy_from_reg return_reg waddress function_decl.return_type rprogram
        ) |> Option.value ~default:[] in
        return_reg, instructions @ call_instructions @ copy_instruction (* Is not the same check the instructions order*)
      | false -> 
      let return_reg = return_register_ktype ~float:(KosuIrTyped.Asttyhelper.RType.is_64bits_float function_decl.return_type) return_size in
      let copy_instruction = where |> Option.map (fun waddress -> 
           match is_deref with
          | Some pointer -> [
            Instruction (LDR {data_size = None; destination = xr; adress_src = pointer; adress_mode = Immediat});
            Instruction (LDR {data_size = None; destination = xr; adress_src = create_adress xr; adress_mode = Immediat})
          ] 
          | None  -> [Instruction (ADD {destination = Register.xr; operand1 = waddress.base; operand2 = `ILitteral waddress.offset; offset = false})]

        ) |> Option.value ~default:[] in
      return_reg, instructions @ copy_instruction @ call_instructions
      end
    )
  end
      | RVTuple ttes -> 
        let ktlis = ttes |> List.map (fun {expr_rktype; _} -> expr_rktype) in
        let offset_list = ttes |> List.mapi (fun index _value -> 
          offset_of_tuple_index index ktlis rprogram
        )      
        |> List.tl
        |> ( fun l -> l @ [0L] ) in
        let last_reg = tmpreg_of_size (sizeofn rprogram rval_rktype) in
        last_reg, 
        where |> Option.map (fun waddress -> 
          ttes 
          |> List.mapi (fun index value -> index, value) 
          |> List.fold_left (fun (accumuled_adre, acc) (index, tte) -> 
            let reg_texp, instructions = translate_tac_expression rprogram ~str_lit_map fd tte in
          increment_adress (List.nth offset_list index ) accumuled_adre, acc @ instructions @ copy_from_reg reg_texp accumuled_adre tte.expr_rktype rprogram
          ) (waddress, []) |> snd
        ) |> Option.value ~default:[]

      | RVFieldAcess {first_expr = {expr_rktype; tac_expression = TEIdentifier struct_id}; field} -> 
        let struct_decl = 
          match KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye expr_rktype rprogram with
          | Some (RDecl_Struct s) -> s
          | Some (RDecl_Enum _) -> failwith "Expected to find a struct get an enum"
          | None -> failwith "Non type decl ??? my validation is very weak" in

          let generics = 
            expr_rktype 
            |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
            |> List.combine struct_decl.generics
            |> List.to_seq
            |> Hashtbl.of_seq
          in
        
        let offset = offset_of_field ~generics field struct_decl rprogram in
        let struct_address = FrameManager.address_of (struct_id, expr_rktype) fd |> (fun adr -> match adr with Some a -> a | None -> failwith "field access null address") in
        let field_address = increment_adress offset struct_address in
        let sizeof = sizeofn rprogram rval_rktype in
        
        let size = compute_data_size rval_rktype sizeof in
        let tmpreg = tmpreg_of_size_2 sizeof in
        let copy_instructions = where |> Option.map (fun waddress ->
          [Instruction (LDR {data_size = size; destination = tmpreg; adress_src = field_address; adress_mode = Immediat})] @ copy_from_reg tmpreg waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in
        tmpreg, (Line_Com (Comment ("Field access of "^field)))::copy_instructions
      | RVFieldAcess _ -> failwith "Wierd : Fields access force struct as an identifier"
      | RVAdress id -> 
        let pointee_type = rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee in
        let adress = FrameManager.address_of (id, pointee_type) fd |> (fun adr -> match adr with Some a -> a | None -> failwith "address of null address") in
        let copy_instruction =  where |> Option.map (fun waddress -> 
          [Instruction (ADD {destination = tmp64reg; operand1 = adress.base; operand2 = `ILitteral adress.offset; offset = false})] @ copy_from_reg tmp64reg waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        tmp64reg, copy_instruction 
      | RVDefer id ->
        let adress = FrameManager.address_of (id, rval_rktype |> KosuIrTyped.Asttyhelper.RType.rpointer) fd |> (fun adr -> match adr with Some a -> a | None -> failwith "defer of null address") in
        let load_instruction = [Instruction (LDR {data_size = None; destination = tmp64reg_2; adress_src = adress; adress_mode = Immediat})] in
        let last_reg, load_indirect = 
          if is_register_size @@ KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram rval_rktype 
            then tmp64reg, [Instruction (LDR {data_size = None; destination = tmp64reg; adress_src = create_adress tmp64reg_2; adress_mode = Immediat})] 
        else tmp64reg_2, [] in
        (* let sizeof = sizeofn rprogram rval_rktype in *)
        let copy_instructions = where |> Option.map (fun waddress -> 
          load_instruction @ load_indirect @ copy_from_reg last_reg waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        
       last_reg, copy_instructions
      | RVEnum {variant; assoc_tac_exprs; _} -> 
        let enum_decl = 
          match KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye rval_rktype rprogram with
          | Some (RDecl_Struct _) -> failwith "Expected to find an enum get an struct"
          | Some (RDecl_Enum e) -> e
          | None -> failwith "Non type decl ??? my validation is very weak" in 
        let tag = KosuIrTyped.Asttyhelper.Renum.tag_of_variant variant enum_decl in
        let enum_tte_list = assoc_tac_exprs |> List.cons {expr_rktype = (RTInteger (Unsigned, I32)); tac_expression = (TEInt (Unsigned, I32, Int64.of_int32 tag))} in
        let enum_type_list = enum_tte_list |> List.map (fun {expr_rktype; _} -> expr_rktype) in
        let offset_list = enum_tte_list
        |> List.mapi (fun index {expr_rktype = _; _} -> offset_of_tuple_index index enum_type_list rprogram)
      in
      (* let () = offset_list |> List.map (Printf.sprintf "%Lu") |> String.concat ", " |> Printf.printf "%s::%s off = [%s]\n" enum_decl.renum_name variant in *)
      let last_reg = tmpreg_of_size (sizeofn rprogram rval_rktype) in
        last_reg, 

        enum_tte_list |> List.mapi (fun index value -> index, value)  |> List.fold_left (fun acc (index, tte) -> 
          let reg_texp, instructions = translate_tac_expression rprogram ~str_lit_map fd tte in 
          let copy_instructions = where |> Option.map (fun waddress -> 
            let current_address = increment_adress (List.nth offset_list index) waddress in
            copy_from_reg reg_texp current_address tte.expr_rktype rprogram
          ) |> Option.value ~default:[] in
          acc @ instructions @ copy_instructions
        ) []

      | RVDiscard | RVLater -> tmp32reg, []
      | RVBuiltinBinop { binop = TacBool (TacOr); blhs; brhs} -> 
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in 
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r8 rprogram fd blhs in
        let copy_instructions = where |> Option.map (fun waddress -> 
          let or_instruction = [Instruction (ORR {destination = r8; operand1 = left_reg; operand2 = `Register right_reg})] in
          or_instruction @ copy_from_reg r8 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        r8, rinstructions @ linstructions @ copy_instructions 
      | RVBuiltinBinop { binop = TacBool (TacAnd); blhs; brhs} -> 
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in 
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r8 rprogram fd blhs in
        let copy_instructions = where |> Option.map (fun waddress -> 
          let or_instruction = [Instruction (AND {destination = r8; operand1 = left_reg; operand2 = `Register right_reg})] in
          or_instruction @ copy_from_reg r8 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        r8, rinstructions @ linstructions @ copy_instructions 
      | RVBuiltinBinop {binop = TacBool (TacEqual); blhs; brhs} -> 
        let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in
        let zero_reg = reg_of_size (size_of_ktype_size (sizeofn rprogram rval_rktype)) (Register64 XZR) in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd blhs in
        let copy_instructions = where |> Option.map (fun waddress -> 
          let equal_instruction = [
            Instruction (Mov {destination = r8; flexsec_operand = `ILitteral 0L});
            Instruction (SUBS {destination = r9; operand1 = left_reg; operand2 = `Register right_reg });
            Instruction (CSINC {destination = r8; operand1 = r8; operand2 = zero_reg; condition = NE})
          ] in
          equal_instruction @ copy_from_reg r8 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        r8, rinstructions @ linstructions @ copy_instructions 
      | RVBuiltinBinop {binop = TacBool (TacDiff); blhs; brhs} -> 
          let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
          let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
          let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in
          let zero_reg = reg_of_size (size_of_ktype_size (sizeofn rprogram rval_rktype)) (Register64 XZR) in
          let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd brhs in
          let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd blhs in
          let copy_instructions = where |> Option.map (fun waddress -> 
            let equal_instruction = [
              Instruction (Mov {destination = r8; flexsec_operand = `ILitteral 0L});
              Instruction (SUBS {destination = r9; operand1 = left_reg; operand2 = `Register right_reg });
              Instruction (CSINC {destination = r8; operand1 = r8; operand2 = zero_reg; condition = EQ})
            ] in
            equal_instruction @ copy_from_reg r8 waddress rval_rktype rprogram
            ) |> Option.value ~default:[] in
          r8, rinstructions @ linstructions @ copy_instructions 
      | RVBuiltinBinop {binop = TacBool (TacSupEq); blhs; brhs} -> 
        let condition = if KosuIrTyped.Asttyhelper.RType.is_pointer blhs.expr_rktype then CC else LT in 
        let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in
        let zero_reg = reg_of_size (size_of_ktype_size (sizeofn rprogram rval_rktype)) (Register64 XZR) in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd blhs in
        let copy_instructions = where |> Option.map (fun waddress -> 
          let equal_instruction = [
            Instruction (Mov {destination = r8; flexsec_operand = `ILitteral 0L});
            Instruction (SUBS {destination = r9; operand1 = left_reg; operand2 = `Register right_reg });
            Instruction (CSINC {destination = r8; operand1 = r8; operand2 = zero_reg; condition})
          ] in
          equal_instruction @ copy_from_reg r8 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        r8, rinstructions @ linstructions @ copy_instructions 
      | RVBuiltinBinop {binop = TacBool (TacSup); blhs; brhs} -> 
        let condition = if KosuIrTyped.Asttyhelper.RType.is_pointer blhs.expr_rktype then LS else LE in 
        let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in
        let zero_reg = reg_of_size (size_of_ktype_size (sizeofn rprogram rval_rktype)) (Register64 XZR) in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd blhs in
        let copy_instructions = where |> Option.map (fun waddress -> 
          let equal_instruction = [
            Instruction (Mov {destination = r8; flexsec_operand = `ILitteral 0L});
            Instruction (SUBS {destination = r9; operand1 = left_reg; operand2 = `Register right_reg });
            Instruction (CSINC {destination = r8; operand1 = r8; operand2 = zero_reg; condition})
          ] in
          equal_instruction @ copy_from_reg r8 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        r8, rinstructions @ linstructions @ copy_instructions 
      | RVBuiltinBinop {binop = TacBool (TacInf); blhs; brhs} -> 
        let condition = if KosuIrTyped.Asttyhelper.RType.is_pointer blhs.expr_rktype then CS else GE in 
        let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in
        let zero_reg = reg_of_size (size_of_ktype_size (sizeofn rprogram rval_rktype)) (Register64 XZR) in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd blhs in
        let copy_instructions = where |> Option.map (fun waddress -> 
          let equal_instruction = [
            Instruction (Mov {destination = r8; flexsec_operand = `ILitteral 0L});
            Instruction (SUBS {destination = r9; operand1 = left_reg; operand2 = `Register right_reg });
            Instruction (CSINC {destination = r8; operand1 = r8; operand2 = zero_reg; condition})
          ] in
          equal_instruction @ copy_from_reg r8 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        r8, rinstructions @ linstructions @ copy_instructions 
      | RVBuiltinBinop {binop = TacBool (TacInfEq); blhs; brhs} -> 
        let condition = if KosuIrTyped.Asttyhelper.RType.is_pointer blhs.expr_rktype then HI else GT in 
        let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in
        let zero_reg = reg_of_size (size_of_ktype_size (sizeofn rprogram rval_rktype)) (Register64 XZR) in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd blhs in
        let copy_instructions = where |> Option.map (fun waddress -> 
          let equal_instruction = [
            Instruction (Mov {destination = r8; flexsec_operand = `ILitteral 0L});
            Instruction (SUBS {destination = r9; operand1 = left_reg; operand2 = `Register right_reg });
            Instruction (CSINC {destination = r8; operand1 = r8; operand2 = zero_reg; condition})
          ] in
          equal_instruction @ copy_from_reg r8 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in
        r8, linstructions @ rinstructions @ copy_instructions 
        
      | RVBuiltinBinop {binop = TacSelf TacAdd; blhs; brhs}  -> 
        begin match KosuIrTyped.Asttyhelper.RType.is_pointer rval_rktype with
        | true ->          
           let pointee_size = rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee |>  KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram in
          let r9 = tmp64reg_2 in
          let r10 = tmp64reg_3 in
          let r11 = tmp64reg_4 in
          let operand_instructions = begin match blhs.expr_rktype with
            | KosuIrTyped.Asttyped.RTPointer _ -> 
            let _ptr_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:(r9) rprogram fd blhs in
            let _nb_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:(r10) rprogram fd brhs in
            linstructions @ rinstructions 
          | _ -> failwith ""  
          end in
            let add_instructions = 
              if pointee_size = 1L 
                then [Instruction (ADD {destination = r9; operand1 = r9; operand2 = `Register r10; offset = false})] 
            else
              [
                Instruction (Mov {destination = r11; flexsec_operand = `ILitteral pointee_size});
                Instruction (MADD {destination = r9; operand1_base = r9; operand2 = r10; scale = r11})
              ]
            in
          let copy_instruction = where |> Option.map (fun waddress -> 
            copy_from_reg r9 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in 
        r9, operand_instructions @ add_instructions @ copy_instruction
        | false ->       
          let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
          let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
          let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
          let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
          let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
          
          let copy_instruction = where |> Option.map (fun waddress -> 
          let add_instruction = Instruction (ADD {destination = r9; operand1 = left_reg; operand2 = `Register right_reg; offset = false }) in
          add_instruction::(Instruction.copy_from_reg r9 waddress rval_rktype rprogram )
          ) |>  Option.value ~default:[] in
          r9, linstructions @ rinstructions @ copy_instruction
        end
      | RVBuiltinBinop {binop = TacSelf TacMinus; blhs; brhs} ->
        begin match KosuIrTyped.Asttyhelper.RType.is_pointer rval_rktype with
        | true ->          
          let pointee_size = rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee |>  KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram in
         let r9 = tmp64reg_2 in
         let r10 = tmp64reg_3 in
         let r11 = tmp64reg_4 in
         let operand_instructions = begin match blhs.expr_rktype with
           | KosuIrTyped.Asttyped.RTPointer _ -> 
           let _ptr_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:(r9) rprogram fd blhs in
           let _nb_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:(r10) rprogram fd brhs in
           linstructions @ rinstructions 
         | _ -> failwith ""  
         end in
           let add_instructions = 
             if pointee_size = 1L 
               then [Instruction (SUB {destination = r9; operand1 = r9; operand2 = `Register r10})] 
           else
             [
               Instruction (Mov {destination = r11; flexsec_operand = `ILitteral pointee_size});
               Instruction (MSUB {destination = r9; operand1_base = r9; operand2 = r10; scale = r11})
             ]
           in
         let copy_instruction = where |> Option.map (fun waddress -> 
           copy_from_reg r9 waddress rval_rktype rprogram
         ) |> Option.value ~default:[] in 
       r9, operand_instructions @ add_instructions @ copy_instruction
       | false ->           
          let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
          let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
          let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
          let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
          let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
       
          let copy_instruction = where |> Option.map (fun waddress -> 
            let sub_instruction = Instruction (SUB {destination = r9; operand1 = left_reg; operand2 = `Register right_reg }) in
            sub_instruction::(copy_from_reg r9 waddress rval_rktype rprogram)
          ) |>  Option.value ~default:[] in
          r9,  linstructions @ rinstructions @ copy_instruction
        end 
      | RVBuiltinBinop {binop = TacSelf TacMult; blhs; brhs} ->
        let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
        let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
        let mult_instruction = Instruction (MUL {destination = r9; operand1 = left_reg; operand2 = right_reg}) in
        let copy_instruction = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in 
        r9, linstructions @ rinstructions @ mult_instruction::copy_instruction
      | RVBuiltinBinop {binop = TacSelf TacDiv; blhs; brhs} ->
          let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
          let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
          let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
          let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
          let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
          let div_instruction = if 
            KosuIrTyped.Asttyhelper.RType.is_unsigned_integer rval_rktype then
              Instruction (UDIV {destination = r9; operand1 = left_reg; operand2 = right_reg})  
            else 
              Instruction (SDIV {destination = r9; operand1 = left_reg; operand2 = right_reg}) in
          let copy_instruction = where |> Option.map (fun waddress -> 
            copy_from_reg r9 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in 
          r9, linstructions @ rinstructions @ div_instruction::copy_instruction
      | RVBuiltinBinop {binop = TacSelf TacModulo; blhs; brhs} ->
        let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
        let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
        let div_instruction = 
          if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer rval_rktype 
            then Instruction (UDIV {destination = r9; operand1 = left_reg; operand2 = right_reg}) 
          else Instruction (UDIV {destination = r9; operand1 = left_reg; operand2 = right_reg}) in
        let modulo_instruction = [
                  (* udiv x2, x0, x1 // Thank you StackOverflow
                msub x3, x2, x1, x0 *)
          div_instruction;
          Instruction (MSUB {destination = r9; operand1_base = r9; operand2 = right_reg; scale = left_reg})
        ] in
        let copy_instruction = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in 
        r9, linstructions @ rinstructions @ modulo_instruction @ copy_instruction
        
      | RVBuiltinBinop {binop = TacSelf TacBitwiseAnd; blhs; brhs} ->
        let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
        let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
        let and_instruction = Instruction (AND {destination = r9; operand1 = left_reg; operand2 = `Register right_reg}) in
        let copy_instruction = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in 
        r9, linstructions @ rinstructions @ and_instruction::copy_instruction
      | RVBuiltinBinop {binop = TacSelf TacBitwiseOr; blhs; brhs} ->
        let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
        let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
        let or_instruction = Instruction (ORR {destination = r9; operand1 = left_reg; operand2 = `Register right_reg}) in
        let copy_instruction = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
          ) |> Option.value ~default:[] in 
          r9, linstructions @ rinstructions @ or_instruction::copy_instruction
      | RVBuiltinBinop {binop = TacSelf TacBitwiseXor; blhs; brhs} ->
        let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
        let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
        let xor_instruction = Instruction (EOR {destination = r9; operand1 = left_reg; operand2 = `Register right_reg}) in
        let copy_instruction = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in 
        r9, linstructions @ rinstructions @ xor_instruction::copy_instruction
      | RVBuiltinBinop {binop = TacSelf TacShiftLeft; blhs; brhs} ->
        let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
        let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
        let shift_left_instruction = Instruction (LSL {destination = r9; operand1 = left_reg; operand2 = `Register right_reg}) in
        let copy_instruction = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in 
        r9, linstructions @ rinstructions @ shift_left_instruction::copy_instruction
      | RVBuiltinBinop {binop = TacSelf TacShiftRight; blhs; brhs} ->
        let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
        let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs in
        let shift_left_instruction = 
          if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer rval_rktype
            then  
              Instruction (LSR {destination = r9; operand1 = left_reg; operand2 = `Register right_reg}) 
          else  
            Instruction (ASR {destination = r9; operand1 = left_reg; operand2 = `Register right_reg})
        in
        let copy_instruction = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in 
        r9, linstructions @ rinstructions @ shift_left_instruction::copy_instruction
      | RVBuiltinUnop {unop = TacUminus; expr} ->
        let r9 = tmpreg_of_ktype_2 rprogram rval_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram expr.expr_rktype in
        let last_reg, instructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd expr in
        let uminus_instructions = Instruction (Neg {destination = r9; source = last_reg}) in
        let copy_instructions = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in 
        r9, instructions @ uminus_instructions::copy_instructions
      | RVBuiltinUnop {unop = TacNot; expr} ->
        let r9 = tmpreg_of_ktype_2 rprogram rval_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram expr.expr_rktype in
        let last_reg, instructions = translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd expr in
        let not_instructions = 
          if KosuIrTyped.Asttyhelper.RType.is_bool rval_rktype 
            then Instruction (EOR {destination = r9; operand1 = last_reg; operand2 = `ILitteral 1L}) 
          else
            Instruction (Mvn {destination = r9; operand = `Register last_reg}) 
        in
        let copy_instructions = where |> Option.map (fun waddress -> 
          copy_from_reg r9 waddress rval_rktype rprogram
        ) |> Option.value ~default:[] in 
        r9, instructions @ not_instructions::copy_instructions
        | _ -> failwith "Mostly binop"

let rec translate_tac_statement ~str_lit_map current_module rprogram (fd: FrameManager.frame_desc) = function
      | STacDeclaration {identifier; trvalue} | STacModification {identifier; trvalue} ->
        let address = FrameManager.address_of (identifier, trvalue.rval_rktype) fd in
        let last_reg, instructions = translate_tac_rvalue ~str_lit_map ~where:address current_module rprogram fd trvalue in
        last_reg, instructions
      | STDerefAffectation {identifier; trvalue} -> 
        let tmpreg = tmpreg_of_ktype rprogram (KosuIrTyped.Asttyhelper.RType.rpointer trvalue.rval_rktype) in
        let intermediary_adress = FrameManager.address_of (identifier, KosuIrTyped.Asttyhelper.RType.rpointer trvalue.rval_rktype) fd in
        let instructions = Instruction ( LDR { data_size = None; destination = tmpreg; adress_src = intermediary_adress |> Option.get; adress_mode = Immediat} ) in
        let true_adress =  (create_adress tmpreg) in
        let last_reg, true_instructions = translate_tac_rvalue ~str_lit_map ~is_deref:(intermediary_adress) ~where:(Some true_adress) current_module rprogram fd trvalue  in
        last_reg, (Line_Com (Comment "Defered Start"))::instructions::true_instructions @ [Line_Com (Comment "Defered end")]
      | STIf {
        statement_for_bool;
        condition_rvalue;
        goto1;
        goto2;
        exit_label;
        if_tac_body;
        else_tac_body;
      } -> 
        let stmts_bool = statement_for_bool |> List.fold_left (fun acc stmt -> 
          acc @ (translate_tac_statement ~str_lit_map current_module rprogram fd stmt |> snd)
        ) [] in
        let last_reg, condition_rvalue_inst = translate_tac_expression ~str_lit_map rprogram fd condition_rvalue in 
        let cmp = Instruction (CMP {operand1 = last_reg; operand2 = `ILitteral 1L}) in
        let jmp = Instruction (B { cc = Some EQ ; label = goto1}) in
        let jmp2 = Instruction (B { cc = None; label = goto2}) in
        let if_block = translate_tac_body ~str_lit_map ~end_label:(Some exit_label) current_module rprogram fd if_tac_body in
        let else_block = translate_tac_body ~str_lit_map ~end_label:(Some exit_label) current_module rprogram fd else_tac_body in
        let exit_label_instr = Label (exit_label) in
        last_reg, stmts_bool @ condition_rvalue_inst @ cmp::jmp::jmp2::if_block @ else_block @ [exit_label_instr]
      | STSwitch {
        statemenets_for_case;
        condition_switch;
        sw_cases;
        wildcard_label;
        wildcard_body;
        sw_exit_label;
      } -> 
        let tag_of_variant = KosuIrTyped.Asttyhelper.Renum.tag_of_variant in
        
        let enum_decl = 
          match KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye condition_switch.expr_rktype rprogram with
          | Some (RDecl_Struct _) -> failwith "Expected to find an enum get an struct"
          | Some (RDecl_Enum e) -> e
          | None -> failwith "Non type decl ??? my validation is very weak" in 
        let enum_decl = 
          let generics = condition_switch.expr_rktype |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype |> List.combine enum_decl.generics in
          KosuIrTyped.Asttyhelper.Renum.instanciate_enum_decl generics enum_decl 
        in
        let exit_label_instruction = Label (sw_exit_label) in
        let _, setup_instructions = statemenets_for_case |> List.fold_left (fun (_, acc_stmts) value -> 
          let last_reg, insts = translate_tac_statement ~str_lit_map current_module rprogram fd value in
          last_reg, acc_stmts @ insts
        ) (tmp64reg, []) in
        let last_reg, condition_switch_instruction = translate_tac_expression ~str_lit_map rprogram fd condition_switch in
        let copy_tag = if is_register_size (sizeofn rprogram condition_switch.expr_rktype) then 
          Instruction (Mov {destination = tmp32reg_4; flexsec_operand = `Register (to_32bits last_reg)})
        else 
          Instruction (LDR {data_size = None; destination = tmp32reg_4; adress_src = create_adress last_reg; adress_mode = Immediat})
        in
        let switch_variable_name = 
          match condition_switch.tac_expression with
          | TEIdentifier id -> id
          | _ -> failwith "I need to get the id" in
        (* Tag fetch proper doing*)
        let cmp_instrution_list, fn_block = sw_cases |> List.map (fun sw_case -> 
          let jump_condition = sw_case.variants_to_match |> List.map (fun mvariant -> 
            let tag = tag_of_variant mvariant enum_decl in
            let compare = Instruction (CMP {operand1 = tmp32reg_4; operand2 = `ILitteral (Int64.of_int32 tag)}) in
            let assoc_type_for_variants = KosuIrTyped.Asttyhelper.Renum.assoc_types_of_variant ~tagged:true mvariant enum_decl in
            let fetch_offset_instruction = sw_case.assoc_bound |> List.map (fun (index, id, ktyte) -> 
              let offset_a = offset_of_tuple_index (index + 1) assoc_type_for_variants rprogram in
              let switch_variable_address = FrameManager.address_of (switch_variable_name, condition_switch.expr_rktype) fd |> Option.get in
              let destination_address = FrameManager.address_of (id, ktyte) fd |> Option.get in
              let size_of_ktype = sizeofn rprogram ktyte in
              let data_size = compute_data_size ktyte size_of_ktype in
              let copy_instructions = 
                if is_register_size size_of_ktype then
                
                 [
                  Instruction (LDR {data_size; destination = reg_of_size (size_of_ktype_size size_of_ktype) tmp64reg; adress_src = increment_adress offset_a (switch_variable_address); adress_mode = Immediat});
                  Instruction (STR {data_size; source = reg_of_size (size_of_ktype_size size_of_ktype) tmp64reg; adress = destination_address; adress_mode = Immediat })
                ]
                else (Instruction (ADD {destination = tmp64reg; operand1 = switch_variable_address.base; operand2 = `ILitteral (Int64.add switch_variable_address.offset offset_a);  offset = false})) :: copy_from_reg tmp64reg destination_address ktyte rprogram
              in

              copy_instructions
            ) |> List.flatten in
            let jump_true = Instruction (B {cc = Some EQ; label = sw_case.sw_goto}) in
            fetch_offset_instruction @ compare::jump_true::[]
          ) |> List.flatten in
          let genete_block = translate_tac_body ~str_lit_map ~end_label:(Some sw_case.sw_exit_label) current_module  rprogram fd sw_case.switch_tac_body in
          jump_condition, genete_block
        ) |> List.split |> fun (lhs, rhs) -> List.flatten lhs, (List.flatten rhs) in
        let wildcard_case_jmp = wildcard_label |> Option.map (fun lab ->  Instruction (B {cc = None; label = lab})) |> Option.to_list in
        let wildcard_body_block = wildcard_body |> Option.map (fun body -> 
          translate_tac_body ~str_lit_map ~end_label:(Some sw_exit_label) current_module rprogram fd body
        ) |> Option.value ~default:[] in
        tmp64reg, setup_instructions @ condition_switch_instruction @ copy_tag::cmp_instrution_list @ wildcard_case_jmp @ fn_block @ wildcard_body_block @ [exit_label_instruction]
      | SCases {cases; else_tac_body; exit_label} -> 
        let cases_body, cases_condition = cases |> List.map (fun scases -> 
          (* furute optimisation : do only si else branch*)
          let setup_next_label_instr = scases.condition_label |> Option.map (fun label -> Label label) |> Option.to_list in
          let setup_condition_insts = scases.statement_for_condition |> List.map (fun stmt -> 
           snd @@ translate_tac_statement ~str_lit_map current_module rprogram fd stmt 
          ) |> List.flatten in
          let last_reg, condition = translate_tac_expression ~str_lit_map rprogram fd scases.condition in
          let cmp = Instruction (CMP {operand1 = last_reg; operand2 = `ILitteral 1L}) in
          let if_true_instruction = Instruction (B { cc = Some EQ; label = scases.goto}) in
          let if_false_instruction = Instruction (B {cc = None; label = scases.jmp_false}) in
          let body_instruction = translate_tac_body ~str_lit_map ~end_label:(Some scases.end_label) current_module rprogram fd scases.tac_body in
          body_instruction, setup_next_label_instr @ setup_condition_insts @ condition @ [cmp; if_true_instruction; if_false_instruction]
        ) |> List.split |> (fun (lhs, rhs) -> List.flatten lhs, List.flatten rhs) in
        let _else_jump = Instruction (B {cc = None; label = else_tac_body.label}) in
        let end_label_instruction = Label (exit_label) in
        let else_body_instruction = translate_tac_body ~str_lit_map ~end_label:(Some exit_label) current_module rprogram fd else_tac_body in
        tmp64reg, cases_condition @ cases_body @ else_body_instruction @ [end_label_instruction]
and translate_tac_body ~str_lit_map ?(end_label = None) current_module rprogram (fd: FrameManager.frame_desc) {label; body} = 
  let label_instr = Label label in
  let stmt_instr = body |> fst |> List.map (fun stmt -> snd @@ translate_tac_statement ~str_lit_map current_module rprogram fd stmt) |> List.flatten  in
  let end_label_inst = end_label |> Option.map (fun lab ->  Instruction (B {cc = None; label = lab})) |> Option.to_list in
  let return_instr = body |> snd |> Option.map (fun tte -> 
      let last_reg, instructions = translate_tac_expression ~str_lit_map  rprogram fd tte in
      let sizeof = sizeofn rprogram tte.expr_rktype in
      let return_reg = return_register_ktype ~float:(KosuIrTyped.Asttyhelper.RType.is_64bits_float tte.expr_rktype) sizeof in
      instructions @ (if is_register_size sizeof then 
        Instruction (Mov {destination = return_reg; flexsec_operand = `Register last_reg})::[]
      else
        let x8_address = Option.get @@ FrameManager.(address_of (indirect_return_vt) fd) in 
        let str = Instruction ( LDR {data_size = None; destination = xr; adress_src = x8_address; adress_mode = Immediat}) in
      str::copy_large (create_adress xr) last_reg sizeof
      )
  ) |> Option.value ~default:[] in
  label_instr::stmt_instr @ return_instr @ end_label_inst
end

let asm_module_of_tac_module ~str_lit_map current_module rprogram  = let open KosuIrTyped.Asttyped in function
| TacModule tac_nodes -> 
  tac_nodes |> List.filter_map (fun node -> match node with 
  | TNFunction function_decl -> 
    let register_param_count = List.length argument_registers in 
    let fn_register_params, stack_param = function_decl.rparameters |> List.mapi (fun index -> fun value -> index, value) |> List.partition_map (fun (index, value) -> 
      if index < register_param_count then Either.left value else Either.right value
      ) in
    let stack_param_count = Int64.of_int (function_decl.stack_params_count * 8) in
    let locals_var = function_decl.locale_var |> List.map (fun {locale_ty; locale} -> match locale with Locale s -> (s, locale_ty) | Enum_Assoc_id {name; _} -> (name, locale_ty) )in
    (* let () = locals_var |> List.map (fun (s, kt) -> Printf.sprintf "%s : %s " (s) (KosuIrTyped.Asttypprint.string_of_rktype kt)) |> String.concat ", " |> Printf.printf "%s : locale variables = [%s]\n" function_decl.rfn_name in *)
    let asm_name = KosuIrTAC.Asttachelper.Function.label_of_fn_name current_module function_decl in
    let fd = FrameManager.frame_descriptor ~stack_future_call:(stack_param_count) ~fn_register_params ~stack_param:stack_param ~return_type:function_decl.return_type ~locals_var ~discarded_values:(function_decl.discarded_values) rprogram in 
    let prologue = FrameManager.function_prologue ~fn_register_params:function_decl.rparameters ~stack_params:stack_param rprogram fd in
    let conversion = Codegen.translate_tac_body ~str_lit_map current_module rprogram fd function_decl.tac_body in
    let epilogue = FrameManager.function_epilogue fd in
    (* let () = Printf.printf "\n\n%s:\n" function_decl.rfn_name in
    let () = fd.stack_map |> IdVarMap.to_seq |> Seq.iter (fun ((s, kt), adr) -> 
      Printf.printf "%s : %s == [%s, %Ld]\n" 
      (s) 
      (KosuIrTyped.Asttypprint.string_of_rktype kt) 
      (Aarch64Pprint.string_of_register adr.base)
      (adr.offset) 
      ) in *)
    Some (Afunction {
      asm_name;
      asm_body = prologue @ (conversion |> List.tl) @ epilogue 
    })
  | TNOperator _ -> failwith "TNOperator todo"
  | TNConst {rconst_name; value = { rktype = RTInteger _; rexpression = REInteger (_ssign, size, value)}} -> 
    Some (AConst {
      asm_const_name = asm_const_name current_module rconst_name;
      value = `IntVal (size, value)
    })
  | TNConst {rconst_name; value = { rktype = RTFloat; rexpression = REFloat f}} ->
    Some (AConst {
      asm_const_name = asm_const_name current_module rconst_name;
      value = `IntVal (KosuFrontend.Ast.I64, Int64.bits_of_float f)
    })
  | TNConst {rconst_name; value = {rktype = _; rexpression = REstring s}} ->
    Some (
      AConst {
        asm_const_name = asm_const_name current_module rconst_name;
        value = `StrVal s
      }
    )
  | TNEnum _ | TNStruct _ | TNSyscall _ | TNExternFunc _ | _ -> None
  )

let asm_module_path_of_tac_module_path ~str_lit_map rprogram {path; tac_module = tac_module} = {
  apath = path;
  asm_module = AsmModule (asm_module_of_tac_module ~str_lit_map path rprogram tac_module)
}

let asm_program_of_tac_program tac_program = 
  tac_program |> List.map (fun ({filename; tac_module_path; rprogram} as named) -> 
    let str_lit_map = map_string_litteral_of_named_rmodule_path named () in
    {
      filename = filename |> Filename.chop_extension |> Printf.sprintf "%s.S";
      asm_module_path = asm_module_path_of_tac_module_path ~str_lit_map rprogram tac_module_path;
      rprogram;
      str_lit_map;
    }
  )

let sort_asm_module (AsmModule anodes) = AsmModule( anodes |> List.sort (fun lhs rhs ->
   match lhs, rhs with
   | Afunction _, AConst _ -> -1 
   | AConst _, Afunction _ -> 1
   | _ -> 0
))
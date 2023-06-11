(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms            *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kosu.         *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Util
open BytecodeCore
open BytecodeCore.LineInstruction
open BytecodeCore.Register
open BytecodeCore.BytecodeProgram
open BytecodeCore.Location
open KosuIrTAC.Asttachelper.StringLitteral
open KosuIrTAC.Asttac

let store_instruction ~large_cp ~rval_rktype ~reg ~where =
  match where with
  | None -> []
  | Some (LocReg rloc) ->
      if rloc = reg then []
      else LineInstruction.smv rloc @@ Operande.iregister reg
  | Some (LocAddr address) ->
      if large_cp then LineInstruction.scopy reg address rval_rktype
      else
        let data_size = ConditionCode.data_size_of_kt rval_rktype in
        LineInstruction.sstr data_size reg address

let translate_tac_expression ~litterals ~target_reg fd tte =
  match tte.tac_expression with
  | TEString s ->
      let (SLit str_labl) = Hashtbl.find litterals.str_lit_map s in
      lea_label target_reg str_labl
  | TEFalse | TECmpLesser | TEmpty | TENullptr -> mv_integer target_reg 0L
  | TETrue | TECmpEqual -> mv_integer target_reg 1L
  | TECmpGreater -> mv_integer target_reg 2L
  | TEInt (_, _, int64) -> mv_integer target_reg int64
  | TEChar c ->
      let int_repr = Int64.of_int @@ Char.code c in
      mv_integer target_reg int_repr
  | TEFloat float ->
      let (FLit float_label) = Hashtbl.find litterals.float_lit_map float in
      let bits_repr = Int64.bits_of_float @@ snd float in
      mv_integer target_reg bits_repr
  | TEIdentifier id -> (
      let loc =
        match FrameManager.location_of (id, tte.expr_rktype) fd with
        | None -> failwith "tte identifier setup null address"
        | Some loc -> loc
      in
      match loc with
      | LocReg reg ->
          if reg = target_reg then []
          else smv target_reg @@ Operande.iregister reg
      | LocAddr address ->
          if Register.does_return_hold_in_register_kt tte.expr_rktype then
            let ds = ConditionCode.data_size_of_kt tte.expr_rktype in
            sldr ds target_reg address
          else sadd target_reg address.base address.offset)
  | TESizeof kt ->
      let size = KosuIrTyped.Sizeof.sizeof_kt kt in
      mv_integer target_reg size
  | TEConst { module_path; name } when tte.expr_rktype = RTString_lit ->
      lea_label target_reg ~module_path name
  | TEConst _ -> failwith "Other constant todo"

let translate_and_store ~where ~litterals ~target_reg fd tte =
  match where with
  | Some (LocReg reg) ->
      translate_tac_expression ~litterals ~target_reg:reg fd tte
  | Some (LocAddr address) ->
      let target_reg = Register.r13 in
      let insts = translate_tac_expression ~litterals ~target_reg fd tte in
      let cp_insts = scopy target_reg address tte.expr_rktype in
      insts @ cp_insts
  | None ->
      let target_reg = Register.r13 in
      let insts = translate_tac_expression ~litterals ~target_reg fd tte in
      insts

let translate_tac_rvalue ?is_deref ~litterals ~(where : location option)
    current_module rprogram (fd : FrameManager.description) rvalue =
  match rvalue.rvalue with
  | RVExpression tte ->
      translate_and_store ~where ~litterals ~target_reg:Register.r13 fd tte
  | RVStruct { fields; module_path = _; struct_name = _ } ->
      let struct_decl =
        match
          KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
            rvalue.rval_rktype rprogram
        with
        | Some (RDecl_Struct s) -> s
        | Some (RDecl_Enum _) ->
            failwith "Expected to find a struct get an enum"
        | None -> failwith "Non type decl ??? my validation is very weak"
      in

      let generics =
        rvalue.rval_rktype
        |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
        |> List.combine struct_decl.generics
        |> List.to_seq |> Hashtbl.of_seq
      in
      let offset_list =
        fields
        |> List.map (fun (field, _) ->
               KosuIrTyped.Sizeof.offset_of_field ~generics field struct_decl
                 rprogram)
      in

      fields |> List.mapi Util.couple
      |> List.fold_left
           (fun acc (index, (_field, tte)) ->
             let where =
               where
               |> Option.map (function
                    | LocAddr address ->
                        LocAddr
                          (increment_adress
                             (List.nth offset_list index)
                             address)
                    | LocReg _ as loc -> loc)
             in
             acc
             @ translate_and_store ~where ~litterals ~target_reg:Register.r13 fd
                 tte)
           []
  | RVFunction { module_path; fn_name; generics_resolver = _; tac_parameters }
    -> (
      let fn_module =
        if module_path = "" then current_module else module_path
      in
      let fn_decl =
        Option.get
        @@ KosuIrTyped.Asttyhelper.RProgram.find_function_decl_of_name fn_module
             fn_name rprogram
      in

      match fn_decl with
      | RSyscall_Decl syscall_decl ->
          let iparams, _, _ =
            Args.consume_args ~fregs:Register.float_argument_registers
              ~iregs:Register.syscall_register
              ~fpstyle:(fun { expr_rktype; _ } ->
                if KosuIrTyped.Asttyhelper.RType.is_float expr_rktype then
                  Simple_Reg Float
                else Simple_Reg Other)
              tac_parameters
          in
          let args_instructions =
            iparams
            |> List.map (fun (tte, return_kind) ->
                   let open Args in
                   match return_kind with
                   | Double_return _ ->
                       failwith "Float are passed as a simple reg"
                   | Simple_return r ->
                       translate_tac_expression ~litterals ~target_reg:r fd tte)
            |> List.flatten
          in
          let mov_syscall_code_instruction =
            LineInstruction.mv_integer Register.sc syscall_decl.opcode
          in
          let syscall_instruction = Line.instruction Instruction.syscall in
          let store_res_instructions =
            store_instruction ~rval_rktype:rvalue.rval_rktype ~large_cp:false
              ~reg:Register.r0 ~where
          in
          args_instructions @ mov_syscall_code_instruction
          @ (syscall_instruction :: store_res_instructions)
      | RKosufn_Decl kosu_function_decl -> (
          let typed_parameters =
            tac_parameters |> List.map (fun { expr_rktype; _ } -> expr_rktype)
          in
          let function_decl =
            Option.get
            @@ KosuIrTyped.Asttyhelper.RProgram
               .find_function_decl_exact_param_types ~module_name:fn_module
                 ~fn_name ~ktypes:typed_parameters rprogram
          in
          let fn_label =
            NamingConvention.label_of_kosu_function ~module_path function_decl
          in
          let iparams, fparams, stack_params =
            Args.consume_args ~fregs:Register.float_argument_registers
              ~iregs:Register.non_float_argument_registers
              ~fpstyle:(fun { expr_rktype; _ } ->
                if KosuIrTyped.Asttyhelper.RType.is_float expr_rktype then
                  Simple_Reg Float
                else Simple_Reg Other)
              tac_parameters
          in
          let args_instructions =
            iparams
            |> List.map (fun (tte, return_kind) ->
                   let open Args in
                   match return_kind with
                   | Double_return _ ->
                       failwith "Float are passed as a simple reg"
                   | Simple_return r ->
                       translate_tac_expression ~litterals ~target_reg:r fd tte)
            |> List.flatten
          in
          let float_args_instructions =
            fparams
            |> List.map (fun (tte, return_kind) ->
                   let open Args in
                   match return_kind with
                   | Double_return _ ->
                       failwith "Float are passed as a simple reg"
                   | Simple_return r ->
                       translate_tac_expression ~litterals ~target_reg:r fd tte)
            |> List.flatten
          in

          let set_on_stack_instructions = [] (* TODO *) in

          let call_instruction = LineInstruction.scall_label fn_label in
          match Register.does_return_hold_in_register_kt rvalue.rval_rktype with
          | true ->
              let normal_store_instructions =
                store_instruction ~rval_rktype:rvalue.rval_rktype
                  ~large_cp:false ~reg:Register.r0 ~where
              in
              args_instructions @ float_args_instructions
              @ set_on_stack_instructions
              @ (call_instruction :: normal_store_instructions)
          | false ->
              let return_address =
                match where with
                | None ->
                    failwith
                      "Function call: need stack location for function \
                       indirect return"
                | Some (LocReg _) ->
                    failwith
                      "Function call: return indirect value need to be on \
                       stack not register"
                | Some (LocAddr address) -> address
              in

              let mv_address_instruction =
                LineInstruction.slea_address Register.ir return_address
              in
              args_instructions @ float_args_instructions
              @ set_on_stack_instructions @ mv_address_instruction
              @ [ call_instruction ])
      | RExternal_Decl external_func_decl ->
          failwith "External function: Find a way for the vm to call them"
      (* let fn_label =
             NamingConvention.label_of_external_function external_func_decl
           in
         let novariadic_args = if external_func_decl.is_variadic then
             Option.some @@ List.length external_func_decl.fn_parameters
           else Option.none
         in
         let iparams, fparams, stack_params = Args.consume_args
           ?novariadic_args
           ~fregs:Register.float_argument_registers
           ~iregs:Register.non_float_argument_registers
           ~fpstyle:(fun {expr_rktype; _} ->
             if KosuIrTyped.Asttyhelper.RType.is_float expr_rktype then
               Simple_Reg Float
             else
               Simple_Reg Other
           ) tac_parameters
         in
         let args_instructions = iparams |> List.map (fun (tte, return_kind) ->
           let open Args in
           match return_kind with
           | Double_return _ -> failwith "Float are passed as a simple reg"
           | Simple_return r -> translate_tac_expression ~litterals ~target_reg:r fd tte
         ) in
         let float_args_instructions = fparams |> List.map (fun (tte, return_kind) ->
           let open Args in
           match return_kind with
           | Double_return _ -> failwith "Float are passed as a simple reg"
           | Simple_return r -> translate_tac_expression ~litterals ~target_reg:r fd tte
         ) in
         let call_instructions =
           LineInstruction.scall_label fn_label
         in
         match does_return_hold_in_register_kt external_func_decl.
      *))
  | RVTuple ttes ->
      let ktlis = ttes |> List.map (fun { expr_rktype; _ } -> expr_rktype) in
      let offset_list =
        ttes
        |> List.mapi (fun index _value ->
               KosuIrTyped.Sizeof.offset_of_tuple_index index ktlis rprogram)
        |> List.tl
        |> fun l -> l @ [ 0L ]
      in
      ttes
      |> List.mapi (fun index tte ->
             let where =
               where
               |> Option.map (function
                    | LocAddr address ->
                        let offset = List.nth offset_list index in
                        Location.loc_addr @@ increment_adress offset address
                    | loc -> loc)
             in
             translate_and_store ~where ~litterals ~target_reg:Register.r13 fd
               tte)
      |> List.flatten
  | RVFieldAcess
      {
        first_expr = { expr_rktype; tac_expression = TEIdentifier struct_id };
        field;
      } ->
      let struct_decl =
        match
          KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye expr_rktype
            rprogram
        with
        | Some (RDecl_Struct s) -> s
        | Some (RDecl_Enum _) ->
            failwith "Expected to find a struct get an enum"
        | None -> failwith "Non type decl ??? my validation is very weak"
      in

      let data_size = ConditionCode.data_size_of_kt rvalue.rval_rktype in

      let generics =
        expr_rktype |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
        |> List.combine struct_decl.generics
        |> List.to_seq |> Hashtbl.of_seq
      in
      let offset =
        KosuIrTyped.Sizeof.offset_of_field ~generics field struct_decl rprogram
      in
      let struct_address =
        FrameManager.location_of (struct_id, expr_rktype) fd |> fun adr ->
        match adr with
        | Some a -> a
        | None -> failwith "field access null address"
      in
      let field_address = increment_location offset struct_address in
      let is_lea = not @@ does_return_hold_in_register_kt rvalue.rval_rktype in
      let mov_field_adrress_instruction =
        LineInstruction.smv_location ~lea:is_lea ~data_size Register.r13
          field_address
      in
      let sis =
        store_instruction ~rval_rktype:rvalue.rval_rktype ~large_cp:true
          ~reg:Register.r13 ~where
      in
      mov_field_adrress_instruction @ sis
  | RVFieldAcess _ -> failwith ""
  | _ -> failwith "TODO"

let rec translate_tac_statement ~litterals current_module rprogram fd = function
| STacDeclaration { identifier; trvalue }
| STacModification { identifier; trvalue } ->  
  let location = FrameManager.location_of (identifier, trvalue.rval_rktype) fd in
  translate_tac_rvalue ~litterals ~where:location rprogram  current_module fd trvalue
| STDerefAffectation { identifier; trvalue } ->
  failwith ""
| _ -> failwith ""

and translate_tac_body ~litterals ?(end_label = None) current_module rprogram
(fd : FrameManager.description) { label; body } = 
  let label = Line.label label in
  let stmt_instrs =
    body 
    |> fst
    |> List.map (fun stmt ->
           translate_tac_statement ~litterals current_module rprogram fd stmt)
    |> List.flatten
  in
  let end_label_insts =
    end_label
    |> Option.map sjump_label
    |> Option.to_list
  in
  let return_instructions = 
    body 
    |> snd
    |> Option.map (fun tte -> 
      match does_return_hold_in_register_kt tte.expr_rktype with
      | true -> translate_tac_expression ~litterals ~target_reg:Register.r0 fd tte
      | false -> 
        let address_indirect_return = Location.get_address @@ Option.get @@ FrameManager.location_of FrameManager.indirect_return_vt fd in
        let tte_instructions = translate_tac_expression ~litterals ~target_reg:Register.r0 fd tte in
        let ldr_indirect_address = LineInstruction.sldr SIZE_64 Register.ir address_indirect_return in
        let copy_address = Location.create_address Register.ir in
        let store_res_instructions =  LineInstruction.scopy Register.r0 copy_address tte.expr_rktype in
        tte_instructions @ ldr_indirect_address @ store_res_instructions
    )
    |> Option.value ~default:[]
  in
  (label :: stmt_instrs) @ return_instructions @ end_label_insts 

let asm_module_of_tac_module ~litterals current_module rprogram = 
  let open KosuIrTyped.Asttyped in
  function
  | TacModule tac_nodes -> 
    tac_nodes |> List.filter_map (function
    | TNFunction function_decl -> 
      let asm_name =
        NamingConvention.label_of_tac_function ~module_path:current_module
          function_decl
      in
      let fd = FrameManager.frame_descriptor rprogram function_decl in

      let prologue = FrameManager.prologue function_decl fd in
      let epilogue = FrameManager.epilogue fd in
      let conversion =
        translate_tac_body ~litterals rprogram current_module fd
          function_decl.tac_body
      in
      let asm_body = prologue @ (conversion |> List.tl) @ epilogue in
      Option.some @@ 
      Afunction {
        asm_name;
        asm_body
      }
      | TNOperator operator -> 
        let function_decl = 
          KosuIrTAC.Asttachelper.OperatorDeclaration.tac_function_of_operator operator
        in
        let fd = 
          FrameManager.frame_descriptor rprogram function_decl 
        in
        let asm_name =
          NamingConvention.label_of_tac_operator 
            ~module_path:current_module
            operator
        in
        let prologue = FrameManager.prologue function_decl fd in
        let epilogue = FrameManager.epilogue fd in
        let conversion =
          translate_tac_body ~litterals rprogram current_module fd
            function_decl.tac_body
        in
        let asm_body = prologue @ (conversion |> List.tl) @ epilogue in
        Option.some @@ 
        Afunction {
          asm_name;
          asm_body
        }
      | TNConst _ -> failwith ""
      | TNEnum _ | TNStruct _ | TNSyscall _ | TNExternFunc _ ->
        None
  )

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

let rec store_instruction ~large_cp ~rval_rktype ~reg ~where =
  match where with
  | Addr_Direct None ->
      []
  | Addr_Direct (Some address) ->
      if large_cp then
        LineInstruction.scopy reg address rval_rktype
      else
        let data_size = ConditionCode.data_size_of_kt rval_rktype in
        LineInstruction.sstr data_size reg address
  | Addr_Indirect { address; offset } ->
      let tmp_reg = Register.tmp_register ~exclude:reg in
      let ldr = LineInstruction.sldr SIZE_64 tmp_reg address in
      let incr_addres =
        match offset with
        | 0L ->
            []
        | offset ->
            LineInstruction.sadd tmp_reg tmp_reg @@ Operande.ilitteral offset
      in
      let address = create_address tmp_reg in
      ldr @ incr_addres
      @ store_instruction ~large_cp ~rval_rktype ~reg
          ~where:(addr_direct address)

let return_non_register_size ~where =
  match where with
  | Addr_Direct None ->
      []
  | Addr_Direct (Some address) ->
      LineInstruction.slea_address Register.ir address
  | Addr_Indirect { address; offset } ->
      let ldr_instrs = LineInstruction.sldr SIZE_64 Register.ir address in
      let offset_instrs =
        match offset with
        | 0L ->
            []
        | offset ->
            LineInstruction.sadd Register.ir Register.ir
            @@ Operande.ilitteral offset
      in
      ldr_instrs @ offset_instrs

let address_of_addressage ?(f = fun () -> failwith "address is null") ~tmp_reg =
  function
  | Addr_Direct (Some a) ->
      ([], a)
  | Addr_Direct None ->
      f ()
  | Addr_Indirect { address; offset } ->
      let ldr = LineInstruction.sldr SIZE_64 tmp_reg address in
      let incr_addres =
        match offset with
        | 0L ->
            []
        | offset ->
            LineInstruction.sadd tmp_reg tmp_reg @@ Operande.ilitteral offset
      in
      let address = create_address tmp_reg in
      (ldr @ incr_addres, address)

let translate_tac_expression ~litterals ~target_reg fd tte =
  match tte.tac_expression with
  | TEString s ->
      let (SLit str_labl) = Hashtbl.find litterals.str_lit_map s in
      lea_label target_reg str_labl
  | TEFalse | TECmpLesser | TEmpty | TENullptr ->
      mv_integer target_reg 0L
  | TETrue | TECmpEqual ->
      mv_integer target_reg 1L
  | TECmpGreater ->
      mv_integer target_reg 2L
  | TEInt (_, _, int64) ->
      mv_integer target_reg int64
  | TEChar c ->
      let int_repr = Int64.of_int @@ Char.code c in
      mv_integer target_reg int_repr
  | TEFloat float ->
      let (FLit _float_label) = Hashtbl.find litterals.float_lit_map float in
      let bits_repr = Int64.bits_of_float @@ snd float in
      mv_integer target_reg bits_repr
  | TEIdentifier id -> (
      let loc =
        match FrameManager.location_of (id, tte.expr_rktype) fd with
        | None ->
            failwith "tte identifier setup null address"
        | Some loc ->
            loc
      in
      match loc with
      | address ->
          if Register.does_return_hold_in_register_kt tte.expr_rktype then
            let ds = ConditionCode.data_size_of_kt tte.expr_rktype in
            sldr ds target_reg address
          else
            sadd target_reg address.base address.offset
    )
  | TESizeof kt ->
      let size = KosuIrTyped.Sizeof.sizeof_kt kt in
      mv_integer target_reg size
  | TEConst { module_path; name } when tte.expr_rktype = RTString_lit ->
      lea_label target_reg ~module_path name
  | TEConst _ ->
      failwith "Other constant todo"

let translate_and_store ~where ~litterals ~target_reg fd tte =
  match where with
  | Addr_Direct (Some address) ->
      let insts = translate_tac_expression ~litterals ~target_reg fd tte in
      let cp_insts = scopy target_reg address tte.expr_rktype in
      insts @ cp_insts
  | Addr_Direct None ->
      let insts = translate_tac_expression ~litterals ~target_reg fd tte in
      insts
  | Addr_Indirect { address; offset } ->
      let tmp_reg = Register.tmp_register ~exclude:target_reg in
      let ldr = LineInstruction.sldr SIZE_64 tmp_reg address in
      let incr_addres =
        LineInstruction.sadd tmp_reg tmp_reg @@ Operande.ilitteral offset
      in
      let insts = translate_tac_expression ~litterals ~target_reg fd tte in
      let address = create_address tmp_reg in
      let cp_insts = scopy target_reg address tte.expr_rktype in
      ldr @ incr_addres @ insts @ cp_insts

let translate_tac_binop ~litterals ~cc ~blhs ~brhs ~where ~rval_rktype fd =
  let r13 = Register.r13 in
  let r14 = Register.r14 in
  let ir = Register.ir in

  let linstructions =
    translate_tac_expression ~litterals ~target_reg:r13 fd blhs
  in
  let rinstructions =
    translate_tac_expression ~litterals ~target_reg:r14 fd brhs
  in
  let cset_instruction =
    Line.instruction @@ Instruction.icset cc ir r13 r14 true
  in

  let str_instrs =
    store_instruction ~large_cp:true ~rval_rktype ~reg:ir ~where
  in
  linstructions @ rinstructions @ (cset_instruction :: str_instrs)

let translate_tac_rvalue ~litterals ~where current_module rprogram
    (fd : FrameManager.description) rvalue =
  match rvalue.rvalue with
  | RVDiscard | RVLater ->
      []
  | RVExpression tte ->
      translate_and_store ~where ~litterals ~target_reg:Register.r13 fd tte
  | RVStruct { fields; module_path = _; struct_name = _ } ->
      let struct_decl =
        match
          KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
            rvalue.rval_rktype rprogram
        with
        | Some (RDecl_Struct s) ->
            s
        | Some (RDecl_Enum _) ->
            failwith "Expected to find a struct get an enum"
        | None ->
            failwith "Non type decl ??? my validation is very weak"
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
                 rprogram
           )
      in

      fields |> List.mapi Util.couple
      |> List.fold_left
           (fun acc (index, (_field, tte)) ->
             let where =
               increment_addressage (List.nth offset_list index) where
             in
             acc
             @ translate_and_store ~where ~litterals ~target_reg:Register.r13 fd
                 tte
           )
           []
  | RVFunction { module_path; fn_name; generics_resolver = _; tac_parameters }
    -> (
      let fn_module =
        if module_path = "" then
          current_module
        else
          module_path
      in
      let fn_decl =
        Option.get
        @@ KosuIrTyped.Asttyhelper.RProgram.find_function_decl_of_name fn_module
             fn_name rprogram
      in

      match fn_decl with
      | RSyscall_Decl syscall_decl ->
          let iparams, _, _, _ =
            Args.consume_args ~fregs:Register.float_argument_registers
              ~iregs:Register.syscall_register
              ~fpstyle:(fun { expr_rktype; _ } ->
                if KosuIrTyped.Asttyhelper.RType.is_float expr_rktype then
                  Simple_Reg Float
                else
                  Simple_Reg Other
              )
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
                       translate_tac_expression ~litterals ~target_reg:r fd tte
               )
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
      | RKosufn_Decl _kosu_function_decl -> (
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
          let iparams, fparams, _stack_params, _ =
            Args.consume_args ~fregs:Register.float_argument_registers
              ~iregs:Register.non_float_argument_registers
              ~fpstyle:(fun { expr_rktype; _ } ->
                if KosuIrTyped.Asttyhelper.RType.is_float expr_rktype then
                  Simple_Reg Float
                else
                  Simple_Reg Other
              )
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
                       translate_tac_expression ~litterals ~target_reg:r fd tte
               )
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
                       translate_tac_expression ~litterals ~target_reg:r fd tte
               )
            |> List.flatten
          in

          let set_on_stack_instructions = [] (* TODO *) in

          let call_instruction = LineInstruction.sbr_label fn_label in
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
              let ldr_addrres_instr, return_address =
                address_of_addressage ~tmp_reg:Register.ir where
              in

              let mv_address_instruction =
                LineInstruction.slea_address Register.ir return_address
              in
              args_instructions @ float_args_instructions
              @ set_on_stack_instructions @ ldr_addrres_instr
              @ mv_address_instruction @ [ call_instruction ]
        )
      | RExternal_Decl _external_func_decl ->
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
      *)
    )
  | RVTuple ttes | RVArray ttes ->
      let ktlis = ttes |> List.map (fun { expr_rktype; _ } -> expr_rktype) in
      let offset_list =
        ttes
        |> List.mapi (fun index _value ->
               KosuIrTyped.Sizeof.offset_of_tuple_index index ktlis rprogram
           )
      in
      ttes
      |> List.mapi (fun index tte ->
             let where =
               increment_addressage (List.nth offset_list index) where
             in
             translate_and_store ~where ~litterals ~target_reg:Register.r13 fd
               tte
         )
      |> List.flatten
  | RVTupleAccess
      {
        first_expr = { expr_rktype; tac_expression = TEIdentifier tuple_id };
        index;
      } ->
      let kts_tuples =
        match expr_rktype with
        | KosuIrTyped.Asttyped.RTTuple rkts ->
            rkts
        | _ ->
            failwith "Weird: The typechecker for tuple"
      in
      let data_size = ConditionCode.data_size_of_kt rvalue.rval_rktype in

      let generics = Hashtbl.create 0 in

      let offset =
        KosuIrTyped.Sizeof.offset_of_tuple_index ~generics (Int64.to_int index)
          kts_tuples rprogram
      in
      let tuple_address =
        FrameManager.location_of (tuple_id, expr_rktype) fd
        |> fun adr ->
        match adr with
        | Some a ->
            a
        | None ->
            failwith "field access null address"
      in
      let field_address = increment_location offset tuple_address in
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
        | Some (RDecl_Struct s) ->
            s
        | Some (RDecl_Enum _) ->
            failwith "Expected to find a struct get an enum"
        | None ->
            failwith "Non type decl ??? my validation is very weak"
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
        FrameManager.location_of (struct_id, expr_rktype) fd
        |> fun adr ->
        match adr with
        | Some a ->
            a
        | None ->
            failwith "field access null address"
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
  | RVFieldAcess _ ->
      failwith "KosuTac force RVFieldAcess to be an tmp variable"
  | RVTupleAccess _ ->
      failwith "KosuTac force RVTupleAccess to be an tmp variable"
  | RVArrayAccess { array_expr; index_expr } ->
      let r13 = Register.r13 in
      let r14 = Register.r14 in
      let r12 = Register.r12 in
      let array_instructions =
        translate_tac_expression ~litterals ~target_reg:r13 fd array_expr
      in
      let index_instructions =
        translate_tac_expression ~litterals ~target_reg:r14 fd index_expr
      in

      let elt_type_size = KosuIrTyped.Sizeof.sizeof_kt rvalue.rval_rktype in

      let reg, mov_instrs =
        match does_return_hold_in_register_kt array_expr.expr_rktype with
        | true ->
            let mov_pointee_size_ins =
              LineInstruction.mv_integer r12 (Int64.mul 8L elt_type_size)
            in
            let shift_instructions =
              Line.instructions
                [
                  Instruction.imul r14 r14 @@ Operande.iregister r12;
                  Instruction.ilshiftright r13 r13 @@ Operande.iregister r14;
                ]
            in
            (r13, mov_pointee_size_ins @ shift_instructions)
        | false ->
            let ptr_arithmetic_instrs =
              match elt_type_size with
              | 1L ->
                  ( r13,
                    Line.instructions
                      [ Instruction.add r13 r13 @@ Operande.iregister r14 ]
                  )
              | elt_type_size ->
                  let mv_size_instr =
                    LineInstruction.mv_integer r12 elt_type_size
                  in
                  let scale_size_instructions =
                    Line.instructions
                      [
                        Instruction.imul r14 r14 @@ Operande.iregister r12;
                        Instruction.add r13 r13 @@ Operande.iregister r14;
                      ]
                  in
                  let load_instructions =
                    match
                      does_return_hold_in_register_kt rvalue.rval_rktype
                    with
                    | false ->
                        []
                    | true ->
                        let data_size =
                          ConditionCode.data_size_of_kt rvalue.rval_rktype
                        in
                        LineInstruction.sldr data_size Register.ir
                          (create_address r13)
                  in
                  ( ir,
                    mv_size_instr @ scale_size_instructions @ load_instructions
                  )
            in
            ptr_arithmetic_instrs
      in
      let store_instrs =
        store_instruction ~large_cp:true ~rval_rktype:rvalue.rval_rktype ~where
          ~reg
      in
      array_instructions @ index_instructions @ mov_instrs @ store_instrs
  | RVAdress id ->
      let pointee_type =
        KosuIrTyped.Asttyhelper.RType.rtpointee rvalue.rval_rktype
      in
      let address =
        FrameManager.location_of (id, pointee_type) fd
        |> fun adr ->
        match adr with
        | Some address ->
            address
        | None ->
            failwith "address of null address"
      in
      let r13 = Register.r13 in
      let compute_instructions = LineInstruction.slea_address r13 address in
      let sis =
        store_instruction ~rval_rktype:rvalue.rval_rktype ~large_cp:false
          ~reg:r13 ~where
      in
      compute_instructions @ sis
  | RVAdressof ra ->
      let variable, offset =
        match ra with
        | KosuIrTyped.Asttyped.RAFVariable variable ->
            (variable, 0L)
        | KosuIrTyped.Asttyped.RAFField { variable; fields } ->
            let offset =
              KosuCommon.OffsetHelper.offset_of_field_access (snd variable)
                ~fields rprogram
            in
            (variable, offset)
      in
      let base_address = Option.get @@ FrameManager.location_of variable fd in
      let lea_base_address =
        LineInstruction.slea_address Register.ir base_address
      in
      let offset_instrs =
        match offset with
        | 0L ->
            []
        | offset ->
            LineInstruction.sadd ir ir @@ Operande.ilitteral offset
      in

      let store_instrs =
        store_instruction ~where ~large_cp:false ~rval_rktype:rvalue.rval_rktype
          ~reg:Register.ir
      in
      lea_base_address @ offset_instrs @ store_instrs
  | RVDefer id ->
      let address =
        Option.get
        @@ FrameManager.location_of
             (id, rvalue.rval_rktype |> KosuIrTyped.Asttyhelper.RType.rpointer)
             fd
      in
      let load_instrs = LineInstruction.sldr SIZE_64 Register.ir address in
      let indirect_load_instrs =
        match does_return_hold_in_register_kt rvalue.rval_rktype with
        | true ->
            let data_size = ConditionCode.data_size_of_kt rvalue.rval_rktype in
            LineInstruction.sldr data_size Register.r13 (create_address ir)
        | false ->
            []
      in
      let store_instrucs =
        store_instruction ~where ~large_cp:true ~rval_rktype:rvalue.rval_rktype
          ~reg:r13
      in
      load_instrs @ indirect_load_instrs @ store_instrucs
  | RVEnum { variant; assoc_tac_exprs; _ } ->
      let enum_decl =
        match
          KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
            rvalue.rval_rktype rprogram
        with
        | Some (RDecl_Struct _) ->
            failwith "Expected to find an enum get an struct"
        | Some (RDecl_Enum e) ->
            e
        | None ->
            failwith "Non type decl ??? my validation is very weak"
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
               KosuIrTyped.Sizeof.offset_of_tuple_index index enum_type_list
                 rprogram
           )
      in
      enum_tte_list
      |> List.mapi (fun index value -> (index, value))
      |> List.fold_left
           (fun acc (index, tte) ->
             let r13 = Register.r13 in
             let instructions =
               translate_tac_expression ~target_reg:r13 ~litterals fd tte
             in
             let where =
               increment_addressage (List.nth offset_list index) where
             in
             let copy_instructions =
               store_instruction ~large_cp:true ~where ~reg:r13
                 ~rval_rktype:tte.expr_rktype
             in
             acc @ instructions @ copy_instructions
           )
           []
  | RVBuiltinBinop
      { binop = TacBool ((TacOr | TacAnd) as tac_bool); blhs; brhs } ->
      let r13 = Register.r13 in
      let r14 = Register.r14 in
      let rinstructions =
        translate_tac_expression ~litterals ~target_reg:r13 fd brhs
      in
      let linstructions =
        translate_tac_expression ~litterals ~target_reg:r14 fd blhs
      in
      let and_or_instruction =
        Line.instruction
        @@ LineInstruction.and_or_instruction tac_bool r14 r14
             (Operande.iregister r13)
      in
      let copy_instructions =
        store_instruction ~where ~large_cp:false ~rval_rktype:rvalue.rval_rktype
          ~reg:r14
      in
      linstructions @ rinstructions @ (and_or_instruction :: copy_instructions)
  | RVBuiltinBinop { binop = TacBool bool_binop; blhs; brhs } ->
      let is_unsigned =
        KosuIrTyped.Asttyhelper.RType.is_raw_unsigned blhs.expr_rktype
        || KosuIrTyped.Asttyhelper.RType.is_raw_unsigned brhs.expr_rktype
      in
      let cc =
        Option.get @@ ConditionCode.cc_of_tac_bin ~is_unsigned bool_binop
      in
      translate_tac_binop ~litterals ~cc ~blhs ~brhs ~where
        ~rval_rktype:rvalue.rval_rktype fd
  | RVBuiltinBinop { binop = TacCmp TacOrdered; blhs; brhs } ->
      let r13 = Register.r13 in
      let r14 = Register.r14 in
      let ir = Register.ir in
      let linstructions =
        translate_tac_expression ~litterals ~target_reg:r13 fd blhs
      in
      let rinstructions =
        translate_tac_expression ~litterals ~target_reg:r14 fd brhs
      in
      let cmp_instructions =
        Line.instructions
          [
            Instruction.icset ConditionCode.SUPEQ ir r13 r14 true;
            Instruction.icset ConditionCode.SUP r13 r13 r14 true;
            Instruction.add ir ir @@ Operande.iregister r13;
          ]
      in
      let str_instrs =
        store_instruction ~large_cp:true ~rval_rktype:rvalue.rval_rktype ~reg:ir
          ~where
      in
      linstructions @ rinstructions @ cmp_instructions @ str_instrs
  | RVBuiltinBinop
      { binop = TacSelf ((TacDiv | TacModulo) as tac_self); blhs; brhs } ->
      let signed =
        not
        @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer rvalue.rval_rktype
      in

      let operator_instruction =
        match tac_self with
        | TacDiv ->
            Instruction.idiv signed
        | TacModulo ->
            Instruction.imod signed
        | _ ->
            failwith "Unreachable: previously filtered"
      in

      let r13 = Register.r13 in
      let r14 = Register.r14 in

      let linstructions =
        translate_tac_expression ~litterals ~target_reg:r13 fd blhs
      in
      let rinstructions =
        translate_tac_expression ~litterals ~target_reg:r14 fd brhs
      in

      let instructions =
        Line.instruction
        @@ operator_instruction r13 r14
        @@ Operande.iregister r14
      in

      let str_instrs =
        store_instruction ~large_cp:true ~rval_rktype:rvalue.rval_rktype
          ~reg:r13 ~where
      in

      linstructions @ rinstructions @ (instructions :: str_instrs)
  | RVBuiltinBinop
      { binop = TacSelf ((TacAdd | TacMinus) as self_binop); blhs; brhs } ->
      let r13 = Register.r13 in
      let r14 = Register.r14 in
      let ir = Register.ir in

      let linstructions =
        translate_tac_expression ~litterals ~target_reg:r13 fd blhs
      in
      let rinstructions =
        translate_tac_expression ~litterals ~target_reg:r14 fd brhs
      in

      let op_i =
        match self_binop with
        | TacAdd ->
            Instruction.add
        | TacMinus ->
            Instruction.sub
        | _ ->
            failwith "Unreachable: filtered"
      in
      let instructions =
        match KosuIrTyped.Asttyhelper.RType.is_pointer rvalue.rval_rktype with
        | false ->
            let instructions =
              Line.instruction @@ op_i r13 r13 @@ Operande.iregister r14
            in
            let str_instrs =
              store_instruction ~large_cp:true ~rval_rktype:rvalue.rval_rktype
                ~reg:r13 ~where
            in
            linstructions @ rinstructions @ (instructions :: str_instrs)
        | true ->
            (* In pointer arith, the compiler force the pointer to be the lhs operande *)
            let pointee_size =
              rvalue.rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee
              |> KosuIrTyped.Sizeof.sizeof_kt
            in

            let offset_instructions =
              match pointee_size = 1L with
              | true ->
                  []
              | false ->
                  let mov_size_instructions =
                    LineInstruction.mv_integer ir pointee_size
                  in
                  let scale_size_instructions =
                    Line.instruction @@ Instruction.imul r14 r14
                    @@ Operande.iregister ir
                  in

                  mov_size_instructions @ [ scale_size_instructions ]
            in

            let instructions =
              Line.instruction @@ op_i r13 r13 @@ Operande.iregister r14
            in
            let str_instrs =
              store_instruction ~large_cp:true ~rval_rktype:rvalue.rval_rktype
                ~reg:r13 ~where
            in

            linstructions @ rinstructions @ offset_instructions
            @ (instructions :: str_instrs)
      in
      instructions
  | RVBuiltinBinop
      {
        binop =
          TacSelf
            ( ( TacMult
              | TacBitwiseAnd
              | TacBitwiseOr
              | TacBitwiseXor
              | TacShiftLeft
              | TacShiftRight ) as self_binop
            );
        blhs;
        brhs;
      } ->
      let is_unsigned =
        KosuIrTyped.Asttyhelper.RType.is_unsigned_integer blhs.expr_rktype
      in
      let operator_instruction =
        match self_binop with
        | TacMult ->
            Instruction.imul
        | TacBitwiseAnd ->
            Instruction.iand
        | TacBitwiseOr ->
            Instruction.ior
        | TacBitwiseXor ->
            Instruction.ixor
        | TacShiftLeft ->
            Instruction.ishiftleft
        | TacShiftRight when is_unsigned ->
            Instruction.ilshiftright
        | TacShiftRight ->
            Instruction.iashiftright
        | _ ->
            failwith "Unreachable: filtered before"
      in

      let r13 = Register.r13 in
      let r14 = Register.r14 in

      let linstructions =
        translate_tac_expression ~litterals ~target_reg:r13 fd blhs
      in
      let rinstructions =
        translate_tac_expression ~litterals ~target_reg:r14 fd brhs
      in

      let instructions =
        Line.instruction
        @@ operator_instruction r13 r14
        @@ Operande.iregister r14
      in

      let str_instrs =
        store_instruction ~large_cp:true ~rval_rktype:rvalue.rval_rktype
          ~reg:r13 ~where
      in

      linstructions @ rinstructions @ (instructions :: str_instrs)
  | RVBuiltinUnop { unop = TacUminus; expr } ->
      let r13 = Register.r13 in
      let instructions =
        translate_tac_expression ~litterals ~target_reg:r13 fd expr
      in
      let uminus_instruction =
        Line.instruction @@ Instruction.mvng r13 @@ Operande.iregister r13
      in
      let str_instrs =
        store_instruction ~large_cp:true ~rval_rktype:rvalue.rval_rktype
          ~reg:r13 ~where
      in

      instructions @ (uminus_instruction :: str_instrs)
  | RVBuiltinUnop { unop = TacNot; expr } ->
      let r13 = Register.r13 in
      let instructions =
        translate_tac_expression ~litterals ~target_reg:r13 fd expr
      in
      let not_instr =
        Line.instruction
        @@
        match KosuIrTyped.Asttyhelper.RType.is_bool rvalue.rval_rktype with
        | true ->
            Instruction.ixor r13 r13 @@ Operande.ilitteral 1L
        | false ->
            Instruction.mvnt r13 @@ Operande.iregister r13
      in
      let str_instrs =
        store_instruction ~large_cp:true ~rval_rktype:rvalue.rval_rktype
          ~reg:r13 ~where
      in
      instructions @ (not_instr :: str_instrs)
  | RVBuiltinCall { fn_name; parameters } ->
      let () = ignore parameters in
      let builin_call_instructions =
        match fn_name with
        | Tagof ->
          let tte = List.hd parameters in
          let address =
            match tte.tac_expression with
            | TEIdentifier i ->
                Option.get @@ FrameManager.location_of (i, tte.expr_rktype) fd
            | _ ->
                failwith
                  "Enum are rvalue, therethore they are converted as \
                   identifier"
          in
          let load_tag_instructions =
            let tag_size = ConditionCode.SIZE_32 in
            LineInstruction.sldr tag_size Register.r13 address
          in
          let copy_instructions =
            store_instruction ~large_cp:false ~where ~reg:r13 ~rval_rktype:rvalue.rval_rktype
          in
          load_tag_instructions @ copy_instructions
        | Array_len ->
          let tte = List.hd parameters in
          let array_len =
            match tte.expr_rktype with
            | RTArray { size; rktype = _ } ->
                size
            | _ ->
                failwith "Weird: it should be an pointer array type"
          in
          let mov_instrs = LineInstruction.mv_integer Register.r13 array_len in
          let copy_instructions = 
            store_instruction ~where ~reg:Register.r13 ~large_cp:false ~rval_rktype:rvalue.rval_rktype 
          in
          mov_instrs @ copy_instructions
        | Array_ptr ->
          let tte = List.hd parameters in
          let instructions =
            translate_tac_expression ~litterals ~target_reg:Register.ir fd
              tte
          in
          let copy_instructions =
            store_instruction ~large_cp:false ~where ~reg:Register.ir ~rval_rktype:rvalue.rval_rktype
          in
          instructions @ copy_instructions
        | _ ->
            failwith "BUILIN CALL TODO"
      in
      builin_call_instructions
  | RVCustomUnop { unop; expr } ->
      let open KosuIrTAC.Asttachelper.Operator in
      let op_decls =
        KosuIrTyped.Asttyhelper.RProgram.find_unary_operator_decl
          (parser_unary_op_of_tac_unary_op unop)
          expr.expr_rktype ~r_type:rvalue.rval_rktype rprogram
      in
      let op_decl =
        match op_decls with
        | t :: [] ->
            t
        | _ ->
            failwith "What the type checker has done: No unary op declaration"
      in
      let fn_label =
        NamingConvention.label_of_kosu_operator ~module_path:current_module
          op_decl
      in
      let instructions =
        translate_tac_expression ~litterals ~target_reg:Register.r0 fd expr
      in

      let br_i = LineInstruction.sbr_label fn_label in
      let return_type =
        KosuIrTyped.Asttyhelper.OperatorDeclaration.op_return_type op_decl
      in

      let all_instructions =
        match does_return_hold_in_register_kt return_type with
        | true ->
            instructions
            @ br_i
              :: store_instruction ~where ~large_cp:true
                   ~rval_rktype:return_type ~reg:Register.r0
        | false ->
            let indirect_return_instructions =
              return_non_register_size ~where
            in
            instructions @ (br_i :: indirect_return_instructions)
      in
      all_instructions
  | RVCustomBinop
      ({ binop = TacSelf _ | TacBool _ | TacCmp TacOrdered; blhs; brhs } as self)
    ->
      let open KosuIrTAC.Asttachelper.Operator in
      let op_decls =
        KosuIrTyped.Asttyhelper.RProgram.find_binary_operator_decl
          (parser_binary_op_of_tac_binary_op self.binop)
          (blhs.expr_rktype, brhs.expr_rktype)
          ~r_type:rvalue.rval_rktype rprogram
      in
      let op_decl =
        match op_decls with
        | t :: [] ->
            t
        | _ ->
            failwith
              "What the type checker has done: No binary op declaration | Too \
               much"
      in
      let fn_label =
        NamingConvention.label_of_kosu_operator ~module_path:current_module
          op_decl
      in
      let linstructions =
        translate_tac_expression ~litterals ~target_reg:r0 fd blhs
      in
      let rinstructions =
        translate_tac_expression ~litterals ~target_reg:r1 fd brhs
      in
      let br_i = LineInstruction.sbr_label fn_label in
      let return_type =
        KosuIrTyped.Asttyhelper.OperatorDeclaration.op_return_type op_decl
      in

      let all_instructions =
        match does_return_hold_in_register_kt return_type with
        | true ->
            linstructions @ rinstructions
            @ br_i
              :: store_instruction ~where ~large_cp:true
                   ~rval_rktype:return_type ~reg:Register.r0
        | false ->
            let indirect_return_instructions =
              return_non_register_size ~where
            in
            linstructions @ rinstructions
            @ (br_i :: indirect_return_instructions)
      in
      all_instructions

let rec translate_tac_statement ~litterals current_module rprogram fd = function
  | STacDeclaration { identifier; trvalue }
  | STacModification { identifier; trvalue } ->
      let location =
        FrameManager.location_of (identifier, trvalue.rval_rktype) fd
      in
      translate_tac_rvalue ~litterals ~where:(adrr_direct_raw location)
        current_module rprogram fd trvalue
  | STDerefAffectation { identifier; trvalue } ->
      let intermediary_adress =
        Option.get
        @@ FrameManager.location_of
             ( identifier,
               KosuIrTyped.Asttyhelper.RType.rpointer trvalue.rval_rktype
             )
             fd
      in
      let addressage = addr_indirect intermediary_adress in
      translate_tac_rvalue ~litterals ~where:addressage current_module rprogram
        fd trvalue
  | STacModificationField { identifier_root; fields; trvalue } ->
      let root_adress =
        Option.get @@ FrameManager.location_of identifier_root fd
      in
      let field_offset =
        KosuCommon.OffsetHelper.offset_of_field_access (snd identifier_root)
          ~fields rprogram
      in
      let target_adress =
        addr_direct @@ increment_adress field_offset root_adress
      in
      translate_tac_rvalue ~litterals ~where:target_adress current_module
        rprogram fd trvalue
  | STDerefAffectationField { identifier_root; fields; trvalue } ->
      let intermediary_address =
        Option.get @@ FrameManager.location_of identifier_root fd
      in
      let pointee_type =
        (fun (_, kt) -> KosuIrTyped.Asttyhelper.RType.rtpointee kt)
          identifier_root
      in
      let field_offset =
        KosuCommon.OffsetHelper.offset_of_field_access pointee_type ~fields
          rprogram
      in
      let where = addr_indirect ~offset:field_offset intermediary_address in
      translate_tac_rvalue ~litterals ~where current_module rprogram fd trvalue
  | STWhile
      {
        statements_condition;
        condition;
        loop_body;
        self_label;
        inner_body_label = _;
        exit_label;
      } ->
      let label = Line.label self_label in
      let stmts_bools =
        statements_condition
        |> List.map
             (translate_tac_statement ~litterals current_module rprogram fd)
        |> List.flatten
      in
      let condition_rvalue_inst =
        translate_tac_expression ~target_reg:Register.r13 ~litterals fd
          condition
      in
      let mov_condition = LineInstruction.mv_integer r14 0L in
      let cmp =
        Line.instruction @@ Instruction.cmp ConditionCode.EQUAL r13 r14
      in
      let jmp = LineInstruction.sjump_label self_label in
      let if_block =
        translate_tac_body ~litterals ~end_label:(Some self_label)
          current_module rprogram fd loop_body
      in
      let exit_label = Line.label exit_label in
      (label :: stmts_bools) @ condition_rvalue_inst @ mov_condition
      @ (cmp :: jmp :: if_block) @ [ exit_label ]
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
      let stmts_bools =
        statement_for_bool
        |> List.map
             (translate_tac_statement ~litterals current_module rprogram fd)
        |> List.flatten
      in
      let condition_rvalue_inst =
        translate_tac_expression ~target_reg:Register.r13 ~litterals fd
          condition_rvalue
      in
      let mov_condition = LineInstruction.mv_integer r14 1L in
      let cmp =
        Line.instruction @@ Instruction.cmp ConditionCode.EQUAL r13 r14
      in
      let jmp = LineInstruction.sjump_label goto1 in
      let cmptrue = Line.instruction @@ Instruction.cmp ALWAYS r0 r0 in
      let jmp2 = LineInstruction.sjump_label goto2 in

      let if_block =
        translate_tac_body ~litterals ~end_label:(Some exit_label)
          current_module rprogram fd if_tac_body
      in
      let else_block =
        translate_tac_body ~litterals ~end_label:(Some exit_label)
          current_module rprogram fd else_tac_body
      in

      let exit_label_instr = Line.label exit_label in
      stmts_bools @ condition_rvalue_inst @ mov_condition
      @ (cmp :: jmp :: cmptrue :: jmp2 :: if_block)
      @ else_block @ [ exit_label_instr ]
  | SCases { cases; else_tac_body; exit_label } ->
      let map_case scases =
        let setup_next_label_instr =
          scases.condition_label
          |> Option.map LineInstruction.sjump_always
          |> Option.value ~default:[]
        in

        let setup_conditions_instructions =
          scases.statement_for_condition
          |> List.map
               (translate_tac_statement ~litterals current_module rprogram fd)
          |> List.flatten
        in
        let condition =
          translate_tac_expression ~litterals ~target_reg:Register.r13 fd
            scases.condition
        in
        let mv_rhs_instr =
          Line.instruction
          @@ Instruction.mv Register.r14
          @@ Operande.ilitteral 1L
        in
        let cmp_instr =
          Line.instruction @@ Instruction.cmp EQUAL Register.r13 r14
        in
        let if_true_instruction = LineInstruction.sjump_label scases.goto in

        let if_false_instructions =
          LineInstruction.sjump_always scases.jmp_false
        in
        let body_instruction =
          translate_tac_body ~litterals ~end_label:(Some scases.end_label)
            current_module rprogram fd scases.tac_body
        in
        ( body_instruction,
          setup_next_label_instr @ setup_conditions_instructions @ condition
          @ [ mv_rhs_instr; cmp_instr; if_true_instruction ]
          @ if_false_instructions
        )
      in

      let cases_body, cases_condition =
        cases |> List.map map_case |> List.split
        |> fun (lhs, rhs) -> (List.flatten lhs, List.flatten rhs)
      in
      let end_label_instruction = Line.label exit_label in
      let else_body_instruction =
        translate_tac_body ~litterals ~end_label:(Some exit_label)
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
        | Some (RDecl_Enum e) ->
            e
        | None ->
            failwith "Non type decl ??? my validation is very weak"
      in
      let enum_decl =
        let generics =
          condition_switch.expr_rktype
          |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
          |> List.combine enum_decl.generics
        in
        KosuIrTyped.Asttyhelper.Renum.instanciate_enum_decl generics enum_decl
      in
      let exit_label_instruction = Line.label sw_exit_label in
      let setup_instructions =
        statemenets_for_case
        |> List.map
             (translate_tac_statement ~litterals current_module rprogram fd)
        |> List.flatten
      in
      let condition_switch_instruction =
        translate_tac_expression ~litterals ~target_reg:Register.r13 fd
          condition_switch
      in
      let copy_tag_instructions =
        match does_return_hold_in_register_kt condition_switch.expr_rktype with
        | true ->
            failwith ""
        | false ->
            let tag_size = ConditionCode.SIZE_32 in
            LineInstruction.sldr tag_size Register.r13
              (create_address Register.r13)
      in
      let switch_variable_name =
        match condition_switch.tac_expression with
        | TEIdentifier id ->
            id
        | _ ->
            failwith "I need to get the id"
      in

      (* For each associate bound variable, copy thoses variable before the comparison of tag *)
      let fetch_offset_instructions assoc_type_for_variants (index, id, ktyte) =
        let offset_a =
          KosuIrTyped.Sizeof.offset_of_tuple_index (index + 1)
            assoc_type_for_variants rprogram
        in
        let switch_variable_address =
          Option.get
          @@ FrameManager.location_of
               (switch_variable_name, condition_switch.expr_rktype)
               fd
        in

        let destination_address =
          Option.get @@ FrameManager.location_of (id, ktyte) fd
        in

        let data_size = ConditionCode.data_size_of_kt ktyte in
        let copy_instructions =
          match does_return_hold_in_register_kt ktyte with
          | true ->
              let ldr_instr =
                LineInstruction.sldr data_size Register.ir
                  (increment_adress offset_a switch_variable_address)
              in
              let str_instrs =
                LineInstruction.sstr data_size Register.ir destination_address
              in
              ldr_instr @ str_instrs
          | false ->
              let i = increment_adress offset_a switch_variable_address in
              let lea_instr = LineInstruction.slea_address Register.r13 i in
              let str_instructions =
                store_instruction
                  ~where:(addr_direct destination_address)
                  ~large_cp:true ~rval_rktype:ktyte ~reg:Register.r13
              in
              lea_instr @ str_instructions
        in
        copy_instructions
      in

      let map_switch sw_case =
        let map_variant mvariant =
          let tag = tag_of_variant mvariant enum_decl in
          let mov_tag_instr =
            LineInstruction.mv_integer r14 @@ Int64.of_int32 tag
          in
          let compare =
            Line.instruction @@ Instruction.cmp EQUAL Register.r13 r14
          in
          let assoc_type_for_variants =
            KosuIrTyped.Asttyhelper.Renum.assoc_types_of_variant ~tagged:true
              mvariant enum_decl
          in
          let fetch_offset_instructions =
            sw_case.assoc_bound
            |> List.map (fetch_offset_instructions assoc_type_for_variants)
            |> List.flatten
          in
          let jmp_true = LineInstruction.sjump_label sw_case.sw_goto in
          fetch_offset_instructions @ mov_tag_instr @ [ compare; jmp_true ]
        in

        let jump_conditions =
          sw_case.variants_to_match |> List.map map_variant |> List.flatten
        in
        let genete_block =
          translate_tac_body ~litterals ~end_label:(Some sw_case.sw_exit_label)
            current_module rprogram fd sw_case.switch_tac_body
        in
        (jump_conditions, genete_block)
      in

      let cmp_instrution_list, fn_block =
        sw_cases |> List.map map_switch |> List.split
        |> fun (lhs, rhs) -> (List.flatten lhs, List.flatten rhs)
      in
      let wildcard_case_jmp =
        wildcard_label
        |> Option.map LineInstruction.sjump_always
        |> Option.value ~default:[]
      in
      let wildcard_body_block =
        wildcard_body
        |> Option.map (fun body ->
               translate_tac_body ~litterals ~end_label:(Some sw_exit_label)
                 current_module rprogram fd body
           )
        |> Option.value ~default:[]
      in
      setup_instructions @ condition_switch_instruction @ copy_tag_instructions
      @ cmp_instrution_list @ wildcard_case_jmp @ fn_block @ wildcard_body_block
      @ [ exit_label_instruction ]
  | STSwitchTmp
      {
        tmp_statemenets_for_case;
        enum_tte;
        tag_atom;
        tmp_switch_list;
        tmp_wildcard_label;
        tmp_wildcard_body;
        tmp_sw_exit_label;
      } ->
      let enum_decl =
        match
          KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
            enum_tte.expr_rktype rprogram
        with
        | Some (RDecl_Struct _) ->
            failwith "Expected to find an enum get an struct"
        | Some (RDecl_Enum e) ->
            e
        | None ->
            failwith "Non type decl ??? my validation is very weak"
      in
      let enum_decl =
        let generics =
          enum_tte.expr_rktype
          |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
          |> List.combine enum_decl.generics
        in
        KosuIrTyped.Asttyhelper.Renum.instanciate_enum_decl generics enum_decl
      in
      let exit_label_instruction = Line.label tmp_sw_exit_label in
      let setup_instructions =
        tmp_statemenets_for_case
        |> List.map
             (translate_tac_statement ~litterals current_module rprogram fd)
        |> List.flatten
      in
      let mov_tag_register_instructions =
        translate_tac_expression ~litterals ~target_reg:Register.r13 fd tag_atom
      in
      let enum_variabe =
        match enum_tte.tac_expression with
        | TEIdentifier id ->
            (id, enum_tte.expr_rktype)
        | _ ->
            failwith "I need to get the id of the enum"
      in

      let fetch_offset_instructions assoc_type_for_variants (index, id, ktyte) =
        let offset_a =
          KosuIrTyped.Sizeof.offset_of_tuple_index (index + 1)
            assoc_type_for_variants rprogram
        in
        let switch_variable_address =
          Option.get @@ FrameManager.location_of enum_variabe fd
        in

        let destination_address =
          Option.get @@ FrameManager.location_of (id, ktyte) fd
        in

        let data_size = ConditionCode.data_size_of_kt ktyte in
        let copy_instructions =
          match does_return_hold_in_register_kt ktyte with
          | true ->
              let ldr_instr =
                LineInstruction.sldr data_size Register.ir
                  (increment_adress offset_a switch_variable_address)
              in
              let str_instrs =
                LineInstruction.sstr data_size Register.ir destination_address
              in
              ldr_instr @ str_instrs
          | false ->
              let i = increment_adress offset_a switch_variable_address in
              let lea_instr = LineInstruction.slea_address Register.r13 i in
              let str_instructions =
                store_instruction
                  ~where:(addr_direct destination_address)
                  ~large_cp:true ~rval_rktype:ktyte ~reg:Register.r13
              in
              lea_instr @ str_instructions
        in
        copy_instructions
      in

      let map_switch switch =
        let map_variant variant =
          let mov_rhs_instr =
            LineInstruction.mv_integer Register.r14
              (Int64.of_int variant.variant_index)
          in
          let compare_instruction =
            Line.instruction @@ Instruction.cmp EQUAL Register.r13 Register.r14
          in
          let assoc_type_for_variants =
            KosuIrTyped.Asttyhelper.Renum.assoc_types_of_variant_tag
              ~tagged:true variant.variant_index enum_decl
          in
          let fetch_offset_instructions =
            switch.tmp_assoc_bound
            |> List.map (fetch_offset_instructions assoc_type_for_variants)
            |> List.flatten
          in
          let jmp_true = LineInstruction.sjump_label switch.tmp_sw_goto in
          fetch_offset_instructions @ mov_rhs_instr
          @ [ compare_instruction; jmp_true ]
        in
        let jump_conditions =
          switch.variants |> List.map map_variant |> List.flatten
        in
        let genete_block =
          translate_tac_body ~litterals
            ~end_label:(Some switch.tmp_sw_exit_label) current_module rprogram
            fd switch.tmp_switch_tac_body
        in
        (jump_conditions, genete_block)
      in
      let cmp_instrution_list, fn_block =
        tmp_switch_list |> List.map map_switch |> List.split
        |> fun (lhs, rhs) -> (List.flatten lhs, List.flatten rhs)
      in
      let wildcard_case_jmp =
        tmp_wildcard_label
        |> Option.map LineInstruction.sjump_always
        |> Option.value ~default:[]
      in
      let wildcard_body_block =
        tmp_wildcard_body
        |> Option.map (fun body ->
               translate_tac_body ~litterals ~end_label:(Some tmp_sw_exit_label)
                 current_module rprogram fd body
           )
        |> Option.value ~default:[]
      in

      setup_instructions @ mov_tag_register_instructions @ cmp_instrution_list
      @ wildcard_case_jmp @ fn_block @ wildcard_body_block
      @ [ exit_label_instruction ]

and translate_tac_body ~litterals ?(end_label = None) current_module rprogram
    (fd : FrameManager.description) { label; body } =
  let label = Line.label label in
  let stmt_instrs =
    body |> fst
    |> List.map (fun stmt ->
           translate_tac_statement ~litterals current_module rprogram fd stmt
       )
    |> List.flatten
  in
  let end_label_insts =
    end_label
    |> Option.map (fun end_label ->
           let true_exit = Line.instruction @@ Instruction.cmp ALWAYS r0 r0 in
           [ true_exit; sjump_label end_label ]
       )
    |> Option.value ~default:[]
  in
  let return_instructions =
    body |> snd
    |> Option.map (fun tte ->
           match does_return_hold_in_register_kt tte.expr_rktype with
           | true ->
               translate_tac_expression ~litterals ~target_reg:Register.r0 fd
                 tte
           | false ->
               let address_indirect_return =
                 Location.get_address @@ Option.get
                 @@ FrameManager.location_of FrameManager.indirect_return_vt fd
               in
               let tte_instructions =
                 translate_tac_expression ~litterals ~target_reg:Register.r0 fd
                   tte
               in
               let ldr_indirect_address =
                 LineInstruction.sldr SIZE_64 Register.ir
                   address_indirect_return
               in
               let copy_address = Location.create_address Register.ir in
               let store_res_instructions =
                 LineInstruction.scopy Register.r0 copy_address tte.expr_rktype
               in
               tte_instructions @ ldr_indirect_address @ store_res_instructions
       )
    |> Option.value ~default:[]
  in
  (label :: stmt_instrs) @ return_instructions @ end_label_insts

let asm_module_of_tac_module ~litterals current_module rprogram = function
  | TacModule tac_nodes ->
      tac_nodes
      |> List.filter_map (function
           | TNFunction function_decl ->
               let asm_name =
                 NamingConvention.label_of_tac_function
                   ~module_path:current_module function_decl
               in
               let fd = FrameManager.frame_descriptor rprogram function_decl in

               let prologue = FrameManager.prologue function_decl fd in
               let epilogue = FrameManager.epilogue fd in
               let conversion =
                 translate_tac_body ~litterals current_module rprogram fd
                   function_decl.tac_body
               in
               let asm_body = prologue @ List.tl conversion @ epilogue in
               Option.some @@ Afunction { asm_name; asm_body }
           | TNOperator operator ->
               let function_decl =
                 KosuIrTAC.Asttachelper.OperatorDeclaration
                 .tac_function_of_operator operator
               in
               let fd = FrameManager.frame_descriptor rprogram function_decl in
               let asm_name =
                 NamingConvention.label_of_tac_operator
                   ~module_path:current_module operator
               in
               let prologue = FrameManager.prologue function_decl fd in
               let epilogue = FrameManager.epilogue fd in
               let conversion =
                 translate_tac_body ~litterals current_module rprogram fd
                   function_decl.tac_body
               in
               let asm_body = prologue @ List.tl conversion @ epilogue in
               Option.some @@ Afunction { asm_name; asm_body }
           | TNConst _ ->
               failwith "Const"
           | TNEnum _ | TNStruct _ | TNSyscall _ | TNExternFunc _ ->
               None
           )

let asm_module_path_of_tac_module_path ~litterals rprogram { path; tac_module }
    =
  {
    apath = path;
    asm_module =
      AsmModule (asm_module_of_tac_module ~litterals path rprogram tac_module);
  }

let asm_program_of_tac_program ~(start : string option) tac_program =
  ignore start;
  tac_program
  |> List.map (fun ({ filename; tac_module_path; rprogram } as named) ->
         let str_lit_map = map_string_litteral_of_named_rmodule_path named () in
         let float_lit_map =
           KosuIrTAC.Asttachelper.FloatLitteral
           .map_float_litteral_of_named_rmodule_path named ()
         in
         let litterals = { str_lit_map; float_lit_map } in
         {
           filename =
             filename |> Filename.chop_extension |> Printf.sprintf "%s.bc.s";
           asm_module_path =
             asm_module_path_of_tac_module_path ~litterals rprogram
               tac_module_path;
           rprogram;
           litterals;
         }
     )

let sort_asm_module (AsmModule anodes) =
  AsmModule
    (anodes
    |> List.sort (fun lhs rhs ->
           match (lhs, rhs) with
           | Afunction _, AConst _ ->
               1
           | AConst _, Afunction _ ->
               -1
           | _ ->
               0
       )
    )

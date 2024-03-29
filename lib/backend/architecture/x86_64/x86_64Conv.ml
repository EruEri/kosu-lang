(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
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

open X86_64Core
open X86_64Core.Register
open X86_64Core.Operande
open X86_64Core.Instruction
open KosuIrTyped.Sizeof
open KosuIrTAC.Asttac
open Util

let sizeofn = KosuIrTyped.Sizeof.sizeof

module X86Program = KosuCommon.AsmProgram (X86_64Core.Instruction)
open X86Program

module Make (Spec : X86_64AsmSpec.X86_64AsmSpecification) = struct
  module X86_64Pprint = X86_64Pprint.Make (Spec)

  let copy_result ~where ~register ~rval_rktype rprogram =
    where
    |> Option.map (fun waddress ->
           copy_from_reg register waddress rval_rktype rprogram
       )
    |> Option.value ~default:[]

  let return_function_instruction_reg_size ~where ~is_deref ~return_reg
      ~return_type rprogram =
    where
    |> Option.map (fun waddress ->
           match is_deref with
           | Some pointer ->
               Instruction
                 (Mov
                    {
                      size = IntSize Q;
                      destination = `Register rdi;
                      source = `Address pointer;
                    }
                 )
               :: copy_from_reg return_reg
                    (create_address_offset rdi)
                    return_type rprogram
           | None ->
               copy_from_reg return_reg waddress return_type rprogram
       )
    |> Option.value ~default:[]

  let return_function_instruction_none_reg_size ~where ~is_deref =
    where
    |> Option.map (fun waddress ->
           match is_deref with
           | Some pointer ->
               [
                 Instruction
                   (Mov
                      {
                        size = iq;
                        destination = `Register rdi;
                        source = `Address pointer;
                      }
                   );
                 Instruction
                   (Mov
                      {
                        size = iq;
                        destination = `Register rdi;
                        source = `Address (create_address_offset rdi);
                      }
                   );
               ]
           | None ->
               [
                 Instruction
                   (Lea { size = iq; destination = rdi; source = waddress });
               ]
       )
    |> Option.value ~default:[]

  let translate_tac_expression ~litterals ~(target_dst : dst) rprogram
      (fd : FrameManager.frame_desc) tte =
    let expr_rktype = tte.expr_rktype in
    match tte.tac_expression with
    | TEString s ->
        let (SLit str_labl) = Hashtbl.find litterals.str_lit_map s in
        (target_dst, load_label (Spec.label_of_constant str_labl) target_dst)
    | TEFalse | TEmpty -> (
        match target_dst with
        | `Register _ as rreg ->
            ( target_dst,
              [
                Instruction (Xor { size = L; source = rreg; destination = rreg });
              ]
            )
        | `Address _ as addr ->
            ( target_dst,
              [
                Instruction
                  (Mov { size = ib; destination = addr; source = `ILitteral 0L });
              ]
            )
      )
    | TENullptr -> (
        match target_dst with
        | `Register _ as rreg ->
            ( target_dst,
              [
                Instruction (Xor { size = Q; source = rreg; destination = rreg });
              ]
            )
        | `Address _ as addr ->
            ( target_dst,
              [
                Instruction
                  (Mov { size = iq; destination = addr; source = `ILitteral 0L });
              ]
            )
      )
    | TETrue ->
        ( target_dst,
          [
            Instruction
              (Mov
                 {
                   size = IntSize L;
                   destination = target_dst;
                   source = `ILitteral 1L;
                 }
              );
          ]
        )
    | TEChar c ->
        let code = Char.code c |> Int64.of_int in

        ( target_dst,
          [
            Instruction
              (Mov
                 {
                   size = IntSize B;
                   destination = target_dst;
                   source = `ILitteral code;
                 }
              );
          ]
        )
    | TEInt (_, isize, int64) ->
        let is_32bits_encodable = int64 < Int64.of_int32 Int32.max_int in
        let size = data_size_of_isize isize in
        let scaled_data_size = (function Q -> Q | _ -> L) size in
        let instrs =
          match (target_dst, is_32bits_encodable) with
          | (`Register _ as dst), _ | dst, true ->
              [
                Instruction
                  (Mov
                     {
                       size = IntSize scaled_data_size;
                       source = `ILitteral int64;
                       destination = dst;
                     }
                  );
              ]
          | (`Address _ as dst), _ ->
              let r0 = `Register Register.rax in
              let mov_rax =
                Instruction
                  (Mov
                     {
                       size = IntSize scaled_data_size;
                       source = `ILitteral int64;
                       destination = r0;
                     }
                  )
              in
              let mov_memory =
                Instruction
                  (Mov
                     {
                       size = IntSize scaled_data_size;
                       source = r0;
                       destination = dst;
                     }
                  )
              in
              [ mov_rax; mov_memory ]
        in
        (target_dst, instrs)
    | TEFloat float ->
        let (FLit float_label) = Hashtbl.find litterals.float_lit_map float in
        let fsize = fst float in
        let instructions = load_float_label fsize float_label target_dst in
        (target_dst, instructions)
    | TEIdentifier id -> (
        let adress =
          FrameManager.address_of (id, expr_rktype) fd
          |> fun adr ->
          match adr with
          | Some a ->
              a
          | None ->
              failwith "X86_64: tte identifier setup null address"
        in
        let sizeof = sizeofn rprogram expr_rktype in
        let data_size = data_size_of_ktype rprogram expr_rktype in
        match is_register_size sizeof with
        | true -> (
            let is_integer =
              KosuIrTyped.Asttyhelper.RType.is_any_integer expr_rktype
            in
            match target_dst with
            | `Register _ as reg ->
                ( target_dst,
                  if is_integer then
                    let sign, isize =
                      Option.get
                      @@ KosuIrTyped.Asttyhelper.RType.integer_info expr_rktype
                    in
                    let idsize = data_size_of_isize isize in
                    mov_promote_sign ~sign ~size:idsize ~src:(`Address adress)
                      reg
                  else
                    [
                      Instruction
                        (Mov
                           {
                             size = data_size;
                             destination = reg;
                             source = `Address adress;
                           }
                        );
                    ]
                )
            | `Address _ ->
                let rax = tmp_rax_ktype expr_rktype in
                ( target_dst,
                  [
                    Instruction
                      (Mov
                         {
                           size = data_size;
                           destination = `Register rax;
                           source = `Address adress;
                         }
                      );
                    (* Line_Com (Comment "Miiddle"); *)
                    Instruction
                      (Mov
                         {
                           size = data_size;
                           destination = target_dst;
                           source = `Register rax;
                         }
                      );
                  ]
                )
          )
        | false -> (
            match target_dst with
            | `Register reg as vreg ->
                ( vreg,
                  [
                    (* Line_Com (Comment "Here"); *)
                    Instruction
                      (Lea { size = iq; source = adress; destination = reg });
                  ]
                )
            | `Address _ ->
                ( `Register r11,
                  [
                    Line_Com (Comment "Mov identifier larger than reg");
                    Instruction
                      (Lea { size = iq; source = adress; destination = r11 });
                  ]
                )
          )
      )
    | TESizeof kt ->
        let sizeof = sizeofn rprogram kt in
        ( target_dst,
          [
            Line_Com (Comment "Sizeof ");
            Instruction
              (Mov
                 {
                   size = iq;
                   destination = target_dst;
                   source = `ILitteral sizeof;
                 }
              );
          ]
        )
    | TEConst { name; module_path }
      when KosuIrTyped.Asttyhelper.RType.is_float expr_rktype ->
        let fsize =
          Option.get @@ KosuIrTyped.Asttyhelper.RType.float_info expr_rktype
        in
        let instructions =
          load_float_label fsize
            (Spec.label_of_constant ~module_path name)
            target_dst
        in
        (target_dst, instructions)
    | TEConst { name; module_path } when expr_rktype = RTString_lit ->
        ( target_dst,
          load_label (Spec.label_of_constant ~module_path name) target_dst
        )
    | TEConst { name; module_path }
      when KosuIrTyped.Asttyhelper.RType.is_any_integer expr_rktype ->
        let _, size =
          Option.get @@ KosuIrTyped.Asttyhelper.RType.integer_info expr_rktype
        in
        let data_size = data_size_of_isize size in
        let const_decl =
          match
            rprogram
            |> KosuIrTyped.Asttyhelper.RProgram.find_const_decl ~name
                 ~module_path
          with
          | None ->
              failwith
                (Printf.sprintf "No const decl for %s::%s" module_path name)
          | Some s ->
              s
        in
        let int_value =
          match const_decl.value.rexpression with
          | REInteger (_, _, value) ->
              value
          | _ ->
              failwith "Not an Integer"
        in

        ( target_dst,
          [
            Instruction
              (Mov
                 {
                   size = IntSize data_size;
                   source = `ILitteral int_value;
                   destination = target_dst;
                 }
              );
          ]
        )
    | TEConst { name = _; module_path = _ } ->
        failwith "Other constant"
    | TECmpEqual ->
        ( target_dst,
          [
            Instruction
              (Mov
                 { size = ib; destination = target_dst; source = `ILitteral 1L }
              );
          ]
        )
    | TECmpLesser ->
        ( target_dst,
          [
            Instruction
              (Mov
                 { size = ib; destination = target_dst; source = `ILitteral 0L }
              );
          ]
        )
    | TECmpGreater ->
        ( target_dst,
          [
            Instruction
              (Mov
                 { size = ib; destination = target_dst; source = `ILitteral 2L }
              );
          ]
        )

  let move_tte ~litterals ~where ?(offset = 0L) rprogram fd tte =
    let where = increment_dst_address offset where in
    let size_of_tte = sizeofn rprogram tte.expr_rktype in
    match is_register_size size_of_tte with
    | true ->
        translate_tac_expression ~litterals ~target_dst:where rprogram fd tte
    | false ->
        let last_dst, tte_instructions =
          translate_tac_expression ~litterals ~target_dst:where rprogram fd tte
        in
        let last_reg =
          match last_dst with
          | `Register reg ->
              reg
          | `Address _ ->
              failwith "Tte size doesnt hold in reg : need address in reg"
        in
        let copy_if_addre =
          match where with
          | `Register _ ->
              []
          | `Address addr ->
              copy_large ~address_str:addr
                ~base_address_reg:(create_address_offset last_reg)
                size_of_tte
        in
        (last_dst, tte_instructions @ copy_if_addre)

  let translate_tac_binop ~litterals ~cc ~blhs ~brhs ~where rval_rktype rprogram
      fd =
    let tr10 = tmp_r10_ktype brhs.expr_rktype in
    let trax = tmp_rax_ktype brhs.expr_rktype in
    let data_size = data_size_of_ktype rprogram blhs.expr_rktype in
    let _right_reg, rinstructions =
      translate_tac_expression ~litterals ~target_dst:(`Register tr10) rprogram
        fd brhs
    in
    let _left_reg, linstructions =
      translate_tac_expression ~litterals ~target_dst:(`Register trax) rprogram
        fd blhs
    in
    let copy_instructions =
      where
      |> Option.map (fun waddress ->
             let cmp_instruction =
               Instruction
                 (cmp_instruction data_size ~lhs:(`Register trax)
                    ~rhs:(`Register tr10)
                 )
             in
             let equal_instruction =
               Instruction (Set { cc; size = B; register = Register.rax })
             in
             [ cmp_instruction; equal_instruction ]
             @ copy_from_reg rax waddress rval_rktype rprogram
         )
      |> Option.value ~default:[]
    in
    rinstructions @ linstructions @ copy_instructions

  let translate_tac_binop_self ~is_shift ~litterals ~blhs ~brhs ~where fbinop
      rval_rktype rprogram fd =
    let rax = tmp_rax_ktype blhs.expr_rktype in
    let rcx = tmp_rcx_ktype brhs.expr_rktype in
    let right_reg, rinstructions =
      translate_tac_expression ~litterals ~target_dst:(`Register rcx) rprogram
        fd brhs
    in
    let left_reg, linstructions =
      translate_tac_expression ~litterals ~target_dst:(`Register rax) rprogram
        fd blhs
    in

    let left_reg = Operande.register_of_dst left_reg in
    let right_reg = Operande.register_of_dst right_reg in

    let reg_size = data_size_of_ktype ~default:Q rprogram rval_rktype in
    let mult_instruction =
      fbinop ~size:reg_size ~destination:(`Register left_reg)
        ~source:
          (`Register
            ( if is_shift then
                Register.cl
              else
                right_reg
            )
            )
    in
    let copy_instruction =
      where
      |> Option.map (fun waddress ->
             copy_from_reg left_reg waddress rval_rktype rprogram
         )
      |> Option.value ~default:[]
    in

    linstructions @ rinstructions @ mult_instruction @ copy_instruction

  let translate_tac_rvalue ?(is_deref = None) ~litterals
      ~(where : address option) current_module rprogram
      (fd : FrameManager.frame_desc) { rvalue; rval_rktype } =
    match rvalue with
    | RVExpression tte ->
        let size_to_move = sizeofn rprogram tte.expr_rktype in
        let _last_dst, instructions =
          where
          |> Option.map (fun waddress ->
                 let last_dst, tte_instructions =
                   translate_tac_expression ~litterals
                     ~target_dst:(`Address waddress) rprogram fd tte
                 in
                 let copy_instructions =
                   match last_dst with
                   | `Address _ ->
                       []
                   | `Register reg ->
                       copy_large ~address_str:waddress
                         ~base_address_reg:(create_address_offset reg)
                         size_to_move
                 in
                 (last_dst, tte_instructions @ copy_instructions)
             )
          |> Option.value ~default:(dummy_dst, [])
        in
        instructions
    | RVStruct { module_path = _; struct_name = _; fields } ->
        let struct_decl =
          match
            KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye
              rval_rktype rprogram
          with
          | Some (RDecl_Struct s) ->
              s
          | Some (RDecl_Enum _) ->
              failwith "Expected to find a struct get an enum"
          | None ->
              failwith "Non type decl ??? my validation is very weak"
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
                 offset_of_field ~generics field struct_decl rprogram
             )
        in
        let instructions =
          fields |> List.mapi couple
          |> List.fold_left
               (fun acc (index, (_, tte)) ->
                 let offset = List.nth offset_list index in
                 let instructions =
                   where
                   |> Option.map (fun waddress ->
                          let _, instructions =
                            move_tte ~litterals ~where:(`Address waddress)
                              ~offset rprogram fd tte
                          in
                          instructions
                      )
                   |> Option.value ~default:[]
                 in
                 acc @ instructions
               )
               []
        in
        instructions
    | RVFunction { module_path; fn_name; generics_resolver = _; tac_parameters }
      -> (
        let _typed_parameters =
          tac_parameters |> List.map (fun { expr_rktype; _ } -> expr_rktype)
        in
        let fn_module =
          if module_path = "" then
            current_module
          else
            module_path
        in
        let fn_decl =
          Option.get
          @@ KosuIrTyped.Asttyhelper.RProgram.find_function_decl_of_name
               fn_module fn_name rprogram
        in
        match fn_decl with
        | RExternal_Decl external_func_decl ->
            let fn_label = Spec.label_of_external_function external_func_decl in

            let arguments_registers = passing_register_kt rval_rktype in
            let iparas, fparams, stack_parameters =
              Util.Args.consume_args_sysv ~reversed_stack:true
                ~fregs:Register.float_arguments_register
                ~iregs:arguments_registers
                ~fpstyle:KosuCommon.Function.kosu_passing_style_tte
                tac_parameters
            in

            let reg_instructions =
              iparas @ fparams
              |> List.map (fun (variable, return_kind) ->
                     let tte, reg =
                       match return_kind with
                       | Util.Args.Simple_return reg ->
                           (variable, reg)
                       | Double_return _ ->
                           failwith "Unreachable"
                     in
                     let tte_instructions =
                       snd
                       @@ translate_tac_expression ~litterals
                            ~target_dst:(`Register reg) rprogram fd tte
                     in
                     let promote_float_instruction =
                       if
                         external_func_decl.is_variadic
                         && KosuIrTyped.Asttyhelper.RType.is_32bits_float
                              tte.expr_rktype
                       then
                         Instruction
                           (Cvts2s
                              {
                                source_size = fss;
                                dst_size = fsd;
                                source = `Register reg;
                                destination = `Register reg;
                              }
                           )
                         :: []
                       else
                         []
                     in
                     tte_instructions @ promote_float_instruction
                 )
              |> List.flatten
            in

            let stack_args_ktype_list =
              List.map (fun tte -> tte.expr_rktype) stack_parameters
            in
            (* let total_size = stack_args_ktype_list |> KosuIrTyped.Asttyhelper.RType.rtuple |> KosuIrTyped.Sizeof.sizeof rprogram in *)
            let set_on_stack_instructions =
              stack_parameters
              |> List.mapi (fun index tte ->
                     let last_reg, instructions =
                       translate_tac_expression ~litterals
                         ~target_dst:(`Register Register.r10) rprogram fd tte
                     in
                     let offset =
                       KosuIrTyped.Sizeof.offset_of_tuple_index index
                         stack_args_ktype_list rprogram
                     in
                     (* let () = Printf.printf "offset %Lu\n%!" offset in *)
                     let address = create_address_offset ~offset rsp in
                     (* Maybe promote float if is call in variadic function *)
                     let set_instructions =
                       copy_from_reg (register_of_dst last_reg) address
                         tte.expr_rktype rprogram
                     in
                     instructions @ set_instructions
                 )
              |> List.flatten
            in

            let count_float_parameters =
              tac_parameters
              |> List.fold_left
                   (fun acc tte ->
                     acc
                     +
                     if KosuIrTyped.Asttyhelper.RType.is_float tte.expr_rktype
                     then
                       1
                     else
                       0
                   )
                   0
              |> Int64.of_int
            in

            let variadic_float_use_instruction =
              if external_func_decl.is_variadic then
                [
                  Instruction
                    (Mov
                       {
                         size = ib;
                         destination = `Register rax;
                         source = `ILitteral count_float_parameters;
                       }
                    );
                ]
              else
                []
            in

            let call_instructions =
              FrameManager.call_instruction ~origin:(`Label fn_label) [] fd
            in
            let return_reg = return_register rprogram rval_rktype in

            let extern_instructions =
              match return_reg with
              | Some return_reg ->
                  let copy_instruction =
                    return_function_instruction_reg_size ~where ~is_deref
                      ~return_reg ~return_type:external_func_decl.return_type
                      rprogram
                  in
                  reg_instructions @ set_on_stack_instructions
                  @ variadic_float_use_instruction @ call_instructions
                  @ copy_instruction
              | None ->
                  let set_indirect_return_instruction =
                    return_function_instruction_none_reg_size ~where ~is_deref
                  in
                  reg_instructions @ set_on_stack_instructions
                  @ set_indirect_return_instruction @ call_instructions
            in
            extern_instructions
        | RSyscall_Decl syscall_decl ->
            (* Totally use that syscall args cannot contains float according to the Sys V ABI *)
            let set_on_reg_instructions =
              tac_parameters
              |> Util.Ulist.combine_safe syscall_arguments_register
              |> List.fold_left
                   (fun acc (reg, tte) ->
                     let reg = iregister reg in
                     let _last_reg, instructions =
                       translate_tac_expression ~litterals
                         ~target_dst:(`Register reg) rprogram fd tte
                     in
                     acc @ instructions
                   )
                   []
            in
            let move_code_syscall_instructions =
              [
                Line_Com (Comment ("syscall " ^ syscall_decl.rsyscall_name));
                Instruction
                  (Mov
                     {
                       size = il;
                       source = `ILitteral syscall_decl.opcode;
                       destination = `Register rax;
                     }
                  );
                Instruction Syscall;
              ]
            in

            let copy_result_instruction =
              where
              |> Option.map (fun waddress ->
                     match is_deref with
                     | Some pointer ->
                         Instruction
                           (Mov
                              {
                                size = iq;
                                destination = `Register r9;
                                source = `Address pointer;
                              }
                           )
                         :: copy_from_reg rax (create_address_offset r9)
                              syscall_decl.return_type rprogram
                     | None ->
                         copy_from_reg rax waddress syscall_decl.return_type
                           rprogram
                 )
              |> Option.value ~default:[]
            in
            set_on_reg_instructions @ move_code_syscall_instructions
            @ copy_result_instruction
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
            let fn_label =
              Spec.label_of_kosu_function ~module_path function_decl
            in

            let arguments_registers = passing_register_kt rval_rktype in
            let iparas, fparams, stack_parameters =
              Util.Args.consume_args_sysv ~reversed_stack:true
                ~fregs:Register.float_arguments_register
                ~iregs:arguments_registers
                ~fpstyle:KosuCommon.Function.kosu_passing_style_tte
                tac_parameters
            in

            let reg_instructions =
              iparas @ fparams
              |> List.map (fun (variable, return_kind) ->
                     let tte, reg =
                       match return_kind with
                       | Util.Args.Simple_return reg ->
                           (variable, reg)
                       | Double_return _ ->
                           failwith "Unreachable"
                     in
                     snd
                     @@ translate_tac_expression ~litterals
                          ~target_dst:(`Register reg) rprogram fd tte
                 )
              |> List.flatten
            in

            let stack_args_ktype_list =
              List.map (fun tte -> tte.expr_rktype) stack_parameters
            in
            (* let total_size = stack_args_ktype_list |> KosuIrTyped.Asttyhelper.RType.rtuple |> KosuIrTyped.Sizeof.sizeof rprogram in *)
            let set_on_stack_instructions =
              stack_parameters
              |> List.mapi (fun index tte ->
                     let last_reg, instructions =
                       translate_tac_expression ~litterals
                         ~target_dst:(`Register Register.r10) rprogram fd tte
                     in
                     let offset =
                       KosuIrTyped.Sizeof.offset_of_tuple_index index
                         stack_args_ktype_list rprogram
                     in
                     (* let () = Printf.printf "offset %Lu\n%!" offset in *)
                     let address = create_address_offset ~offset rsp in
                     (* Maybe promote float if is call in variadic function *)
                     let set_instructions =
                       copy_from_reg (register_of_dst last_reg) address
                         tte.expr_rktype rprogram
                     in
                     instructions @ set_instructions
                 )
              |> List.flatten
            in
            let call_instructions =
              FrameManager.call_instruction ~origin:(`Label fn_label) [] fd
            in
            let fn_call_comm =
              Line_Com
                (Comment (Printf.sprintf "%s::%s call end" module_path fn_name))
            in
            let return_reg = return_register rprogram rval_rktype in
            let kosu_fn_instructions =
              match return_reg with
              | Some return_reg ->
                  let copy_instruction =
                    return_function_instruction_reg_size ~where ~is_deref
                      ~return_reg ~return_type:function_decl.return_type
                      rprogram
                  in
                  reg_instructions @ set_on_stack_instructions
                  @ call_instructions @ copy_instruction
              | None ->
                  let set_indirect_return_instructions =
                    return_function_instruction_none_reg_size ~where ~is_deref
                  in

                  reg_instructions @ set_on_stack_instructions
                  @ set_indirect_return_instructions @ call_instructions
            in
            kosu_fn_instructions @ [ fn_call_comm ]
      )
    | RVTuple ttes ->
        let ktlis = ttes |> List.map (fun { expr_rktype; _ } -> expr_rktype) in
        let offset_list =
          ttes
          |> List.mapi (fun index _value ->
                 offset_of_tuple_index index ktlis rprogram
             )
        in
        where
        |> Option.map (fun waddress ->
               ttes
               |> List.mapi (fun index value -> (index, value))
               |> List.fold_left
                    (fun acc (index, tte) ->
                      let offset = List.nth offset_list index in
                      let incremented_adress =
                        increment_adress offset waddress
                      in
                      let reg_texp, instructions =
                        translate_tac_expression rprogram ~litterals
                          ~target_dst:(`Address incremented_adress) fd tte
                      in

                      let acc_plus = acc @ instructions in
                      match reg_texp with
                      | `Address _ ->
                          acc_plus
                      | `Register reg ->
                          acc_plus
                          @ copy_from_reg reg incremented_adress tte.expr_rktype
                              rprogram
                    )
                    []
           )
        |> Option.value ~default:[]
    | RVArray ttes ->
        let ktlis = ttes |> List.map (fun { expr_rktype; _ } -> expr_rktype) in
        let offset_list =
          ttes
          |> List.mapi (fun index _ ->
                 offset_of_tuple_index index ktlis rprogram
             )
        in
        where
        |> Option.map (fun waddress ->
               ttes
               |> List.mapi (fun index value -> (index, value))
               |> List.fold_left
                    (fun acc (index, tte) ->
                      let offset = List.nth offset_list index in
                      let incremented_adress =
                        increment_adress offset waddress
                      in
                      let reg_texp, instructions =
                        translate_tac_expression rprogram ~litterals
                          ~target_dst:(`Address incremented_adress) fd tte
                      in

                      let acc_plus = acc @ instructions in
                      match reg_texp with
                      | `Address _ ->
                          acc_plus
                      | `Register reg ->
                          acc_plus
                          @ copy_from_reg reg incremented_adress tte.expr_rktype
                              rprogram
                    )
                    []
           )
        |> Option.value ~default:[]
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
        let generics = Hashtbl.create 0 in
        let int_index = Int64.to_int index in
        let offset =
          offset_of_tuple_index ~generics int_index kts_tuples rprogram
        in
        let tuple_address =
          FrameManager.address_of (tuple_id, expr_rktype) fd
          |> fun adr ->
          match adr with
          | Some a ->
              a
          | None ->
              failwith "field access null address"
        in
        let field_address = increment_adress offset tuple_address in
        let sizeof = sizeofn rprogram rval_rktype in
        let size = data_size_of_ktype rprogram rval_rktype in
        let locale_r10 = tmp_r10_ktype rval_rktype in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 let debug =
                   Printf.sprintf
                     "%s.%Lu : size = %Lu, register = %s : target address = %s"
                     tuple_id index sizeof
                     (X86_64Pprint.string_of_register size locale_r10)
                     (X86_64Pprint.string_of_address waddress)
                 in
                 let preinstructions =
                   Line_Com (Comment debug)
                   ::
                   ( if is_register_size sizeof then
                       [
                         Instruction
                           (Mov
                              {
                                size;
                                destination = `Register locale_r10;
                                source = `Address field_address;
                              }
                           );
                       ]
                     else
                       [
                         Instruction
                           (Lea
                              {
                                size = iq;
                                source = field_address;
                                destination = locale_r10;
                              }
                           );
                       ]
                   )
                 in
                 let cp_instrucion =
                   copy_from_reg locale_r10 waddress rval_rktype rprogram
                 in
                 preinstructions @ cp_instrucion
             )
          |> Option.value ~default:[]
        in
        Line_Com (Comment ("Field access of " ^ Int.to_string int_index))
        :: copy_instructions
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
          | Some (RDecl_Struct s) ->
              s
          | Some (RDecl_Enum _) ->
              failwith "Expected to find a struct get an enum"
          | None ->
              failwith "Non type decl ??? my validation is very weak"
        in

        let generics =
          expr_rktype
          |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
          |> List.combine struct_decl.generics
          |> List.to_seq |> Hashtbl.of_seq
        in

        let offset = offset_of_field ~generics field struct_decl rprogram in
        let struct_address =
          FrameManager.address_of (struct_id, expr_rktype) fd
          |> fun adr ->
          match adr with
          | Some a ->
              a
          | None ->
              failwith "field access null address"
        in
        let field_address = increment_adress offset struct_address in
        let sizeof = sizeofn rprogram rval_rktype in

        let size = data_size_of_ktype rprogram rval_rktype in
        let locale_r10 = tmp_r10_ktype rval_rktype in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 let debug =
                   Printf.sprintf
                     "%s.%s : size = %Lu, register = %s : target address = %s"
                     struct_id field sizeof
                     (X86_64Pprint.string_of_register size locale_r10)
                     (X86_64Pprint.string_of_address waddress)
                 in
                 let preinstructions =
                   Line_Com (Comment debug)
                   ::
                   ( if is_register_size sizeof then
                       [
                         Instruction
                           (Mov
                              {
                                size;
                                destination = `Register locale_r10;
                                source = `Address field_address;
                              }
                           );
                       ]
                     else
                       [
                         Instruction
                           (Lea
                              {
                                size = iq;
                                source = field_address;
                                destination = locale_r10;
                              }
                           );
                       ]
                   )
                 in
                 let cp_instrucion =
                   copy_from_reg locale_r10 waddress rval_rktype rprogram
                 in
                 preinstructions @ cp_instrucion
             )
          |> Option.value ~default:[]
        in
        Line_Com (Comment ("Field access of " ^ field)) :: copy_instructions
    | RVTupleAccess _ ->
        failwith "Weird: Tuple access force tuple as an idnentifier"
    | RVFieldAcess _ ->
        failwith "Wierd : Fields access force struct as an identifier"
    | RVArrayAccess { array_expr; index_expr } ->
        let r9 = tmp_r9_ktype array_expr.expr_rktype in
        let r10 = tmp_r10_ktype index_expr.expr_rktype in
        (* let r11 = tmp_r11_ktype rval_rktype in  *)
        let r10, index_instructions =
          translate_tac_expression ~litterals ~target_dst:(`Register r10)
            rprogram fd index_expr
        in
        let r9, array_instructions =
          translate_tac_expression ~litterals ~target_dst:(`Register r9)
            rprogram fd array_expr
        in
        let elt_type_size = KosuIrTyped.Sizeof.sizeof rprogram rval_rktype in
        let mov_instructions =
          match is_register_size @@ sizeofn rprogram array_expr.expr_rktype with
          | true ->
              let offset_reg_instruction =
                Instruction
                  (Mov
                     {
                       size = iq;
                       destination = `Register rax;
                       source = `ILitteral (Int64.mul 8L elt_type_size);
                     }
                  )
              in
              let mult_offset_instruction =
                Instruction
                  (Mul
                     { size = iq; destination = None; source = src_of_dst r10 }
                  )
              in
              let offset_instruction =
                Instruction
                  (Mov
                     {
                       size = ib;
                       destination = `Register rcx;
                       source = `Register rax;
                     }
                  )
              in
              let shift_instruction =
                Instruction
                  (Shr { size = Q; destination = r9; shift = `Register cl })
              in
              [
                offset_reg_instruction;
                mult_offset_instruction;
                offset_instruction;
                shift_instruction;
              ]
          | false ->
              let scale_instruction =
                if elt_type_size = 1L then
                  []
                else
                  ins_mult ~size:iq ~destination:r10
                    ~source:(`ILitteral elt_type_size)
              in

              let address =
                {
                  base = register_of_dst r9;
                  offset = Offset 0L;
                  index = Option.some @@ register_of_dst r10;
                  scale = 1;
                }
              in
              let load_index_instruction =
                let rvalue_size = sizeofn rprogram rval_rktype in
                match is_register_size @@ rvalue_size with
                | true ->
                    Instruction
                      (Mov
                         {
                           size =
                             IntSize
                               (Option.get @@ data_size_of_int64 @@ rvalue_size);
                           destination = r9;
                           source = `Address address;
                         }
                      )
                | false ->
                    Instruction
                      (Lea
                         {
                           destination = register_of_dst r9;
                           source = address;
                           size = iq;
                         }
                      )
              in
              scale_instruction @ (load_index_instruction :: [])
        in
        let copy_instructions =
          copy_result ~where ~register:Register.r9 ~rval_rktype rprogram
        in
        index_instructions @ array_instructions @ mov_instructions
        @ copy_instructions
    | RVAdress id ->
        let pointee_type =
          rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee
        in
        let adress =
          FrameManager.address_of (id, pointee_type) fd
          |> fun adr ->
          match adr with
          | Some a ->
              a
          | None ->
              failwith "address of null address"
        in
        let copy_instruction =
          where
          |> Option.map (fun waddress ->
                 [
                   Instruction
                     (Lea { size = iq; destination = rax; source = adress });
                 ]
                 @ copy_from_reg rax waddress rval_rktype rprogram
             )
          |> Option.value ~default:[]
        in
        copy_instruction
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
        let base_address = Option.get @@ FrameManager.address_of variable fd in
        let copy_instru_opt waddress =
          let lea_instr =
            Instruction
              (Lea { size = iq; destination = rax; source = base_address })
          in
          let offset_instructions =
            match offset with
            | 0L ->
                []
            | n ->
                [
                  Instruction
                    (Add
                       {
                         size = iq;
                         destination = `Register rax;
                         source = `ILitteral n;
                       }
                    );
                ]
          in
          let copy_instructions =
            copy_from_reg rax waddress rval_rktype rprogram
          in
          (lea_instr :: offset_instructions) @ copy_instructions
        in
        let instructions =
          where |> Option.map copy_instru_opt |> Option.value ~default:[]
        in
        instructions
    | RVDefer id ->
        let adress =
          FrameManager.address_of
            (id, rval_rktype |> KosuIrTyped.Asttyhelper.RType.rpointer)
            fd
          |> fun adr ->
          match adr with
          | Some a ->
              a
          | None ->
              failwith "defer of null address"
        in
        let r10q = r10 in
        let load_instruction =
          [
            Instruction
              (Mov
                 {
                   size = iq;
                   destination = `Register r10q;
                   source = `Address adress;
                 }
              );
          ]
        in
        let last_reg, load_indirect =
          if is_register_size @@ KosuIrTyped.Sizeof.sizeof rprogram rval_rktype
          then
            ( rax,
              [
                Instruction
                  (Mov
                     {
                       size = iq;
                       destination = `Register rax;
                       source = `Address (create_address_offset r10q);
                     }
                  );
              ]
            )
          else
            (r10q, [])
        in

        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 load_instruction @ load_indirect
                 @ copy_from_reg last_reg waddress rval_rktype rprogram
             )
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
                 offset_of_tuple_index index enum_type_list rprogram
             )
        in
        (* let () = offset_list |> List.map (Printf.sprintf "%Lu") |> String.concat ", " |> Printf.printf "%s::%s off = [%s]\n" enum_decl.renum_name variant in *)
        where
        |> Option.map (fun waddress ->
               enum_tte_list
               |> List.mapi (fun index value -> (index, value))
               |> List.fold_left
                    (fun acc (index, tte) ->
                      let offset = List.nth offset_list index in
                      let incremented_adress =
                        increment_adress offset waddress
                      in
                      let reg_texp, instructions =
                        translate_tac_expression rprogram ~litterals
                          ~target_dst:(`Address incremented_adress) fd tte
                      in

                      let acc_plus = acc @ instructions in
                      match reg_texp with
                      | `Address _ ->
                          acc_plus
                      | `Register reg ->
                          acc_plus
                          @ copy_from_reg reg incremented_adress tte.expr_rktype
                              rprogram
                    )
                    []
           )
        |> Option.value ~default:[]
    | RVDiscard | RVLater ->
        []
    | RVBuiltinBinop { binop = TacBool TacOr; blhs; brhs } ->
        let r9 = tmp_r9_ktype brhs.expr_rktype in
        let rax = tmp_rax_ktype blhs.expr_rktype in
        let _right_reg, rinstructions =
          translate_tac_expression ~litterals ~target_dst:(`Register r9)
            rprogram fd brhs
        in
        let left_reg, linstructions =
          translate_tac_expression ~litterals ~target_dst:(`Register rax)
            rprogram fd blhs
        in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 let or_instruction =
                   [
                     Instruction
                       (Or
                          {
                            size = B;
                            destination = left_reg;
                            source = `Register r9;
                          }
                       );
                   ]
                 in
                 or_instruction
                 @ copy_from_reg rax waddress rval_rktype rprogram
             )
          |> Option.value ~default:[]
        in
        rinstructions @ linstructions @ copy_instructions
    | RVBuiltinBinop { binop = TacBool TacAnd; blhs; brhs } ->
        let r9 = tmp_r9_ktype brhs.expr_rktype in
        let rax = tmp_rax_ktype blhs.expr_rktype in
        let _right_reg, rinstructions =
          translate_tac_expression ~litterals ~target_dst:(`Register r9)
            rprogram fd brhs
        in
        let left_reg, linstructions =
          translate_tac_expression ~litterals ~target_dst:(`Register rax)
            rprogram fd blhs
        in
        let copy_instructions =
          where
          |> Option.map (fun waddress ->
                 let or_instruction =
                   [
                     Instruction
                       (And
                          {
                            size = B;
                            destination = left_reg;
                            source = `Register r9;
                          }
                       );
                   ]
                 in
                 or_instruction
                 @ copy_from_reg rax waddress rval_rktype rprogram
             )
          |> Option.value ~default:[]
        in
        rinstructions @ linstructions @ copy_instructions
    | RVBuiltinBinop { binop = TacBool bool_binop; blhs; brhs } ->
        let is_unsigned =
          KosuIrTyped.Asttyhelper.RType.is_raw_unsigned blhs.expr_rktype
          || KosuIrTyped.Asttyhelper.RType.is_raw_unsigned brhs.expr_rktype
          || KosuIrTyped.Asttyhelper.RType.is_float blhs.expr_rktype
        in
        let cc =
          Option.get @@ Condition_Code.cc_of_tac_bin ~is_unsigned bool_binop
        in
        translate_tac_binop ~litterals ~cc ~blhs ~brhs ~where rval_rktype
          rprogram fd
    | RVBuiltinBinop { binop = TacCmp TacOrdered; blhs; brhs } ->
        let rr9 = tmp_r9_ktype blhs.expr_rktype in
        let rr10 = tmp_r10_ktype brhs.expr_rktype in
        let rrax = tmp_rax_ktype rval_rktype in
        let data_size = data_size_of_ktype rprogram blhs.expr_rktype in
        let rr9, linstructions =
          translate_tac_expression ~litterals ~target_dst:(`Register rr9)
            rprogram fd blhs
        in
        let rr10, rinstructions =
          translate_tac_expression ~litterals ~target_dst:(`Register rr10)
            rprogram fd brhs
        in
        let cmp_instructions =
          [
            Instruction
              (cmp_instruction data_size ~lhs:(src_of_dst rr10)
                 ~rhs:(src_of_dst rr9)
              );
            Instruction (Set { size = B; cc = GE; register = rrax });
            Instruction
              (cmp_instruction data_size ~lhs:(src_of_dst rr10)
                 ~rhs:(src_of_dst rr9)
              );
            Instruction (Set { size = B; cc = G; register = r9 });
            Instruction
              (Add
                 {
                   size = ib;
                   destination = `Register rrax;
                   source = `Register Register.r9;
                 }
              );
          ]
        in

        let cp_instructions =
          copy_result ~where ~register:Register.rax ~rval_rktype rprogram
        in
        linstructions @ rinstructions @ cmp_instructions @ cp_instructions
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
        (* let size = int_data_size_of_int64_def @@ sizeofn rprogram blhs.expr_rktype in *)
        let is_unsigned =
          KosuIrTyped.Asttyhelper.RType.is_raw_unsigned blhs.expr_rktype
        in
        let is_shift =
          match self_binop with
          | TacShiftLeft | TacShiftRight ->
              true
          | _ ->
              false
        in
        let binop_func =
          binop_instruction_of_tacself ~unsigned:is_unsigned self_binop
        in
        translate_tac_binop_self ~is_shift ~litterals ~blhs ~brhs ~where
          binop_func rval_rktype rprogram fd
    | RVBuiltinBinop
        { binop = TacSelf ((TacAdd | TacMinus) as self_binop); blhs; brhs } -> (
        match KosuIrTyped.Asttyhelper.RType.is_pointer rval_rktype with
        | false ->
            let binop_func = binop_instruction_of_tacself self_binop in
            translate_tac_binop_self ~is_shift:false ~litterals ~blhs ~brhs
              ~where binop_func rval_rktype rprogram fd
        | true ->
            let pointee_size =
              rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee
              |> KosuIrTyped.Sizeof.sizeof rprogram
            in
            let ptr_reg, linstructions =
              translate_tac_expression ~litterals ~target_dst:(`Register rax)
                rprogram fd blhs
            in
            let nb_reg, rinstructions =
              translate_tac_expression ~litterals ~target_dst:(`Register r10)
                rprogram fd brhs
            in

            (* If not set to zero before : If the register size is lower than 32 bits: It seems that unused bits aren't zerroed extented*)
            let null_instruction =
              Instruction
                (Mov
                   {
                     size = iq;
                     source = `ILitteral 0L;
                     destination = `Register r10;
                   }
                )
            in

            let size_reg = register_of_dst @@ nb_reg in

            let data_size = data_size_of_ktype rprogram rval_rktype in

            let operator_function_instruction =
              binop_instruction_of_tacself self_binop
            in

            let scale_instruction =
              if pointee_size = 1L then
                []
              else
                ins_mult ~size:iq ~destination:(`Register size_reg)
                  ~source:(`ILitteral pointee_size)
            in

            let ptr_true_reg = register_of_dst ptr_reg in
            let copy_instructions =
              copy_result ~where ~register:ptr_true_reg ~rval_rktype rprogram
            in
            linstructions
            @ (null_instruction :: rinstructions)
            @ scale_instruction
            @ operator_function_instruction ~size:data_size ~destination:ptr_reg
                ~source:(`Register size_reg)
            @ copy_instructions
      )
    | RVBuiltinBinop
        { binop = TacSelf TacDiv; blhs = dividende; brhs = divisor } ->
        if KosuIrTyped.Asttyhelper.RType.is_float rval_rktype then
          let lsource, linstructions =
            translate_tac_expression ~litterals ~target_dst:(`Register xmm1)
              rprogram fd divisor
          in
          let _, rinstructions =
            translate_tac_expression ~litterals ~target_dst:(`Register xmm0)
              rprogram fd dividende
          in
          let lsource = Operande.src_of_dst lsource in
          let fsize =
            if KosuIrTyped.Asttyhelper.RType.is_64bits_float divisor.expr_rktype
            then
              SD
            else
              SS
          in
          let fdiv =
            Instruction
              (Fdiv { size = fsize; source = lsource; destination = XMM0 })
          in
          linstructions @ rinstructions
          @ (fdiv :: copy_result ~where ~register:xmm0 ~rval_rktype rprogram)
        else
          let unsigned =
            KosuIrTyped.Asttyhelper.RType.is_unsigned_integer
              dividende.expr_rktype
          in
          let last_reg10, divisor_instructions =
            translate_tac_expression ~litterals ~target_dst:(`Register r10)
              rprogram fd divisor
          in
          let raw_data_size =
            int_data_size_of_int64_def @@ sizeofn rprogram dividende.expr_rktype
          in
          let scaled_data_size = match raw_data_size with Q -> Q | _ -> L in

          let _, instructions =
            translate_tac_expression ~litterals ~target_dst:(`Register rax)
              rprogram fd dividende
          in
          let setup_div = Instruction (division_split scaled_data_size) in

          let r10 = register_of_dst last_reg10 in
          let divi_instruction =
            division_instruction ~unsigned scaled_data_size (`Register r10)
          in

          let copy_instructions =
            copy_result ~where ~register:rax ~rval_rktype rprogram
          in
          instructions @ divisor_instructions
          @ (setup_div :: divi_instruction :: copy_instructions)
    | RVBuiltinBinop
        { binop = TacSelf TacModulo; blhs = dividende; brhs = divisor } ->
        let () =
          if KosuIrTyped.Asttyhelper.RType.is_float rval_rktype then
            failwith "Modulo not done for X86 float"
        in
        let unsigned =
          KosuIrTyped.Asttyhelper.RType.is_unsigned_integer
            dividende.expr_rktype
        in
        let last_reg10, divisor_instructions =
          translate_tac_expression ~litterals ~target_dst:(`Register r10)
            rprogram fd divisor
        in
        let raw_data_size =
          int_data_size_of_int64_def @@ sizeofn rprogram dividende.expr_rktype
        in
        let scaled_data_size = match raw_data_size with Q -> Q | _ -> L in

        let _, instructions =
          translate_tac_expression ~litterals ~target_dst:(`Register rax)
            rprogram fd dividende
        in
        let setup_div = Instruction (division_split scaled_data_size) in

        let r10 = register_of_dst last_reg10 in
        let divi_instruction =
          division_instruction ~unsigned scaled_data_size (`Register r10)
        in

        let copy_instructions =
          copy_result ~where ~register:rdx ~rval_rktype rprogram
        in
        instructions @ divisor_instructions
        @ (setup_div :: divi_instruction :: copy_instructions)
    | RVBuiltinUnop { unop = TacUminus; expr } ->
        let rax = tmp_rax_ktype expr.expr_rktype in
        let last_reg, instructions =
          translate_tac_expression ~litterals ~target_dst:(`Register rax)
            rprogram fd expr
        in
        let size = data_size_of_ktype rprogram expr.expr_rktype in
        let last_reg = Operande.register_of_dst last_reg in
        let uminus_instructions =
          last_reg |> neg_instruction size
          |> List.map (fun inst -> Instruction inst)
        in
        let copy_instructions =
          copy_result ~where ~register:last_reg ~rval_rktype rprogram
        in
        instructions @ uminus_instructions @ copy_instructions
    | RVBuiltinUnop { unop = TacNot; expr } ->
        let rax = tmp_rax_ktype expr.expr_rktype in
        let size = data_size_of_ktype rprogram expr.expr_rktype in
        let dlast_reg, instructions =
          translate_tac_expression ~litterals ~target_dst:(`Register rax)
            rprogram fd expr
        in
        let last_reg = Operande.register_of_dst dlast_reg in

        let not_instruction =
          if KosuIrTyped.Asttyhelper.RType.is_bool rval_rktype then
            let idata_size =
              match size with
              | IntSize i ->
                  i
              | FloatSize _ ->
                  failwith
                    "Proabily unreachable exist if bool are encoded as float"
            in
            Instruction
              (Xor
                 {
                   size = idata_size;
                   source = `ILitteral 1L;
                   destination = dlast_reg;
                 }
              )
          else
            Instruction (Not { size; source = last_reg })
        in
        let copy_instructions =
          copy_result ~where ~register:last_reg ~rval_rktype rprogram
        in
        instructions @ (not_instruction :: copy_instructions)
    | RVBuiltinCall { fn_name; parameters } -> (
        let open KosuFrontend.Ast.Builtin_Function in
        match fn_name with
        | Tagof ->
            let tte = List.hd parameters in
            let address =
              match tte.tac_expression with
              | TEIdentifier i ->
                  Option.get @@ FrameManager.address_of (i, tte.expr_rktype) fd
              | _ ->
                  failwith
                    "Enum are rvalue, therethore they are converted as \
                     identifier"
            in
            let rr9 = Register.r9 in
            let load_tag_instructions =
              Instruction
                (Instruction.Mov
                   {
                     size = il;
                     destination = `Register rr9;
                     source = `Address address;
                   }
                )
            in
            let copy_instructions =
              copy_result ~where ~register:rr9 ~rval_rktype rprogram
            in
            load_tag_instructions :: copy_instructions
        | Array_len ->
            let tte = List.hd parameters in
            let array_len =
              match tte.expr_rktype with
              | RTArray { size; rktype = _ } ->
                  size
              | _ ->
                  failwith "Weird: it should be an pointer array type"
            in
            let rr9 = Register.r9 in
            let mov_instr =
              Instruction
                (Mov
                   {
                     size = iq;
                     source = `ILitteral array_len;
                     destination = `Register rr9;
                   }
                )
            in
            let copy_instructions =
              copy_result ~where ~register:rr9 ~rval_rktype rprogram
            in
            mov_instr :: copy_instructions
        | Array_ptr ->
            let tte = List.hd parameters in
            let rr9 = Register.r9 in
            let _, instructions =
              translate_tac_expression ~litterals ~target_dst:(`Register rr9)
                rprogram fd tte
            in
            let copy_instructions =
              copy_result ~where ~register:rr9 ~rval_rktype rprogram
            in
            instructions @ copy_instructions
        | Tos8
        | Tou8
        | Tos16
        | Tou16
        | Tos32
        | Tou32
        | Tos64
        | Tou64
        | Stringl_ptr ->
            let parameter = List.hd parameters in
            let source_data_size =
              data_size_of_ktype rprogram parameter.expr_rktype
            in
            let output_data_size = data_size_of_ktype rprogram rval_rktype in
            let r0 = tmp_rax_ktype parameter.expr_rktype in
            let _, instructions =
              translate_tac_expression ~litterals ~target_dst:(`Register r0)
                rprogram fd parameter
            in

            let float_convert_instruct =
              if KosuIrTyped.Asttyhelper.RType.is_float parameter.expr_rktype
              then
                [
                  Instruction
                    (Cvtts2s
                       {
                         source_size = source_data_size;
                         dst_size = data_size_min_l output_data_size;
                         source = `Register r0;
                         destination = `Register rax;
                       }
                    );
                ]
              else
                []
            in
            let copy_instructions =
              copy_result ~where ~register:rax ~rval_rktype rprogram
            in
            instructions @ float_convert_instruct @ copy_instructions
        | Tof32 | Tof64 ->
            let parameter = List.hd parameters in
            let source_data_size =
              data_size_of_ktype rprogram parameter.expr_rktype
            in
            let source_data_size = data_size_min_l source_data_size in
            let output_data_size = data_size_of_ktype rprogram rval_rktype in
            let r0 = tmp_rax_ktype parameter.expr_rktype in
            let _, mov_instructtion =
              translate_tac_expression ~litterals ~target_dst:(`Register r0)
                rprogram fd parameter
            in
            let conv_instrc =
              if source_data_size = output_data_size then
                []
              else
                Instruction
                  (Cvts2s
                     {
                       source_size = source_data_size;
                       dst_size = output_data_size;
                       source = `Register r0;
                       destination = `Register xmm0;
                     }
                  )
                :: []
            in
            let copy_instructions =
              copy_result ~where ~register:xmm0 ~rval_rktype rprogram
            in
            mov_instructtion @ conv_instrc @ copy_instructions
      )
    | RVCustomUnop unary ->
        let open KosuIrTAC.Asttachelper.Operator in
        let op_decls =
          KosuIrTyped.Asttyhelper.RProgram.find_unary_operator_decl
            (parser_unary_op_of_tac_unary_op unary.unop)
            unary.expr.expr_rktype ~r_type:rval_rktype rprogram
        in

        let op_decl =
          match op_decls with
          | t :: [] ->
              t
          | _ ->
              failwith "What the type checker has done: No unary op declaration"
        in
        let fn_label =
          Spec.label_of_kosu_operator ~module_path:current_module op_decl
        in
        let return_type =
          KosuIrTyped.Asttyhelper.OperatorDeclaration.op_return_type op_decl
        in
        let return_size = sizeofn rprogram return_type in
        let return_reg = return_register rprogram rval_rktype in
        let object_params_regs =
          if is_register_size return_size then
            rdi
          else
            rsi
        in
        let _, instructions =
          translate_tac_expression ~litterals
            ~target_dst:(`Register object_params_regs) rprogram fd unary.expr
        in
        let call_instruction =
          FrameManager.call_instruction ~origin:(`Label fn_label) [] fd
        in

        let operator_instructions =
          match is_register_size return_size with
          | true ->
              let copy_instruction =
                return_function_instruction_reg_size ~where ~is_deref
                  ~return_reg:(Option.get return_reg) ~return_type rprogram
              in

              instructions @ call_instruction @ copy_instruction
              (* Is not the same check the instructions order*)
          | false ->
              let set_indirect_return_instructions =
                return_function_instruction_none_reg_size ~where ~is_deref
              in
              instructions @ set_indirect_return_instructions @ call_instruction
        in
        operator_instructions
    | RVCustomBinop
        ({ binop = TacSelf _ | TacBool _ | TacCmp TacOrdered; _ } as self) ->
        let op_decls =
          KosuIrTyped.Asttyhelper.RProgram.find_binary_operator_decl
            (KosuIrTAC.Asttachelper.Operator.parser_binary_op_of_tac_binary_op
               self.binop
            )
            (self.blhs.expr_rktype, self.brhs.expr_rktype)
            ~r_type:rval_rktype rprogram
        in
        let op_decl =
          match op_decls with
          | t :: [] ->
              t
          | _ ->
              failwith
                "What the type checker has done: No binary op declaration | \
                 Too much"
        in
        let fn_label =
          Spec.label_of_kosu_operator ~module_path:current_module op_decl
        in
        let return_type =
          KosuIrTyped.Asttyhelper.OperatorDeclaration.op_return_type op_decl
        in
        let return_size = sizeofn rprogram return_type in
        let return_reg = return_register rprogram return_type in

        let r0, r1 =
          match KosuIrTyped.Asttyhelper.RType.is_float rval_rktype with
          | true ->
              (xmm0, xmm1)
          | false ->
              if is_register_size return_size then
                (rdi, rsi)
              else
                (rsi, rdx)
        in
        let _, lhs_instructions =
          translate_tac_expression ~litterals ~target_dst:(`Register r0)
            rprogram fd self.blhs
        in
        let _, rhs_instructions =
          translate_tac_expression ~litterals ~target_dst:(`Register r1)
            rprogram fd self.brhs
        in
        let call_instruction =
          FrameManager.call_instruction ~origin:(`Label fn_label) [] fd
        in

        let operator_instructions =
          match is_register_size return_size with
          | true ->
              let copy_instruction =
                return_function_instruction_reg_size ~where ~is_deref
                  ~return_reg:(Option.get return_reg) ~return_type rprogram
              in

              lhs_instructions @ rhs_instructions @ call_instruction
              @ copy_instruction
              (* Is not the same check the instructions order*)
          | false ->
              let set_indirect_return_instructions =
                return_function_instruction_none_reg_size ~where ~is_deref
              in
              lhs_instructions @ rhs_instructions
              @ set_indirect_return_instructions @ call_instruction
        in
        operator_instructions

  let rec translate_tac_statement ~litterals current_module rprogram
      (fd : FrameManager.frame_desc) = function
    | STacDeclaration { identifier; trvalue }
    | STacModification { identifier; trvalue } ->
        let address =
          FrameManager.address_of (identifier, trvalue.rval_rktype) fd
        in
        let instructions =
          translate_tac_rvalue ~litterals ~where:address current_module rprogram
            fd trvalue
        in
        instructions
    | STDerefAffectation { identifier; trvalue } ->
        let tmpreg =
          tmp_r9_ktype
            (KosuIrTyped.Asttyhelper.RType.rpointer trvalue.rval_rktype)
        in
        let intermediary_adress =
          FrameManager.address_of
            ( identifier,
              KosuIrTyped.Asttyhelper.RType.rpointer trvalue.rval_rktype
            )
            fd
        in
        let instructions =
          Instruction
            (Mov
               {
                 size = iq;
                 destination = `Register tmpreg;
                 source = `Address (Option.get intermediary_adress);
               }
            )
        in
        let true_adress = create_address_offset tmpreg in
        let true_instructions =
          translate_tac_rvalue ~litterals ~is_deref:intermediary_adress
            ~where:(Some true_adress) current_module rprogram fd trvalue
        in
        (Line_Com (Comment "Defered Start") :: instructions :: true_instructions)
        @ [ Line_Com (Comment "Defered end") ]
    | STacModificationField { identifier_root; fields; trvalue } ->
        let root_address =
          Option.get @@ FrameManager.address_of identifier_root fd
        in
        let field_offset =
          KosuCommon.OffsetHelper.offset_of_field_access (snd identifier_root)
            ~fields rprogram
        in
        let target_adress = increment_adress field_offset root_address in
        let instructions =
          translate_tac_rvalue ~litterals ~where:(Some target_adress)
            current_module rprogram fd trvalue
        in
        instructions
    | STDerefAffectationField { identifier_root; fields; trvalue } ->
        let r9 = tmp_r9_ktype trvalue.rval_rktype in
        (* Since it hold an address *)
        let intermediary_adress =
          Option.get @@ FrameManager.address_of identifier_root fd
        in
        let pointee_type =
          (fun (_, kt) -> KosuIrTyped.Asttyhelper.RType.rtpointee kt)
            identifier_root
        in
        let field_offset =
          KosuCommon.OffsetHelper.offset_of_field_access pointee_type ~fields
            rprogram
        in
        let _target_adress =
          increment_adress field_offset intermediary_adress
        in
        let fetch_address_offset_instruction =
          [
            Instruction
              (Mov
                 {
                   size = iq;
                   source = `Address intermediary_adress;
                   destination = `Register r9;
                 }
              );
            Instruction
              (Add
                 {
                   size = iq;
                   destination = `Register r9;
                   source = `ILitteral field_offset;
                 }
              );
          ]
        in
        let true_adress = create_address_offset r9 in
        let true_instructions =
          translate_tac_rvalue ~litterals
            ~is_deref:(Some intermediary_adress)
              (* Very susciptous abode the use of immediary adress *)
            ~where:(Some true_adress) current_module rprogram fd trvalue
        in

        (Line_Com (Comment "Field Defered Start")
         :: fetch_address_offset_instruction
        @ true_instructions
        )
        @ [ Line_Com (Comment "Field Defered end") ]
    | STWhile
        {
          statements_condition;
          condition;
          loop_body;
          self_label;
          inner_body_label = _;
          exit_label;
        } ->
        let start_label = Label self_label in
        let stmts_bool =
          statements_condition
          |> List.fold_left
               (fun acc stmt ->
                 acc
                 @ translate_tac_statement ~litterals current_module rprogram fd
                     stmt
               )
               []
        in
        let last_reg, condition_rvalue_inst =
          translate_tac_expression ~litterals
            ~target_dst:(`Register Register.r10) rprogram fd condition
        in
        let data_size =
          match last_reg with
          | `Address _ ->
              iq
          | `Register _ ->
              data_size_of_ktype rprogram condition.expr_rktype
        in
        let rhs = src_of_dst last_reg in
        let cmp =
          Instruction (Cmp { size = data_size; rhs; lhs = `ILitteral 0L })
        in
        let jmp =
          Instruction (Jmp { cc = Some E; where = `Label exit_label })
        in

        let if_block =
          translate_tac_body ~litterals ~end_label:(Some self_label)
            current_module rprogram fd loop_body
        in
        let exit_label = Label exit_label in

        (start_label :: stmts_bool)
        @ condition_rvalue_inst @ (cmp :: jmp :: if_block) @ [ exit_label ]
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
                 @ translate_tac_statement ~litterals current_module rprogram fd
                     stmt
               )
               []
        in
        let last_reg, condition_rvalue_inst =
          translate_tac_expression ~litterals
            ~target_dst:(`Register Register.r10) rprogram fd condition_rvalue
        in
        let data_size =
          match last_reg with
          | `Address _ ->
              iq
          | `Register _ ->
              data_size_of_ktype rprogram condition_rvalue.expr_rktype
        in
        let rhs = src_of_dst last_reg in
        let cmp =
          Instruction (Cmp { size = data_size; rhs; lhs = `ILitteral 1L })
        in
        let jmp = Instruction (Jmp { cc = Some E; where = `Label goto1 }) in
        let jmp2 = Instruction (Jmp { cc = None; where = `Label goto2 }) in
        let if_block =
          translate_tac_body ~litterals ~end_label:(Some exit_label)
            current_module rprogram fd if_tac_body
        in
        let else_block =
          translate_tac_body ~litterals ~end_label:(Some exit_label)
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
                          translate_tac_statement ~litterals current_module
                            rprogram fd stmt
                      )
                   |> List.flatten
                 in
                 let last_reg, condition =
                   translate_tac_expression ~litterals
                     ~target_dst:(`Register r10) rprogram fd scases.condition
                 in
                 let lhs = src_of_dst last_reg in
                 (* let rhs = resize_src ib lhs in *)
                 let rhs = lhs in
                 let cmp =
                   Instruction (Cmp { size = ib; rhs; lhs = `ILitteral 1L })
                 in
                 let if_true_instruction =
                   Instruction (Jmp { cc = Some E; where = `Label scases.goto })
                 in
                 let if_false_instruction =
                   Instruction
                     (Jmp { cc = None; where = `Label scases.jmp_false })
                 in
                 let body_instruction =
                   translate_tac_body ~litterals
                     ~end_label:(Some scases.end_label) current_module rprogram
                     fd scases.tac_body
                 in
                 ( body_instruction,
                   setup_next_label_instr @ setup_condition_insts @ condition
                   @ [ cmp; if_true_instruction; if_false_instruction ]
                 )
             )
          |> List.split
          |> fun (lhs, rhs) -> (List.flatten lhs, List.flatten rhs)
        in
        let _else_jump =
          Instruction (Jmp { cc = None; where = `Label else_tac_body.label })
        in
        let end_label_instruction = Label exit_label in
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
        let exit_label_instruction = Label sw_exit_label in
        let setup_instructions =
          statemenets_for_case
          |> List.fold_left
               (fun acc_stmts value ->
                 let insts =
                   translate_tac_statement ~litterals current_module rprogram fd
                     value
                 in
                 acc_stmts @ insts
               )
               []
        in
        let last_dst, condition_switch_instruction =
          translate_tac_expression ~litterals ~target_dst:(`Register r10)
            rprogram fd condition_switch
        in
        let copy_tag =
          if is_register_size (sizeofn rprogram condition_switch.expr_rktype)
          then
            Instruction
              (Mov
                 {
                   size = il;
                   destination = `Register r10;
                   source = src_of_dst last_dst;
                 }
              )
          else
            Instruction
              (Mov
                 {
                   size = il;
                   destination = `Register r10;
                   source = `Address (address_of_dst last_dst);
                 }
              )
        in
        let switch_variable_name =
          match condition_switch.tac_expression with
          | TEIdentifier id ->
              id
          | _ ->
              failwith "I need to get the id"
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
                                   size = il;
                                   rhs = `Register r10;
                                   lhs = `ILitteral (Int64.of_int32 tag);
                                 }
                              )
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
                                         condition_switch.expr_rktype
                                       )
                                       fd
                                     |> Option.get
                                   in
                                   let destination_address =
                                     FrameManager.address_of (id, ktyte) fd
                                     |> Option.get
                                   in
                                   let size_of_ktype = sizeofn rprogram ktyte in
                                   let data_size =
                                     int_data_size_of_int64_def size_of_ktype
                                   in
                                   let copy_instructions =
                                     if is_register_size size_of_ktype then
                                       [
                                         Instruction
                                           (Mov
                                              {
                                                size = IntSize data_size;
                                                destination = `Register r11;
                                                source =
                                                  `Address
                                                    (increment_adress offset_a
                                                       switch_variable_address
                                                    );
                                              }
                                           );
                                         Instruction
                                           (Mov
                                              {
                                                size = IntSize data_size;
                                                source = `Register r11;
                                                destination =
                                                  `Address destination_address;
                                              }
                                           );
                                       ]
                                     else
                                       let leaq =
                                         Instruction
                                           (Lea
                                              {
                                                size = iq;
                                                destination = r11;
                                                source =
                                                  increment_adress offset_a
                                                    switch_variable_address;
                                              }
                                           )
                                       in
                                       leaq
                                       :: copy_from_reg r11 destination_address
                                            ktyte rprogram
                                   in

                                   copy_instructions
                               )
                            |> List.flatten
                          in
                          let jump_true =
                            Instruction
                              (Jmp
                                 { cc = Some E; where = `Label sw_case.sw_goto }
                              )
                          in
                          fetch_offset_instruction @ [ compare; jump_true ]
                      )
                   |> List.flatten
                 in
                 let genete_block =
                   translate_tac_body ~litterals
                     ~end_label:(Some sw_case.sw_exit_label) current_module
                     rprogram fd sw_case.switch_tac_body
                 in
                 (jump_condition, genete_block)
             )
          |> List.split
          |> fun (lhs, rhs) -> (List.flatten lhs, List.flatten rhs)
        in
        let wildcard_case_jmp =
          wildcard_label
          |> Option.map (fun lab ->
                 Instruction (Jmp { cc = None; where = `Label lab })
             )
          |> Option.to_list
        in
        let wildcard_body_block =
          wildcard_body
          |> Option.map (fun body ->
                 translate_tac_body ~litterals ~end_label:(Some sw_exit_label)
                   current_module rprogram fd body
             )
          |> Option.value ~default:[]
        in
        setup_instructions @ condition_switch_instruction
        @ (copy_tag :: cmp_instrution_list)
        @ wildcard_case_jmp @ fn_block @ wildcard_body_block
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
        let open LineInstruction in
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
        let exit_label_instruction = Label tmp_sw_exit_label in
        let setup_instructions =
          tmp_statemenets_for_case
          |> List.fold_left
               (fun acc_stmts value ->
                 let insts =
                   translate_tac_statement ~litterals current_module rprogram fd
                     value
                 in
                 acc_stmts @ insts
               )
               []
        in
        let reg_tag_11, mov_tag_register_instructions =
          translate_tac_expression ~litterals
            ~target_dst:(`Register Register.r11) rprogram fd tag_atom
        in
        (* let tag_variable =
             match tag_atom.tac_expression with
             | TEIdentifier id -> (id, tag_atom.expr_rktype)
             | _ -> failwith "I need to get the id"
           in *)
        let enum_variabe =
          match enum_tte.tac_expression with
          | TEIdentifier id ->
              (id, enum_tte.expr_rktype)
          | _ ->
              failwith "I need to get the id of the enum"
        in
        let cmp_instrution_list, fn_block =
          tmp_switch_list
          |> List.map (fun switch ->
                 let jump_condition =
                   switch.variants
                   |> List.map (fun variant ->
                          let compare_instruction =
                            instruction
                            @@ Cmp
                                 {
                                   size = il;
                                   rhs = (reg_tag_11 :> src);
                                   lhs =
                                     `ILitteral
                                       (Int64.of_int variant.variant_index);
                                 }
                          in
                          let assoc_type_for_variants =
                            KosuIrTyped.Asttyhelper.Renum
                            .assoc_types_of_variant_tag ~tagged:true
                              variant.variant_index enum_decl
                          in
                          let fetch_offset_instructions =
                            switch.tmp_assoc_bound
                            |> List.map (fun (index, id, ktype) ->
                                   let offset_a =
                                     offset_of_tuple_index (index + 1)
                                       assoc_type_for_variants rprogram
                                   in
                                   let switch_variable_address =
                                     Option.get
                                     @@ FrameManager.address_of enum_variabe fd
                                   in
                                   let destination_address =
                                     Option.get
                                     @@ FrameManager.address_of (id, ktype) fd
                                   in
                                   let size_of_ktype = sizeofn rprogram ktype in
                                   let data_size =
                                     int_data_size_of_int64_def size_of_ktype
                                   in
                                   let copy_instructions =
                                     if is_register_size size_of_ktype then
                                       [
                                         Instruction
                                           (Mov
                                              {
                                                size = IntSize data_size;
                                                destination = `Register r11;
                                                source =
                                                  `Address
                                                    (increment_adress offset_a
                                                       switch_variable_address
                                                    );
                                              }
                                           );
                                         Instruction
                                           (Mov
                                              {
                                                size = IntSize data_size;
                                                source = `Register r11;
                                                destination =
                                                  `Address destination_address;
                                              }
                                           );
                                       ]
                                     else
                                       let leaq =
                                         Instruction
                                           (Lea
                                              {
                                                size = iq;
                                                destination = r11;
                                                source =
                                                  increment_adress offset_a
                                                    switch_variable_address;
                                              }
                                           )
                                       in
                                       leaq
                                       :: copy_from_reg r11 destination_address
                                            ktype rprogram
                                   in
                                   copy_instructions
                               )
                            |> List.flatten
                          in
                          let jump_true =
                            Instruction
                              (Jmp
                                 {
                                   cc = Some E;
                                   where = `Label switch.tmp_sw_goto;
                                 }
                              )
                          in
                          fetch_offset_instructions
                          @ [ compare_instruction; jump_true ]
                      )
                   |> List.flatten
                 in
                 let genete_block =
                   translate_tac_body ~litterals
                     ~end_label:(Some switch.tmp_sw_exit_label) current_module
                     rprogram fd switch.tmp_switch_tac_body
                 in
                 (jump_condition, genete_block)
             )
          |> List.split
          |> fun (lhs, rhs) -> (List.flatten lhs, List.flatten rhs)
        in
        let wildcard_case_jmp =
          tmp_wildcard_label
          |> Option.map (fun lab ->
                 Instruction (Jmp { cc = None; where = `Label lab })
             )
          |> Option.to_list
        in
        let wildcard_body_block =
          tmp_wildcard_body
          |> Option.map (fun body ->
                 translate_tac_body ~litterals
                   ~end_label:(Some tmp_sw_exit_label) current_module rprogram
                   fd body
             )
          |> Option.value ~default:[]
        in

        setup_instructions @ mov_tag_register_instructions @ cmp_instrution_list
        @ wildcard_case_jmp @ fn_block @ wildcard_body_block
        @ [ exit_label_instruction ]

  and translate_tac_body ~litterals ?(end_label = None) current_module rprogram
      (fd : FrameManager.frame_desc) { label; body } =
    let label_instr = Label label in
    let stmt_instr =
      body |> fst
      |> List.map (fun stmt ->
             translate_tac_statement ~litterals current_module rprogram fd stmt
         )
      |> List.flatten
    in
    let end_label_inst =
      end_label
      |> Option.map (fun lab ->
             Instruction (Jmp { cc = None; where = `Label lab })
         )
      |> Option.to_list
    in
    let return_instr =
      body |> snd
      |> Option.map (fun tte ->
             let tr10 = tmp_r10_ktype tte.expr_rktype in
             let last_reg, instructions =
               translate_tac_expression ~litterals ~target_dst:(`Register tr10)
                 rprogram fd tte
             in
             let sizeof = sizeofn rprogram tte.expr_rktype in
             let data_size = data_size_of_ktype rprogram tte.expr_rktype in
             let return_reg = return_register rprogram tte.expr_rktype in
             instructions
             @
             match return_reg with
             | Some return_reg ->
                 Instruction
                   (Mov
                      {
                        size = data_size;
                        destination = `Register return_reg;
                        source = src_of_dst last_reg;
                      }
                   )
                 :: []
             | None ->
                 let x8_address =
                   Option.get @@ FrameManager.(address_of indirect_return_vt fd)
                 in
                 let str =
                   Instruction
                     (Mov
                        {
                          size = iq;
                          destination = `Register rdi;
                          source = `Address x8_address;
                        }
                     )
                 in

                 str
                 :: copy_large
                      ~address_str:(create_address_offset rdi)
                      ~base_address_reg:(address_of_dst last_reg) sizeof
         )
      |> Option.value ~default:[]
    in
    (label_instr :: stmt_instr) @ return_instr @ end_label_inst

  let kosu_crt0_module_node name =
    let instructions =
      [
        Instruction
          (Xor { size = L; source = `Register rbp; destination = `Register rbp });
        Instruction
          (Mov
             {
               size = il;
               source = `Address (create_address_offset rsp);
               destination = `Register rdi;
             }
          );
        Instruction
          (Lea
             {
               size = iq;
               source = create_address_offset ~offset:8L rsp;
               destination = rsi;
             }
          );
        Instruction (Call { what = `Label Spec.main });
        Instruction
          (Mov
             { size = il; source = `Register rax; destination = `Register rdi }
          );
        Instruction (Call { what = `Label "_exit" });
      ]
    in
    Afunction { asm_name = name; asm_body = instructions }

  let kosu_crt0_named_module_path name =
    { apath = name; asm_module = AsmModule [ kosu_crt0_module_node name ] }

  let kosu_crt0_asm entry_name =
    {
      filename = "kosu_crt0";
      asm_module_path = kosu_crt0_named_module_path entry_name;
      rprogram = [];
      litterals =
        { str_lit_map = Hashtbl.create 0; float_lit_map = Hashtbl.create 0 };
    }

  let asm_module_of_tac_module ~litterals current_module rprogram =
    let open KosuIrTyped.Asttyped in
    function
    | TacModule tac_nodes ->
        tac_nodes
        |> List.filter_map (fun node ->
               match node with
               | TNFunction function_decl ->
                   (* let () = locals_var |> List.map (fun (s, kt) -> Printf.sprintf "%s : %s " (s) (KosuIrTyped.Asttypprint.string_of_rktype kt)) |> String.concat ", " |> Printf.printf "%s : locale variables = [%s]\n" function_decl.rfn_name in *)
                   let asm_name =
                     Spec.label_of_tac_function ~module_path:current_module
                       function_decl
                   in
                   (* let () = Printf.printf "%s::%s\n" current_module function_decl.rfn_name in *)
                   let fd =
                     FrameManager.frame_descriptor function_decl rprogram
                   in
                   let prologue =
                     FrameManager.function_prologue function_decl rprogram fd
                   in
                   let conversion =
                     translate_tac_body ~litterals current_module rprogram fd
                       function_decl.tac_body
                   in
                   let epilogue = FrameManager.function_epilogue fd in
                   (* let () = Printf.printf "\n\n%s:\n" function_decl.rfn_name in
                      let () = fd.stack_map |> IdVarMap.to_seq |> Seq.iter (fun ((s, kt), adr) ->
                        Printf.printf "%s : %s == [%s, %s]\n"
                        (s)
                        (KosuIrTyped.Asttypprint.string_of_rktype kt)
                        (X86_64Pprint.string_of_register adr.base)
                        (X86_64Pprint.string_of_address_offset adr.offset)
                        ) in *)
                   Some
                     (Afunction
                        {
                          asm_name;
                          asm_body =
                            [ Directive "cfi_startproc" ]
                            @ prologue @ List.tl conversion @ epilogue
                            @ [ Directive "cfi_endproc" ];
                        }
                     )
               | TNOperator operator ->
                   let function_decl =
                     KosuIrTAC.Asttachelper.OperatorDeclaration
                     .tac_function_of_operator operator
                   in
                   let asm_name =
                     Spec.label_of_tac_operator ~module_path:current_module
                       operator
                   in
                   (* let () = Printf.printf "%s::%s\n" current_module function_decl.rfn_name in *)
                   let fd =
                     FrameManager.frame_descriptor function_decl rprogram
                   in
                   let prologue =
                     FrameManager.function_prologue function_decl rprogram fd
                   in
                   let conversion =
                     translate_tac_body ~litterals current_module rprogram fd
                       function_decl.tac_body
                   in
                   let epilogue = FrameManager.function_epilogue fd in
                   (* let () = Printf.printf "\n\n%s:\n" function_decl.rfn_name in
                      let () = fd.stack_map |> IdVarMap.to_seq |> Seq.iter (fun ((s, kt), adr) ->
                        Printf.printf "%s : %s == [%s, %s]\n"
                        (s)
                        (KosuIrTyped.Asttypprint.string_of_rktype kt)
                        (X86_64Pprint.string_of_register adr.base)
                        (X86_64Pprint.string_of_address_offset adr.offset)
                        ) in *)
                   Some
                     (Afunction
                        {
                          asm_name;
                          asm_body =
                            [ Directive "cfi_startproc" ]
                            @ prologue @ List.tl conversion @ epilogue
                            @ [ Directive "cfi_endproc" ];
                        }
                     )
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
                            Spec.label_of_constant ~module_path:current_module
                              rconst_name;
                          value = `IntVal (size, value);
                        }
                     )
               | TNConst
                   {
                     rconst_name;
                     value =
                       { rktype = RTFloat fsize; rexpression = REFloat (_, f) };
                   } ->
                   let size, bit_float =
                     match fsize with
                     | F32 ->
                         ( KosuFrontend.Ast.I32,
                           f |> Int32.bits_of_float |> Int64.of_int32
                         )
                     | F64 ->
                         (KosuFrontend.Ast.I64, Int64.bits_of_float f)
                   in
                   Some
                     (AConst
                        {
                          asm_const_name =
                            Spec.label_of_constant ~module_path:current_module
                              rconst_name;
                          value = `IntVal (size, bit_float);
                        }
                     )
               | TNConst
                   {
                     rconst_name;
                     value = { rktype = _; rexpression = REstring s };
                   } ->
                   Some
                     (AConst
                        {
                          asm_const_name =
                            Spec.label_of_constant ~module_path:current_module
                              rconst_name;
                          value = `StrVal s;
                        }
                     )
               | TNEnum _ | TNStruct _ | TNSyscall _ | TNExternFunc _ | _ ->
                   None
           )

  let asm_module_path_of_tac_module_path ~litterals rprogram
      { path; tac_module } =
    {
      apath = path;
      asm_module =
        AsmModule (asm_module_of_tac_module ~litterals path rprogram tac_module);
    }

  let asm_program_of_tac_program ~(start : string option) tac_program =
    let () = ignore start in
    let modules =
      tac_program
      |> List.map (fun ({ filename; tac_module_path; rprogram } as named) ->
             let str_lit_map =
               KosuIrTAC.Asttachelper.StringLitteral
               .map_string_litteral_of_named_rmodule_path
                 ~null_terminated_string:false named ()
             in
             let float_lit_map =
               KosuIrTAC.Asttachelper.FloatLitteral
               .map_float_litteral_of_named_rmodule_path named ()
             in
             let litterals = { str_lit_map; float_lit_map } in
             {
               filename =
                 filename |> Filename.chop_extension |> Printf.sprintf "%s.s";
               asm_module_path =
                 asm_module_path_of_tac_module_path ~litterals rprogram
                   tac_module_path;
               rprogram;
               litterals;
             }
         )
    in
    match start with
    | None ->
        modules
    | Some entry_name ->
        kosu_crt0_asm entry_name :: modules

  let sort_asm_module (AsmModule anodes) =
    AsmModule
      (anodes
      |> List.sort (fun lhs rhs ->
             match (lhs, rhs) with
             | Afunction _, AConst _ ->
                 -1
             | AConst _, Afunction _ ->
                 1
             | _ ->
                 0
         )
      )
end

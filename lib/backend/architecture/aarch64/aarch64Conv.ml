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

open Aarch64Core
open Aarch64Core.Instruction
open Aarch64Core.Register
open KosuIrTyped.Asttyhelper.Sizeof
open KosuIrTAC.Asttachelper.StringLitteral
open KosuIrTAC.Asttac
open Util
module AsmProgram = Common.AsmProgram (Aarch64Core.Instruction)
open AsmProgram

module Make (AsmSpec : Aarch64AsmSpec.Aarch64AsmSpecification) = struct
  let mov_integer register n =
    let open Immediat in
    if is_direct_immediat n then
      Instruction
        (Mov { destination = register; flexsec_operand = `ILitteral n })
      :: []
    else
      let int64, int48, int32, int16 = split n in
      let base =
        [
          Instruction
            (Mov { destination = register; flexsec_operand = `ILitteral int16 });
        ]
      in
      ( ( base |> fun l ->
          if int32 = 0L then l
          else
            l
            @ [
                Instruction
                  (Movk
                     {
                       destination = register;
                       operand = `ILitteral int32;
                       shift = Some SH16;
                     });
              ] )
      |> fun l ->
        if int48 = 0L then l
        else
          l
          @ [
              Instruction
                (Movk
                   {
                     destination = register;
                     operand = `ILitteral int48;
                     shift = Some SH32;
                   });
            ] )
      |> fun l ->
      if int64 = 0L then l
      else
        l
        @ [
            Instruction
              (Movk
                 {
                   destination = register;
                   operand = `ILitteral int32;
                   shift = Some SH16;
                 });
          ]

  let sizeofn = KosuIrTyped.Asttyhelper.Sizeof.sizeof

  let copy_result ?(before_copy = fun _ -> []) ~where ~register ~rval_rktype
      rprogram =
    where
    |> Option.map (fun waddress ->
           before_copy waddress
           @ copy_from_reg register waddress rval_rktype rprogram)
    |> Option.value ~default:[]

  let return_function_instruction_none_reg_size ~where ~is_deref =
    where
    |> Option.map (fun waddress ->
           match is_deref with
           | Some pointer ->
               [
                 Instruction
                   (LDR
                      {
                        data_size = None;
                        destination = xr;
                        adress_src = pointer;
                        adress_mode = Immediat;
                      });
                 Instruction
                   (LDR
                      {
                        data_size = None;
                        destination = xr;
                        adress_src = create_adress xr;
                        adress_mode = Immediat;
                      });
               ]
           | None ->
               [
                 Instruction
                   (ADD
                      {
                        destination = Register.xr;
                        operand1 = waddress.base;
                        operand2 = `ILitteral waddress.offset;
                        offset = false;
                      });
               ])
    |> Option.value ~default:[]

  let load_or_lea ~target_reg ~ktype ~address rprogram = 
    let sizeof = sizeofn rprogram ktype in
    let rreg =
      if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then
        FRegister64 D9
      else if sizeof > 4L then to_64bits target_reg
      else to_32bits target_reg
    in
    if is_register_size sizeof then
      ( rreg,
        [
          Instruction
            (LDR
               {
                 data_size = compute_data_size ktype sizeof;
                 destination = rreg;
                 adress_src = address;
                 adress_mode = Immediat;
               });
        ] )
    else
      ( rreg,
        [
          Instruction
            (ADD
               {
                 destination = to_64bits rreg;
                 offset = false;
                 operand1 = address.base;
                 operand2 = `ILitteral address.offset;
               });
        ] )

  let return_function_instruction_reg_size ~where ~is_deref ~return_reg
      ~return_type rprogram =
    where
    |> Option.map (fun waddress ->
           match is_deref with
           | Some pointer ->
               Instruction
                 (LDR
                    {
                      data_size = None;
                      destination = xr;
                      adress_src = pointer;
                      adress_mode = Immediat;
                    })
               :: copy_from_reg return_reg (create_adress xr) return_type
                    rprogram
           | None -> copy_from_reg return_reg waddress return_type rprogram)
    |> Option.value ~default:[]

  let translate_tac_expression ~str_lit_map ?(target_reg = Register32 W9)
      rprogram (fd : FrameManager.frame_desc) = function
    | { tac_expression = TEString s; expr_rktype = _ } ->
        let reg64 = to_64bits target_reg in
        let (SLit str_labl) = Hashtbl.find str_lit_map s in
        (target_reg, load_label (AsmSpec.label_of_constant str_labl) reg64)
    | { tac_expression = TEFalse | TEmpty; expr_rktype = _ } ->
        ( to_32bits target_reg,
          Instruction
            (Mov
               {
                 destination = to_32bits target_reg;
                 flexsec_operand = `Register (Register32 WZR);
               })
          :: [] )
    | { tac_expression = TENullptr; expr_rktype = _ } ->
        let r64 = to_64bits target_reg in
        ( r64,
          Instruction
            (Mov
               {
                 destination = r64;
                 flexsec_operand = `Register (Register64 XZR);
               })
          :: [] )
    | { tac_expression = TETrue; _ } ->
        let s32 = to_32bits target_reg in
        ( s32,
          Instruction
            (Mov { destination = s32; flexsec_operand = `ILitteral 1L })
          :: [] )
    | { tac_expression = TEInt (_, isize, int64); _ } ->
        let rreg =
          match isize with
          | I64 -> to_64bits target_reg
          | _ -> to_32bits target_reg
        in
        (rreg, mov_integer rreg int64)
    | { tac_expression = TEFloat float; _ } ->
        let f64reg = to_64fbits target_reg in
        ( f64reg,
          Instruction
            (Mov { destination = f64reg; flexsec_operand = `F64Litteral float })
          :: [] )
    | { tac_expression = TEIdentifier id; expr_rktype } ->
        let adress =
          FrameManager.address_of (id, expr_rktype) fd |> fun adr ->
          match adr with
          | Some a -> a
          | None -> failwith "tte identifier setup null address"
        in
        let sizeof = sizeofn rprogram expr_rktype in
        let rreg =
          if KosuIrTyped.Asttyhelper.RType.is_64bits_float expr_rktype then
            FRegister64 D9
          else if sizeof > 4L then to_64bits target_reg
          else to_32bits target_reg
        in
        if is_register_size sizeof then
          ( rreg,
            [
              Instruction
                (LDR
                   {
                     data_size = compute_data_size expr_rktype sizeof;
                     destination = rreg;
                     adress_src = adress;
                     adress_mode = Immediat;
                   });
            ] )
        else
          ( rreg,
            [
              Instruction
                (ADD
                   {
                     destination = rreg;
                     offset = false;
                     operand1 = adress.base;
                     operand2 = `ILitteral adress.offset;
                   });
            ] )
    | { tac_expression = TESizeof kt; _ } ->
        let r64 = to_64bits target_reg in
        let sizeof = sizeofn rprogram kt in
        ( r64,
          [
            Line_Com
              (Comment
                 (Printf.sprintf "sizeof %s = %Lu"
                    (KosuIrTyped.Asttypprint.string_of_rktype kt) sizeof));
            Instruction
              (Mov { destination = r64; flexsec_operand = `ILitteral sizeof });
          ] )
    | {
        tac_expression = TEConst { name; module_path };
        expr_rktype = RTString_lit;
      } ->
        let reg64 = to_64bits target_reg in
        ( target_reg,
          load_label (AsmSpec.label_of_constant ~module_path name) reg64 )
    | {
        tac_expression = TEConst { name; module_path };
        expr_rktype = RTInteger (_, size) as kt;
      } ->
        let data_size =
          compute_data_size kt
            (Int64.of_int @@ KosuFrontend.Ast.Isize.size_of_isize size)
        in
        (* let open KosuIrTyped.Asttyped in *)
        let const_decl =
          match
            rprogram
            |> KosuIrTyped.Asttyhelper.RProgram.find_const_decl ~name
                 ~module_path
          with
          | None ->
              failwith
                (Printf.sprintf "No const decl for %s::%s" module_path name)
          | Some s -> s
        in

        let int_value =
          match const_decl.value.rexpression with
          | REInteger (_, _, value) -> value
          | _ -> failwith "Not an Integer"
        in

        if int_value > 65535L || int_value < -65535L then
          let tmp11 = tmp64reg_4 in
          let load_instruction = load_label ~module_path name tmp11 in
          let fetch =
            Instruction
              (LDR
                 {
                   data_size;
                   destination = target_reg;
                   adress_src = create_adress tmp11;
                   adress_mode = Immediat;
                 })
          in
          (target_reg, load_instruction @ [ fetch ])
        else
          let rreg =
            match size with
            | I64 -> to_64bits target_reg
            | _ -> to_32bits target_reg
          in
          (rreg, mov_integer rreg int_value)
    | _ -> failwith "Other expression"

  let translate_tac_binop ~str_lit_map ~cc ~blhs ~brhs ~where ~rval_rktype
      rprogram fd =
    let r10 = tmpreg_of_ktype_3 rprogram brhs.expr_rktype in
    let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
    let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in
    let zero_reg =
      reg_of_size
        (size_of_ktype_size (sizeofn rprogram blhs.expr_rktype))
        (Register64 XZR)
    in
    let right_reg, rinstructions =
      translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd brhs
    in
    let left_reg, linstructions =
      translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd blhs
    in
    let equal_instruction =
      [
        Instruction (Mov { destination = r8; flexsec_operand = `ILitteral 0L });
        Instruction
          (SUBS
             {
               destination = r9;
               operand1 = left_reg;
               operand2 = `Register right_reg;
             });
        Instruction
          (CSINC
             {
               destination = r8;
               operand1 = r8;
               operand2 = zero_reg;
               condition = cc;
             });
      ]
    in
    copy_result
      ~before_copy:(fun _ -> linstructions @ rinstructions @ equal_instruction)
      ~where ~register:r8 ~rval_rktype rprogram

  let translate_tac_binop_self ~str_lit_map ~blhs ~brhs ~where ~rval_rktype
      fbinop rprogram fd =
    let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
    let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
    let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
    let right_reg, rinstructions =
      translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs
    in
    let left_reg, linstructions =
      translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs
    in
    let binop_instructtio =
      fbinop ~destination:r9 ~operand1:left_reg ~operand2:right_reg
    in
    let before_copy _ = linstructions @ rinstructions @ binop_instructtio in
    copy_result ~before_copy ~where ~register:r9 ~rval_rktype rprogram

  let translate_tac_rvalue ?(is_deref = None) ~str_lit_map
      ~(where : address option) current_module rprogram
      (fd : FrameManager.frame_desc) { rval_rktype; rvalue } =
    match rvalue with
    | RVExpression tac_typed_expression ->
        let last_reg, instructions =
          translate_tac_expression ~str_lit_map rprogram fd tac_typed_expression
        in
        let copy_instruction =
          copy_result ~where ~register:last_reg ~rval_rktype rprogram
        in
        instructions @ copy_instruction
    | RVLambda {fn_pointer_name; parameters = _; return_ktype = _; captured_env} ->
      where |> Option.map (fun waddress -> 
        let ktlist = captured_env |> List.map snd in
        let offset_list = match captured_env with
          | [] -> []
          | _::_ ->
            captured_env
            |> List.mapi (fun index _value ->
                  offset_of_tuple_index index ktlist rprogram)
            |> List.tl
            |> fun l -> l @ [ 0L ]
        in

        let name_of_fn = AsmSpec.label_of_constant fn_pointer_name in
        let r9 = tmp64reg_2 in
        let compute_address = load_label name_of_fn r9 in
        let store_address = Instruction (STR {data_size = None; source = r9; adress = waddress; adress_mode = Immediat}) in
        let incremented_address = increment_adress 8L waddress in
        let copy_instru =  captured_env 
          |> List.mapi Util.couple
          |> List.fold_left (fun acc (index, st ) -> 
            let st_name, st_kt = st in
            (* let source_address = match FrameManager.address_of st fd with
              | Some addr -> addr
              | None -> Printf.sprintf "cannot not locate %s\n" (fst st) |> failwith
            in *)
            let reg_texp, instructions = translate_tac_expression ~str_lit_map rprogram fd ({expr_rktype = st_kt; tac_expression = TEIdentifier st_name}) in
            let target_address = increment_adress (List.nth offset_list index) incremented_address in
            let copy_instructions = copy_result ~where:(Some target_address) ~register:reg_texp ~rval_rktype:(snd st) rprogram in
            instructions @ copy_instructions @ acc
          ) [] in
          compute_address @ store_address::copy_instru
        ) |> Option.value ~default:[]
        
    | RVStruct { module_path = _; struct_name = _s; fields } ->
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

        (* let () = offset_list |> List.map (Printf.sprintf "%Lu") |> String.concat ", " |> Printf.printf "%s off = [%s]\n" _s in *)
        fields
        |> List.mapi (fun index value -> (index, value))
        |> List.fold_left
             (fun acc (index, (_field, tte)) ->
               let reg_texp, instructions =
                 translate_tac_expression ~str_lit_map rprogram fd tte
               in
               let copy_instruction =
                 copy_result
                   ~where:
                     (where
                     |> Option.map
                          (increment_adress (List.nth offset_list index)))
                   ~register:reg_texp ~rval_rktype:tte.expr_rktype rprogram
               in
               acc @ instructions @ copy_instruction)
             []
    | RVClosureCall {variable_name; parameters; return_ktype; captured_env; closure_rktype} ->
      let () = print_endline @@ Printf.sprintf "Closurename %s {%s}" variable_name 
      (captured_env |> List.map snd |> List.map KosuIrTyped.Asttypprint.string_of_rktype |> String.concat ", ") 
      in
      let named_env = KosuIrTyped.Asttyped.RTNamedTuple captured_env in
      let env_size = sizeofn rprogram named_env in
      let closure_address = Option.get @@ FrameManager.address_of (variable_name, closure_rktype) fd in
      let env_address = increment_adress (8L) closure_address in
      let register_param_count =
        min
          (List.length parameters)
          (List.length argument_registers)
      in
      let args_in_reg, _args_on_stack =
        parameters
        |> List.mapi (fun index value -> (index, value))
        |> List.partition_map (fun (index, value) ->
               if index < register_param_count then Either.left value
               else Either.right value)
      in
      let env_in_reg = register_param_count <  (List.length argument_registers) in

      let instructions, _regs =
        let instruction, regs =  args_in_reg
          |> Util.ListHelper.combine_safe argument_registers
          |> List.fold_left_map
              (fun acc (reg, tte) ->
                let reg, instruction =
                  translate_tac_expression ~str_lit_map ~target_reg:reg
                    rprogram fd tte
                in
                (acc @ instruction, `Register reg))
              []
          in
          if env_in_reg && (closure_rktype |> KosuIrTyped.Asttyhelper.RType.is_closure) then 
            let env_reg = List.nth (argument_registers) (register_param_count) in
            let sized_reg = Register.reg_of_size (size_of_ktype_size env_size) env_reg in
            [Line_Com (Comment (Printf.sprintf "reg = %d" (register_param_count)) )] @ snd (load_or_lea ~target_reg:(sized_reg) ~ktype:named_env ~address:env_address rprogram) @ instruction, regs
          else
            instruction, regs
          in
        let set_on_stack_instructions = [] in (* TOODO *)
        let ldr_address = Instruction (LDR {data_size = None; destination = tmp64reg_2; adress_mode = Immediat; adress_src = closure_address}) in
        let call_instructions = FrameManager.call_instruction ~origin:(`Reg tmp64reg_2) fd in
        let return_size = sizeofn rprogram return_ktype in
        let return_reg =
          return_register_ktype
            ~float:
              (KosuIrTyped.Asttyhelper.RType.is_64bits_float
                 return_ktype)
            return_size
        in
        begin match is_register_size return_size with
        | true ->
            let copy_instruction =
              return_function_instruction_reg_size ~where ~is_deref
                ~return_reg ~return_type:return_ktype rprogram
            in
            instructions @ set_on_stack_instructions @ ldr_address::call_instructions
            @ copy_instruction
            (* Is not the same check the instructions order*)
        | false ->
            let copy_instruction =
              return_function_instruction_none_reg_size ~where ~is_deref
            in
            instructions @ set_on_stack_instructions @ copy_instruction
            @ ldr_address::call_instructions
          end
    | RVFunction { module_path; fn_name; generics_resolver = _; tac_parameters }
      -> (
        let typed_parameters =
          tac_parameters |> List.map (fun { expr_rktype; _ } -> expr_rktype)
        in
        let fn_module =
          if module_path = "" then current_module else module_path
        in
        let fn_decl =
          KosuIrTyped.Asttyhelper.RProgram.find_function_decl_of_name fn_module
            fn_name rprogram
          |> Option.get
        in
        match fn_decl with
        | RExternal_Decl external_func_decl ->
            let fn_label =
              AsmSpec.label_of_external_function external_func_decl
            in
            (* let () = tac_parameters |> List.map KosuIrTAC.Asttacpprint.string_of_typed_tac_expression |> String.concat ", " |> Printf.printf "printf param = %s" in *)
            (* let fn_register_params, _stack_param = tac_parameters |> List.mapi (fun index -> fun value -> index, value) |> List.partition_map (fun (index, value) ->
               if index < 8 then Either.left value else Either.right value
               ) in *)
            let register_param_count =
              min
                (List.length external_func_decl.fn_parameters)
                (List.length argument_registers)
            in
            let args_in_reg, args_on_stack =
              tac_parameters
              |> List.mapi (fun index value -> (index, value))
              |> List.partition_map (fun (index, value) ->
                     if index < register_param_count then Either.left value
                     else Either.right value)
            in

            (* let stack_params_offset = stack_param |> List.map (fun {expr_rktype; _} ->
                   if KosuIrTyped.Asttyhelper.Sizeof.sizeof rprogram expr_rktype > 8L then KosuIrTyped.Asttyhelper.RType.rpointer expr_rktype else expr_rktype
                 ) in

               let stack_store = stack_param |> List.map *)
            let instructions, _regs =
              args_in_reg
              |> Util.ListHelper.combine_safe argument_registers
              |> List.fold_left_map
                   (fun acc (reg, tte) ->
                     let reg, instruction =
                       translate_tac_expression ~str_lit_map ~target_reg:reg
                         rprogram fd tte
                     in
                     (acc @ instruction, `Register reg))
                   []
            in
            let set_on_stack_instructions =
              args_on_stack
              |> List.mapi (fun index tte ->
                     let last_reg, instructions =
                       translate_tac_expression ~str_lit_map rprogram fd tte
                     in
                     let address =
                       create_adress
                         ~offset:(Int64.of_int (index * 8))
                         (Register64 SP)
                     in
                     let set =
                       Instruction
                         (STR
                            {
                              data_size = None;
                              source = to_64bits last_reg;
                              adress = address;
                              adress_mode = Immediat;
                            })
                     in
                     instructions @ [ set ])
              |> List.flatten
            in
            let call_instructions =
              FrameManager.call_instruction ~origin:(`Label fn_label) fd
            in
            let return_size = sizeofn rprogram external_func_decl.return_type in
            let return_reg =
              return_register_ktype
                ~float:
                  (KosuIrTyped.Asttyhelper.RType.is_64bits_float
                     external_func_decl.return_type)
                return_size
            in
            let extern_instructions =
              match is_register_size return_size with
              | true ->
                  let copy_instruction =
                    return_function_instruction_reg_size ~where ~is_deref
                      ~return_reg ~return_type:external_func_decl.return_type
                      rprogram
                  in
                  instructions @ set_on_stack_instructions @ call_instructions
                  @ copy_instruction
                  (* Is not the same check the instructions order*)
              | false ->
                  let copy_instruction =
                    return_function_instruction_none_reg_size ~where ~is_deref
                  in
                  instructions @ set_on_stack_instructions @ copy_instruction
                  @ call_instructions
            in
            extern_instructions
        | RSyscall_Decl syscall_decl ->
            let instructions, _regs =
              tac_parameters
              |> Util.ListHelper.combine_safe argument_registers
              |> List.fold_left_map
                   (fun acc (reg, tte) ->
                     let reg, instruction =
                       translate_tac_expression ~str_lit_map ~target_reg:reg
                         rprogram fd tte
                     in
                     (acc @ instruction, reg))
                   []
            in
            let return_size = sizeofn rprogram syscall_decl.return_type in
            let return_reg =
              return_register_ktype
                ~float:
                  (KosuIrTyped.Asttyhelper.RType.is_64bits_float
                     syscall_decl.return_type)
                return_size
            in
            instructions
            @ [
                Line_Com (Comment ("syscall " ^ syscall_decl.rsyscall_name));
                Instruction
                  (Mov
                     {
                       destination = Register64 X16;
                       flexsec_operand = `ILitteral syscall_decl.opcode;
                     });
                Instruction SVC;
              ]
            @ (where
              |> Option.map (fun waddress ->
                     match is_deref with
                     | Some pointer ->
                         Instruction
                           (LDR
                              {
                                data_size = None;
                                destination = xr;
                                adress_src = pointer;
                                adress_mode = Immediat;
                              })
                         :: copy_from_reg return_reg (create_adress xr)
                              syscall_decl.return_type rprogram
                     | None ->
                         copy_from_reg return_reg waddress
                           syscall_decl.return_type rprogram)
              |> Option.value ~default:[])
        | RKosufn_Decl kdl -> (
          let () = Printf.printf "FN = %s\n%!" kdl.rfn_name in
            let function_decl =
              rprogram
              |> KosuIrTyped.Asttyhelper.RProgram
                 .find_function_decl_exact_param_types ~module_name:fn_module
                   ~fn_name ~ktypes:typed_parameters
              |> Option.get
            in
            let fn_label =
              AsmSpec.label_of_kosu_function ~module_path function_decl
            in

            let instructions, _regs =
              tac_parameters
              |> Util.ListHelper.combine_safe argument_registers
              |> List.fold_left_map
                   (fun acc (reg, tte) ->
                     let reg, instruction =
                       translate_tac_expression ~str_lit_map ~target_reg:reg
                         rprogram fd tte
                     in
                     (acc @ instruction, reg))
                   []
            in

            let set_on_stack_instructions = [] (* TODO *) in

            let call_instructions =
              FrameManager.call_instruction ~origin:(`Label fn_label) fd
            in
            let return_size = sizeofn rprogram function_decl.return_type in
            let return_reg =
              return_register_ktype
                ~float:
                  (KosuIrTyped.Asttyhelper.RType.is_64bits_float
                     function_decl.return_type)
                return_size
            in
            (* let () = Printf.printf "Return size : %s = %Lu" function_decl.rfn_name return_size in *)
            match is_register_size return_size with
            | true ->
                let copy_instruction =
                  return_function_instruction_reg_size ~where ~is_deref
                    ~return_reg ~return_type:function_decl.return_type rprogram
                in
                instructions @ set_on_stack_instructions @ call_instructions
                @ copy_instruction
                (* Is not the same check the instructions order*)
            | false ->
                let copy_instruction =
                  return_function_instruction_none_reg_size ~where ~is_deref
                in
                instructions @ set_on_stack_instructions @ copy_instruction
                @ call_instructions))
    | RVTuple ttes ->
        let ktlis = ttes |> List.map (fun { expr_rktype; _ } -> expr_rktype) in
        let offset_list =
          ttes
          |> List.mapi (fun index _value ->
                 offset_of_tuple_index index ktlis rprogram)
          |> List.tl
          |> fun l -> l @ [ 0L ]
        in
        ttes
        |> List.mapi (fun index value -> (index, value))
        |> List.fold_left
             (fun acc (index, tte) ->
               let reg_texp, instructions =
                 translate_tac_expression rprogram ~str_lit_map fd tte
               in
               let copy_instructions =
                 copy_result
                   ~where:
                     (where
                     |> Option.map
                          (increment_adress (List.nth offset_list index)))
                   ~register:reg_texp ~rval_rktype:tte.expr_rktype rprogram
               in
               acc @ instructions @ copy_instructions)
             []
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

        let size = compute_data_size rval_rktype sizeof in
        let tmpreg = tmpreg_of_size_2 sizeof in
        let copy_instructions =
          copy_result
            ~before_copy:(fun _ ->
              [
                Instruction
                  (LDR
                     {
                       data_size = size;
                       destination = tmpreg;
                       adress_src = field_address;
                       adress_mode = Immediat;
                     });
              ])
            ~where ~register:tmpreg ~rval_rktype rprogram
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
          copy_result
            ~before_copy:(fun _ ->
              [
                Instruction
                  (ADD
                     {
                       destination = tmp64reg;
                       operand1 = adress.base;
                       operand2 = `ILitteral adress.offset;
                       offset = false;
                     });
              ])
            ~where ~register:tmp64reg ~rval_rktype rprogram
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
        let load_instruction =
          [
            Instruction
              (LDR
                 {
                   data_size = None;
                   destination = tmp64reg_2;
                   adress_src = adress;
                   adress_mode = Immediat;
                 });
          ]
        in
        let last_reg, load_indirect =
          if
            is_register_size
            @@ KosuIrTyped.Asttyhelper.Sizeof.sizeof rprogram rval_rktype
          then
            ( tmp64reg,
              [
                Instruction
                  (LDR
                     {
                       data_size = None;
                       destination = tmp64reg;
                       adress_src = create_adress tmp64reg_2;
                       adress_mode = Immediat;
                     });
              ] )
          else (tmp64reg_2, [])
        in
        (* let sizeof = sizeofn rprogram rval_rktype in *)
        let copy_instructions =
          copy_result
            ~before_copy:(fun _ -> load_instruction @ load_indirect)
            ~where ~register:last_reg ~rval_rktype rprogram
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
        enum_tte_list
        |> List.mapi (fun index value -> (index, value))
        |> List.fold_left
             (fun acc (index, tte) ->
               let reg_texp, instructions =
                 translate_tac_expression rprogram ~str_lit_map fd tte
               in
               let copy_instructions =
                 copy_result
                   ~where:
                     (where
                     |> Option.map
                          (increment_adress (List.nth offset_list index)))
                   ~register:reg_texp ~rval_rktype:tte.expr_rktype rprogram
               in
               acc @ instructions @ copy_instructions)
             []
    | RVDiscard | RVLater -> []
    | RVBuiltinBinop
        { binop = TacBool ((TacOr | TacAnd) as tac_bool); blhs; brhs } ->
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram blhs.expr_rktype in
        let right_reg, rinstructions =
          translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd brhs
        in
        let left_reg, linstructions =
          translate_tac_expression ~str_lit_map ~target_reg:r8 rprogram fd blhs
        in
        let and_or_instruction =
          and_or_or_instruction tac_bool ~destination:r8 ~operand1:left_reg
            ~operand2:right_reg
        in
        let copy_instructions =
          copy_result
            ~before_copy:(fun _ ->
              linstructions @ rinstructions @ and_or_instruction)
            ~where ~register:r8 ~rval_rktype rprogram
        in

        copy_instructions
    | RVBuiltinBinop { binop = TacBool bool_binop; blhs; brhs } ->
        let is_unsigned =
          KosuIrTyped.Asttyhelper.RType.is_raw_unsigned blhs.expr_rktype
          || KosuIrTyped.Asttyhelper.RType.is_raw_unsigned brhs.expr_rktype
        in
        let cc =
          Option.get @@ Condition_Code.cc_of_tac_bin ~is_unsigned bool_binop
        in
        translate_tac_binop ~str_lit_map ~cc ~blhs ~brhs ~where ~rval_rktype
          rprogram fd
    | RVBuiltinBinop
        {
          binop =
            TacSelf
              (( TacMult | TacDiv | TacBitwiseAnd | TacBitwiseOr | TacBitwiseXor
               | TacShiftLeft | TacShiftRight ) as self_binop);
          blhs;
          brhs;
        } ->
        let is_unsigned =
          KosuIrTyped.Asttyhelper.RType.is_unsigned_integer blhs.expr_rktype
        in
        let binop_func =
          binop_instruction_of_tacself ~unsigned:is_unsigned self_binop
        in
        translate_tac_binop_self ~str_lit_map ~blhs ~brhs ~where ~rval_rktype
          binop_func rprogram fd
    | RVBuiltinBinop
        { binop = TacSelf ((TacAdd | TacMinus) as self_binop); blhs; brhs } -> (
        match KosuIrTyped.Asttyhelper.RType.is_pointer rval_rktype with
        | false ->
            let binop_func = binop_instruction_of_tacself self_binop in
            translate_tac_binop_self ~str_lit_map ~blhs ~brhs ~where
              ~rval_rktype binop_func rprogram fd
        | true ->
            let pointee_size =
              rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee
              |> KosuIrTyped.Asttyhelper.Sizeof.sizeof rprogram
            in
            let r9 = tmp64reg_2 in
            let r10 = tmp64reg_3 in
            let r11 = tmp64reg_4 in
            let operand_instructions =
              match blhs.expr_rktype with
              | KosuIrTyped.Asttyped.RTPointer _ ->
                  let _ptr_reg, linstructions =
                    translate_tac_expression ~str_lit_map ~target_reg:r9
                      rprogram fd blhs
                  in
                  let _nb_reg, rinstructions =
                    translate_tac_expression ~str_lit_map ~target_reg:r10
                      rprogram fd brhs
                  in
                  linstructions @ rinstructions
              | _ ->
                  failwith
                    "The typechecker force in pointer arithmetic the pointer \
                     to be left side of add"
            in
            let add_or_sub_instructions =
              if pointee_size = 1L then
                binop_instruction_of_tacself self_binop ~destination:r9
                  ~operand1:r9 ~operand2:r10
              else
                (instruction
                @@ Mov
                     {
                       destination = r11;
                       flexsec_operand = `ILitteral pointee_size;
                     })
                :: mult_add_or_sub self_binop ~destination:r9 ~operand1_base:r9
                     ~operand2:r10 ~scale:r11
            in
            let before_copy _ =
              operand_instructions @ add_or_sub_instructions
            in
            copy_result ~before_copy ~where ~register:r9 ~rval_rktype rprogram)
    | RVBuiltinBinop { binop = TacSelf TacModulo; blhs; brhs } ->
        let r9 = tmpreg_of_ktype_2 rprogram blhs.expr_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram blhs.expr_rktype in
        let r11 = tmpreg_of_ktype_4 rprogram brhs.expr_rktype in
        let left_reg, linstructions =
          translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd blhs
        in
        let right_reg, rinstructions =
          translate_tac_expression ~str_lit_map ~target_reg:r11 rprogram fd brhs
        in
        let div_instruction =
          if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer rval_rktype then
            Instruction
              (UDIV
                 { destination = r9; operand1 = left_reg; operand2 = right_reg })
          else
            Instruction
              (SDIV
                 { destination = r9; operand1 = left_reg; operand2 = right_reg })
        in
        let modulo_instruction =
          [
            (* udiv x2, x0, x1 // Thank you StackOverflow
               msub x3, x2, x1, x0 *)
            div_instruction;
            Instruction
              (MSUB
                 {
                   destination = r9;
                   operand1_base = left_reg;
                   operand2 = right_reg;
                   scale = r9;
                 });
          ]
        in
        let before_copy _ =
          linstructions @ rinstructions @ modulo_instruction
        in
        copy_result ~before_copy ~where ~register:r9 ~rval_rktype rprogram
    | RVBuiltinUnop { unop = TacUminus; expr } ->
        let r9 = tmpreg_of_ktype_2 rprogram rval_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram expr.expr_rktype in
        let last_reg, instructions =
          translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd expr
        in
        let uminus_instructions =
          Instruction (Neg { destination = r9; source = last_reg })
        in
        let before_copy _ = instructions @ [ uminus_instructions ] in
        copy_result ~before_copy ~where ~register:r9 ~rval_rktype rprogram
    | RVBuiltinUnop { unop = TacNot; expr } ->
        let r9 = tmpreg_of_ktype_2 rprogram rval_rktype in
        let r10 = tmpreg_of_ktype_3 rprogram expr.expr_rktype in
        let last_reg, instructions =
          translate_tac_expression ~str_lit_map ~target_reg:r10 rprogram fd expr
        in
        let not_instructions =
          if KosuIrTyped.Asttyhelper.RType.is_bool rval_rktype then
            Instruction
              (EOR
                 {
                   destination = r9;
                   operand1 = last_reg;
                   operand2 = `ILitteral 1L;
                 })
          else
            Instruction (Mvn { destination = r9; operand = `Register last_reg })
        in
        let before_copy _ = instructions @ [ not_instructions ] in
        copy_result ~before_copy ~where ~register:r9 ~rval_rktype rprogram
    | RVBuiltinCall { fn_name; parameters } -> (
        let open KosuFrontend.Ast.Builtin_Function in
        match fn_name with
        | Tos8 | Tou8 | Tos16 | Tou16 | Tos32 | Tou32 | Tos64 | Tou64
        | Stringl_ptr ->
            let tte = parameters |> List.hd in
            let r9 = tmp32reg_2 in
            let last_reg, instructions =
              translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd
                tte
            in
            let before_copy _ = instructions in
            copy_result ~before_copy ~where ~register:last_reg ~rval_rktype
              rprogram)
    | RVCustomUnop record ->
        let open KosuIrTAC.Asttachelper.Operator in
        let op_decls =
          KosuIrTyped.Asttyhelper.RProgram.find_unary_operator_decl
            (parser_unary_op_of_tac_unary_op record.unop)
            record.expr.expr_rktype ~r_type:rval_rktype rprogram
        in
        let op_decl =
          match op_decls with
          | t :: [] -> t
          | _ ->
              failwith "What the type checker has done: No unary op declaration"
        in
        let fn_label =
          AsmSpec.label_of_kosu_operator ~module_path:current_module op_decl
        in
        let _, instructions =
          translate_tac_expression ~str_lit_map ~target_reg:(Register64 X0)
            rprogram fd record.expr
        in
        let call_instruction =
          FrameManager.call_instruction ~origin:(`Label fn_label) fd
        in
        let return_type =
          KosuIrTyped.Asttyhelper.OperatorDeclaration.op_return_type op_decl
        in
        let return_size = sizeofn rprogram return_type in
        let return_reg =
          return_register_ktype
            ~float:(KosuIrTyped.Asttyhelper.RType.is_64bits_float return_type)
            return_size
        in
        let operator_instructions =
          match is_register_size return_size with
          | true ->
              let copy_instruction =
                return_function_instruction_reg_size ~where ~is_deref
                  ~return_reg ~return_type rprogram
              in
              instructions @ call_instruction @ copy_instruction
              (* Is not the same check the instructions order*)
          | false ->
              let copy_instruction =
                return_function_instruction_none_reg_size ~where ~is_deref
              in
              instructions @ copy_instruction @ call_instruction
        in
        operator_instructions
    | RVCustomBinop
        ({
           binop =
             TacSelf _ | TacBool TacSup | TacBool TacInf | TacBool TacEqual;
           _;
         } as self) ->
        let open KosuIrTAC.Asttachelper.Operator in
        let op_decls =
          KosuIrTyped.Asttyhelper.RProgram.find_binary_operator_decl
            (parser_binary_op_of_tac_binary_op self.binop)
            (self.blhs.expr_rktype, self.brhs.expr_rktype)
            ~r_type:rval_rktype rprogram
        in
        let op_decl =
          match op_decls with
          | t :: [] -> t
          | _ ->
              failwith
                "What the type checker has done: No binary op declaration | \
                 Too much"
        in
        let fn_label =
          AsmSpec.label_of_kosu_operator ~module_path:current_module op_decl
        in
        let _, linstructions =
          translate_tac_expression ~str_lit_map ~target_reg:(Register64 X0)
            rprogram fd self.blhs
        in
        let _, rinstructions =
          translate_tac_expression ~str_lit_map ~target_reg:(Register64 X1)
            rprogram fd self.brhs
        in

        let call_instruction =
          FrameManager.call_instruction ~origin:(`Label fn_label) fd
        in
        let return_type =
          KosuIrTyped.Asttyhelper.OperatorDeclaration.op_return_type op_decl
        in
        let return_size = sizeofn rprogram return_type in
        let return_reg =
          return_register_ktype
            ~float:(KosuIrTyped.Asttyhelper.RType.is_64bits_float return_type)
            return_size
        in
        let operator_instructions =
          match is_register_size return_size with
          | true ->
              let copy_instruction =
                return_function_instruction_reg_size ~where ~is_deref
                  ~return_reg ~return_type rprogram
              in
              linstructions @ rinstructions @ call_instruction
              @ copy_instruction
              (* Is not the same check the instructions order*)
          | false ->
              let copy_instruction =
                return_function_instruction_none_reg_size ~where ~is_deref
              in
              linstructions @ rinstructions @ copy_instruction
              @ call_instruction
        in
        operator_instructions
    | RVCustomBinop { binop = TacBool TacDiff; _ } ->
        failwith "Todo : Deep cgange into ast "
    | s ->
        let () =
          Printf.printf "trvalue = %s\n"
            (KosuIrTAC.Asttacpprint.string_of_tac_rvalue s)
        in
        failwith
          "Todo : Redefinition of supeq and infeq => Deep change into ast"

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
          tmpreg_of_ktype rprogram
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
            (LDR
               {
                 data_size = None;
                 destination = tmpreg;
                 adress_src = Option.get intermediary_adress;
                 adress_mode = Immediat;
               })
        in
        let true_adress = create_adress tmpreg in
        let true_instructions =
          translate_tac_rvalue ~str_lit_map ~is_deref:intermediary_adress
            ~where:(Some true_adress) current_module rprogram fd trvalue
        in

        (Line_Com (Comment "Defered Start") :: instructions :: true_instructions)
        @ [ Line_Com (Comment "Defered end") ]
    | STWhile
        {
          statements_condition;
          condition;
          loop_body;
          self_label;
          inner_body_label = _;
          exit_label;
        } ->
        let label = Label self_label in
        let stmts_bool =
          statements_condition
          |> List.fold_left
               (fun acc stmt ->
                 acc
                 @ translate_tac_statement ~str_lit_map current_module rprogram
                     fd stmt)
               []
        in
        let last_reg, condition_rvalue_inst =
          translate_tac_expression ~str_lit_map rprogram fd condition
        in
        let cmp =
          Instruction (CMP { operand1 = last_reg; operand2 = `ILitteral 0L })
        in
        let jmp = Instruction (B { cc = Some EQ; label = exit_label }) in
        let if_block =
          translate_tac_body ~str_lit_map ~end_label:(Some self_label)
            current_module rprogram fd loop_body
        in
        let exit_label = Label exit_label in
        (label :: stmts_bool) @ condition_rvalue_inst @ (cmp :: jmp :: if_block)
        @ [ exit_label ]
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
                 @ translate_tac_statement ~str_lit_map current_module rprogram
                     fd stmt)
               []
        in
        let last_reg, condition_rvalue_inst =
          translate_tac_expression ~str_lit_map rprogram fd condition_rvalue
        in
        let cmp =
          Instruction (CMP { operand1 = last_reg; operand2 = `ILitteral 1L })
        in
        let jmp = Instruction (B { cc = Some EQ; label = goto1 }) in
        let jmp2 = Instruction (B { cc = None; label = goto2 }) in
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
                 acc_stmts @ insts)
               []
        in
        let last_reg, condition_switch_instruction =
          translate_tac_expression ~str_lit_map rprogram fd condition_switch
        in
        let copy_tag =
          if is_register_size (sizeofn rprogram condition_switch.expr_rktype)
          then
            Instruction
              (Mov
                 {
                   destination = tmp32reg_4;
                   flexsec_operand = `Register (to_32bits last_reg);
                 })
          else
            Instruction
              (LDR
                 {
                   data_size = None;
                   destination = tmp32reg_4;
                   adress_src = create_adress last_reg;
                   adress_mode = Immediat;
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
                              (CMP
                                 {
                                   operand1 = tmp32reg_4;
                                   operand2 = `ILitteral (Int64.of_int32 tag);
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
                                   let data_size =
                                     compute_data_size ktyte size_of_ktype
                                   in
                                   let copy_instructions =
                                     if is_register_size size_of_ktype then
                                       [
                                         Instruction
                                           (LDR
                                              {
                                                data_size;
                                                destination =
                                                  reg_of_size
                                                    (size_of_ktype_size
                                                       size_of_ktype)
                                                    tmp64reg;
                                                adress_src =
                                                  increment_adress offset_a
                                                    switch_variable_address;
                                                adress_mode = Immediat;
                                              });
                                         Instruction
                                           (STR
                                              {
                                                data_size;
                                                source =
                                                  reg_of_size
                                                    (size_of_ktype_size
                                                       size_of_ktype)
                                                    tmp64reg;
                                                adress = destination_address;
                                                adress_mode = Immediat;
                                              });
                                       ]
                                     else
                                       Instruction
                                         (ADD
                                            {
                                              destination = tmp64reg;
                                              operand1 =
                                                switch_variable_address.base;
                                              operand2 =
                                                `ILitteral
                                                  (Int64.add
                                                     switch_variable_address
                                                       .offset offset_a);
                                              offset = false;
                                            })
                                       :: copy_from_reg tmp64reg
                                            destination_address ktyte rprogram
                                   in

                                   copy_instructions)
                            |> List.flatten
                          in
                          let jump_true =
                            Instruction
                              (B { cc = Some EQ; label = sw_case.sw_goto })
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
          |> Option.map (fun lab -> Instruction (B { cc = None; label = lab }))
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
                 let cmp =
                   Instruction
                     (CMP { operand1 = last_reg; operand2 = `ILitteral 1L })
                 in
                 let if_true_instruction =
                   Instruction (B { cc = Some EQ; label = scases.goto })
                 in
                 let if_false_instruction =
                   Instruction (B { cc = None; label = scases.jmp_false })
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
          Instruction (B { cc = None; label = else_tac_body.label })
        in
        let end_label_instruction = Label exit_label in
        let else_body_instruction =
          translate_tac_body ~str_lit_map ~end_label:(Some exit_label)
            current_module rprogram fd else_tac_body
        in

        cases_condition @ cases_body @ else_body_instruction
        @ [ end_label_instruction ]

  and translate_tac_body ~str_lit_map ?(end_label = None) current_module
      rprogram (fd : FrameManager.frame_desc) { label; body } =
    let label_instr = Label label in
    let () = Printf.printf "Lable = %s\n%!" label in
    let stmt_instr =
      body |> fst
      |> List.map (fun stmt ->
        let () = Printf.printf "%s\n%!" (KosuIrTAC.Asttacpprint.string_of_tac_statement stmt) in
             translate_tac_statement ~str_lit_map current_module rprogram fd
               stmt)
      |> List.flatten
    in
    let end_label_inst =
      end_label
      |> Option.map (fun lab -> Instruction (B { cc = None; label = lab }))
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
               return_register_ktype
                 ~float:
                   (KosuIrTyped.Asttyhelper.RType.is_64bits_float
                      tte.expr_rktype)
                 sizeof
             in
             instructions
             @
             if is_register_size sizeof then
               Instruction
                 (Mov
                    {
                      destination = return_reg;
                      flexsec_operand = `Register last_reg;
                    })
               :: []
             else
               let x8_address =
                 Option.get @@ FrameManager.(address_of indirect_return_vt fd)
               in
               let str =
                 Instruction
                   (LDR
                      {
                        data_size = None;
                        destination = xr;
                        adress_src = x8_address;
                        adress_mode = Immediat;
                      })
               in

               str :: copy_large (create_adress xr) last_reg sizeof)
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
                            if index < register_param_count then
                              Either.left value
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
                   (* let () = locals_var
                        |> List.map (fun (s, kt) ->
                            Printf.sprintf "%s : %s " (s) (KosuIrTyped.Asttypprint.string_of_rktype kt)
                        )
                        |> String.concat ", "
                        |> Printf.printf "%s : locale variables = [%s]\n"
                        function_decl.rfn_name
                      in *)
                   let asm_name =
                     AsmSpec.label_of_tac_function ~module_path:current_module
                       function_decl
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
                     translate_tac_body ~str_lit_map current_module rprogram fd
                       function_decl.tac_body
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
                          is_global = true;
                          asm_body =
                            [ Directive "cfi_startproc" ]
                            @ prologue @ (conversion |> List.tl) @ epilogue
                            @ [ Directive "cfi_endproc" ];
                        })
                | TNClosure closure_fn_decl -> 
                  let struct_of_env = RTNamedTuple closure_fn_decl.rclosure_env in
                  let register_param_count = List.length argument_registers in
                  let fn_register_params, stack_param =
                    closure_fn_decl.rparameters
                    |> List.mapi (Util.couple)
                    |> List.partition_map (fun (index, value) ->
                          if index < register_param_count then
                            Either.left (value)
                          else Either.right value)
                  in
                let fn_register_params, stack_param = 
                  if (List.length fn_register_params < register_param_count ) then
                    fn_register_params @ [(FrameManager.env_closure_name, struct_of_env)], stack_param
                  else
                    fn_register_params, stack_param @ [(FrameManager.env_closure_name, struct_of_env)]
                  in

                  let stack_param_count =
                    Int64.of_int (closure_fn_decl.stack_params_count * 8)
                  in
                  let locals_var =
                    closure_fn_decl.locale_var
                    |> List.map (fun { locale_ty; locale } ->
                           match locale with
                           | Locale s -> (s, locale_ty)
                           | Enum_Assoc_id { name; _ } -> (name, locale_ty))
                  in
                   (* let () = fn_register_params
                    |> List.map (fun (s, kt) ->
                        Printf.sprintf "%s : %s\n" (s) (KosuIrTyped.Asttypprint.string_of_rktype kt))
                        |> String.concat ", "
                        |> Printf.printf "%s : locale variables = [%s]\n"
                        closure_fn_decl.rclosure_name
                    in *)

                  let asm_name = AsmSpec.label_of_constant closure_fn_decl.rclosure_name in
                  let fd =
                    FrameManager.frame_descriptor
                      ~stack_future_call:stack_param_count ~fn_register_params
                      ~stack_param ~return_type:closure_fn_decl.return_type
                      ~locals_var
                      ~discarded_values:closure_fn_decl.discarded_values rprogram
                  in

                  let prologue =
                    FrameManager.function_prologue
                      ~fn_register_params
                      ~stack_params:stack_param rprogram fd
                  in
                  let conversion =
                    translate_tac_body ~str_lit_map current_module rprogram fd
                    closure_fn_decl.tac_body
                  in
                  let epilogue = FrameManager.function_epilogue fd in
                  let map = KosuIrTyped.Asttyhelper.Sizeof.map_size |> KosuIrTyped.Asttyhelper.Sizeof.KtypeHashTbl.to_seq |> List.of_seq in
                  let () = map |> List.map (fun (kt, size) -> 
                    Printf.sprintf "%s => %Lu" (KosuIrTyped.Asttypprint.string_of_rktype kt) size
                  ) |> String.concat "\n" |> Printf.printf "\n\n[%s]\n" in
                    Some
                    (Afunction
                      {
                        asm_name;
                        is_global = false;
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
                   let asm_name =
                     AsmSpec.label_of_tac_operator ~module_path:current_module
                       self
                   in
                   let fd =
                     FrameManager.frame_descriptor
                       ~stack_future_call:stack_param_count
                       ~fn_register_params:[ unary_decl.rfield ] ~stack_param:[]
                       ~return_type:unary_decl.return_type ~locals_var
                       ~discarded_values:unary_decl.discarded_values rprogram
                   in
                   let prologue =
                     FrameManager.function_prologue
                       ~fn_register_params:[ unary_decl.rfield ]
                       ~stack_params:[] rprogram fd
                   in
                   let conversion =
                     translate_tac_body ~str_lit_map current_module rprogram fd
                       (KosuIrTAC.Asttachelper.OperatorDeclaration.tac_body self)
                   in
                   let epilogue = FrameManager.function_epilogue fd in
                   Some
                     (AsmProgram.Afunction
                        {
                          asm_name;
                          is_global = true;
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
                   (* let () = locals_var
                      |> List.map (fun (s, kt) ->
                          Printf.sprintf "%s : %s\n" (s) (KosuIrTyped.Asttypprint.string_of_rktype kt))
                          |> String.concat ", "
                          |> Printf.printf "%s : locale variables = [%s]\n"
                          binary.asm_name
                      in *)
                   let asm_name =
                     AsmSpec.label_of_tac_operator ~module_path:current_module
                       self
                   in
                   let lhs_param, rhs_param = binary.rfields in
                   let fn_register_params = [ lhs_param; rhs_param ] in
                   let fd =
                     FrameManager.frame_descriptor
                       ~stack_future_call:stack_param_count ~fn_register_params
                       ~stack_param:[] ~return_type:binary.return_type
                       ~locals_var ~discarded_values:binary.discarded_values
                       rprogram
                   in
                   let prologue =
                     FrameManager.function_prologue ~fn_register_params
                       ~stack_params:[] rprogram fd
                   in
                   let conversion =
                     translate_tac_body ~str_lit_map current_module rprogram fd
                       (KosuIrTAC.Asttachelper.OperatorDeclaration.tac_body self)
                   in
                   let epilogue = FrameManager.function_epilogue fd in
                   Some
                     (Afunction
                        {
                          asm_name;
                          is_global = true;
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
                            AsmSpec.label_of_constant
                              ~module_path:current_module rconst_name;
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
                            AsmSpec.label_of_constant
                              ~module_path:current_module rconst_name;
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
                            AsmSpec.label_of_constant
                              ~module_path:current_module rconst_name;
                          value = `StrVal s;
                        })
               | TNEnum _ | TNStruct _ | TNSyscall _ | TNExternFunc _ | _ ->
                   None)

  let asm_module_path_of_tac_module_path ~str_lit_map rprogram
      { path; tac_module } =
    {
      apath = path;
      asm_module =
        AsmModule
          (asm_module_of_tac_module ~str_lit_map path rprogram tac_module);
    }

  let asm_program_of_tac_program ~(start: string option) tac_program = ignore start;
    tac_program
    |> List.map (fun ({ filename; tac_module_path; rprogram } as named) ->
           let str_lit_map =
             map_string_litteral_of_named_rmodule_path named ()
           in
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

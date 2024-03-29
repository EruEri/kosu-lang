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

module IdVar = KosuCommon.IdVar
module IdVarMap = KosuCommon.IdVarMap

module NamingConvention = KosuCommon.NamingConvention.Make (struct
  let label_prefix = ""
  let main = "main"
end)

module Immediat = struct
  module VmConst = struct
    let mv_immediat_size = 21
    let mva_immediat_size = 19
    let str_immediat_size = 13
    let binop_immediat_size = 16
    let ldr_immediat_size = str_immediat_size
  end

  let mask_6_8bytes = 0xFFFF_0000_0000_0000L
  let mask_4_6bytes = 0x0000_FFFF_0000_0000L
  let mask_2_4bytes = 0x0000_0000_FFFF_0000L
  let mask_0_2bytes = 0x0000_0000_0000_FFFFL
  let max_16bits_immediat = 65535L
  let min_16bits_immediat = -65535L

  let max_nbits_immediat n =
    let tmp = Int64.shift_left 1L (n - 1) in
    Int64.sub tmp 1L

  let min_nbits_immediat n =
    let tmp = Int64.shift_left 1L (n - 1) in
    Int64.neg tmp

  let is_encodable nbits value =
    let max = max_nbits_immediat nbits in
    let min = min_nbits_immediat nbits in
    value >= min && value <= max

  let is_direct_immediat int16 =
    int16 >= min_16bits_immediat && int16 <= max_16bits_immediat

  let split n =
    let int16 = Int64.logand mask_0_2bytes n in
    let int32 = Int64.logand mask_2_4bytes n in
    let int48 = Int64.logand mask_4_6bytes n in
    let int64 = Int64.logand mask_6_8bytes n in
    ( Int64.shift_right_logical int64 48,
      Int64.shift_right_logical int48 32,
      Int64.shift_right_logical int32 16,
      int16
    )
end

module ConditionCode = struct
  type shift = SH0 | SH16 | SH32 | SH48
  type data_size = SIZE_8 | SIZE_16 | SIZE_32 | SIZE_64

  type condition_code =
    | ALWAYS
    | EQUAL
    | DIFF
    | SUP
    | UNSIGNED_SUP
    | SUPEQ
    | UNSIGNED_SUPEQ
    | INF
    | UNSIGNED_INF
    | INFEQ
    | UNSIGNED_INFEQ

  let data_size_of_kt kt =
    let size = KosuIrTyped.Sizeof.sizeof_kt kt in
    match size with
    | 1L ->
        SIZE_8
    | 2L ->
        SIZE_16
    | 4L ->
        SIZE_32
    | _ ->
        SIZE_64

  let cc_of_tac_bin ?(is_unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacOr | TacAnd ->
        None
    | TacEqual ->
        Some EQUAL
    | TacDiff ->
        Some DIFF
    | TacSup ->
        Some
          ( if is_unsigned then
              UNSIGNED_SUP
            else
              SUP
          )
    | TacSupEq ->
        Some
          ( if is_unsigned then
              UNSIGNED_SUPEQ
            else
              SUPEQ
          )
    | TacInfEq ->
        Some
          ( if is_unsigned then
              UNSIGNED_INFEQ
            else
              INFEQ
          )
    | TacInf ->
        Some
          ( if is_unsigned then
              UNSIGNED_INF
            else
              INF
          )
end

module Register = struct
  type variable = string * KosuIrTyped.Asttyped.rktype

  type register =
    | R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    (* Float register*)
    | FR0
    | FR1
    | FR2
    | FR3
    | FR4
    | FR5
    | FR6
    | FR7
    | FR8
    | FR9
    | FR10
    (* Indirect return register *)
    | IR
    (* Syscall code register*)
    | SC
    (* Frame pointer register*)
    | FP
    (* Return Address register *)
    | RAP
    (* Stack pointer register*)
    | SP

  type t = register

  type return_strategy =
    | Indirect_return
    | Simple_return of t
    | Splitted_return of t * t

  let compare = Stdlib.compare

  let is_float_register = function
    | FR0 | FR1 | FR2 | FR3 | FR4 | FR5 | FR6 | FR7 | FR8 | FR9 | FR10 ->
        true
    | _ ->
        false

  let caller_saved_register =
    [
      R0;
      R1;
      R2;
      R3;
      R4;
      R5;
      R6;
      R7;
      R8;
      R9;
      R10;
      R11;
      R12;
      R13;
      R14;
      FR0;
      FR1;
      FR2;
      FR3;
      FR4;
      FR5;
      FR6;
      FR7;
      FR8;
      FR9;
      FR10;
      IR;
      SC;
    ]

  let callee_saved_register = [ FP; SP; RAP ]
  let non_float_argument_registers = [ R0; R1; R2; R3; R4; R5; R6; R7 ]
  let float_argument_registers = [ FR0; FR1; FR2; FR3; FR4; FR5; FR6; FR7 ]

  let arguments_register variable =
    if KosuIrTyped.Asttyhelper.RType.is_float @@ snd variable then
      float_argument_registers
    else
      non_float_argument_registers

  let syscall_register = [ R0; R1; R2; R3; R4; R5 ]
  let available_register = [ R8; R9; R10; R11; FR0; FR1; FR2 ]
  let indirect_return_register = IR

  let color_map =
    [
      (R0, "aqua");
      (R1, "red");
      (R2, "fuchsia");
      (R3, "green");
      (R4, "navyblue");
      (R5, "pink");
      (R6, "orange");
      (R7, "yellow");
      (R8, "hotpink");
      (R9, "indigo");
      (R10, "magenta");
      (R11, "purple");
      (R12, "cyan");
    ]

  let temporary_register = [ R12; R13; R14; IR ]

  let tmp_register ~exclude =
    temporary_register |> List.filter (( <> ) exclude) |> List.hd

  let is_valid_register (variable : variable) (register : t) =
    let _, rktype = variable in
    let is_float = KosuIrTyped.Asttyhelper.RType.is_float rktype in
    let is_float_register = is_float_register register in
    is_float = is_float_register
  (* float == float || other == other *)

  let does_return_hold_in_register_kt kt =
    match KosuIrTyped.Sizeof.sizeof_kt kt with
    | 1L | 2L | 4L | 8L ->
        true
    | _ ->
        false

  let does_return_hold_in_register variable =
    does_return_hold_in_register_kt @@ snd variable

  let return_strategy variable =
    match does_return_hold_in_register variable with
    | true ->
        Simple_return R0
    | false ->
        Indirect_return

  let r0 = R0
  let r1 = R1
  let f8 = FR8
  let f9 = FR9
  let r12 = R12
  let r13 = R13
  let r14 = R14
  let sp = SP
  let ir = IR
  let sc = SC
  let rap = RAP
  let fp = FP
end

module Location = struct
  type address_offset = [ `ILitteral of int64 | `Register of Register.register ]
  type address = { base : Register.register; offset : address_offset }
  type location = LocAddr of address

  type addressage =
    | Addr_Direct of address option
    | Addr_Indirect of { address : address; offset : int64 }

  let addr_direct a = Addr_Direct (Option.some a)
  let adrr_direct_raw a = Addr_Direct a
  let addr_indirect ?(offset = 0L) address = Addr_Indirect { address; offset }
  let loc_addr a = LocAddr a
  let create_address ?(offset = 0L) base = { base; offset = `ILitteral offset }
  let address_register base offset = { base; offset = `Register offset }

  let increment_adress off adress =
    match adress.offset with
    | `ILitteral offset ->
        { adress with offset = `ILitteral (Int64.add offset off) }
    | `Register _reg ->
        failwith "Increment register based address"

  let increment_addressage by = function
    | Addr_Direct address ->
        Addr_Direct (address |> Option.map @@ increment_adress by)
    | Addr_Indirect { address; offset } ->
        Addr_Indirect { address; offset = Int64.add offset by }

  let increment_location off = function
    | address ->
        loc_addr @@ increment_adress off address

  let get_address = function address -> address
end

module Operande = struct
  type src = [ `ILitteral of int64 | `Register of Register.register ]
  type jump_src = [ `Label of string | `Register of Register.register ]
  type dst = Register.register
  type single_operande = { destination : dst; source : src }

  type bin_op_operande = {
    destination : Register.register;
    operande1 : Register.register;
    operande2 : src;
  }

  type lea_operande = LeaPcRel of string | LeaRegAbs of Location.address

  let ilitteral n = (`ILitteral n :> src)
  let iregister reg : src = `Register reg
  let lea_pc_relatif s = LeaPcRel s
  let lea_reg_abs address = LeaRegAbs address
end

module Instruction = struct
  open ConditionCode
  open Operande

  (* open Register *)
  open Location

  type bc_symbol = BclocalSymbol of string | BcGlobalSymbol of string

  type bc_args =
    | BcValue of int64
    | BcAddress of address
    | BcPcRel of bc_symbol

  type t =
    | Halt
    | Ret
    | Syscall
    | CCall of bc_args KosuVirtualMachine.FFIType.ccall_entry
    | Mvnt of single_operande
    | Mvng of single_operande
    | Mv of single_operande
    | Mva of { operandes : single_operande; shift : shift }
    | Jump of jump_src
    | Br of jump_src
    | Lea of { destination : Register.register; operande : lea_operande }
    | Add of bin_op_operande
    | Sub of bin_op_operande
    | Mult of bin_op_operande
    | Div of { operandes : bin_op_operande; signed : bool }
    | Mod of { operandes : bin_op_operande; signed : bool }
    | And of bin_op_operande
    | Or of bin_op_operande
    | Xor of bin_op_operande
    | Lsl of bin_op_operande
    | Asr of bin_op_operande
    | Lsr of bin_op_operande
    | Cmp of {
        cc : condition_code;
        lhs : Register.register;
        rhs : Register.register;
      }
    | Cset of {
        cc : condition_code;
        destination : Register.register;
        lhs : Register.register;
        rhs : Register.register;
        update_last_cmp : bool;
      }
    | Ldr of {
        data_size : data_size;
        destination : Register.register;
        address : address;
      }
    | Str of {
        data_size : data_size;
        destination : Register.register;
        address : address;
      }
    | Itof of {
        data_size : data_size;
        destination : Register.register;
        source : Register.register;
        signed : bool;
      }
    | Ftoi of {
        data_size : data_size;
        destination : Register.register;
        source : Register.register;
        signed : bool;
      }

  let halt = Halt
  let ret = Ret
  let syscall = Syscall
  let mvnt destination source = Mvnt { destination; source }
  let mvng destination source = Mvng { destination; source }
  let mv destination source = Mv { destination; source }

  let mva destination source shift =
    Mva { operandes = { destination; source }; shift }

  let iand destination operande1 operande2 =
    And { destination; operande1; operande2 }

  let ior destination operande1 operande2 =
    Or { destination; operande1; operande2 }

  let ixor destination operande1 operande2 =
    Xor { destination; operande1; operande2 }

  let imul destination operande1 operande2 =
    Mult { destination; operande1; operande2 }

  let ishiftleft destination operande1 operande2 =
    Lsl { destination; operande1; operande2 }

  let iashiftright destination operande1 operande2 =
    Asr { destination; operande1; operande2 }

  let ilshiftright destination operande1 operande2 =
    Lsr { destination; operande1; operande2 }

  let jump src = Jump src
  let br src = Br src
  let lea destination operande = Lea { destination; operande }

  let ccall function_name arity args ty_args ty_return =
    CCall { function_name; arity; args; ty_args; ty_return }

  let sub destination operande1 operande2 =
    Sub { destination; operande1; operande2 }

  let add destination operande1 operande2 =
    Add { destination; operande1; operande2 }

  let cmp cc lhs rhs = Cmp { cc; lhs; rhs }

  let icset cc destination lhs rhs update =
    Cset { cc; destination; lhs; rhs; update_last_cmp = update }

  let idiv signed destination lhs rhs =
    Div
      { operandes = { destination; operande1 = lhs; operande2 = rhs }; signed }

  let imod signed destination lhs rhs =
    Mod
      { operandes = { destination; operande1 = lhs; operande2 = rhs }; signed }

  let str data_size destination address =
    Str { data_size; destination; address }

  let ldr data_size destination address =
    Ldr { data_size; destination; address }

  let ftoi signed data_size destination source =
    Ftoi { signed; data_size; destination; source }

  let itof signed data_size destination source =
    Itof { signed; data_size; destination; source }
end

module Line = struct
  type raw_line =
    | Instruction of Instruction.t
    | Comment of string
    | Label of string

  type line = AsmLine of raw_line * string option

  let instruction ?comment instr = AsmLine (Instruction instr, comment)
  let instructions instrs = instrs |> List.map instruction
  let sinstruction instruction = instructions [ instruction ]
  let comment message = AsmLine (Comment message, None)
  let label ?comment l = AsmLine (Label l, comment)
end

module BytecodeProgram = KosuCommon.AsmProgram (Line)

module CType = struct
  open KosuVirtualMachine.FFIType
  open KosuIrTyped.Asttyped

  let rec ffi_type_of_ktype rprogram = function
    | RTBool | RTUnit | RTUnknow | RTChar | RTInteger (Unsigned, I8) ->
        FFI_U8
    | RTOrdered | RTInteger (Signed, I8) | RTInteger (Signed, I16) ->
        FFI_S16
    | RTInteger (Unsigned, I16) ->
        FFI_U16
    | RTInteger (Signed, I32) ->
        FFI_S32
    | RTInteger (Unsigned, I32) ->
        FFI_U32
    | RTInteger (Signed, I64) ->
        FFI_S64
    | RTInteger (Unsigned, I64) ->
        FFI_U64
    | RTFloat F32 ->
        FFI_F32
    | RTFloat F64 ->
        FFI_F64
    | RTString_lit | RTPointer _ | RTFunction _ | RTOpaque _ ->
        FFI_Pointer
    | RTArray { size; rktype } ->
        let ktype = ffi_type_of_ktype rprogram rktype in
        let types = List.init (Int64.to_int size) (fun _ -> ktype) in
        FFI_Struct types
    | RTTuple types ->
        let types = types |> List.map @@ ffi_type_of_ktype rprogram in
        FFI_Struct types
    | ( RTType_Identifier { module_path = _; name = _ }
      | RTParametric_identifier _ ) as kt ->
        ffi_of_type_identifier ~ktype:kt rprogram

  and ffi_of_type_identifier ~ktype rprogram =
    match
      KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye ktype rprogram
    with
    | Some (RDecl_Struct s) ->
        ffi_of_struct ~ktype rprogram s
    | Some (RDecl_Enum _) ->
        FFI_U32
    | None ->
        failwith "Non type decl ??? my validation is very weak"

  and ffi_of_struct ~ktype rprogram struct_decl =
    let types =
      ktype |> KosuIrTyped.Asttyhelper.RType.extract_parametrics_rktype
      |> List.combine struct_decl.generics
      |> List.map @@ fun (_, kt) -> ffi_type_of_ktype rprogram kt
    in
    FFI_Struct types
end

module LineInstruction = struct
  open Instruction
  open Location
  open Line
  open Immediat

  let mv_integer register n =
    let open Immediat in
    if is_encodable Immediat.VmConst.mv_immediat_size n then
      instructions @@ [ mv register @@ Operande.ilitteral n ]
    else
      let int64, int48, int32, int16 = split n in
      let mvs = [ mv register @@ Operande.ilitteral int16 ] in
      let mvs =
        if int32 = 0L then
          mvs
        else
          mva register (Operande.ilitteral int32) ConditionCode.SH16 :: mvs
      in
      let mvs =
        if int48 = 0L then
          mvs
        else
          mva register (Operande.ilitteral int48) ConditionCode.SH32 :: mvs
      in
      let mvs =
        if int64 = 0L then
          mvs
        else
          mva register (Operande.ilitteral int64) ConditionCode.SH48 :: mvs
      in
      instructions @@ List.rev mvs

  let and_or_instruction =
    let open KosuIrTAC.Asttac in
    function
    | TacAnd ->
        Instruction.iand
    | TacOr ->
        Instruction.ior
    | _ ->
        failwith "Should be and or or"

  let smv register src =
    match src with
    | `Register _ ->
        sinstruction @@ mv register src
    | `ILitteral n ->
        mv_integer register n

  let lea_label destination ?module_path label =
    let label =
      module_path
      |> Option.map (fun mp -> NamingConvention.const_label_format mp label)
      |> Option.value ~default:label
    in
    sinstruction @@ lea destination @@ Operande.lea_pc_relatif label

  let ssub destination source (operande : Operande.src) =
    match operande with
    | `Register _ ->
        sinstruction @@ sub destination source operande
    | `ILitteral n when is_encodable VmConst.binop_immediat_size n ->
        sinstruction @@ sub destination source operande
    | `ILitteral n ->
        let large_mov = mv_integer Register.r14 n in
        let sub_i =
          instruction @@ sub destination source
          @@ Operande.iregister Register.r14
        in
        large_mov @ [ sub_i ]

  let sadd destination source (operande : Operande.src) =
    match operande with
    | `Register _ ->
        sinstruction @@ add destination source operande
    | `ILitteral n when is_encodable VmConst.binop_immediat_size n ->
        sinstruction @@ add destination source operande
    | `ILitteral n ->
        let large_mov = mv_integer Register.r14 n in
        let add_i =
          instruction @@ add destination source
          @@ Operande.iregister Register.r14
        in
        large_mov @ [ add_i ]

  let sstr data_size destination address =
    match address.offset with
    | `ILitteral n -> (
        match n with
        | n when is_encodable VmConst.str_immediat_size n ->
            instructions [ str data_size destination address ]
        | n ->
            let mov_int = mv_integer Register.r14 n in
            let address = Location.address_register address.base Register.r14 in
            let str_i = instruction @@ str data_size destination address in
            mov_int @ [ str_i ]
      )
    | `Register _ ->
        instructions [ str data_size destination address ]

  let sldr data_size destination address =
    match address.offset with
    | `ILitteral n -> (
        match n with
        | n when is_encodable VmConst.ldr_immediat_size n ->
            sinstruction @@ ldr data_size destination address
        | n ->
            let mov_int = mv_integer Register.r14 n in
            let address = Location.address_register address.base Register.r14 in
            let ldr_i = instruction @@ ldr data_size destination address in
            mov_int @ [ ldr_i ]
      )
    | `Register _ ->
        instructions [ ldr data_size destination address ]

  let simple_copy register (address : address) ktype =
    let data_size = ConditionCode.data_size_of_kt ktype in
    sstr data_size register address

  let rec large_copy_size ~increment register address size =
    if size < 0L then
      failwith "negative size to copy"
    else if size = 0L then
      []
    else
      let (data_size, offset) : ConditionCode.data_size * int64 =
        let open ConditionCode in
        if size = 0L then
          (SIZE_8, 0L)
        else if 1L <= size && size < 2L then
          (SIZE_8, 1L)
        else if 2L <= size && size < 4L then
          (SIZE_16, 2L)
        else if 4L <= size && size < 8L then
          (SIZE_32, 4L)
        else
          (SIZE_64, 8L)
      in
      let register_address = create_address ?offset:increment register in

      let ldr_instructions = sldr data_size Register.r14 register_address in
      let str_instructions = sstr data_size Register.r14 address in
      let next_adresss_store = increment_adress offset address in

      let next_size = Int64.sub size offset in
      let next_instructions =
        large_copy_size ~increment:(Some offset) register next_adresss_store
          next_size
      in

      ldr_instructions @ str_instructions @ next_instructions

  let large_copy register address kt =
    large_copy_size ~increment:None register address
    @@ KosuIrTyped.Sizeof.sizeof_kt kt

  let scopy register (address : address) ktype =
    match Register.does_return_hold_in_register_kt ktype with
    | true ->
        simple_copy register address ktype
    | false ->
        large_copy register address ktype

  let sbr_label label = instruction @@ br @@ `Label label
  let sbr_reg reg = instruction @@ br @@ `Register reg
  let sjump_label label = instruction @@ jump @@ `Label label

  let sbr_label_always label =
    [ instruction @@ cmp ALWAYS Register.r0 Register.r0; sbr_label label ]

  let sjump_always label =
    [ instruction @@ cmp ALWAYS Register.r0 Register.r0; sjump_label label ]

  let slea_address target address =
    sadd target address.Location.base address.Location.offset

  let smv_location ~lea ~data_size destination = function
    | LocAddr address ->
        if lea then
          slea_address destination address
        else
          sldr data_size destination address

  let sccall ~extra_types rprogram args
      (external_function : KosuIrTyped.Asttyped.rexternal_func_decl) =
    let func_name =
      external_function.c_name
      |> Option.value ~default:external_function.rsig_name
    in
    let arity = Int64.of_int @@ List.length external_function.fn_parameters in
    let params_type =
      external_function.fn_parameters
      |> List.map @@ CType.ffi_type_of_ktype rprogram
    in
    let extra_types = List.map (CType.ffi_type_of_ktype rprogram) extra_types in
    let params_type = params_type @ extra_types in
    let return_type =
      CType.ffi_type_of_ktype rprogram external_function.return_type
    in
    Line.instruction ~comment:"CCall"
    @@ Instruction.ccall func_name arity args params_type return_type
end

module FrameManager = struct
  type description = {
    (* need to save indirect return register*)
    need_ir : bool;
    local_space : int64;
    discarded_values : Register.variable list;
    variable_map : Location.address KosuCommon.IdVarMap.t;
  }

  let indirect_return_var = "@xreturn"
  let indirect_return_type = KosuIrTyped.Asttyped.(RTPointer RTUnknow)
  let indirect_return_vt = (indirect_return_var, indirect_return_type)

  let variable_of_tac_locale_variable =
    let open KosuIrTAC.Asttac in
    fun { locale_ty; locale } ->
      match locale with
      | Locale s ->
          (s, locale_ty)
      | Enum_Assoc_id { name; _ } ->
          (name, locale_ty)

  let location_of variable fd =
    if List.mem variable fd.discarded_values then
      None
    else
      match IdVarMap.find_opt variable fd.variable_map with
      | None ->
          Printf.sprintf "location of: %s failed" (fst variable) |> failwith
      | Some _ as loc ->
          loc

  let stack_args_passed_in_function rprogram
      (fn_info_calls : KosuIrTyped.Asttyped.function_call_infos) =
    let open KosuIrTyped.Asttyped in
    let open Util.Args in
    fn_info_calls
    |> List.fold_left
         (fun acc { varia_index; parameters; return_type = _ } ->
           let fpstyle kt =
             if KosuIrTyped.Asttyhelper.RType.is_float kt then
               Simple_Reg Float
             else
               Simple_Reg Other
           in

           let _, _, stack_args, variadic_args =
             consume_args ?novariadic_args:varia_index
               ~fregs:Register.float_argument_registers
               ~iregs:Register.non_float_argument_registers ~fpstyle parameters
           in

           let stack_args_size =
             stack_args |> KosuIrTyped.Asttyhelper.RType.rtuple
             |> KosuIrTyped.Sizeof.sizeof rprogram
           in

           let variadic_size =
             variadic_args
             |> List.map (fun kt ->
                    KosuIrTyped.(
                      Sizeof.align_8 @@ KosuIrTyped.Sizeof.sizeof rprogram kt
                    )
                )
             |> List.fold_left Int64.add 0L
           in

           max (Int64.add stack_args_size variadic_size) acc
         )
         0L

  let frame_descriptor rprogram
      (function_decl : KosuIrTAC.Asttac.tac_function_decl) =
    let open KosuIrTAC.Asttac in
    let open Util.Args in
    let iparas, fparams, stack_parameters, _ =
      Util.Args.consume_args ~fregs:Register.float_argument_registers
        ~iregs:Register.non_float_argument_registers
        ~fpstyle:(fun (_, kt) ->
          if KosuIrTyped.Asttyhelper.RType.is_float kt then
            Simple_Reg Float
          else
            Simple_Reg Other
        )
        function_decl.rparameters
    in

    let parameters =
      iparas |> ( @ ) fparams
      |> List.map (fun (variable, return_kind) ->
             match return_kind with
             | Simple_return reg ->
                 (variable, reg)
             | Double_return _ ->
                 failwith "Unreachable"
         )
    in

    let reg_parameters, _ = List.split parameters in

    let locale_variables =
      function_decl.locale_var
      |> List.sort (fun lhs rhs ->
             let lsize = KosuIrTyped.Sizeof.sizeof_kt lhs.locale_ty in
             let rsize = KosuIrTyped.Sizeof.sizeof_kt rhs.locale_ty in
             compare rsize lsize
         )
      |> List.map
           KosuIrTAC.Asttachelper.LocaleVariable.variable_of_tac_locale_variable
    in
    let need_ir =
      not @@ Register.does_return_hold_in_register_kt function_decl.return_type
    in

    let stack_concat = reg_parameters @ locale_variables in
    let stack_concat =
      if need_ir then
        indirect_return_vt :: stack_concat
      else
        stack_concat
    in
    let fake_tuple = List.map snd stack_concat in
    let locals_space =
      fake_tuple |> KosuIrTyped.Asttyhelper.RType.rtuple
      |> KosuIrTyped.Sizeof.sizeof rprogram
    in
    let stack_future_call =
      stack_args_passed_in_function rprogram function_decl.fn_call_infos
    in

    let locals_space = Int64.add locals_space stack_future_call in

    let map =
      stack_concat
      |> List.mapi (fun index value -> (index, value))
      |> List.fold_left
           (fun acc (index, st) ->
             let offset =
               KosuIrTyped.Sizeof.offset_of_tuple_index index fake_tuple
                 rprogram
             in
             let fp_relative_address =
               locals_space |> Int64.neg |> Int64.add offset
             in
             let address =
               Location.create_address ~offset:fp_relative_address Register.fp
             in
             (* let () = Printf.printf "-> %s : %s == [x29, %Ld] \n" (fst st) (KosuIrTyped.Asttypprint.string_of_rktype @@ snd @@ st) (offset) in *)
             IdVarMap.add st address acc
           )
           IdVarMap.empty
    in
    let stack_args_rktype = List.map snd stack_parameters in

    let stack_fp_offset = 16L in
    let map =
      stack_parameters |> List.mapi Util.couple
      |> List.fold_left
           (fun acc (index, st) ->
             let offset =
               KosuIrTyped.Sizeof.offset_of_tuple_index index stack_args_rktype
                 rprogram
             in
             let offset = Int64.add stack_fp_offset offset in
             let address = Location.create_address ~offset Register.fp in
             IdVarMap.add st address acc
           )
           map
    in
    {
      need_ir;
      local_space = locals_space;
      variable_map = map;
      discarded_values = function_decl.discarded_values;
    }

  let prologue (function_decl : KosuIrTAC.Asttac.tac_function_decl) fd =
    let open Util.Args in
    let ( ++ ) = Int64.add in
    let ( -- ) = Int64.sub in
    let stack_sub_size = KosuIrTyped.Sizeof.align_16 (16L ++ fd.local_space) in
    let variable_frame = stack_sub_size -- 16L in
    let fp_address =
      Location.create_address ~offset:variable_frame Register.sp
    in
    let rap_address =
      Location.create_address ~offset:(variable_frame ++ 8L) Register.sp
    in
    let str_fp =
      LineInstruction.sstr ConditionCode.SIZE_64 Register.fp fp_address
    in
    let str_rap =
      LineInstruction.sstr ConditionCode.SIZE_64 Register.rap rap_address
    in
    let sub_sp_instructions =
      LineInstruction.ssub Register.sp Register.sp
      @@ Operande.ilitteral stack_sub_size
    in
    let align_fp_instructions =
      LineInstruction.sadd Register.fp Register.sp
      @@ Operande.ilitteral variable_frame
    in

    let iparas, fparams, _stack_parameters, _ =
      Util.Args.consume_args ~fregs:Register.float_argument_registers
        ~iregs:Register.non_float_argument_registers
        ~fpstyle:(fun (_, kt) ->
          if KosuIrTyped.Asttyhelper.RType.is_float kt then
            Simple_Reg Float
          else
            Simple_Reg Other
        )
        function_decl.rparameters
    in
    let store_value_instructions =
      iparas |> ( @ ) fparams
      |> List.filter_map (fun (variable, return_kind) ->
             match return_kind with
             | Double_return _ ->
                 failwith "Unreachable"
             | Simple_return reg -> (
                 match location_of variable fd with
                 | Some address ->
                     Some (variable, reg, address)
                 | None ->
                     failwith "From register setup null address"
               )
         )
      |> List.map (fun (variable, reg, address) ->
             LineInstruction.scopy reg address @@ snd variable
         )
      |> List.flatten
    in
    sub_sp_instructions @ str_rap @ str_fp @ align_fp_instructions
    @ store_value_instructions

  let epilogue ?(halt = false) fd =
    let open Line in
    let ( ++ ) = Int64.add in
    let ( -- ) = Int64.sub in
    let stack_spaces = KosuIrTyped.Sizeof.align_16 (16L ++ fd.local_space) in
    let variable_frame = stack_spaces -- 16L in
    let fp_address =
      Location.create_address ~offset:variable_frame Register.sp
    in
    let rap_address =
      Location.create_address ~offset:(variable_frame ++ 8L) Register.sp
    in

    let ldr_fp_instructions =
      LineInstruction.sldr ConditionCode.SIZE_64 Register.fp fp_address
    in
    let ldr_rad_instructions =
      LineInstruction.sldr ConditionCode.SIZE_64 Register.rap rap_address
    in
    let add_sp_instructions =
      LineInstruction.sadd Register.sp Register.sp
      @@ Operande.ilitteral stack_spaces
    in
    let return_instruction =
      sinstruction
      @@ match halt with true -> Instruction.halt | false -> Instruction.ret
    in
    ldr_rad_instructions @ ldr_fp_instructions @ add_sp_instructions
    @ return_instruction
end

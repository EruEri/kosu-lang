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
      int16 )
end

module ConditionCode = struct
  type shift = SH0 | SH16 | SH32 | SH48

  type data_size = 
    | SIZE_8 
    | SIZE_16
    | SIZE_32
    | SIZE_64

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
    | 1L -> SIZE_8
    | 2L -> SIZE_16
    | 4L -> SIZE_32
    | _ -> SIZE_64 
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
  | FR11
  | FR12
  (* Indirect return register *)
  | IR
  (* Syscall code register*)
  | SC
  (* Frame pointer register*)
  | FP
  (* Stack pointer register*)
  | SP
  type t = register

  type return_strategy =
  | Indirect_return
  | Simple_return of t
  | Splitted_return of t * t

  let compare = Stdlib.compare

  let is_float_register = function
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
    | FR11
    | FR12 -> true
    | _ -> false 

    let caller_saved_register = [
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
      FR11;
      FR12;
      IR;
      SC;
    ]
  
    let callee_saved_register = [
      FP;
      SP
    ]

  let non_float_argument_registers = [
    R0;
    R1;
    R2;
    R3;
    R4;
    R5;
    R6;
    R7;
  ]

  let float_argument_registers = [
    FR0;
    FR1;
    FR2;
    FR3;
    FR4;
    FR5;
    FR6;
    FR7;
  ]
  let arguments_register variable = 
    if KosuIrTyped.Asttyhelper.RType.is_float @@ snd variable then
      float_argument_registers
    else
      non_float_argument_registers

  let syscall_register = [
    R0;
    R1;
    R2;
    R3;
    R4;
    R5;
  ]

  let available_register = [
    R8;
    R9;
    R10;
    R11;
  ]

  let indirect_return_register = IR

  let is_valid_register (variable: variable) (register: t) = 
    let _, rktype = variable in
    let is_float = KosuIrTyped.Asttyhelper.RType.is_float rktype in
    let is_float_register = is_float_register register in
    is_float = is_float_register
    (* float == float || other == other *)

  let does_return_hold_in_register_kt kt = 
    match KosuIrTyped.Sizeof.sizeof_kt kt with
      | 1L | 2L | 4L | 8L -> true
      | _ -> false

  let does_return_hold_in_register variable = 
    does_return_hold_in_register_kt @@ snd variable

  let return_strategy variable = 
    match does_return_hold_in_register variable with
    | true -> Simple_return R0
    | false -> Indirect_return


  let r13 = R13
  let r14 = R14
  let sp = SP

  let fp = FP
end

module GreedyColoration = KosuIrCfg.Asttaccfg.KosuRegisterAllocator.GreedyColoring(Register)


module Location = struct
  type address_offset = [ `ILitteral of int64 | `Register of Register.register ]
  type address = { base : Register.register; offset : address_offset}

  type location = 
  | LocReg of Register.register
  | LocAddr of address

  let loc_reg r = LocReg r
  let loc_addr a = LocAddr a

  let create_address ?(offset = 0L) base = { base; offset = `ILitteral offset }

  let address_register base offset = {base; offset = `Register offset}

  let increment_adress off adress =
    match adress.offset with
    | `ILitteral offset ->
        { adress with offset = `ILitteral (Int64.add offset off) }
    | `Register _reg -> failwith "Increment register based address"
  
end

module Operande = struct
  type src = [
    `ILitteral of int64
    | `Register of Register.register
  ]

  type dst = Register.register

  type single_operande = {
    destination: dst;
    source: src
  }

  type lea_operande = 
    | LeaPcRel of int64
    | LeaRegAbs of Location.address

  let ilitteral n = (`ILitteral n :> src)
  let iregister reg : src = `Register reg

  let lea_pc_relatif n = LeaPcRel n
  let lea_reg_abs address = LeaRegAbs address
end

module Instruction = struct
  open ConditionCode
  open Operande
  (* open Register *)
  open Location



  type bin_op_operande = {
    destination : Register.register;
    operande1: Register.register;
    operande2: src;
  }

  type t = 
    | Halt
    | Ret
    | Syscall
    | CCall of src
    | Mvnt of single_operande
    | Mvng of single_operande
    | Mv of single_operande
    | Mva of {
      operandes : single_operande;
      shift : shift
    }
    | Jump of src
    | Br of src
    | Lea of {
      destination : Register.register;
      operande: lea_operande
    }
    | Add of bin_op_operande
    | Sub of bin_op_operande
    | Mult of bin_op_operande
    | Div of {
      operande: bin_op_operande;
      signed: bool
    }
    | Mod of {
      operande: bin_op_operande;
      signed: bool
    }
    | And of bin_op_operande
    | Or of bin_op_operande
    | Xor of bin_op_operande
    | Lsl of bin_op_operande
    | Asr of bin_op_operande
    | Lsr of bin_op_operande
    | Cmp of {
      cc: condition_code;
      lhs: Register.register;
      rhs: Register.register;
    }
    | Cset of {
      cc: condition_code;
      destination: Register.register;
      lhs: Register.register;
      rhs: Register.register;
      update_last_cmp: bool;
    }
    | Ldr of {
      data_size: data_size;
      destination: Register.register;
      address: address
    }
    | Str of {
      data_size: data_size;
      destination: Register.register;
      address: address
    }

    let halt = Halt

    let ret = Ret

    let syscall = Syscall

    let ccall src = CCall src
    
    let mvnt destination source = Mvnt {destination; source}

    let mvng destination source = Mvng {destination; source}

    let mv destination source = Mv {destination; source}

    let mva destination source shift = Mva {
      operandes = {destination; source};
      shift
    }

    let jump src = Jump src

    let br src = Br src

    let lea destination operande = Lea {destination; operande}

    let sub destination operande1 operande2 = 
      Sub {destination; operande1; operande2}

    let add destination operande1 operande2 = 
      Add {destination; operande1; operande2}

    let str data_size destination address = 
      Str {data_size; destination; address}

    let ldr data_size destination address = 
      Ldr {data_size; destination; address}
    
  
end

module Line = struct
  type line = 
  | Instruction of Instruction.t
  | Comment of string
  | Label of string

  type asmline = AsmLine of line * string option

  let instruction ?comment instr = AsmLine (Instruction instr, comment)

  let instructions instrs = instrs |> List.map instruction

  let sinstruction instruction = instructions [instruction]

  let comment message = AsmLine (Comment message, None)

  let label ?comment l = AsmLine (Label l, comment)
end

module LineInstruction = struct
  open Instruction
  open Location
  open Line
  open Immediat

  let mv_integer register n = 
    let open Immediat in
    if is_encodable Immediat.VmConst.mv_immediat_size n then
    instructions @@ [mv register @@ Operande.ilitteral n]
    else
      let int64, int48, int32, int16 = split n in
      let mvs = [mv register @@ Operande.ilitteral int16] in
      let mvs = 
        if int32 = 0L then mvs 
        else mva register (Operande.ilitteral int32) ConditionCode.SH16 :: mvs
      in
      let mvs = 
        if int48 = 0L then mvs 
        else mva register (Operande.ilitteral int48) ConditionCode.SH32 :: mvs
      in
      let mvs = 
        if int64 = 0L then mvs 
        else mva register (Operande.ilitteral int64) ConditionCode.SH48 :: mvs
      in
    instructions mvs

  let ssub destination source (operande: Operande.src) = 
    match operande with
    | `Register _ -> 
      sinstruction @@ sub destination source operande
    | `ILitteral n when is_encodable VmConst.binop_immediat_size n ->
      sinstruction @@ sub destination source operande
    | `ILitteral n -> 
      let large_mov = mv_integer Register.r14 n in
      let sub_i = instruction @@ sub destination source @@ Operande.iregister Register.r14 in
      large_mov @ [sub_i]

    let sadd destination source (operande: Operande.src) = 
      match operande with
      | `Register _ -> 
        sinstruction @@ sub destination source operande
      | `ILitteral n when is_encodable VmConst.binop_immediat_size n ->
        sinstruction @@ sub destination source operande
      | `ILitteral n -> 
        let large_mov = mv_integer Register.r14 n in
        let sub_i = instruction @@ sub destination source @@ Operande.iregister Register.r14 in
        large_mov @ [sub_i]

  let sstr data_size destination address = 
    match address.offset with
    | `ILitteral n -> begin 
      match n with
      | n when is_encodable VmConst.str_immediat_size n -> 
        instructions [str data_size destination address]
      | n ->
        let mov_int = mv_integer Register.r14 n in
        let address = Location.address_register address.base Register.r14 in
        let str_i = instruction @@ str data_size destination address in
        mov_int @ [str_i]
    end
    | `Register _ -> instructions [str data_size destination address]

    let sldr data_size destination address = 
      match address.offset with
      | `ILitteral n -> begin 
        match n with
        | n when is_encodable VmConst.ldr_immediat_size n -> 
          sinstruction @@ ldr data_size destination address
        | n ->
          let mov_int = mv_integer Register.r14 n in
          let address = Location.address_register address.base Register.r14 in
          let ldr_i = instruction @@ ldr data_size destination address in
          mov_int @ [ldr_i]
      end
      | `Register _ -> instructions [ldr data_size destination address]

  let simple_copy register (address : address) ktype = 
    let data_size = ConditionCode.data_size_of_kt ktype in
    sstr data_size register address

  let rec large_copy_size ~increment register address size = 
    if size < 0L then failwith "negative size to copy"
    else if size = 0L then []
    else 
      let (data_size, offset) : ConditionCode.data_size * int64 =
      let open ConditionCode in
      if size = 0L then (SIZE_8, 0L)
      else if 1L <= size && size < 2L then SIZE_8, 1L
      else if 2L <= size && size < 4L then SIZE_16, 2L
      else if 4L <= size && size < 8L then SIZE_32, 4L
      else SIZE_64, 8L
    in
    let register_address = create_address ?offset:increment register in

    let ldr_instructions = sldr data_size Register.r13 register_address in
    let str_instructions = sstr data_size Register.r13 address in
    let next_adresss_store = increment_adress offset address in

    let next_size = Int64.sub size offset in
    let next_instructions =
      large_copy_size ~increment:(Some offset) register next_adresss_store  next_size 
    in

    ldr_instructions @ str_instructions @ next_instructions

  let large_copy register address kt = 
    large_copy_size ~increment:None register address @@ KosuIrTyped.Sizeof.sizeof_kt kt


    let scopy register (address : address) ktype = 
      match Register.does_return_hold_in_register_kt ktype with
      | true -> simple_copy register address ktype
      | false -> large_copy register address ktype


end

module FrameManager = struct
  type description = {
    (* need to save indirect return register*)
    need_ir: bool;
    local_space: int64;
    discarded_values: Register.variable list;
    variable_map: Location.location KosuCommon.IdVarMap.t
  }

  let indirect_return_var = "@xreturn"
  let indirect_return_type = KosuIrTyped.Asttyped.(RTPointer RTUnknow)
  let indirect_return_vt = (indirect_return_var, indirect_return_type)

  let variable_of_tac_locale_variable = 
    let open KosuIrTAC.Asttac in
    fun { locale_ty; locale } ->
      match locale with
      | Locale s -> (s, locale_ty)
      | Enum_Assoc_id { name; _ } -> (name, locale_ty)

  let location_of variable fd = 
    if List.mem variable fd.discarded_values then None 
    else match IdVarMap.find_opt variable fd.variable_map with
    | None -> Printf.sprintf "location of: %s failed" (fst variable) |> failwith
    | Some _ as loc -> loc

    let frame_descriptor rprogram (function_decl: KosuIrTAC.Asttac.tac_function_decl) = 
      let open KosuIrTAC.Asttac in
      let open Util.Args in
      let iparas, fparams, stack_parameters = 
        Util.Args.consume_args 
        ~fregs:Register.float_argument_registers 
        ~iregs:Register.non_float_argument_registers
        ~fpstyle:(fun (_, kt) -> 
          if KosuIrTyped.Asttyhelper.RType.is_float kt then 
            Simple_Reg Float
          else
            Simple_Reg Other
        ) function_decl.rparameters 
      in

      let parameters = iparas |> ( @ ) fparams |> List.map (fun (variable, return_kind) -> 
        match return_kind with
        | Simple_return reg -> variable, reg
        | Double_return _ -> failwith "Unreachable"
      ) in

      let cfg = KosuIrCfg.Astcfgconv.cfg_liveness_of_tac_function function_decl in
      let colored_graph = GreedyColoration.coloration 
        ~parameters:parameters
        ~available_color:Register.available_register cfg
      in

    let need_ir = not @@ Register.does_return_hold_in_register ((), function_decl.return_type) in

    let colored_graph = 
      if not need_ir 
        then colored_graph 
    else
      let colored_node = GreedyColoration.ColoredGraph.create_colored None indirect_return_vt in
      GreedyColoration.ColoredGraph.add_node colored_node colored_graph
    in


      
    let base_address = Location.create_address ~offset:0L Register.sp in

    let variable_map, stack_variable = function_decl.locale_var |> List.sort (fun lhs rhs ->
        let lsize = KosuIrTyped.Sizeof.sizeof_kt lhs.locale_ty in
        let rsize = KosuIrTyped.Sizeof.sizeof_kt rhs.locale_ty in
        compare rsize lsize
        ) |> List.fold_left (fun (acc_map, acc_stack_variable) variable -> 
          let variable = variable_of_tac_locale_variable variable in
          let open GreedyColoration.ColoredGraph in
          let node = GreedyColoration.ColoredGraph.find variable colored_graph in
          match node.color with
          | Some color -> 
            (* let () = Printf.printf "%s : %d\n" (fst variable) (Obj.magic color) in *)
            let reg = Location.loc_reg color in
            IdVarMap.add variable reg acc_map, acc_stack_variable
          | None -> 
            acc_map, variable::acc_stack_variable
      )  (IdVarMap.empty, []) in

    let stack_variable_types = List.map snd stack_variable in

    let variable_map = stack_variable |> List.mapi Util.couple |> List.fold_left (fun (acc_address, acc_map) (index, variable) ->
      let offset = KosuIrTyped.Sizeof.offset_of_tuple_index index stack_variable_types rprogram in
      let address = Location.increment_adress offset base_address in
      let loc_adrress =  Location.loc_addr address in
      address, IdVarMap.add variable loc_adrress acc_map
    ) (base_address, variable_map) |> snd 
    in

    let local_space = KosuIrTyped.Sizeof.sizeof rprogram ( KosuIrTyped.Asttyhelper.RType.rtuple stack_variable_types) in
    {
      need_ir;
      local_space;
      variable_map;
      discarded_values = function_decl.discarded_values
    }

    let prologue (function_decl: KosuIrTAC.Asttac.tac_function_decl) fd =
      let open Util.Args in
      let open Location in
      let ( ++ ) = Int64.add in
      let ( -- ) = Int64.sub in
      let stack_sub_size = KosuIrTyped.Sizeof.align_8 (8L ++ fd.local_space) in
      let variable_frame = stack_sub_size -- 8L in
      let address = Location.create_address ~offset:variable_frame Register.sp in
      let str_fp = LineInstruction.sstr ConditionCode.SIZE_64 Register.fp address in
      let sub_sp_instructions = LineInstruction.ssub Register.sp Register.sp @@ Operande.ilitteral stack_sub_size in
      let align_fp_instructions = LineInstruction.sadd Register.fp Register.sp @@ Operande.ilitteral variable_frame in

      let iparas, fparams, stack_parameters = 
        Util.Args.consume_args 
        ~fregs:Register.float_argument_registers 
        ~iregs:Register.non_float_argument_registers
        ~fpstyle:(fun (_, kt) -> 
          if KosuIrTyped.Asttyhelper.RType.is_float kt then 
            Simple_Reg Float
          else
            Simple_Reg Other
        ) function_decl.rparameters 
      in
      let store_value_instructions = 
        iparas |> ( @ ) fparams 
          |> List.filter_map ( fun (variable, return_kind) -> 
            match return_kind with
            | Double_return _ -> failwith "Unreachable"
            | Simple_return reg -> begin 
              match location_of variable fd with
              | Some LocReg _ | None -> None
              | Some LocAddr address -> 
                Some (variable, reg, address)
            end
          )
          |> List.map (fun (variable, reg, address) -> 
            let ds = ConditionCode.data_size_of_kt @@ snd variable in
            LineInstruction.sstr ds reg address
          )
          |> List.flatten
      in
      sub_sp_instructions @ str_fp @ align_fp_instructions @ store_value_instructions


      let epilogue fd = 
        let open Instruction in
        let open Register in
        let open Line in
        let ( ++ ) = Int64.add in
        let ( -- ) = Int64.sub in
        let stack_spaces = KosuIrTyped.Sizeof.align_8 (8L ++ fd.local_space) in
        let variable_frame = stack_spaces -- 8L in
        let address = Location.create_address ~offset:variable_frame Register.sp in
        let ldr_fp_instructions = LineInstruction.sldr ConditionCode.SIZE_64 Register.fp address in
        let add_sp_instructions = LineInstruction.sadd Register.sp Register.sp @@ Operande.ilitteral stack_spaces in
        let return_instruction = sinstruction Instruction.ret in
        ldr_fp_instructions @ add_sp_instructions @ return_instruction


end
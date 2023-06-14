(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
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

module Condition_Code = struct
  type data_size = B | SB | H | SH
  type shift = SH16 | SH32 | SH48

  type condition_code =
    | EQ  (** Equal *)
    | NE  (** Not Equal *)
    | CS  (** Carry Set *)
    | CC  (** Carry clear *)
    | MI  (** Minus / Negative *)
    | PL  (** Plus , Posivite ./ Zero *)
    | VS  (** Overflow *)
    | VC  (** No overflow*)
    | HI  (** Unsigned higher*)
    | LS  (** Unsigned lower or same *)
    | GE  (** Signed greater than or equal*)
    | LT  (** Signed less than*)
    | GT  (** Signed greather than *)
    | LE  (** Signed less than or equal *)
    | AL  (** Always*)

  let cc_of_tac_bin ?(is_unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacOr | TacAnd -> None
    | TacEqual -> Some EQ
    | TacDiff -> Some NE
    | TacSup -> Some (if is_unsigned then HI else GT)
    | TacSupEq -> Some (if is_unsigned then CS else GE)
    | TacInfEq -> Some (if is_unsigned then LS else LE)
    | TacInf -> Some (if is_unsigned then CC else LT)
end

module Register = struct
  type variable = string * KosuIrTyped.Asttyped.rktype
  type register_size = SReg32 | SReg64

  type float_register =
    | D0
    | D1
    | D2
    | D3
    | D4
    | D5
    | D6
    | D7
    | D8 (* XR *)
    | D9
    | D10
    | D11
    | D12
    | D13
    | D14
    | D15
    | D16
    | D29
    | D30
    | DZR

  type int_register =
    | X0
    | X1
    | X2
    | X3
    | X4
    | X5
    | X6
    | X7
    | X8 (* XR *)
    | X9
    | X10
    | X11
    | X12
    | X13
    | X14
    | X15
    | X16
    | X29
    | X30
    | XZR
    | SP

  type raw_register = FloatReg of float_register | IntegerReg of int_register
  type register = raw_register
  type t = register
  type register_t = { register : raw_register; size : register_size }

  type return_strategy =
    | Indirect_return
    | Simple_return of t
    | Splitted_return of t * t

  let compare = Stdlib.compare
  let caller_saved_register = []
  let callee_saved_register = []
  let raw_r0 = IntegerReg X0
  let raw_r1 = IntegerReg X1
  let raw_r2 = IntegerReg X2
  let raw_r3 = IntegerReg X3
  let raw_r4 = IntegerReg X4
  let raw_r5 = IntegerReg X5
  let raw_r6 = IntegerReg X6
  let raw_r7 = IntegerReg X7
  let raw_r8 = IntegerReg X8
  let raw_r9 = IntegerReg X9
  let raw_r10 = IntegerReg X10
  let raw_r11 = IntegerReg X11
  let raw_r12 = IntegerReg X12

  let non_float_argument_registers =
    [
      IntegerReg X0;
      IntegerReg X1;
      IntegerReg X2;
      IntegerReg X3;
      IntegerReg X4;
      IntegerReg X5;
      IntegerReg X6;
      IntegerReg X7;
    ]

  let float_argument_registers =
    [
      FloatReg D0;
      FloatReg D1;
      FloatReg D2;
      FloatReg D3;
      FloatReg D4;
      FloatReg D5;
      FloatReg D6;
      FloatReg D7;
    ]

  let arguments_register variable =
    if KosuIrTyped.Asttyhelper.RType.is_float @@ snd variable then
      float_argument_registers
    else non_float_argument_registers

  let syscall_register = [ raw_r0; raw_r1; raw_r2; raw_r3; raw_r4; raw_r5 ]
  let available_register = [ raw_r8; raw_r9; raw_r10; raw_r11 ]
  let indirect_return_register = raw_r8

  let is_valid_register (variable : variable) (register : t) =
    let _, rktype = variable in
    let is_float = KosuIrTyped.Asttyhelper.RType.is_float rktype in
    let is_float_register =
      match register with FloatReg _ -> true | IntegerReg _ -> false
    in
    is_float = is_float_register

  let does_return_hold_in_register_kt kt =
    match KosuIrTyped.Sizeof.sizeof_kt kt with
    | 1L | 2L | 4L | 8L -> true
    | _ -> false

  let does_return_hold_in_register variable =
    does_return_hold_in_register_kt @@ snd variable

  let return_strategy variable =
    match does_return_hold_in_register variable with
    | true -> Simple_return raw_r0
    | false -> Indirect_return
end

module GreedyColoration =
  KosuIrCfg.Asttaccfg.KosuRegisterAllocatorImpl.GreedyColoring (Register)

module Location = struct
  type address_mode =
    | Immediat (* out = *intptr; *)
    | Prefix (* out = *(++intptr);*)
    | Postfix (* out = *(intptr++);*)

  type address_offset =
    [ `ILitteral of int64 | `Register of Register.register_t ]

  type address = { base : Register.register_t; offset : address_offset }
  type location = LocReg of Register.register_t | LocAddr of address

  let loc_reg r = LocReg r
  let loc_addr a = LocAddr a
  let create_address ?(offset = 0L) base = { base; offset = `ILitteral offset }
  let address_register base offset = { base; offset = `Register offset }

  let increment_adress off adress =
    match adress.offset with
    | `ILitteral offset ->
        { adress with offset = `ILitteral (Int64.add offset off) }
    | `Register _reg -> failwith "Increment register based address"
end

module Operande = struct
  type src =
    [ `ILitteral of int64 | `Register of Register.register_t | `Label of string ]

  type dst = Register.register_t

  let ilitteral n = (`ILitteral n :> src)
  let iregister reg : src = `Register reg
  let ilabel label : src = `Label label

  let is_str_offset_range reg n =
    let open Register in
    if n < 0L then -256L < n
    else
      match reg.size with
      | SReg32 -> n < 255L || (Int64.unsigned_rem n 4L = 0L && n < 16380L)
      | SReg64 -> n < 255L || (Int64.unsigned_rem n 8L = 0L && n < 32760L)

  let is_ldr_offset_range = is_str_offset_range

  let is_str_offset_too_far reg address =
    match address with
    | `ILitteral i when not @@ is_ldr_offset_range reg i -> true
    | `ILitteral _ | `Register _ -> false
end

module Instruction = struct
  open Condition_Code
  open Operande
  open Location

  type instruction =
    | Mov of {
        destination : Register.register_t;
        (* Careful int max INT16 *)
        flexsec_operand : src;
      }
    | Movk of {
        destination : Register.register_t;
        operand : src;
        shift : shift option;
      }
    | Mvn of { destination : Register.register_t; operand : src }
    | Not of { destination : Register.register_t; source : src }
    | Neg of { destination : Register.register_t; source : Register.register_t }
    | FCVT of { into : Register.register_t; turn : Register.register_t }
      (* For float convert *)
    | FCVTZU of {
        int_register : Register.register_t;
        float_register : Register.register_t;
      }
      (* Float -> Uint *)
    | FCVTZS of {
        int_register : Register.register_t;
        float_register : Register.register_t;
      }
      (* Float -> Sint *)
    | SCVTF of {
        float_register : Register.register_t;
        int_register : Register.register_t;
      }
      (* int -> float *)
    | ADD of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        (* Int12 litteral oprand*)
        operand2 : src;
        offset : bool;
      }
    | ADDS of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        (* Int12 litteral oprand*)
        operand2 : src;
      }
    | MADD of {
        destination : Register.register_t;
        operand1_base : Register.register_t;
        operand2 : Register.register_t;
        scale : Register.register_t;
      }
    | MSUB of {
        destination : Register.register_t;
        operand1_base : Register.register_t;
        operand2 : Register.register_t;
        scale : Register.register_t;
      }
    | SUB of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        (* Int12 litteral oprand*)
        operand2 : src;
      }
    | SUBS of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        (* Int12 litteral oprand*)
        operand2 : src;
      }
    | MUL of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        operand2 : Register.register_t;
      }
    | UDIV of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        operand2 : Register.register_t;
      }
    | SDIV of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        operand2 : Register.register_t;
      }
    | LSL of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        (* LIteral range [0-31] *)
        operand2 : src;
      }
    | LSR of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        (* LIteral range [0-31] *)
        operand2 : src;
      }
    | ASR of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        (* LIteral range [0-31] *)
        operand2 : src;
      }
    | CSINC of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        operand2 : Register.register_t;
        condition : condition_code;
      }
    | CMP of { operand1 : Register.register_t; operand2 : src }
    | CSET of { register : Register.register_t; cc : condition_code }
    (* Bitwise And*)
    | AND of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        operand2 : src;
      }
    (* Bitwise OR*)
    | ORR of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        operand2 : src;
      }
    (* Bitwise XOR*)
    | EOR of {
        destination : Register.register_t;
        operand1 : Register.register_t;
        operand2 : src;
      }
    | LDR of {
        data_size : data_size option;
        destination : Register.register_t;
        adress_src : address;
        adress_mode : address_mode;
      }
    | LDUR of {
        data_size : data_size option;
        destination : Register.register_t;
        address : Location.address;
        address_mode : Location.address;
      }
    | STR of {
        data_size : data_size option;
        source : Register.register_t;
        address : Location.address;
        adress_mode : Location.address_mode;
      }
    | STP of {
        x1 : Register.register_t;
        x2 : Register.register_t;
        address : Location.address;
        adress_mode : Location.address_mode;
      }
    | ADRP of { dst : Register.register_t; label : string }
    | LDP of {
        x1 : Register.register_t;
        x2 : Register.register_t;
        address : address;
        adress_mode : address_mode;
      }
    | B of { cc : condition_code option; label : string }
    | BL of { cc : condition_code option; label : string }
    | BR of { cc : condition_code option; reg : Register.register_t }
    | BLR of { cc : condition_code option; reg : Register.register_t }
    | SVC
    | RET
end

module Line = struct
  type line =
    | Instruction of Instruction.instruction
    | Comment of string
    | Label of string

  type asmline = AsmLine of line * string option

  let instruction ?comment instr = AsmLine (Instruction instr, comment)
  let instructions instrs = instrs |> List.map instruction
  let sinstruction instruction = instructions [ instruction ]
  let comment message = AsmLine (Comment message, None)
  let label ?comment l = AsmLine (Label l, comment)
end

module AarchProgramType = KosuCommon.AsmProgram (Line)

module LineInstruction = struct
  (* open Instruction
     open Location
     open Line
     open Immediat *)
end

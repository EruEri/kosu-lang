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

module IdVar = Common.IdVar
module IdVarMap = Common.IdVarMap

module Immediat = struct
  let mask_6_8bytes = 0xFFFF_0000_0000_0000L
  let mask_4_6bytes = 0x0000_FFFF_0000_0000L
  let mask_2_4bytes = 0x0000_0000_FFFF_0000L
  let mask_0_2bytes = 0x0000_0000_0000_FFFFL
  let max_16bits_immediat = 65535L
  let min_16bits_immediat = -65535L

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
end

module Register = struct
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

  let compare = Stdlib.compare

  let arguments_register = [
    R0;
    R1;
    R2;
    R3;
    R4;
    R5;
    R6;
    R7;
  ]

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
    R12
  ]

  let indirect_return_register = IR

end

module Operande = struct
  type src = [
    `ILitteral of int64
    | `Register of Register.register
  ]

  type dst = Register.register
end

module Location = struct
  type address_offset = [ `ILitteral of int64 | `Register of Register.register ]
  type address = { base : Register.register; offset : address_offset}
end

module Instruction = struct
  open ConditionCode
  open Operande
  open Register
  open Location

  type single_operande = {
    destination: dst;
    source: src
  }

  type lea_operande = 
    | LeaPcRel of int64
    | LeaRegAbs of address

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

end
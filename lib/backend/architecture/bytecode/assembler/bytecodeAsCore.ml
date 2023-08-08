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

open BytecodeCompiler.BytecodeProgram
module Operande = BytecodeCompiler.Operande
module Location = BytecodeCompiler.Location
module ConditionCode = BytecodeCompiler.ConditionCode
module Register = BytecodeCompiler.Register
module PcRelatifMap = Map.Make (String)

module PcInfo = struct
  type pc_info = {
    pc : int64;
    local_map : int64 PcRelatifMap.t;
    global_map : int64 PcRelatifMap.t;
  }
end

module AsInstruction = struct
  open ConditionCode

  type address_offset = Location.address_offset
  type address = Location.address
  type dst = Register.register
  type src = Operande.src
  type single_operande = Operande.single_operande
  type bin_op_operande = Operande.bin_op_operande
  type lea_operande = BaLeaPcRel of int64 | BaLeaRegAbs of address
  type jump_src = [ `PcRel of int64 | `Register of Register.register ]

  type as_args =
    [ `ArgsValue of int64
    | `ArgsPcReal of int64
    | `ArgsAddr of Location.address ]

  type t =
    | AsHalt
    | AsRet
    | AsSyscall
    | AsCCall of as_args KosuVirtualMachine.FFIType.ccall_entry
    | AsMvnt of single_operande
    | AsMvng of single_operande
    | AsMv of single_operande
    | AsMva of { operandes : single_operande; shift : shift }
    | AsJump of jump_src
    | AsBr of jump_src
    | AsLea of { destination : Register.register; operande : lea_operande }
    | AsAdd of bin_op_operande
    | AsSub of bin_op_operande
    | AsMult of bin_op_operande
    | AsDiv of { operandes : bin_op_operande; signed : bool }
    | AsMod of { operandes : bin_op_operande; signed : bool }
    | AsAnd of bin_op_operande
    | AsOr of bin_op_operande
    | AsXor of bin_op_operande
    | AsLsl of bin_op_operande
    | AsAsr of bin_op_operande
    | AsLsr of bin_op_operande
    | AsCmp of {
        cc : condition_code;
        lhs : Register.register;
        rhs : Register.register;
      }
    | AsCset of {
        cc : condition_code;
        destination : Register.register;
        lhs : Register.register;
        rhs : Register.register;
        update_last_cmp : bool;
      }
    | AsLdr of {
        data_size : data_size;
        destination : Register.register;
        address : address;
      }
    | AsStr of {
        data_size : data_size;
        destination : Register.register;
        address : address;
      }
    | AsItof of {
        data_size : data_size;
        destination : Register.register;
        source : Register.register;
        signed : bool;
      }
    | AsFtoi of {
        data_size : data_size;
        destination : Register.register;
        source : Register.register;
        signed : bool;
      }
end

module AsLine = struct
  type asline =
    | AsComment of string
    | AsInstruction of AsInstruction.t
    | AsLabel of string
end

module AsNode = struct
  type as_function_decl = { as_name : string; as_body : AsLine.asline list }
  type as_const_decl = asm_const_decl

  type as_node =
    | AsFunction of as_function_decl
    | AsConst of as_const_decl
    | AsStringLitteral of { name : string; value : string }
    | AsFloatLitteral of {
        fname : string;
        fvalue : KosuFrontend.Ast.fsize * float;
      }
end

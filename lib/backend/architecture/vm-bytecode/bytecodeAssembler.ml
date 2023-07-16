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

open BytecodeCore.BytecodeProgram
open BytecodeCore.Line
module Operande = BytecodeCore.Operande
module Location = BytecodeCore.Location
module ConditionCode = BytecodeCore.ConditionCode
module Register = BytecodeCore.Register
module PcRelatifMap = Map.Make (String)

module InstructionPass1 = struct
  open ConditionCode

  type address_offset = Location.address_offset
  type address = Location.address
  type dst = Register.register
  type src = Operande.src
  type single_operande = Operande.single_operande
  type bin_op_operande = Operande.bin_op_operande
  type lea_operande = BaLeaPcRel of int64 | BaLeaRegAbs of address
  type jump_src = [ `PcRel of int64 | `Register of Register.register ]

  type t =
    | AsHalt
    | AsRet
    | AsSyscall
    | AsCCall of src
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

let ( ++ ) = Int64.add
let ( !+ ) = ( ++ ) 1L

type pc_info = {
  pc : int64;
  local_map : int64 PcRelatifMap.t;
  global_map : int64 PcRelatifMap.t;
}

let next_pc info = { info with pc = !+(info.pc) }
let incr_pc n info = { info with pc = info.pc ++ n }

let add_local_map elt info =
  { info with local_map = PcRelatifMap.add elt info.pc info.local_map }

let add_global_map elt info =
  { info with global_map = PcRelatifMap.add elt info.pc info.global_map }

let pc_relatif_line info (AsmLine (raw_line, _)) =
  match raw_line with
  | Instruction _ ->
      next_pc info
  | Comment _ ->
      info
  | Label s ->
      add_local_map s info

let pc_relatif_map info = function
  | AFloat_Litteral { fvalue = _; fname } ->
      info |> add_local_map fname |> incr_pc 2L
  | AStringLitteral { name; value } ->
      let stringlen = String.length value in
      let next = KosuIrTyped.Sizeof.align_4 @@ Int64.of_int stringlen in
      info |> add_local_map name |> incr_pc next
  | AConst { asm_const_name; value } -> (
      match value with
      | `IntVal (_, _) ->
          (* vm will store all number as 64 bits *)
          info |> add_global_map asm_const_name |> incr_pc 2L
      | `StrVal string ->
          let stringlen = String.length string in
          let next = KosuIrTyped.Sizeof.align_4 @@ Int64.of_int stringlen in
          info |> add_global_map asm_const_name |> incr_pc next
    )
  | Afunction { asm_name; asm_body } ->
      let info = add_global_map asm_name info in
      List.fold_left pc_relatif_line info asm_body

let pc_relatif pc (asm_module_node_list : asm_module_node list) =
  let local_map = PcRelatifMap.empty in
  let global_map = PcRelatifMap.empty in
  let info = { pc; local_map; global_map } in
  asm_module_node_list |> List.fold_left pc_relatif_map info

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

let vm_register_value = BytecodeCompiler.VmValue.vm_register_value
let vm_shift_value = BytecodeCompiler.VmValue.vm_shift_value
let vm_data_size_value = BytecodeCompiler.VmValue.vm_data_size_value
let vm_cc_value = BytecodeCompiler.VmValue.vm_cc_value
let reg_encode reg = Int32.of_int @@ vm_register_value reg
let shift_encode shift = Int32.of_int @@ vm_shift_value shift
let mask_14_low = 0x00_00_7F_FFl
let maks_15_low = 0x00_00_FF_FFl
let instruction_size = 4
let ( & ) = Int32.logand
let ( &| ) = Int32.logor
let ( ! ) = Int32.lognot
let ( << ) = Int32.shift_left
let ( >> ) = Int32.shift_right_logical
let ( >>> ) = Int32.shift_right

let vm_instruction_value = function
  | BytecodeAsCore.AsInstruction.AsHalt | AsRet | AsSyscall | AsCCall _ ->
      0
  | AsMvnt _ ->
      1
  | AsMvng _ ->
      2
  | AsMv _ ->
      3
  | AsMva _ ->
      4
  | AsJump _ | AsBr _ ->
      5
  | AsLea _ ->
      6
  | AsAdd _ ->
      7
  | AsSub _ ->
      8
  | AsMult _ ->
      9
  | AsDiv _ ->
      10
  | AsMod _ ->
      11
  | AsAnd _ ->
      12
  | AsOr _ ->
      13
  | AsXor _ ->
      14
  | AsLsl _ ->
      15
  | AsLsr _ ->
      16
  | AsAsr _ ->
      17
  | AsCmp _ | AsCset _ ->
      18
  | AsLdr _ | AsStr _ ->
      19
  | AsItof _ | AsFtoi _ ->
      20

let vm_instruction_encode i =
  let opcode = Int32.of_int @@ vm_instruction_value i in
  let base = opcode << 27 in
  match i with
  | BytecodeAsCore.AsInstruction.AsHalt ->
      opcode
  | AsRet ->
      1l << 24
  | AsSyscall ->
      1l << 25
  | AsCCall _ ->
      failwith ""
  | AsMvnt { destination; source }
  | AsMvng { destination; source }
  | AsMv { destination; source } ->
      let dst_value = reg_encode destination in
      let base = base &| (dst_value << 22) in

      let base =
        match source with
        | `Register source ->
            let src_val = reg_encode source in
            let base = base &| (1l << 21) in
            let base = base &| (src_val << 16) in
            base
        | `ILitteral n ->
            let base = base &| (0l << 21) in
            let value = Int64.to_int32 n in
            let base = base &| value in
            base
      in
      base
  | AsMva { operandes = { destination; source }; shift } ->
      let dst_value = reg_encode destination in
      let base = base &| (dst_value << 22) in
      let shift_value = shift_encode shift in
      let base = base &| (shift_value << 20) in
      let base =
        match source with
        | `Register source ->
            let src_val = reg_encode source in
            let base = base &| (1l << 19) in
            let base = base &| (src_val << 14) in
            base
        | `ILitteral n ->
            let base = base &| (0l << 19) in
            let value = Int64.to_int32 n in
            let base = base &| value in
            base
      in
      base
  | (AsJump jump_src | AsBr jump_src) as e ->
      let base =
        match e with
        | AsJump _ ->
            base &| (0l << 26)
        | AsBr _ ->
            base &| (1l << 26)
        | _ ->
            failwith "unreachable"
      in
      let base =
        match jump_src with
        | `PcRel n ->
            let n = !0xFE_00_00_00l & Int64.to_int32 n in
            base &| n
        | `Register r ->
            let r_enc = reg_encode r << 20 in
            base &| r_enc
      in
      base
  | AsLea { destination; operande } ->
      let dst_value = reg_encode destination in
      let base = base &| (dst_value << 22) in
      let base =
        match operande with
        | BaLeaPcRel n ->
            let n = 0x00_1F_FF_FFl & Int64.to_int32 n in
            base &| n
        | BaLeaRegAbs { base = base_addr; offset } ->
            let base = base &| (1l << 21) in
            let reg_value = reg_encode base_addr in
            let base = base &| (reg_value << 16) in
            let base =
              match offset with
              | `ILitteral n ->
                  let n = Int64.to_int32 n & mask_14_low in
                  let base = base &| (0l << 15) in
                  let base = base &| n in
                  base
              | `Register reg ->
                  let reg_b = reg_encode reg in
                  let base = base &| (1l << 15) in
                  let base = base &| (reg_b << 10) in
                  base
            in
            base
      in
      base
  | AsAdd { destination; operande1; operande2 }
  | AsSub { destination; operande1; operande2 }
  | AsMult { destination; operande1; operande2 }
  | AsAnd { destination; operande1; operande2 }
  | AsOr { destination; operande1; operande2 }
  | AsXor { destination; operande1; operande2 }
  | AsLsl { destination; operande1; operande2 }
  | AsLsr { destination; operande1; operande2 }
  | AsAsr { destination; operande1; operande2 } ->
      let reg_dst = reg_encode destination in
      let reg_src1 = reg_encode operande1 in
      let base = base &| (reg_dst << 22) in
      let base = base &| (reg_src1 << 21) in
      let base =
        match operande2 with
        | `ILitteral n ->
            let n = maks_15_low & Int64.to_int32 n in
            let base = base &| (0l << 16) in
            let base = base &| (n << 15) in
            base
        | `Register reg ->
            let r_value = reg_encode reg in
            let base = base &| (1l << 16) in
            let base = base &| (r_value << 12) in
            base
      in
      base
  | AsDiv _ | AsMod _ ->
      failwith ""
  | AsCmp _ | AsCset _ ->
      failwith ""
  | AsLdr _ | AsStr _ ->
      failwith ""
  | AsItof _ | AsFtoi _ ->
      failwith ""

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

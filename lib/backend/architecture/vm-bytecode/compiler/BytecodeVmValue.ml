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

let vm_register_value =
  let open BytecodeCore.Register in
  function
  | R0 ->
      0
  | R1 ->
      1
  | R2 ->
      2
  | R3 ->
      3
  | R4 ->
      4
  | R5 ->
      5
  | R6 ->
      6
  | R7 ->
      7
  | R8 ->
      8
  | R9 ->
      9
  | R10 ->
      10
  | R11 ->
      11
  | R12 ->
      12
  | R13 ->
      13
  | R14 ->
      14
  | FR0 ->
      15
  | FR1 ->
      16
  | FR2 ->
      17
  | FR3 ->
      18
  | FR4 ->
      19
  | FR5 ->
      20
  | FR6 ->
      21
  | FR7 ->
      22
  | FR8 ->
      23
  | FR9 ->
      24
  | FR10 ->
      25
  | IR ->
      26
  | SC ->
      27
  | FP ->
      28
  | RAP ->
      29
  | SP ->
      30

let vm_shift_value =
  let open BytecodeCore.ConditionCode in
  function SH0 -> 0 | SH16 -> 1 | SH32 -> 2 | SH48 -> 3

let data_size =
  let open BytecodeCore.ConditionCode in
  function SIZE_8 -> 0 | SIZE_16 -> 1 | SIZE_32 -> 2 | SIZE_64 -> 3

let vm_cc_value =
  let open BytecodeCore.ConditionCode in
  function
  | ALWAYS ->
      0
  | EQUAL ->
      1
  | DIFF ->
      2
  | SUP ->
      3
  | UNSIGNED_SUP ->
      4
  | SUPEQ ->
      5
  | UNSIGNED_SUPEQ ->
      6
  | INF ->
      7
  | UNSIGNED_INF ->
      8
  | INFEQ ->
      9
  | UNSIGNED_INFEQ ->
      10

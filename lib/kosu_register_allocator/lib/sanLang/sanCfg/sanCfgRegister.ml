(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

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

type t = register
type any = unit
type variable = string * SanTyped.SanTyAst.san_type

type return_strategy =
  | Indirect_return
  | Simple_return of register
  | Splitted_return of register * register

let compare = compare
let any = ()
let syscall_register = []
let callee_saved_register = []

let caller_saved_register =
  [ R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13 ]

let arguments_register = [ R0; R1; R2; R3; R4; R5; R6; R7 ]
let available_register = [ R8; R9; R10; R11; R12 ]

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

let does_return_hold_in_register _ = true
let indirect_return_register = R8
let is_valid_register _ _ = true
let return_strategy _ = Simple_return R0

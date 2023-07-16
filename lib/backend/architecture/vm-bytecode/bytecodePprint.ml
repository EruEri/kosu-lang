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

open BytecodeCore.Operande
open BytecodeCore.ConditionCode
open BytecodeCore.Instruction
open BytecodeCore.Register
open BytecodeCore.Location
open BytecodeCore.Line
open Printf

let string_of_register = function
  | R0 ->
      "r0"
  | R1 ->
      "r1"
  | R2 ->
      "r2"
  | R3 ->
      "r3"
  | R4 ->
      "r4"
  | R5 ->
      "r5"
  | R6 ->
      "r6"
  | R7 ->
      "r7"
  | R8 ->
      "r8"
  | R9 ->
      "r9"
  | R10 ->
      "r10"
  | R11 ->
      "r11"
  | R12 ->
      "r12"
  | R13 ->
      "r13"
  | R14 ->
      "r14"
  | FR0 ->
      "fr0"
  | FR1 ->
      "fr1"
  | FR2 ->
      "fr2"
  | FR3 ->
      "fr3"
  | FR4 ->
      "fr4"
  | FR5 ->
      "fr5"
  | FR6 ->
      "fr6"
  | FR7 ->
      "fr7"
  | FR8 ->
      "fr8"
  | FR9 ->
      "fr9"
  | FR10 ->
      "fr10"
  | IR ->
      "ir"
  | SC ->
      "sc"
  | FP ->
      "fp"
  | RAP ->
      "rap"
  | SP ->
      "sp"

let string_of_data_size (data_size : data_size) =
  let i =
    match data_size with
    | SIZE_8 ->
        8
    | SIZE_16 ->
        16
    | SIZE_32 ->
        32
    | SIZE_64 ->
        64
  in
  Printf.sprintf ".%u" i

let string_of_src : src -> string = function
  | `ILitteral lit ->
      Printf.sprintf "%Ld" lit
  | `Register reg ->
      string_of_register reg

let string_of_shift = function
  | SH0 ->
      "shl0"
  | SH16 ->
      "shl16"
  | SH32 ->
      "shl32"
  | SH48 ->
      "shl48"

let string_of_condition_code = function
  | ALWAYS ->
      ".al"
  | EQUAL ->
      ".eq"
  | DIFF ->
      ".neq"
  | SUP ->
      ".a"
  | UNSIGNED_SUP ->
      ".ua"
  | SUPEQ ->
      ".aeq"
  | UNSIGNED_SUPEQ ->
      ".uaeq"
  | INF ->
      ".b"
  | UNSIGNED_INF ->
      ".ub"
  | INFEQ ->
      ".beq"
  | UNSIGNED_INFEQ ->
      ".ubeq"

let string_of_single_operande { destination; source } =
  Printf.sprintf "%s, %s" (string_of_register destination) (string_of_src source)

let string_of_double_operande { destination; operande1; operande2 } =
  sprintf "%s, %s, %s"
    (string_of_register destination)
    (string_of_register operande1)
    (string_of_src operande2)

let string_of_jump_src = function
  | `Label string ->
      string
  | `Register reg ->
      string_of_register reg

let string_of_address address =
  match address.offset with
  | `ILitteral n ->
      sprintf "*(%s, %Ld)" (string_of_register address.base) n
  | `Register reg ->
      sprintf "*(%s, %s)"
        (string_of_register address.base)
        (string_of_register reg)

let string_of_lea_instruction = function
  | LeaPcRel string ->
      string
  | LeaRegAbs address ->
      string_of_address address

let string_of_instruction_format instruction f op =
  Printf.sprintf "%s %s" instruction (f op)

let string_of_instruction = function
  | Halt ->
      "halt"
  | Ret ->
      "ret"
  | Syscall ->
      "syscall"
  | CCall src ->
      Printf.sprintf "ccall %s" (string_of_src src)
  | Mvnt so ->
      sprintf "mvnt %s" (string_of_single_operande so)
  | Mvng so ->
      sprintf "mvng %s" (string_of_single_operande so)
  | Mv so ->
      sprintf "mv %s" (string_of_single_operande so)
  | Mva so ->
      sprintf "mva %s, %s"
        (string_of_single_operande so.operandes)
        (string_of_shift so.shift)
  | Jump jump ->
      string_of_instruction_format "jump" string_of_jump_src jump
  | Br br ->
      string_of_instruction_format "br" string_of_jump_src br
  | Lea { destination; operande } ->
      sprintf "lea %s, %s"
        (string_of_register destination)
        (string_of_lea_instruction operande)
  | Add bin_op_operande ->
      string_of_instruction_format "add" string_of_double_operande
        bin_op_operande
  | Sub bin_op_operande ->
      string_of_instruction_format "sub" string_of_double_operande
        bin_op_operande
  | Mult bin_op_operande ->
      string_of_instruction_format "mult" string_of_double_operande
        bin_op_operande
  | Div { operandes; signed } ->
      let si =
        if signed then
          "div"
        else
          "udiv"
      in
      string_of_instruction_format si string_of_double_operande operandes
  | Mod { operandes; signed } ->
      let si =
        if signed then
          "mod"
        else
          "umod"
      in
      string_of_instruction_format si string_of_double_operande operandes
  | And bin_op_operande ->
      string_of_instruction_format "and" string_of_double_operande
        bin_op_operande
  | Or bin_op_operande ->
      string_of_instruction_format "or" string_of_double_operande
        bin_op_operande
  | Xor bin_op_operande ->
      string_of_instruction_format "xor" string_of_double_operande
        bin_op_operande
  | Lsl bin_op_operande ->
      string_of_instruction_format "lsl" string_of_double_operande
        bin_op_operande
  | Asr bin_op_operande ->
      string_of_instruction_format "asr" string_of_double_operande
        bin_op_operande
  | Lsr bin_op_operande ->
      string_of_instruction_format "lsr" string_of_double_operande
        bin_op_operande
  | Cmp { cc; lhs; rhs } ->
      sprintf "cmp%s, %s, %s"
        (string_of_condition_code cc)
        (string_of_register lhs) (string_of_register rhs)
  | Cset { cc; destination; lhs; rhs; update_last_cmp } ->
      sprintf "cset%s%s, %s, %s, %s"
        ( if update_last_cmp then
            "u"
          else
            ""
        )
        (string_of_condition_code cc)
        (string_of_register destination)
        (string_of_register lhs) (string_of_register rhs)
  | Ldr { data_size; destination; address } ->
      sprintf "ldr%s %s, %s"
        (string_of_data_size data_size)
        (string_of_register destination)
        (string_of_address address)
  | Str { data_size; destination; address } ->
      sprintf "str%s %s, %s"
        (string_of_data_size data_size)
        (string_of_register destination)
        (string_of_address address)
  | Itof { data_size; destination; source; signed } ->
      sprintf "%sitof.%s %s %s"
        ( if signed then
            ""
          else
            "u"
        )
        (string_of_data_size data_size)
        (string_of_register destination)
        (string_of_register source)
  | Ftoi { data_size; destination; source; signed } ->
      sprintf "%sftoi.%s %s %s"
        ( if signed then
            ""
          else
            "u"
        )
        (string_of_data_size data_size)
        (string_of_register destination)
        (string_of_register source)

let string_of_asm_line (AsmLine (line, comment)) =
  let comment =
    comment |> Option.map (( ^ ) "//") |> Option.value ~default:""
  in
  match line with
  | Instruction i ->
      sprintf "\t%s %s" (string_of_instruction i) comment
  | Comment s ->
      sprintf "\t// %s" s
  | Label s ->
      sprintf "%s:" s

let string_of_asm_function
    ({ asm_name; asm_body } : BytecodeCore.BytecodeProgram.asm_function_decl) =
  sprintf "%s:\n%s" asm_name
    (asm_body |> List.map string_of_asm_line |> String.concat "\n")

let string_of_asm_node = function
  | BytecodeCore.BytecodeProgram.Afunction f ->
      string_of_asm_function f
  | AConst _c ->
      failwith "TODO: string_of_asm_node Const"

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

open Aarch64Core
open Aarch64Core.Register
open Aarch64Core.Instruction
open Printf
module AsmProgram = Common.AsmProgram (Aarch64Core.Instruction)
open AsmProgram

module Make (AsmSpec : Aarch64AsmSpec.Aarch64AsmSpecification) = struct
  let string_of_f64bits_reg = function
    | D0 -> "d0"
    | D1 -> "d1"
    | D2 -> "d2"
    | D3 -> "d3"
    | D4 -> "d4"
    | D5 -> "d5"
    | D6 -> "d6"
    | D7 -> "d7"
    | D8 -> "d8"
    | D9 -> "d9"
    | D10 -> "d10"
    | D11 -> "d11"
    | D12 -> "d12"
    | D13 -> "d13"
    | D14 -> "d14"
    | D15 -> "d15"
    | D16 -> "d16"
    | D29 -> "d29"
    | D30 -> "d30"
    | DZR -> "dzr"

  let string_of_32bits_reg = function
    | W0 -> "w0"
    | W1 -> "w1"
    | W2 -> "w2"
    | W3 -> "w3"
    | W4 -> "w4"
    | W5 -> "w5"
    | W6 -> "w6"
    | W7 -> "w7"
    | W8 -> "w8"
    | W9 -> "w9"
    | W10 -> "w10"
    | W11 -> "w11"
    | W12 -> "w12"
    | W13 -> "w13"
    | W14 -> "w14"
    | W15 -> "w15"
    | W16 -> "w16"
    | W29 -> "w29"
    | W30 -> "w30"
    | WZR -> "wzr"
    | WSP -> "wsp"

  let string_of_64bits_reg = function
    | X0 -> "x0"
    | X1 -> "x1"
    | X2 -> "x2"
    | X3 -> "x3"
    | X4 -> "x4"
    | X5 -> "x5"
    | X6 -> "x6"
    | X7 -> "x7"
    | X8 -> "x8"
    | X9 -> "x9"
    | X10 -> "x10"
    | X11 -> "x11"
    | X12 -> "x12"
    | X13 -> "x13"
    | X14 -> "x14"
    | X15 -> "x15"
    | X16 -> "x16"
    | X29 -> "x29"
    | X30 -> "x30"
    | XZR -> "xzr"
    | SP -> "sp"

  let string_of_data_size = function
    | SB -> "sb"
    | B -> "b"
    | H -> "h"
    | SH -> "sh"

  let string_of_condition_code =
    let open Condition_Code in
    function
    | EQ -> "eq"
    | NE -> "ne"
    | CS -> "cs"
    | CC -> "cc"
    | MI -> "mi"
    | PL -> "pl"
    | VS -> "vs"
    | VC -> "vc"
    | HI -> "hi"
    | LS -> "ls"
    | GE -> "ge"
    | LT -> "lt"
    | GT -> "gt"
    | LE -> "le"
    | AL -> "al"

  let string_of_register = function
    | FRegister64 freg64 -> string_of_f64bits_reg freg64
    | Register32 reg32 -> string_of_32bits_reg reg32
    | Register64 reg64 -> string_of_64bits_reg reg64

  let string_of_src = function
    | `F64Litteral float -> Printf.sprintf "#%F" float
    | `ILitteral int64 -> Printf.sprintf "#%Ld" int64
    | `Register reg -> string_of_register reg
    | `Label label -> label

  let prefix_of_float register =
    if Register.is_f64_reg register then "f" else ""

  let string_of_adressage adress_mode { base; offset } =
    match adress_mode with
    | Immediat ->
        sprintf "[%s%s]" (string_of_register base)
          ((if offset = 0L then None else Some offset)
          |> Option.map (sprintf ", #%Ld")
          |> Option.value ~default:"")
    | Prefix ->
        sprintf "[%s%s]!" (string_of_register base)
          ((if offset = 0L then None else Some offset)
          |> Option.map (sprintf ", #%Ld")
          |> Option.value ~default:"")
    | Postfix ->
        sprintf "[%s]%s" (string_of_register base)
          ((if offset = 0L then None else Some offset)
          |> Option.map (sprintf ", #%Ld")
          |> Option.value ~default:"")

  let value_of_shift = function SH16 -> 16 | SH32 -> 32 | SH48 -> 48

  let string_of_instruction = function
    | Mov { destination; flexsec_operand } ->
        sprintf "%smov %s, %s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_src flexsec_operand)
    | Mvn { destination; operand } ->
        sprintf "%smvn %s, %s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_src operand)
    | Movk { destination; operand; shift } ->
        sprintf "%smovk %s, %s%s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_src operand)
          (shift
          |> Option.map (fun sh -> sprintf ", lsl %d" (value_of_shift sh))
          |> Option.value ~default:"")
    | Not { destination; source } ->
        sprintf "mvn %s, %s"
          (string_of_register destination)
          (string_of_src source)
    | Neg { destination; source } ->
        sprintf "neg %s, %s"
          (string_of_register destination)
          (string_of_register source)
    | ADD { destination; operand1; operand2; offset } -> (
        match AsmSpec.adrp_style with
        | AsmSpec.MacOS ->
            sprintf "%sadd %s, %s, %s%s"
              (prefix_of_float destination)
              (string_of_register destination)
              (string_of_register operand1)
              (string_of_src operand2)
              (if offset then "@PAGEOFF" else "")
        | AsmSpec.Other ->
            sprintf "%sadd %s, %s, %s"
              (prefix_of_float destination)
              (string_of_register destination)
              (string_of_register operand1)
              (if not offset then string_of_src operand2
              else sprintf ":lo12:%s" (string_of_src operand2)))
    | MADD { destination; operand1_base; operand2; scale } ->
        sprintf "madd %s, %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand2)
          (string_of_register scale)
          (string_of_register operand1_base)
    (* rd = ra + rn × rm *)
    (* rd, rn, rm, ra *)
    (* { destination = operand2; scale; operand1_base } *)
    | MSUB { destination; operand1_base; operand2; scale } ->
        sprintf "msub %s, %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand2)
          (string_of_register scale)
          (string_of_register operand1_base)
    | ADDS { destination; operand1; operand2 } ->
        sprintf "%sadds %s, %s, %s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | SUB { destination; operand1; operand2 } ->
        sprintf "%ssub %s, %s, %s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | SUBS { destination; operand1; operand2 } ->
        sprintf "%ssubs %s, %s, %s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | MUL { destination; operand1; operand2 } ->
        sprintf "%smul %s, %s, %s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_register operand2)
    | UDIV { destination; operand1; operand2 } ->
        sprintf "udiv %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_register operand2)
    | SDIV { destination; operand1; operand2 } ->
        sprintf "%ssdiv %s, %s, %s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_register operand2)
    | LSR { destination; operand1; operand2 } ->
        sprintf "lsr %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | LSL { destination; operand1; operand2 } ->
        sprintf "lsl %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | ASR { destination; operand1; operand2 } ->
        sprintf "asr %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | CSINC { destination; operand1; operand2; condition } ->
        sprintf "%scsinc %s, %s, %s, %s"
          (prefix_of_float destination)
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_register operand2)
          (string_of_condition_code condition)
    | AND { destination; operand1; operand2 } ->
        sprintf "and %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | ORR { destination; operand1; operand2 } ->
        sprintf "orr %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | EOR { destination; operand1; operand2 } ->
        sprintf "eor %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | CMP { operand1; operand2 } ->
        sprintf "%scmp %s, %s" (prefix_of_float operand1)
          (string_of_register operand1)
          (string_of_src operand2)
    | LDR { data_size; destination; adress_src; adress_mode } ->
        sprintf "ldr%s %s , %s"
          (data_size
          |> Option.map string_of_data_size
          |> Option.value ~default:"")
          (string_of_register destination)
          (string_of_adressage adress_mode adress_src)
    | LDUR { data_size; destination; adress_src; adress_mode } ->
        sprintf "ldur%s %s , %s"
          (data_size
          |> Option.map string_of_data_size
          |> Option.value ~default:"")
          (string_of_register destination)
          (string_of_adressage adress_mode adress_src)
    | STR { data_size; source; adress; adress_mode } ->
        sprintf "str%s %s , %s"
          (data_size
          |> Option.map (fun ds -> string_of_data_size @@ unsigned_data_size ds)
          |> Option.value ~default:"")
          (string_of_register source)
          (string_of_adressage adress_mode adress)
    | STP { x1; x2; address; adress_mode } ->
        sprintf "stp %s, %s, %s" (string_of_register x1) (string_of_register x2)
          (string_of_adressage adress_mode address)
    | LDP { x1; x2; address; adress_mode } ->
        sprintf "ldp %s, %s, %s" (string_of_register x1) (string_of_register x2)
          (string_of_adressage adress_mode address)
    | ADRP { dst; label } -> (
        match AsmSpec.adrp_style with
        | AsmSpec.MacOS ->
            sprintf "adrp %s, %s@PAGE" (string_of_register dst) label
        | AsmSpec.Other -> sprintf "adrp %s, %s" (string_of_register dst) label)
    | B { cc; label } ->
        sprintf "b%s %s"
          (cc
          |> Option.map (fun cc -> sprintf ".%s" (string_of_condition_code cc))
          |> Option.value ~default:"")
          label
    | BL { cc; label } ->
        sprintf "bl%s %s"
          (cc
          |> Option.map (fun cc -> sprintf ".%s" (string_of_condition_code cc))
          |> Option.value ~default:"")
          label
    | BR { cc; reg } ->
        sprintf "br%s %s"
          (cc
          |> Option.map (fun cc -> sprintf ".%s" (string_of_condition_code cc))
          |> Option.value ~default:"")
          (string_of_register reg)
    | BLR { cc; reg } ->
        sprintf "blr%s %s"
          (cc
          |> Option.map (fun cc -> sprintf ".%s" (string_of_condition_code cc))
          |> Option.value ~default:"")
          (string_of_register reg)
    | SVC -> sprintf "svc #0x80"
    | RET -> "ret"

  let string_of_raw_line = function
    | Label s -> s ^ ":"
    | Instruction s -> "\t" ^ string_of_instruction s
    | Line_Com (Comment s) -> sprintf "\t%s %s" AsmSpec.comment_prefix s
    | Directive d -> "\t." ^ d

  let string_asm_const_decl { asm_const_name; value } =
    match value with
    | `IntVal (size, int_value) ->
        sprintf "\n\t%s\n%s:\n\t.%s %s"
          (AsmSpec.constant_directives asm_const_name value
          |> String.concat "\n\t")
          asm_const_name
          (AsmSpec.size_directive_of_size size)
          (sprintf "0x%LX" int_value)
    | `StrVal s ->
        sprintf "\n\t%s\n%s:\n\t%s \"%s\""
          (AsmSpec.constant_directives asm_const_name value
          |> String.concat "\n\t")
          asm_const_name AsmSpec.string_litteral_directive s

  let string_of_asm_function { asm_name; asm_body } =
    sprintf "\t%s\n%s:\n%s"
      (AsmSpec.function_directives asm_name |> String.concat "\n\t")
      asm_name
      (asm_body |> List.map string_of_raw_line |> String.concat "\n")

  let string_of_asm_node = function
    | Afunction f -> string_of_asm_function f
    | AConst c -> string_asm_const_decl c
end

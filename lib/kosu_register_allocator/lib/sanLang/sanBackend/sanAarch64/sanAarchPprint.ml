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

open SanAarchCore
open SanAarchCore.Condition_Code
open SanAarchCore.Instruction
open SanAarchCore.AsmProgram
open SanAarchCore.Operande
open SanAarchCore.Register
open SanAarchCore.Location
open Printf

module Make (AsmSpec : SanAarchSpecification.Aarch64AsmSpecification) = struct
  let string_of_data_size = function SB -> "sb" | B -> "b"

  let string_of_condition_code =
    let open Condition_Code in
    function
    | EQ ->
        "eq"
    | NE ->
        "ne"
    | CS ->
        "cs"
    | CC ->
        "cc"
    | MI ->
        "mi"
    | PL ->
        "pl"
    | VS ->
        "vs"
    | VC ->
        "vc"
    | HI ->
        "hi"
    | LS ->
        "ls"
    | GE ->
        "ge"
    | LT ->
        "lt"
    | GT ->
        "gt"
    | LE ->
        "le"
    | AL ->
        "al"

  let string_of_register register =
    let prefix = match register.size with SReg32 -> 'w' | SReg64 -> 'x' in
    let extension =
      match register.register with
      | X0 ->
          "0"
      | X1 ->
          "1"
      | X2 ->
          "2"
      | X3 ->
          "3"
      | X4 ->
          "4"
      | X5 ->
          "5"
      | X6 ->
          "6"
      | X7 ->
          "7"
      | X8 ->
          "8"
      | X9 ->
          "9"
      | X10 ->
          "10"
      | X11 ->
          "11"
      | X12 ->
          "12"
      | X13 ->
          "13"
      | X14 ->
          "14"
      | X15 ->
          "15"
      | X16 ->
          "16"
      | X29 ->
          "29"
      | X30 ->
          "30"
      | XZR ->
          "zr"
      | SP ->
          "sp"
    in
    match register.register with
    | SP when register.size = SReg64 ->
        extension
    | _ ->
        sprintf "%c%s" prefix extension

  let string_of_src : Operande.src -> string = function
    | `ILitteral int64 ->
        Printf.sprintf "#%Ld" int64
    | `Register reg ->
        string_of_register reg
    | `Label label ->
        label

  let string_of_address_offset = function
    | `Register reg ->
        Printf.sprintf ", %s" (string_of_register reg)
    | `ILitteral f ->
        if f = 0L then
          ""
        else
          Printf.sprintf ", #%Ld" f

  let string_of_adressage adress_mode { base; offset } =
    match adress_mode with
    | Immediat ->
        sprintf "[%s%s]" (string_of_register base)
          (string_of_address_offset offset)
    | Prefix ->
        sprintf "[%s%s]!" (string_of_register base)
          (string_of_address_offset offset)
    | Postfix ->
        sprintf "[%s]%s" (string_of_register base)
          (string_of_address_offset offset)

  let value_of_shift = function SH16 -> 16 | SH32 -> 32 | SH48 -> 48

  let string_of_instruction = function
    | Mov { destination; source } ->
        sprintf "mov %s, %s"
          (string_of_register destination)
          (string_of_src source)
    | Mvn { destination; operand } ->
        sprintf "mvn %s, %s"
          (string_of_register destination)
          (string_of_src operand)
    | Movk { destination; operand; shift } ->
        sprintf "movk %s, %s%s"
          (string_of_register destination)
          (string_of_src operand)
          (shift
          |> Option.map (fun sh -> sprintf ", lsl %d" (value_of_shift sh))
          |> Option.value ~default:""
          )
    | Not { destination; source } ->
        sprintf "mvn %s, %s"
          (string_of_register destination)
          (string_of_src source)
    | Neg { destination; source } ->
        sprintf "neg %s, %s"
          (string_of_register destination)
          (string_of_register source)
    | Add { destination; operand1; operand2; offset } -> (
        match AsmSpec.adrp_style with
        | AsmSpec.MacOS ->
            sprintf "add %s, %s, %s%s"
              (string_of_register destination)
              (string_of_register operand1)
              (string_of_src operand2)
              ( if offset then
                  "@PAGEOFF"
                else
                  ""
              )
        | AsmSpec.Other ->
            sprintf "add %s, %s, %s"
              (string_of_register destination)
              (string_of_register operand1)
              ( if not offset then
                  string_of_src operand2
                else
                  sprintf ":lo12:%s" (string_of_src operand2)
              )
      )
    | Sub { destination; operand1; operand2 } ->
        sprintf "sub %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | Mul { destination; operand1; operand2 } ->
        sprintf "mul %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_register operand2)
    | SDiv { destination; operand1; operand2 } ->
        sprintf "div %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_register operand2)
    | Lsr { destination; operand1; operand2 } ->
        sprintf "lsr %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | Lsl { destination; operand1; operand2 } ->
        sprintf "lsl %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | Asr { destination; operand1; operand2 } ->
        sprintf "asr %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | Csinc { destination; operand1; operand2; condition } ->
        sprintf "csinc %s, %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_register operand2)
          (string_of_condition_code condition)
    | And { destination; operand1; operand2 } ->
        sprintf "and %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | Orr { destination; operand1; operand2 } ->
        sprintf "orr %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | Eor { destination; operand1; operand2 } ->
        sprintf "eor %s, %s, %s"
          (string_of_register destination)
          (string_of_register operand1)
          (string_of_src operand2)
    | Cmp { operand1; operand2 } ->
        sprintf "cmp %s, %s"
          (string_of_register operand1)
          (string_of_src operand2)
    | Cset { register; cc } ->
        sprintf "cset %s, %s"
          (string_of_register register)
          (string_of_condition_code cc)
    | Ldr { data_size; destination; address_src; address_mode } ->
        sprintf "ldr%s %s , %s"
          (data_size
          |> Option.map string_of_data_size
          |> Option.value ~default:""
          )
          (string_of_register destination)
          (string_of_adressage address_mode address_src)
    | Str { data_size; source; address; address_mode } ->
        sprintf "str%s %s , %s"
          (data_size
          |> Option.map (fun ds -> string_of_data_size ds)
          |> Option.value ~default:""
          )
          (string_of_register source)
          (string_of_adressage address_mode address)
    | Stp { x1; x2; address; adress_mode } ->
        sprintf "stp %s, %s, %s" (string_of_register x1) (string_of_register x2)
          (string_of_adressage adress_mode address)
    | Ldp { x1; x2; address; address_mode } ->
        sprintf "ldp %s, %s, %s" (string_of_register x1) (string_of_register x2)
          (string_of_adressage address_mode address)
    | Adrp { dst; label } -> (
        match AsmSpec.adrp_style with
        | AsmSpec.MacOS ->
            sprintf "adrp %s, %s@PAGE" (string_of_register dst) label
        | AsmSpec.Other ->
            sprintf "adrp %s, %s" (string_of_register dst) label
      )
    | B { cc; label } ->
        sprintf "b%s %s"
          (cc
          |> Option.map (fun cc -> sprintf ".%s" (string_of_condition_code cc))
          |> Option.value ~default:""
          )
          label
    | Bl { cc; label } ->
        sprintf "bl%s %s"
          (cc
          |> Option.map (fun cc -> sprintf ".%s" (string_of_condition_code cc))
          |> Option.value ~default:""
          )
          label
    | RET ->
        "ret"

  let string_of_asm_line line =
    let open SanAarchCore.Line in
    let (AsmLine (line, comment)) = line in
    let comment_content = Option.value ~default:"" @@ comment in
    let comment_style_content =
      comment
      |> Option.map (Printf.sprintf "%s %s" AsmSpec.comment_prefix)
      |> Option.value ~default:""
    in

    let line =
      match line with
      | Comment s ->
          sprintf "\t%s %s %s" AsmSpec.comment_prefix s comment_content
      | Instruction i ->
          sprintf "\t%s %s" (string_of_instruction i) comment_style_content
      | Label l ->
          sprintf "%s: %s" l comment_style_content
      | Directive d ->
          sprintf "\t.%s %s" d comment_style_content
    in
    sprintf "%s" line

  let string_of_asm_function { asm_name; asm_body } =
    sprintf "\t%s\n%s:\n%s"
      (AsmSpec.function_directives asm_name |> String.concat "\n\t")
      asm_name
      (asm_body |> List.map string_of_asm_line |> String.concat "\n")

  let string_of_asm_node = function Afunction f -> string_of_asm_function f
end

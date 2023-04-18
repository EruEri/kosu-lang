(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
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

open X86_64Core.Condition_Code
open X86_64Core.Instruction
open X86_64Core.Operande
open X86_64Core.Register
open X86_64Core
open Printf
module X86Program = Common.AsmProgram (X86_64Core.Instruction)
open X86Program
open Util.Operator

module Make (AsmSpec : X86_64AsmSpec.X86_64AsmSpecification) = struct
  let string_of_int_data_size = function B -> "b" | W -> "w" | L -> "l" | Q -> "q"

  let string_of_float_data_size = function SS -> "ss" | SD -> "sd" 

  let string_of_data_size = function
  | IntSize i -> string_of_int_data_size i
  | FloatSize f -> string_of_float_data_size f

  let string_of_raw_register = function
    | RAX -> "rax"
    | RBX -> "rbx"
    | RCX -> "rcx"
    | RDX -> "rdx"
    | RSI -> "rsi"
    | RDI -> "rdi"
    | RBP -> "rbp"
    | RSP -> "rsp"
    | R8 -> "r8"
    | R9 -> "r9"
    | R10 -> "r10"
    | R11 -> "r11"
    | R12 -> "r12"
    | R13 -> "r13"
    | R14 -> "r14"
    | R15 -> "r15"
    | RIP -> "rip"

  let string_of_register_int_reg size register =
    let str_raw_reg = string_of_raw_register register in
    match size with
    | Q -> str_raw_reg
    | L ->
        if Register.is_numerical_register register then
          Printf.sprintf "%sd" str_raw_reg
        else str_raw_reg |> String.map (fun c -> if c = 'r' then 'e' else c)
    | W ->
        if Register.is_numerical_register register then
          Printf.sprintf "%sw" str_raw_reg
        else String.sub str_raw_reg 1 2
    | B -> (
        if Register.is_numerical_register register then
          Printf.sprintf "%sb" str_raw_reg
        else
          match str_raw_reg with
          | "rax" -> "al"
          | "rbx" -> "bl"
          | "rcx" -> "cl"
          | "rdx" -> "dl"
          | other -> Printf.sprintf "%sl" (String.sub other 1 2))

  let string_of_register_int_reg size register =
    Printf.sprintf "%%%s" (string_of_register_int_reg size register)

  let string_of_float_register = function
  | XMM0 -> "%xmm0"
  | XMM1 -> "%xmm1"
  | XMM2 -> "%xmm2"
  | XMM3 -> "%xmm3"
  | XMM4 -> "%xmm4"
  | XMM5 -> "%xmm5"
  | XMM6 -> "%xmm6"
  | XMM7 -> "%xmm7"
  | XMM8 -> "%xmm8"
  | XMM9 -> "%xmm9"
  | XMM10 -> "%xmm10"
  | XMM11 -> "%xmm11"
  | XMM12 -> "%xmm12"
  | XMM13 -> "%xmm13"
  | XMM14 -> "%xmm14"
  | XMM15 -> "%xmm15"


  let int_data_size_of_datasize = function
  | IntSize s -> s 
  | FloatSize _ -> Q (* word size *)

  let string_of_register size reg = 
    let defaultsize = int_data_size_of_datasize size in
  match reg.register with
  | IntegerReg register -> string_of_register_int_reg (reg.register_size >? defaultsize) register
  | FloatReg register -> string_of_float_register register

  let string_of_address_offset = function
    | Addr_label (label, offset) ->
        Printf.sprintf "%s%s" label
          (if offset = 0L then "" else Printf.sprintf "+%Lu" offset)
    | Offset offset -> Printf.sprintf "%Ld" offset

  let string_of_address { offset; base; index; scale } =
    Printf.sprintf "%s(%s%s%s)"
      (string_of_address_offset offset)
      (string_of_register iq base)
      (index
      |> Option.map (fun reg -> sprintf ", %s" (string_of_register iq reg))
      |> Option.value ~default:"")
      (if scale = 1 then "" else sprintf "%u" scale)

  let string_of_dst: data_size -> dst -> string = fun size dst -> match dst with
    | `Register reg -> string_of_register size reg
    | `Address addr -> string_of_address addr

  let string_of_src :data_size -> src -> string = fun size src -> match src with
    | #dst as dst -> string_of_dst size dst
    | `ILitteral n -> sprintf "$%Ld" n
    | `F64Litteral f -> sprintf "%f" f
    | `Label l -> l

  let string_of_condition_code = function
    | E -> "e"
    | NE -> "ne"
    | S -> "s"
    | NS -> "ns"
    | G -> "g"
    | GE -> "ge"
    | L -> "l"
    | LE -> "le"
    | A -> "a"
    | AE -> "ae"
    | B -> "b"
    | BE -> "be"

  let string_of_condition_code_opt = function
    | None -> "mp"
    | Some s -> string_of_condition_code s

  let string_of_jmp_operande = function
    | `Register reg -> sprintf "*%s" (string_of_register iq reg)
    | `Label l -> l

  let string_of_ctv_data_size = function
  | IntSize _ as _is -> "si" 
  | FloatSize _ as fs -> string_of_data_size fs  

  let string_of_cttv_data_size = function
  | IntSize _ as is -> 
    Printf.sprintf "si%s" (string_of_data_size is)
  | FloatSize _ as fs -> string_of_data_size fs  

  let string_of_instruction = function
    | Mov { size; source; destination } ->
        sprintf "mov%s %s, %s" (string_of_data_size size) (string_of_src size source)
          (string_of_dst size destination)
    | Movsl { size; source; destination } ->
        sprintf "movs%sl %s, %s" (string_of_int_data_size size)
          (string_of_src (il) source)
          (string_of_dst (il) destination)
    | Movzl { size; source; destination } ->
        sprintf "movz%sl %s, %s" (string_of_int_data_size size)
          (string_of_src (il) source)
          (string_of_dst (il) destination)
    | Set { cc; size; register } ->
        sprintf "set%s %s"
          (string_of_condition_code cc)
          (string_of_register (intsize size) register)
    | Lea { size; source; destination } ->
        sprintf "lea%s %s, %s" (string_of_data_size size)
          (string_of_address source)
          (string_of_register size destination)
    | Neg { size; source } ->
        sprintf "neg%s %s" (string_of_data_size size)
          (string_of_register size source)
    | Not { size; source } ->
        sprintf "not%s %s" (string_of_data_size size)
          (string_of_register size source)
    | Add { size; source; destination } ->
        sprintf "add%s %s, %s" (string_of_data_size size) (string_of_src size source)
          (string_of_dst size destination)
    | Sub { size; source; destination } ->
        sprintf "sub%s %s, %s" (string_of_data_size size) (string_of_src size source)
          (string_of_dst size destination)
    | Xor { size; source; destination } ->
      let isize = (intsize size) in
        sprintf "xor%s %s, %s" (string_of_int_data_size size) (string_of_src isize source)
          (string_of_dst isize destination)
    | And { size; source; destination } ->
      let isize = (intsize size) in
        sprintf "and%s %s, %s" (string_of_int_data_size size) (string_of_src isize source)
          (string_of_dst isize destination)
    | Cvts2s {source_size; dst_size; source; destination} ->
        sprintf "cvt%s2%s %s, %s"
          (string_of_ctv_data_size source_size)
          (string_of_ctv_data_size dst_size)
          (string_of_src source_size source)
          (string_of_dst dst_size destination)

    | Cvtts2s {source_size; dst_size; source; destination} ->
      sprintf "cvtt%s2%s %s, %s"
        (string_of_ctv_data_size source_size)
        (string_of_cttv_data_size dst_size)
        (string_of_src source_size source)
        (string_of_dst dst_size destination)
    | Or { size; source; destination } ->
      let isize = (intsize size) in
        sprintf "or%s %s, %s" (string_of_int_data_size size) (string_of_src isize source)
          (string_of_dst isize destination)
    | Sal { size; shift; destination } ->
      let isize = (intsize size) in
        sprintf "sal%s %s, %s" (string_of_int_data_size size) (string_of_src isize shift)
          (string_of_dst isize destination)
    | Sar { size; shift; destination } ->
        let isize = (intsize size) in
        sprintf "sar%s %s, %s" (string_of_int_data_size size) (string_of_src isize shift)
          (string_of_dst isize destination)
    | Shr { size; shift; destination } ->
      let isize = (intsize size) in
        sprintf "shr%s %s, %s" (string_of_int_data_size size) (string_of_src isize shift)
          (string_of_dst isize destination)
    | IMul { size; source; destination } ->
        sprintf "imul%s %s, %s" (string_of_data_size size)
          (string_of_src size source)
          (string_of_register size destination)
    | Fdiv {size; destination; source} ->
      sprintf "div%s %s, %s" (string_of_float_data_size size)           
      (string_of_float_register source)
      (string_of_float_register destination)
    | IDivl { size; divisor } ->
      let isize = (intsize size) in
        sprintf "idiv%s %s" (string_of_int_data_size size) (string_of_src isize divisor)
    | Div { size; divisor } ->
      let isize = (intsize size) in
        sprintf "div%s %s" (string_of_int_data_size size) (string_of_src isize divisor)
    | Push { size; source } ->
        sprintf "push%s %s" (string_of_int_data_size size) (string_of_src (intsize size) source)
    | Pop { size; destination } ->
        sprintf "pop%s %s" (string_of_int_data_size size)
          (string_of_dst (intsize size) destination)
    | Cmp { size; lhs; rhs } ->
        sprintf "cmp%s %s, %s" (string_of_data_size size) (string_of_src size lhs)
          (string_of_src size rhs)
    | Jmp { cc; where } ->
        sprintf "j%s %s"
          (string_of_condition_code_opt cc)
          (string_of_jmp_operande where)
    | Call { what } -> sprintf "call %s" (string_of_jmp_operande what)
    | Syscall -> "syscall"
    | Ret -> "ret"
    | Cltd -> "cltd"
    | Cqto -> "cqto"

  let string_of_raw_line = function
    | Label s -> s ^ ":"
    | Instruction s -> "\t" ^ string_of_instruction s
    | Line_Com (Comment s) -> "\t" ^ AsmSpec.comment_prefix ^ " " ^ s
    | Directive d -> "\t." ^ d

  let string_asm_const_decl { asm_const_name; value } =
    match value with
    | `IntVal (size, int_value) ->
        sprintf "\n\t%s\n%s:\n\t.%s %s"
          (AsmSpec.constant_directives asm_const_name value
          |> String.concat "\n\t")
          asm_const_name
          (AsmSpec.size_directive_of_size size)
          (sprintf "%Ld" int_value)
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

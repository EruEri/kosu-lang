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

module X86Program = Common.AsmProgram(X86_64Core.Instruction)
open X86Program

module Make(AsmSpec: Common.AsmSpecification) = struct

let string_of_data_size = function
| B -> "b"
| W -> "w"
| L -> "l"
| Q -> "q"

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
let string_of_register register = 
  let str_raw_reg = string_of_raw_register register.reg in
  match register.size with
  | Q -> str_raw_reg
  | L ->
    if Register.is_numerical_register register.reg
      then Printf.sprintf "%sd" str_raw_reg
    else str_raw_reg |> String.map (fun c -> if c = 'r' then 'e' else c)
  |  W ->  
    if Register.is_numerical_register register.reg
      then Printf.sprintf "%sw" str_raw_reg
    else String.sub str_raw_reg 1 2
  | B -> 
    if Register.is_numerical_register register.reg
      then Printf.sprintf "%sb" str_raw_reg
    else 
      match str_raw_reg with 
      | "rax" -> "al" 
      | "rbx" -> "bl" 
      | "rcx" -> "cl" 
      | "rdx" -> "dl"
      | other -> Printf.sprintf "%sl" (String.sub other 1 2)

let string_of_register register = Printf.sprintf "%%%s" (string_of_register register)


let string_of_address_offset = function
| Addr_label (label, offset) -> Printf.sprintf "%s%s" label (if offset = 0L then "" else Printf.sprintf "+%Lu" offset)
| Offset offset -> Printf.sprintf "%Ld" offset

let string_of_address {offset; base; index; scale} = 
  Printf.sprintf "%s(%s%s%s)"
  (string_of_address_offset offset)
  (string_of_register base)
  (index |> Option.map (fun reg -> sprintf ", %s" (string_of_register reg) ) |> Option.value ~default:"")
  (if scale = 1 then "" else sprintf "%u" scale)

let string_of_dst: dst -> string = function
| `Register reg -> string_of_register reg
| `Address addr -> string_of_address addr

let string_of_src: src -> string = function
| #dst as dst -> string_of_dst dst
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
| `Register reg -> sprintf "*%s" (string_of_register reg)
| `Label l -> l

let string_of_instruction = function
| Mov { size; source; destination } -> 
  sprintf "mov%s %s, %s" (string_of_data_size size) (string_of_src source) (string_of_dst destination)
| Set {cc; register} ->
  sprintf "set%s %s" (string_of_condition_code cc) (string_of_register register)
| Lea {size; source; destination} ->
  sprintf "lea%s %s, %s" (string_of_data_size size) (string_of_address source) (string_of_register destination)
| Neg {size; source} -> 
  sprintf "neg%s %s" (string_of_data_size size) (string_of_register source)
  | Not {size; source} -> 
    sprintf "not%s %s" (string_of_data_size size) (string_of_register source)

| Add {size; source; destination} ->
  sprintf "add%s %s, %s" (string_of_data_size size) (string_of_src source) (string_of_dst destination)
  | Sub {size; source; destination} ->
    sprintf "sub%s %s, %s" (string_of_data_size size) (string_of_src source) (string_of_dst destination)
      | Xor {size; source; destination} ->
        sprintf "xor%s %s, %s" (string_of_data_size size) (string_of_src source) (string_of_dst destination)
        | And {size; source; destination} ->
          sprintf "and%s %s, %s" (string_of_data_size size) (string_of_src source) (string_of_dst destination)
          | Or {size; source; destination} ->
            sprintf "or%s %s, %s" (string_of_data_size size) (string_of_src source) (string_of_dst destination)
            | Sal {size; shift; destination} ->
              sprintf "sal%s %s, %s" (string_of_data_size size) (string_of_src shift) (string_of_dst destination)
              | Sar {size; shift; destination} ->
                sprintf "sar%s %s, %s" (string_of_data_size size) (string_of_src shift) (string_of_dst destination)
                | Shr {size; shift; destination} ->
                  sprintf "shr%s %s, %s" (string_of_data_size size) (string_of_src shift) (string_of_dst destination)
                  | IMul {size; source; destination} ->
                    sprintf "imul%s %s, %s" (string_of_data_size size) (string_of_src source) (string_of_register destination)
                    | IDivl {size; divisor} ->
                      sprintf "idiv%s %s" (string_of_data_size size) (string_of_src divisor)
                      | Div {size; divisor} ->
                        sprintf "div%s %s" (string_of_data_size size) (string_of_src divisor)
                        | Push {size; source} ->
                          sprintf "push%s %s" (string_of_data_size size) (string_of_src source)
                          | Pop {size; destination} ->
                            sprintf "pop%s %s" (string_of_data_size size) (string_of_dst destination)
                            | Cmp {size; lhs; rhs} -> 
                              sprintf "cmp%s %s, %s" (string_of_data_size size) (string_of_src lhs) (string_of_src rhs)
                              | Jmp {cc; where} ->
                                sprintf "j%s %s" (string_of_condition_code_opt cc) (string_of_jmp_operande where)
                                | Call {what} ->
                                  sprintf "call %s" (string_of_jmp_operande what)
                                  | Syscall -> "syscall"
                                  | Ret -> "ret"
                                  | Cltd -> "cltd"
                                  | Cqto -> "cqto"

let string_of_raw_line = function
  | Label s -> s ^ ":"
  | Instruction s -> "\t" ^ string_of_instruction s
  | Line_Com (Comment s) -> "\t"^ AsmSpec.comment_prefix ^ " " ^ s
  | Directive d -> "\t." ^ d





  let string_asm_const_decl { asm_const_name; value } =
    match value with
    | `IntVal (size, value) ->
        sprintf "\n\t.global %s\n\t.align %u\n%s:\n\t.%s %s"
        asm_const_name
        (KosuFrontend.Ast.Isize.size_of_isize size / 8)
        (asm_const_name) 
        (AsmSpec.size_directive_of_size size)
        (sprintf "%Ld" value)
    | `StrVal s ->
        sprintf "\n\t.global %s\n\t.align 8\n%s:\n\t%s \"%s\""
        asm_const_name
        asm_const_name
        (AsmSpec.string_litteral_directive)
        s

let string_of_asm_function { asm_name; asm_body } =
  sprintf "\t.globl %s\n\t%s\n%s:\n%s" asm_name AsmSpec.p2align_function asm_name
    (asm_body |> List.map string_of_raw_line |> String.concat "\n")

let string_of_asm_node = function
  | Afunction f -> string_of_asm_function f
  | AConst c -> string_asm_const_decl c
end


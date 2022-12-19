open Aarch64Core
open Aarch64Core.Register
open Aarch64Core.Instruction
open Printf

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

let string_of_condition_code = function
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
| Register32 reg32 -> string_of_32bits_reg reg32
| Register64 reg64 -> string_of_64bits_reg reg64

let string_of_src = function
| `ILitteral int64 -> Printf.sprintf "#%Ld" int64
| `Register reg -> string_of_register reg
| `Label label -> label

let string_of_adressage adress_mode {base; offset} = 
  match adress_mode with 
  | Immediat -> 
    sprintf "[%s%s]" 
    (string_of_register base) 
    ((if offset = 0L then None else Some offset)  |> Option.map (sprintf ", #%Ld") |> Option.value ~default:"")
  | Prefix ->
    sprintf "[%s%s]!"
    (string_of_register base) 
    ((if offset = 0L then None else Some offset)  |> Option.map (sprintf ", #%Ld") |> Option.value ~default:"")
  | Postfix -> 
    sprintf "[%s]%s"
    (string_of_register base) 
    ((if offset = 0L then None else Some offset)  |> Option.map (sprintf ", #%Ld") |> Option.value ~default:"")

let string_of_instruction = function
| Mov {destination; flexsec_operand} -> 
  sprintf "mov %s, %s" (string_of_register destination) (string_of_src flexsec_operand)
| Not {destination; source} ->
  sprintf "mvn %s, %s" (string_of_register destination) (string_of_src source)
| Neg {destination; source} -> 
  sprintf "neg %s, %s" (string_of_register destination) (string_of_register source)
| ADD {destination; operand1; operand2; offset} ->
  sprintf "add %s, %s, %s%s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2) (if offset then "@PAGEOFF" else "")
| ADDS {destination; operand1; operand2} ->
sprintf "adds %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2)
| SUB {destination; operand1; operand2} ->
sprintf "sub %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2)
| SUBS {destination; operand1; operand2} ->
sprintf "subs %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2)
| MUL {destination; operand1; operand2} ->
sprintf "mul %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_register operand2)
| UDIV {destination; operand1; operand2} ->
  sprintf "udiv %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_register operand2)
| SDIV {destination; operand1; operand2} ->
sprintf "sdiv %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_register operand2)
| ASL {destination; operand1; operand2} ->
sprintf "asl %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2)
| ASR {destination; operand1; operand2} ->
sprintf "asr %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2)
| CSINC {destination; operand1; operand2; condition} ->
sprintf "csinc %s, %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_register operand2) (string_of_condition_code condition)
| AND {destination; operand1; operand2} ->
  sprintf "and %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2)
| ORR {destination; operand1; operand2} ->
sprintf "orr %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2)
| EOR {destination; operand1; operand2} ->
sprintf "eor %s, %s, %s" (string_of_register destination) (string_of_register operand1) (string_of_src operand2)
| CMP {operand1; operand2} ->
  sprintf "cmp %s, %s" (string_of_register operand1) (string_of_src operand2)
| LDR {data_size; destination; adress_src; adress_mode} -> 
sprintf "ldr%s %s , %s" 
(data_size |> Option.map string_of_data_size |> Option.value ~default:"")
(string_of_register destination)
(string_of_adressage adress_mode adress_src)
| STR {data_size; source; adress; adress_mode} -> 
sprintf "str%s %s , %s" 
(data_size |> Option.map ( fun ds -> string_of_data_size @@ unsigned_data_size ds) |> Option.value ~default:"")
(string_of_register source)
(string_of_adressage adress_mode adress)
| STP {x1; x2; address; adress_mode} -> 
  sprintf "stp %s, %s, %s" (string_of_register x1) (string_of_register x2) (string_of_adressage adress_mode address)
| LDP {x1; x2; address; adress_mode} -> 
    sprintf "ldp %s, %s, %s" (string_of_register x1) (string_of_register x2) (string_of_adressage adress_mode address)
| ADRP { dst; label } ->
  sprintf "adrp %s, %s@PAGE" (string_of_register dst) label
| B { cc; label} -> 
  sprintf "b%s %s" 
  (cc |> Option.map ( fun cc -> sprintf ".%s" (string_of_condition_code cc)) |> Option.value ~default:"" )
  label
| BL { cc; label} -> 
  sprintf "bl%s %s" 
  (cc |> Option.map ( fun cc -> sprintf ".%s" (string_of_condition_code cc)) |> Option.value ~default:"" )
  label
| BR { cc; reg} -> 
  sprintf "br%s %s"
  (cc |> Option.map ( fun cc -> sprintf ".%s" (string_of_condition_code cc)) |> Option.value ~default:"" )
  (string_of_register reg)
| BLR { cc; reg} -> 
    sprintf "blr%s %s"
    (cc |> Option.map ( fun cc -> sprintf ".%s" (string_of_condition_code cc)) |> Option.value ~default:"" )
    (string_of_register reg)
| SVC code -> sprintf "svc %Ld" code
| RET -> "ret"

let string_of_raw_line = function
| Label s -> s^":" 
| Instruction s -> "\t"^(string_of_instruction s)
| Line_Com (Comment s) -> "\t;"^(s)
| Directive (d) -> "\t."^(d)

let size_directive_of_size = let open KosuFrontend.Ast in function
| I8 -> "bytes"
| I16 -> "short"
| I32 -> "long"
| I64 -> "quad"

let string_asm_const_decl {asm_const_name; value = `IntVal (size, value)} = 
  sprintf "%s:\n\t%s %s" asm_const_name (size_directive_of_size size) (sprintf "%LX" value)


let string_of_asm_function {asm_name; asm_body} = 
  sprintf "\t.globl %s\n\t%s\n%s:\n%s" (asm_name) (".p2align	2") asm_name (asm_body |> List.map string_of_raw_line |> String.concat "\n")

let string_of_asm_node = function
| Afunction f -> string_of_asm_function f
| AConst c -> string_asm_const_decl c
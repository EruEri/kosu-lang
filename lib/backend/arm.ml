open Common
open KosuIrTyped.Asttyped
(* open Kosu_frontend.Ast *)

(* for i in $(seq 1 16); do echo "| R$i" >> lib/backend/arm.ml; done *)
type register =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8 (* XR *)
  | R9
  | R10
  | R11 
  | R12 
  | R13 
  | R14 
  | R15 
  | R16
  | R29
  | R30
  | SP

let stack_pointer = SP
let syscall_code_register = R0

let all_register =
  [
    R0;
    R1;
    R2;
    R3;
    R4;
    R5;
    R6;
    R7;
    R8;
    R9;
    R10;
    R11;
    R12;
    R13;
    R14;
    R15;
    R16;
    R29;
    R30;
    SP;
  ]

type immediat = Litteral of int64 | Label of string
type flexsec_operand = [ `Litteral of int | `Register of register ]
type data_size = B | SB | H | SH

type adress_mode =
  | Immediat (* out = *intptr; *)
  | Prefix (* out = *(++intptr);*)
  | Postfix (* out = *(intptr++);*)

type condition_code =
  | EQ  (** Equal *)
  | NE  (** Not Equal *)
  | CS  (** Carry Set *)
  | CC  (** Carry clear *)
  | MI  (** Minus / Negative *)
  | PL  (** Plus , Posivite ./ Zero *)
  | VS  (** Overflow *)
  | VC  (** No overflow*)
  | HI  (** Unsigned higher*)
  | LS  (** Unsigned lower or same *)
  | GE  (** Signed greater than or equal*)
  | LT  (** Signed less than*)
  | GT  (** Signed greather than *)
  | LE  (** Signed less than or equal *)
  | AL  (** Always*)

type instruction =
  | Mov of {
      cc : condition_code option;
      destination : register;
      (* Careful int max INT16 *)
      flexsec_operand : flexsec_operand;
    }
  | Add of {
      cc : condition_code option;
      destination : register;
      operand1 : register;
      (* Int12 litteral oprand*)
      operand2 : flexsec_operand;
    }
  | SUB of {
      cc : condition_code option;
      destination : register;
      operand1 : register;
      (* Int12 litteral oprand*)
      operand2 : flexsec_operand;
    }
  | MUL of {
      cc : condition_code option;
      destination : register;
      operand1 : register;
      operand2 : register;
    }
  | ASL of {
      cc : condition_code option;
      destination : register;
      operand1 : register;
      (* LIteral range [0-31] *)
      operand2 : flexsec_operand;
    }
  | ASR of {
      cc : condition_code option;
      destination : register;
      operand1 : register;
      (* LIteral range [0-31] *)
      operand2 : flexsec_operand;
    }
  | CMP of {
      cc : condition_code option;
      operand1 : register;
      operand2 : flexsec_operand;
    }
  (* Bitwise And*)
  | AND of {
      cc : condition_code option;
      destination : register;
      operand1 : register;
      operand2 : flexsec_operand;
    }
  (* Bitwise OR*)
  | ORR of {
      cc : condition_code option;
      destination : register;
      operand1 : register;
      operand2 : flexsec_operand;
    }
  (* Bitwise XOR*)
  | EOR of {
      cc : condition_code option;
      destination : register;
      operand1 : register;
      operand2 : flexsec_operand;
    }
  | LDR of {
      data_size : data_size option;
      cc : condition_code option;
      destination : register;
      source : register;
      offset : int;
      adress_mode : adress_mode;
    }
  | STR of {
      data_size : data_size option;
      cc : condition_code option;
      destination : register;
      source : register;
      offset : int;
      adress_mode : adress_mode;
    }
  | B of { condition_code : condition_code option; label : label }
  | BL of { condition_code : condition_code option; label : label }
  | SVC

let syscall_instruction = SVC

module IStack = struct
  type info = (string * rktype * int) list
  type t = instruction list
  type stacks = info * t

  let empty : stacks = ([], [])

  let push_instruction (inst : instruction) (list, stack) : stacks =
    (list, inst :: stack)

  let current_offset (stacks : stacks) =
    stacks |> fst |> List.fold_left (fun acc (_, _, off) -> acc + off) 0

  let padding_needed rktype rprogram stacks =
    let aligmentof =
      KosuIrTyped.Asttyped.Sizeof.alignmentof rprogram rktype |> Int64.to_int
    in
    let current_offset = current_offset stacks in
    (aligmentof - (current_offset mod aligmentof)) mod aligmentof
end

(**
@return Whenether or not the value can be store 
directly from the register or it needs to be fetched since non trival size are passed by reference before beeing cloned into the stack    
*)
let does_it_hold_in_register program rktype =
  let sizeof = KosuIrTyped.Asttyped.Sizeof.sizeof program rktype in
  sizeof <= 64L && (sizeof = 8L || sizeof = 16L || sizeof = 32L || sizeof = 64L)

let instructions_of_function generics program function_decl =
  let _stacks = IStack.empty in
  let combined =
    generics
    |> List.combine function_decl.generics
    |> List.to_seq |> Hashtbl.of_seq
  in
  function_decl.rparameters
  |> List.map (fun (_field, rktype) ->
         let rktype = RType.remap_generic_ktype combined rktype in
         if does_it_hold_in_register program rktype then ())

module Arm64ABI = struct
  type register =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8 (* XR *)
  | R9
  | R10
  | R11 
  | R12 
  | R13 
  | R14 
  | R15 
  | R16
  | R29
  | R30
  | SP

  type adress_mode =
    | Immediat (* out = *intptr; *)
    | Prefix (* out = *(++intptr);*)
    | Postfix (* out = *(intptr++);*)

  type data_size = B | SB | H | SH

  type condition_code =
  | EQ  (** Equal *)
  | NE  (** Not Equal *)
  | CS  (** Carry Set *)
  | CC  (** Carry clear *)
  | MI  (** Minus / Negative *)
  | PL  (** Plus , Posivite ./ Zero *)
  | VS  (** Overflow *)
  | VC  (** No overflow*)
  | HI  (** Unsigned higher*)
  | LS  (** Unsigned lower or same *)
  | GE  (** Signed greater than or equal*)
  | LT  (** Signed less than*)
  | GT  (** Signed greather than *)
  | LE  (** Signed less than or equal *)
  | AL  (** Always*)

  type dst = register
  type src = [ `Litteral of int | `Label of string | `Register of register ]
  type register_size = [`x86 | `x32]
  type stack_parameter_order = Ordered | Reversed

  type address = {
    offset : Common.imm option;
    base : register option;
    idx : register option;
    scale : Common.scale;
  }

  let stack_pointer_align = 16

  let stack_parameter_order = Ordered

  let string_of_register ?(size = `x64) register = 
    let prefix = match size with `x64 -> 'x' | `x32 -> 'w' in
    match register with
    | R0 -> Printf.sprintf "%c0" prefix
    | R1 -> Printf.sprintf "%c1" prefix
    | R2 -> Printf.sprintf "%c2" prefix
    | R3 -> Printf.sprintf "%c3" prefix
    | R4 -> Printf.sprintf "%c4" prefix
    | R5 -> Printf.sprintf "%c5" prefix
    | R6 -> Printf.sprintf "%c6" prefix
    | R7 -> Printf.sprintf "%c7" prefix
    | R8 -> Printf.sprintf "%c8" prefix
    | R9 -> Printf.sprintf "%c9" prefix
    | R10 -> Printf.sprintf "%c10" prefix
    | R11 -> Printf.sprintf "%c11" prefix
    | R12 -> Printf.sprintf "%c12" prefix
    | R13 -> Printf.sprintf "%c13" prefix
    | R14 -> Printf.sprintf "%c14" prefix
    | R15 -> Printf.sprintf "%c15" prefix
    | R16 -> Printf.sprintf "%c16" prefix
    | R29 -> Printf.sprintf "%c29" prefix
    | R30 -> Printf.sprintf "%c30" prefix
    | SP -> Printf.sprintf "sp"

    let stack_pointer = SP
    let argument_registers = [R0; R1; R2; R3; R4; R5; R6; R7]
    let return_register = R0
    let caller_save = [R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15 ]
    let callee_save = [R30; R29; R16]

    let create_adress
    ?(offset = None)
    ?(base =  None)
    ?(idx = None)
    ?(scale = `One)
    () = {
      offset; base; idx; scale
    }
end

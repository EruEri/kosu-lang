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
  | R8
  | R9
  | R10
  | R11  (** Frame Pointer *)
  | R12  (** Intre Procedural call*)
  | SP  (**SP *)
  | R14 (* Link register*)
  | R15  (** Program Counter (PC)*)
  | R16
  | R29
  | R30

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
    SP;
    R14;
    R15;
    R16;
    R29;
    R30;
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

module ArmABI = struct
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
    | R11  (** Frame Pointer *)
    | R12  (** Intre Procedural call*)
    | SP  (**SP *)
    | R14 (* Link register*)
    | R15  (** Program Counter (PC)*)
    | R16
    | R29
    | R30
end

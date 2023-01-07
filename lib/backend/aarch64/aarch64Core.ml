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

module IdVar = struct
  type t = string * KosuIrTyped.Asttyped.rktype

  let compare = compare
end

module IdVarMap = Map.Make (IdVar)

module Immediat = struct

  let mask_6_8bytes = 0xFFFF_0000_0000_0000L
  let mask_4_6bytes = 0x0000_FFFF_0000_0000L
  let mask_2_4bytes = 0x0000_0000_FFFF_0000L
  let mask_0_2bytes = 0x0000_0000_0000_FFFFL
  let max_16bits_immediat = 65535L
  let min_16bits_immediat = -65535L

  let is_direct_immediat int16 = 
  int16 >= min_16bits_immediat && int16 <= max_16bits_immediat

  let split n = 
    let int16 = Int64.logand mask_0_2bytes n in
    let int32 = Int64.logand mask_2_4bytes n in
    let int48 = Int64.logand mask_4_6bytes n in
    let int64 = Int64.logand mask_6_8bytes n in
    (Int64.shift_right_logical int64 32, Int64.shift_right_logical int48 16 , Int64.shift_right_logical int32 8 , int16)
end

type adress_mode =
| Immediat (* out = *intptr; *)
| Prefix (* out = *(++intptr);*)
| Postfix (* out = *(intptr++);*)

type data_size = B | SB | H | SH

type register_size = 
| SReg32
| SReg64

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

module Register = struct 
  type registerf64b =
  | D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8 (* XR *)
  | D9
  | D10
  | D11
  | D12
  | D13
  | D14
  | D15
  | D16
  | D29
  | D30
  | DZR

  type register64b =
  | X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8 (* XR *)
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X29
  | X30
  | XZR
  | SP
  
  type register32b = 
  | W0
  | W1
  | W2
  | W3
  | W4
  | W5
  | W6
  | W7
  | W8 (* XR *)
  | W9
  | W10
  | W11
  | W12
  | W13
  | W14
  | W15
  | W16
  | W29
  | W30
  | WZR
  | WSP
  
  
  type register = 
  | FRegister64 of registerf64b
  | Register64 of register64b
  | Register32 of register32b

  let reg64_of_32 = function
  | W0 -> X0
  | W1 -> X1
  | W2 -> X2
  | W3 -> X3
  | W4 -> X4
  | W5 -> X5
  | W6 -> X6
  | W7 -> X7
  | W8 -> X8
  | W9 -> X9
  | W10 -> X10
  | W11 -> X11
  | W12 -> X12
  | W13 -> X13
  | W14 -> X14
  | W15 -> X15
  | W16 -> X16
  | W29 -> X29 
  | W30 -> X30 
  | WZR -> XZR 
  | WSP -> SP

  let reg32_of_64 = function
  | X0 -> W0
  | X1 -> W1
  | X2 -> W2
  | X3 -> W3
  | X4 -> W4
  | X5 -> W5
  | X6 -> W6
  | X7 -> W7
  | X8 -> W8
  | X9 -> W9
  | X10 -> W10
  | X11 -> W11
  | X12 -> W12
  | X13 -> W13
  | X14 -> W14
  | X15 -> W15
  | X16 -> W16
  | X29 -> W29 
  | X30 -> W30 
  | XZR -> WZR 
  | SP -> WSP

  let float64reg_of_64bitsreg = function
  | X0 -> D0
  | X1 -> D1
  | X2 -> D2
  | X3 -> D3
  | X4 -> D4
  | X5 -> D5
  | X6 -> D6
  | X7 -> D7
  | X8 -> D8
  | X9 -> D9
  | X10 -> D10
  | X11 -> D11
  | X12 -> D12
  | X13 -> D13
  | X14 -> D14
  | X15 -> D15
  | X16 -> D16
  | X29 -> D29
  | X30 -> D30
  | XZR -> DZR
  | SP -> failwith "SP has not float equivalent" 

  let argument_registers = [ 
    Register64 X0; 
    Register64 X1; 
    Register64 X2; 
    Register64 X3; 
    Register64 X4; 
    Register64 X5; 
    Register64 X6; 
    Register64 X7 
  ]
  
  let syscall_arguments_register = [
    Register64 X0; 
    Register64 X1; 
    Register64 X2; 
    Register64 X3; 
    Register64 X4; 
    Register64 X5; 
  ]
  
  let frame_registers = [
     Register64 X29; 
     Register64 X30
  ]


  let return_register_ktype ~float = function
  | _ when float -> FRegister64 D0 
  | 1L | 2L | 4L -> Register32 W0
  | 8L -> Register64 X0
  | _ -> Register64 X8

  let is_f64_reg = function
  | FRegister64 _ -> true
  | _ -> false

  let are_aliased src dst = 
    match src, dst with
    | Register64 s, Register32 z | Register32 z, Register64 s -> 
      let x_version = reg64_of_32 z in
      s = x_version
    | _ -> false 
  
  let reg_of_32 reg = Register32 reg
  let reg_of_64 reg = Register64 reg

  let to_64bits = function
  | Register32 reg -> Register64 (reg64_of_32 reg)
  | (Register64 _) as t -> t
  | FRegister64 _ as t -> t

  let to_32bits = function
  | FRegister64 _ as t -> t
  | Register64 reg -> Register32 (reg32_of_64 reg)
  | (Register32 _) as t -> t

  let to_64fbits = function
  | FRegister64 _ as t -> t 
  | Register32 reg -> FRegister64 (reg |> reg64_of_32 |> float64reg_of_64bitsreg)
  | Register64 reg64 -> FRegister64 (float64reg_of_64bitsreg reg64)

  let size_of_ktype_size s = if s <= 4L then SReg32 else SReg64
  let size_of_reg = function
  | Register32 _ -> SReg32
  | Register64 _ | FRegister64 _ -> SReg64

  let reg_of_size size reg =
    match size with
    | SReg32 -> to_32bits reg
    | SReg64 -> to_64bits reg

  let xr = Register64 X8
  let x29 = Register64 X29
  let ftmp64reg = FRegister64 D8

  let tmp64reg = Register64 X8
  let tmp32reg = Register32 W8

  let ftmp64reg_2 = FRegister64 D9
  let tmp64reg_2 = Register64 X9
  let tmp32reg_2 = Register32 W9


  let ftmp64reg_3 = FRegister64 D10
  let tmp32reg_3 = Register32 W10
  let tmp64reg_3 = Register64 X10

  let ftmp64reg_4 = FRegister64 D11
  let tmp32reg_4 = Register32 W11
  let tmp64reg_4 = Register64 X11

  let tmpreg_of_size = fun size -> if size <= 4L then tmp32reg else tmp64reg
  let tmpreg_of_size_2 = fun size -> if size <= 4L then tmp32reg_2 else tmp64reg_2

  let tmpreg_of_size_3 = fun size -> if size <= 4L then tmp32reg_3 else tmp64reg_3
  let tmpreg_of_size_4 = fun size -> if size <= 4L then tmp32reg_4 else tmp64reg_4

  let tmpreg_of_ktype rprogram ktype = 
    if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then ftmp64reg 
    else
    let size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
    tmpreg_of_size size

  let tmpreg_of_ktype_2 rprogram ktype = 
    if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then ftmp64reg_2 
    else
    let size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
    tmpreg_of_size_2 size

    let tmpreg_of_ktype_3 rprogram ktype = 
      if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then ftmp64reg_3 
      else
      let size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
      tmpreg_of_size_3 size

    let tmpreg_of_ktype_4 rprogram ktype = 
      if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then ftmp64reg_4 
      else
      let size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
      tmpreg_of_size_4 size 
end

open Register
type src = [ `ILitteral of int64 | `F64Litteral of float | `Register of register | `Label of string]
type address = {
  base : register;
  offset : int64;
}

let create_adress ?(offset = 0L) base = 
  {base; offset}

let increment_adress off adress = {
  adress with offset = Int64.sub adress.offset off
}

let asm_const_name current_module const_name = 
  Printf.sprintf "_%s_%s" const_name (current_module |> String.map (fun c -> if c = ':' then '_' else c))

module Instruction = struct
  
  type shift = 
  | SH16
  | SH32
  | SH48


  type instruction =
  | Mov of {
   
    destination : Register.register;
    (* Careful int max INT16 *)
    flexsec_operand : src;
  }
  | Movk of {
    destination : Register.register;
    operand : src;
    shift: shift option
  }
| Not of {
 
  destination : Register.register;
  source : src
}
| Neg of {
 
  destination : Register.register;
  source : Register.register
}
| ADD of {
   
    destination : Register.register;
    operand1 : Register.register;
    (* Int12 litteral oprand*)
    operand2 : src;
    offset: bool;
  }
| ADDS of {
   
  destination : Register.register;
  operand1 : Register.register;
  (* Int12 litteral oprand*)
  operand2 : src;
}
| SUB of {
   
    destination : Register.register;
    operand1 : Register.register;
    (* Int12 litteral oprand*)
    operand2 : src;
  }
| SUBS of {
   
    destination : Register.register;
    operand1 : Register.register;
    (* Int12 litteral oprand*)
    operand2 : src;
  }
| MUL of {
   
    destination : Register.register;
    operand1 : Register.register;
    operand2 : Register.register;
  }
| UDIV of {
 
  destination : Register.register;
  operand1 : Register.register;
  operand2 : Register.register;
}
| SDIV of {
 
  destination : Register.register;
  operand1 : Register.register;
  operand2 : Register.register;
}
| ASL of {
   
    destination : Register.register;
    operand1 : Register.register;
    (* LIteral range [0-31] *)
    operand2 : src;
  }
| ASR of {
   
    destination : Register.register;
    operand1 : Register.register;
    (* LIteral range [0-31] *)
    operand2 : src;
  }
| CSINC of {
  destination: Register.register;
  operand1: Register.register;
  operand2: Register.register;
  condition: condition_code
}
| CMP of {
    operand1 : Register.register;
    operand2 : src;
  }
(* Bitwise And*)
| AND of {
   
    destination : Register.register;
    operand1 : Register.register;
    operand2 : src;
  }
(* Bitwise OR*)
| ORR of {
   
    destination : Register.register;
    operand1 : Register.register;
    operand2 : src;
  }
(* Bitwise XOR*)
| EOR of {
   
    destination : Register.register;
    operand1 : Register.register;
    operand2 : src;
  }
| LDR of {
    data_size : data_size option;
   
    destination : Register.register;
    adress_src : address;
    adress_mode : adress_mode;
  }
| LDUR of {
  data_size : data_size option;
   
  destination : Register.register;
  adress_src : address;
  adress_mode : adress_mode;
}
| STR of {
    data_size : data_size option;
   
    source : Register.register;
    adress : address;
    adress_mode : adress_mode;
  }
| STP of {
  x1: Register.register;
  x2: Register.register;
  address: address;
  adress_mode : adress_mode;
}
| ADRP of {
  dst: Register.register;
  label: string;
}
| LDP of {
  x1: Register.register;
  x2: Register.register;
  address: address;
  adress_mode : adress_mode;
}
| B of { cc : condition_code option; label : string }
| BL of { cc : condition_code option; label : string }
| BR of { cc : condition_code option; reg : Register.register }
| BLR of { cc : condition_code option; reg : Register.register }
| SVC
| RET

type comment = 
| Comment of string

type raw_line =
| Instruction of instruction
| Directive of string
| Label of string
| Line_Com of comment

type line = (raw_line * comment option)

let minstruction ?(_lcomm = "") instr = Instruction instr

let load_label ?module_path label register = 
  let label = module_path |> Option.map (fun mp -> asm_const_name mp label) |> Option.value ~default:label in
  let register = to_64bits register in
  let load =  Instruction ( ADRP {dst = register; label}) in
  let add = Instruction ( ADD {destination = register; operand1 = register; operand2 = `Label label; offset = true;} ) in
  load::add::[]

  let rec copy_large adress_str base_src_reg size = 
    if size < 0L
      then failwith "Negive size to copy"
  else if size = 0L
    then []
  else if size < 2L && size >= 1L
    then [
      Instruction( LDR {
        data_size = Some B;
        destination = reg_of_32 W10;
        adress_src = create_adress (base_src_reg);
        adress_mode = Immediat
      });
      Instruction (STR {
        data_size = Some B;
        source = reg_of_32 W10;
        adress = adress_str;
        adress_mode = Immediat
      }) ;
      Instruction (ADD {
        destination = base_src_reg;
        operand1 = base_src_reg;
        operand2 = `ILitteral 1L;
        offset = false;
      })
    ] @ (copy_large (increment_adress (Int64.neg 2L) adress_str) base_src_reg (Int64.sub size 1L))
  else if size < 4L && size >= 2L
    then [
      Instruction (LDR {
        data_size = Some H;
        destination = reg_of_32 W10;
        adress_src = create_adress base_src_reg;
        adress_mode = Immediat
      });
      Instruction ( STR {
        data_size = Some H;
        source = reg_of_32 W10;
        adress = adress_str;
        adress_mode = Immediat
      }) ;
      Instruction ( ADD {
        destination = base_src_reg;
        operand1 = base_src_reg;
        operand2 = `ILitteral 2L;
        offset = false;
      })
    ] @ (copy_large (increment_adress (Int64.neg 2L) adress_str) base_src_reg (Int64.sub size 2L))
  else if size < 8L && size >= 4L
    then [
      Instruction ( LDR {
        data_size = None;
        destination = reg_of_32 W10;
        adress_src = create_adress  (base_src_reg);
        adress_mode = Immediat
      });
      Instruction ( STR {
        data_size = None;
        source = reg_of_32 W10;
        adress = adress_str;
        adress_mode = Immediat
      }) ;
      Instruction ( ADD {
        destination = base_src_reg;
        operand1 = base_src_reg;
        offset = false;
        operand2 = `ILitteral 4L
      })
    ] @ (copy_large (increment_adress (Int64.neg 4L) adress_str) base_src_reg (Int64.sub size 4L))
      else (*size >= 8L*) 
        [
          Instruction ( LDR {
            data_size = None;
            destination = reg_of_64 X10;
            adress_src = create_adress base_src_reg;
            adress_mode = Immediat
          });
          Instruction ( STR {
            data_size = None;
            source = reg_of_64 X10;
            adress = adress_str;
            adress_mode = Immediat
          }) ;
          Instruction ( ADD {
            destination = base_src_reg;
            operand1 = base_src_reg;
            offset = false;
            operand2 = `ILitteral 8L
          })
        ] @ (copy_large (increment_adress (Int64.neg 8L) adress_str) base_src_reg (Int64.sub size 8L))
  ;;
  
  
  let copy_from_reg register (adress: address) ktype rprogram =
    let size =  KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
    match size with
    | 1L ->
      let data_size =  Some (if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then (B: data_size) else SB) in
      [Instruction (STR {data_size; source = to_32bits register; adress; adress_mode = Immediat})]
    | 2L -> let data_size = Some( if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then H else SH) in
      [Instruction (STR {data_size; source = to_32bits register; adress; adress_mode = Immediat})]
    | 4L -> 
      [Instruction (STR {data_size = None; source = to_32bits register; adress; adress_mode = Immediat})]
    | 8L ->
      [Instruction (STR {data_size = None; source = to_64bits register; adress; adress_mode = Immediat})]
    | _ -> copy_large adress register size 
  
  let load_register register (address: address) ktype ktype_size = match ktype_size with
  | 1L ->
    let data_size =  Some (if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then (B: data_size) else SB) in
    [Instruction (LDR {data_size; destination = to_32bits register; adress_src = address; adress_mode = Immediat})]
  | 2L -> let data_size = Some( if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then H else SH) in
    [Instruction (LDR {data_size; destination = to_32bits register; adress_src = address; adress_mode = Immediat})]
  | 4L -> 
    [Instruction (LDR {data_size = None; destination = to_32bits register; adress_src = address; adress_mode = Immediat})]
  | 8L ->
    [Instruction (LDR {data_size = None; destination = to_64bits register; adress_src = address; adress_mode = Immediat})]
  | _ -> []

end

let is_register_size = function
| 1L | 2L | 4L | 8L -> true
| _ -> false

let compute_data_size ktype = function
| 1L -> Some (if not @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then SB else B)
| 2L -> Some (if not @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then SH else H)
| _ -> None

let unsigned_data_size = function
| SH -> H
| SB -> B
| t -> t



module FrameManager = struct
  open Instruction
  open Register
  open KosuIrTyped.Asttyconvert.Sizeof
  type frame_desc = {
  stack_param_count : int;
  locals_space : int64;
  need_xr: bool;
  stack_map : address IdVarMap.t;
  discarded_values: (string * KosuIrTyped.Asttyped.rktype) list
}

let indirect_return_var = "@xreturn"
let indirect_return_type = KosuIrTyped.Asttyped.(RTPointer RTUnknow)

let indirect_return_vt = indirect_return_var, indirect_return_type

let align_16 size = 
  let ( ** ) = Int64.mul in
  let (++) = Int64.add in
  let div = Int64.unsigned_div size 16L in
  let modulo = if (Int64.unsigned_rem size 16L) = 0L then 0L else 1L in
  (16L ** (div ++ modulo) )


let frame_descriptor ?(stack_future_call = 0L) ~(fn_register_params: (string * KosuIrTyped.Asttyped.rktype) list) ~(stack_param: (string * KosuIrTyped.Asttyped.rktype) list) ~return_type ~locals_var ~discarded_values rprogram =
  let stack_param_count = stack_param |> List.length in
  let need_xr = return_type |> KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram |> is_register_size |> not in
  let stack_concat = fn_register_params @ stack_param @ locals_var in
  let stack_concat = if need_xr then (indirect_return_var, indirect_return_type)::stack_concat else stack_concat in
  let fake_tuple = stack_concat |> List.map snd in
  let locals_space =
    fake_tuple |> KosuIrTyped.Asttyhelper.RType.rtuple
    |> KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram
  in
  let locals_space = Int64.add locals_space stack_future_call in
  (* let () = Printf.printf "Locale space = %Lu\n" locals_space in *)
  let map =
    stack_concat
    |> List.mapi (fun index value -> (index, value))
    |> List.fold_left
         (fun acc (index, st) ->
           let offset =
             offset_of_tuple_index ~generics:(Hashtbl.create 0) index
               fake_tuple rprogram
           in
           let adress =
             create_adress ~offset:(locals_space |> Int64.neg |> Int64.add offset)
               (Register64 X29)
           in
           (* let () = Printf.printf "-> %s : %s == [x29, %Ld] \n" (fst st) (KosuIrTyped.Asttypprint.string_of_rktype @@ snd @@ st) (offset) in *)
           IdVarMap.add st adress acc)
         IdVarMap.empty
  in
  { stack_param_count; locals_space; stack_map = map; discarded_values; need_xr }

  let address_of (variable,rktype) frame_desc = 
    (* let () = Printf.printf "Lookup => %s : %s\n" (variable) (KosuIrTyped.Asttypprint.string_of_rktype rktype) in *)
  if List.mem (variable, rktype) frame_desc.discarded_values then None else Some (IdVarMap.find (variable, rktype) frame_desc.stack_map)

  let function_prologue ~fn_register_params ~stack_params rprogram fd = 
    let frame_register_offset = Int64.sub (align_16 ( Int64.add 16L fd.locals_space)) 16L in
    let stack_sub_size = align_16 ( Int64.add 16L fd.locals_space) in
    let base = Instruction ( STP {x1 = Register64 X29; x2 = Register64 X30; address = { base = Register64 SP; offset = frame_register_offset}; adress_mode = Immediat} ) in
    let stack_sub = Instruction ( SUB { destination = Register64 SP; operand1 = Register64 SP; operand2 = `ILitteral stack_sub_size} ) in
    let alignx29 = Instruction (ADD { destination = Register64 X29; operand1 = Register64 SP; operand2 = `ILitteral frame_register_offset; offset = false}) in
    let store_x8 = if fd.need_xr then  [Instruction (STR {data_size = None; source = xr; adress = address_of indirect_return_vt fd |> Option.get; adress_mode = Immediat})] else [] in
    let stack_params_offset = stack_params |> List.map (fun (_, kt) -> 
      if KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram kt > 8L then KosuIrTyped.Asttyhelper.RType.rpointer kt else kt
    ) in
    let sp_address = create_adress ~offset:stack_sub_size (Register64 SP) in
    let copy_stack_params_instruction = stack_params |> List.mapi (fun index value -> index, value)|> List.fold_left (fun acc (index , (name, kt)) -> 
      let sizeofkt = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram kt in
      let offset = offset_of_tuple_index index stack_params_offset rprogram in
      let future_address_location = address_of (name, kt) fd |> (fun adr -> match adr with Some a -> a | None -> failwith "On stack setup null address") in
      let tmprreg =  tmpreg_of_ktype rprogram kt in
      let param_stack_address = increment_adress (Int64.neg offset) sp_address in
      let load_instruction = load_register tmprreg param_stack_address kt sizeofkt in
      let str_instruction = copy_from_reg tmprreg future_address_location kt rprogram in
      acc @ str_instruction @ load_instruction
    ) [] in 

    let copy_instructions = fn_register_params |> Util.ListHelper.combine_safe argument_registers |> List.fold_left (fun acc (register , (name, kt)) -> 
      let whereis = address_of (name, kt) fd  |> (fun adr -> match adr with Some a -> a | None -> failwith "From register setup null address") in
      acc @ (copy_from_reg (register) whereis kt rprogram)
      ) [] in
      stack_sub::base::alignx29::[] @ store_x8 @copy_stack_params_instruction @  copy_instructions

  let function_epilogue fd = 
    let stack_space = align_16 ( Int64.add 16L fd.locals_space) in
    let base = Instruction ( LDP {x1 = Register64 X29; x2 = Register64 X30; address = { base = Register64 SP; offset = Int64.sub stack_space 16L}; adress_mode = Immediat} ) in
    let stack_add = Instruction ( ADD { destination = Register64 SP; offset = false; operand1 = Register64 SP; operand2 = `ILitteral stack_space} ) in
    let return = Instruction (RET) in

    base::stack_add::return::[]

  let call_instruction ~origin _stack_param (_fd: frame_desc) = 

    let call = Instruction ( BL { cc = None; label = origin}) in
    [call]


end


type asm_function_decl = {
  asm_name: string;
  asm_body: Instruction.raw_line list;
}
type asm_const_decl = {
  asm_const_name: string;
  value: [`IntVal of ( KosuFrontend.Ast.isize * int64) | `StrVal of string]
}

type asm_module_node = 
| Afunction of asm_function_decl
| AConst of asm_const_decl

type asm_module = AsmModule of asm_module_node list

type asm_module_path = {
  apath: string;
  asm_module: asm_module 
}

type named_asm_module_path = {
  filename: string;
  asm_module_path: asm_module_path;
  rprogram: KosuIrTyped.Asttyped.rprogram;
  str_lit_map: (string, Util.stringlit_label) Hashtbl.t;
}

type asm_program = named_asm_module_path list
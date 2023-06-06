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

module SanVariableMap = SanCommon.SanVariableMap
open SanCommon

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
    ( Int64.shift_right_logical int64 48,
      Int64.shift_right_logical int48 32,
      Int64.shift_right_logical int32 16,
      int16 )
end

module Condition_Code = struct
  type shift = SH16 | SH32 | SH48
  type data_size = B | SB

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

  let cc_of_tac_bin ?(is_unsigned = false) =
    let open SanFrontend.SanAst in
    function
    | TacOr | TacAnd -> None
    | TacEqual -> Some EQ
    | TacDiff -> Some NE
    | TacSup -> Some (if is_unsigned then HI else GT)
    | TacSupEq -> Some (if is_unsigned then CS else GE)
    | TacInfEq -> Some (if is_unsigned then LS else LE)
    | TacInf -> Some (if is_unsigned then CC else LT)

  let data_size_of_type = function
    | (Ssize : SanTyped.SanTyAst.san_type) | Stringl -> None
    | Boolean | Unit -> Some B

  let data_size_of_variable variable = data_size_of_type @@ snd variable
end

module Register = struct
  type variable = string * SanTyped.SanTyAst.san_type
  type register_size = SReg32 | SReg64

  type raw_register =
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

<<<<<<< HEAD
  type register = { register : raw_register; size : register_size }
=======
    let color_map = [
    (X0, "aqua");
    (X1, "red");
    (X2, "fuchsia");
    (X3, "green");
    (X4, "navyblue");
    (X5, "pink");
    (X6, "orange");
    (X7, "yellow");
    (X8, "hotpink");
    (X9, "indigo");
    (X10, "magenta");
    (X11, "purple");
    (X12, "cyan")
  ]

  type register = {
    register: raw_register;
    size: register_size
  }

>>>>>>> 4dfa4e5 ([San]: fix compilation)
  type t = raw_register

  let word_regsize =
    match Nativeint.size with
    | 64 -> SReg64
    | 32 -> SReg32
    | _ -> failwith "Word size unsupported"

  let worded_register register = { register with size = word_regsize }
  let resize32 register = { register with size = SReg32 }

  let resize size register =
    match register.size with
    | s when s = size -> register
    | _ -> { register with size }

  let resize_type san_type register =
    match Sizeof.sizeof san_type with
    | 8 -> { register with size = SReg64 }
    | _ -> { register with size = SReg32 }

  let w0 = { register = X0; size = SReg32 }
  let x0 = { register = X0; size = SReg64 }
  let x13 = { register = X13; size = SReg64 }
  let x14 = { register = X14; size = SReg64 }
  let x15 = { register = X15; size = SReg64 }
  let x29 = { register = X29; size = SReg64 }
  let x30 = { register = X30; size = SReg64 }
  let word_x0 = match word_regsize with SReg64 -> x0 | SReg32 -> w0
  let sp = { register = SP; size = SReg64 }
  let w14 = { register = X14; size = SReg32 }

  let r14_sized = function
    | (Ssize : SanTyped.SanTyAst.san_type) | Stringl ->
        { register = X14; size = word_regsize }
    | Boolean | Unit -> w14

  let according_register raw_register san_type =
    match Sizeof.sizeof san_type with
    | 8 -> { register = raw_register; size = SReg64 }
    | _ -> { register = raw_register; size = SReg32 }

  let according_register_variable raw_register variable =
    according_register raw_register @@ snd variable

  let align_with ~along register = { register with size = along.size }
  let cmp_raw_register lhs rhs = lhs.register = rhs.register

  type return_strategy =
    | Indirect_return
    | Simple_return of t
    | Splitted_return of t * t

  let compare = Stdlib.compare

  let caller_saved_register =
    [
      X0;
      X1;
      X2;
      X3;
      X4;
      X5;
      X6;
      X7;
      X8;
      (* XR *)
      X9;
      X10;
      X11;
      X12;
      X13;
      X14;
      X15;
    ]

<<<<<<< HEAD
  let callee_saved_register = [ X16; X29; X30; SP ]
  let arguments_register = [ X0; X1; X2; X3; X4; X5; X6; X7 ]
  let syscall_register = [ X0; X1; X2; X3; X4; X5 ]
  let available_register = [ X8; X9; X10; X11; X12 ]
=======
  let callee_saved_register = [
    X16;
    X29;
    X30;
    SP
  ]

  let non_float_argument_registers = 
    [
    X0;
    X1;
    X2;
    X3;
    X4;
    X5;
    X6;
    X7;
  ]

  let arguments_register _ = non_float_argument_registers

  let syscall_register = [
    X0;
    X1;
    X2;
    X3;
    X4;
    X5;
  ]

  let available_register = [
    X8;
    X9;
    X10;
    X11;
    X12
  ]
>>>>>>> 4dfa4e5 ([San]: fix compilation)

  let is_valid_register (_: variable) (_: t) = true
  let does_return_hold_in_register _ = true
  let indirect_return_register = X8
  let return_strategy _ = Simple_return X0

  let str_ldr_offset_range reg n =
    if n < 0L then -256L < n
    else
      match reg.size with
      | SReg32 -> n < 255L || (Int64.unsigned_rem n 4L = 0L && n < 16380L)
      | SReg64 -> n < 255L || (Int64.unsigned_rem n 8L = 0L && n < 32760L)

  let is_offset_too_far reg address =
    match address with
    | `ILitteral i when not @@ str_ldr_offset_range reg i -> true
    | `ILitteral _ | `Register _ -> false
end

module Operande = struct
  type src =
    [ `ILitteral of int64 | `Register of Register.register | `Label of string ]

  type dst = Register.register
end

module GreedyColoration = SanCommon.SanRegisterAllocator.GreedyColoring(Register)

module Location = struct
  type adress_offset = [ `ILitteral of int64 | `Register of Register.register ]
  type address = { base : Register.register; offset : adress_offset }

  type adress_mode =
    | Immediat (* out = *intptr; *)
    | Prefix (* out = *(++intptr);*)
    | Postfix (* out = *(intptr++);*)

  type location = LocReg of Register.register | LocAddr of address

  let loc_reg r = LocReg r
  let loc_addr a = LocAddr a
  let create_adress ?(offset = 0L) base = { base; offset = `ILitteral offset }

  let str_ldr_offset_range reg n =
    let open Register in
    if n < 0L then -256L < n
    else
      match reg.size with
      | SReg32 -> n < 255L || (Int64.unsigned_rem n 4L = 0L && n < 16380L)
      | SReg64 -> n < 255L || (Int64.unsigned_rem n 8L = 0L && n < 32760L)

  let is_offset_too_far reg address =
    match address with
    | `ILitteral i when not @@ str_ldr_offset_range reg i -> true
    | `ILitteral _ | `Register _ -> false

  let increment_adress off adress =
    match adress.offset with
    | `ILitteral offset ->
        { adress with offset = `ILitteral (Int64.add offset off) }
    | `Register _reg -> failwith "Increment register based address"

  let reg_opt = function LocReg reg -> Some reg | LocAddr _ -> None
  let address_opt = function LocAddr addr -> Some addr | LocReg _ -> None
end

module Instruction = struct
  open Condition_Code
  open Operande
  open Register
  open Location

  type t =
    | Mov of { destination : register; source : Operande.src }
    | Movk of {
        destination : Register.register;
        operand : src;
        shift : shift option;
      }
    | Mvn of { destination : Register.register; operand : src }
    | Not of { destination : Register.register; source : src }
    | Neg of { destination : Register.register; source : Register.register }
    | Add of {
        destination : Register.register;
        operand1 : Register.register;
        (* Int12 litteral oprand*) operand2 : src;
        offset : bool;
      }
    | Sub of {
        destination : Register.register;
        operand1 : Register.register;
        (* Int12 litteral oprand*) operand2 : src;
      }
    | Mul of {
        destination : Register.register;
        operand1 : Register.register;
        operand2 : Register.register;
      }
    | SDiv of {
        destination : Register.register;
        operand1 : Register.register;
        operand2 : Register.register;
      }
    | Lsl of {
        destination : Register.register;
        operand1 : Register.register;
        (* LIteral range [0-31] *)
        operand2 : src;
      }
    | Lsr of {
        destination : Register.register;
        operand1 : Register.register;
        (* LIteral range [0-31] *)
        operand2 : src;
      }
    | Asr of {
        destination : Register.register;
        operand1 : Register.register;
        (* LIteral range [0-31] *)
        operand2 : src;
      }
    | Csinc of {
        destination : Register.register;
        operand1 : Register.register;
        operand2 : Register.register;
        condition : condition_code;
      }
    | Cmp of { operand1 : Register.register; operand2 : src }
    | Cset of { register : Register.register; cc : condition_code }
    (* Bitwise And*)
    | And of {
        destination : Register.register;
        operand1 : Register.register;
        operand2 : src;
      }
    (* Bitwise OR*)
    | Orr of {
        destination : Register.register;
        operand1 : Register.register;
        operand2 : src;
      }
    (* Bitwise XOR*)
    | Eor of {
        destination : Register.register;
        operand1 : Register.register;
        operand2 : src;
      }
    | Ldr of {
        data_size : data_size option;
        destination : Register.register;
        address_src : address;
        address_mode : adress_mode;
      }
    | Ldp of {
        x1 : Register.register;
        x2 : Register.register;
        address : address;
        address_mode : adress_mode;
      }
    | Str of {
        data_size : data_size option;
        source : Register.register;
        address : address;
        address_mode : adress_mode;
      }
    | Stp of {
        x1 : Register.register;
        x2 : Register.register;
        address : address;
        adress_mode : adress_mode;
      }
    | Adrp of { dst : Register.register; label : string }
    | B of { cc : condition_code option; label : string }
    | Bl of { cc : condition_code option; label : string }
    | RET

  let sub destination operand1 operand2 =
    Sub { destination; operand1; operand2 }

  let sub_r destination operand1 operand2 =
    sub destination operand1 (`Register operand2)

  let add_o ?(offset = false) ~destination ~operand1 operand2 =
    Add { destination; operand1; operand2; offset }

  let add destination operand1 operand2 =
    Add { destination; operand1; operand2; offset = false }

  let add_r destination operand1 operand2 =
    add destination operand1 (`Register operand2)

  let mult destination operand1 operand2 =
    Mul { destination; operand1; operand2 }

  let div destination operand1 operand2 =
    SDiv { destination; operand1; operand2 }

  let bitwiseor destination operand1 operand2 =
    Orr { destination; operand1; operand2 }

  let bitwiseor_r destination operand1 operand2 =
    bitwiseor destination operand1 (`Register operand2)

  let bitwisexor destination operand1 operand2 =
    Eor { destination; operand1; operand2 }

  let bitwisexor_r destination operand1 operand2 =
    bitwisexor destination operand1 (`Register operand2)

  let bitwiseand destination operand1 operand2 =
    And { destination; operand1; operand2 }

  let bitwiseand_r destination operand1 operand2 =
    bitwiseand destination operand1 (`Register operand2)

  let shiftleft destination operand1 operand2 =
    Lsl { destination; operand1; operand2 }

  let shiftleft_r destination operand1 operand2 =
    shiftleft destination operand1 (`Register operand2)

  let shiftright destination operand1 operand2 =
    Asr { destination; operand1; operand2 }

  let shiftright_r destination operand1 operand2 =
    shiftright destination operand1 (`Register operand2)

  let adrp ~dst ~label = Adrp { dst; label }
  let mov ~destination ~source = Mov { destination; source }

  let ldr ~data_size ~destination ~address_src ~address_mode =
    Ldr { data_size; destination; address_src; address_mode }

  let ldp ~x1 ~x2 ~address ~address_mode = Ldp { x1; x2; address; address_mode }
  let mvn ~destination ~operand = Mvn { destination; operand }

  let eor ~destination ~operand1 ~operand2 =
    Eor { destination; operand1; operand2 }

  let neg ~destination ~operand = Neg { destination; source = operand }

  let andi ~destination ~operand1 ~operand2 =
    And { destination; operand1; operand2 }

  let orr ~destination ~operand1 ~operand2 =
    And { destination; operand1; operand2 }

  let cmp ~operand1 ~operand2 = Cmp { operand1; operand2 }
  let cset ~cc ~register = Cset { cc; register }
  let b ?cc label = B { cc; label }
  let bl ?cc label = Bl { cc; label }

  let selfbinop_of_binop =
    let open SanFrontend.SanAst in
    function
    | TacAdd -> add_r
    | TacMinus -> sub_r
    | TacMult -> mult
    | TacDiv -> div
    | TacBitwiseOr -> bitwiseor_r
    | TacBitwiseAnd -> bitwiseand_r
    | TacBitwiseXor -> bitwisexor_r
    | TacShiftLeft -> shiftleft_r
    | TacShiftRight -> shiftright_r

  let is_stp_range n = -512L <= n && n <= 504L
  let ret = RET
end

module Line = struct
  type line =
    | Instruction of Instruction.t
    | Comment of string
    | Label of string
    | Directive of string

  type asmline = AsmLine of line * string option

  let instruction ?comment instr = AsmLine (Instruction instr, comment)
  let instructions instrs = instrs |> List.map instruction
  let comment message = AsmLine (Comment message, None)
  let label ?comment l = AsmLine (Label l, comment)
  let directive ?comment d = AsmLine (Instruction d, comment)
end

module LineInstruction = struct
  open Location
  open Register
  open Instruction
  open Line

  let load_label ~label register =
    let word_register = Register.worded_register register in
    instructions
      [
        adrp ~dst:word_register ~label;
        add_o ~offset:true ~destination:word_register ~operand1:word_register
          (`Label label);
      ]

  let mov_integer register n =
    let open Immediat in
    if is_direct_immediat n then
      instruction (Mov { destination = register; source = `ILitteral n }) :: []
    else
      let int64, int48, int32, int16 = split n in
      let base =
        [
          instruction
            (Mov { destination = register; source = `ILitteral int16 });
        ]
      in
      ( ( base |> fun l ->
          if int32 = 0L then l
          else
            l
            @ [
                instruction
                  (Movk
                     {
                       destination = register;
                       operand = `ILitteral int32;
                       shift = Some SH16;
                     });
              ] )
      |> fun l ->
        if int48 = 0L then l
        else
          l
          @ [
              instruction
                (Movk
                   {
                     destination = register;
                     operand = `ILitteral int48;
                     shift = Some SH32;
                   });
            ] )
      |> fun l ->
      if int64 = 0L then l
      else
        l
        @ [
            instruction
              (Movk
                 {
                   destination = register;
                   operand = `ILitteral int32;
                   shift = Some SH48;
                 });
          ]

  let prologue_epilogue_stack_size framesize =
    if is_stp_range framesize then
      ([], { base = sp; offset = `ILitteral framesize })
    else
      ( mov_integer x15 framesize
        @ instructions
            [
              sub sp sp (`ILitteral (Int64.add 16L framesize)); add_r x15 sp x15;
            ],
        { base = x15; offset = `ILitteral 0L } )

  let str_instr ?(mode = Immediat) ~data_size ~source address =
    match address.offset with
    | `ILitteral i when not @@ str_ldr_offset_range source i ->
        let mov = mov_integer x15 i in
        let address = { base = address.base; offset = `Register x15 } in
        let str =
          [
            instruction
            @@ Str { data_size; source; address; address_mode = mode };
          ]
        in
        mov @ str
    | `Register _ | `ILitteral _ ->
        [
          instruction @@ Str { data_size; source; address; address_mode = mode };
        ]

  let ldr_instr ?(mode = Immediat) ~data_size ~destination address =
    match address.offset with
    | `ILitteral i when not @@ str_ldr_offset_range destination i ->
        let mov = mov_integer x15 i in
        let address = { base = address.base; offset = `Register x15 } in
        let str =
          [
            instruction
            @@ Ldr
                 {
                   data_size;
                   destination;
                   address_src = address;
                   address_mode = mode;
                 };
          ]
        in
        mov @ str
    | `Register _ | `ILitteral _ ->
        [
          instruction
          @@ Ldr
               {
                 data_size;
                 destination;
                 address_src = address;
                 address_mode = mode;
               };
        ]

  let stp_inst ~vframe =
    if is_stp_range vframe then
      let address = { base = sp; offset = `ILitteral vframe } in
      [
        instruction
        @@ Stp { x1 = x29; x2 = x30; address; adress_mode = Immediat };
      ]
    else
      let x29_address = create_adress ~offset:(Int64.add 8L vframe) sp in
      let x30_address = create_adress ~offset:vframe sp in
      instructions
        [
          Str
            {
              data_size = None;
              source = x29;
              address = x29_address;
              address_mode = Immediat;
            };
          Str
            {
              data_size = None;
              source = x30;
              address = x30_address;
              address_mode = Immediat;
            };
        ]

  let ldp_instr ~vframe =
    if is_stp_range vframe then
      let address = { base = sp; offset = `ILitteral vframe } in
      [ instruction @@ ldp ~x1:x29 ~x2:x30 ~address ~address_mode:Immediat ]
    else
      let x29_address = create_adress ~offset:(Int64.add 8L vframe) sp in
      let x30_address = create_adress ~offset:vframe sp in
      instructions
        [
          ldr ~data_size:None ~destination:x29 ~address_src:x29_address
            ~address_mode:Immediat;
          ldr ~data_size:None ~destination:x30 ~address_src:x30_address
            ~address_mode:Immediat;
        ]
end

module AsmProgram = Common.AsmAst.Make (Line)

module FrameManager = struct
  type description = {
    local_space : int;
    variable_map : Location.location SanCfg.SanVariableMap.t;
  }

  let location_of variable fd =
    match SanVariableMap.find_opt variable fd.variable_map with
    | None -> Printf.sprintf "location of: %s failed" "" |> failwith
    | Some loc -> loc

  let frame_descriptor (function_decl : SanTyped.SanTyAst.ty_san_function) =
    let parameter_count = List.length Register.arguments_register in
    let register_parameters, stack_parameters =
      function_decl.parameters |> List.mapi Util.couple
      |> List.partition_map (fun (index, variable) ->
             if index < parameter_count then Either.left variable
             else Either.right variable)
    in
    let cfg = SanCfg.SanCfgConv.liveness_of_san_tyfunction function_decl in
    let colored_graph =
      GreedyColoration.coloration
        ~parameters:
          (Util.combine_safe register_parameters Register.arguments_register)
        ~available_color:Register.available_register cfg
    in

    let base_address = Location.create_adress ~offset:0L Register.sp in

    let variable_map, stack_variable =
      function_decl.locals
      |> List.sort (fun (_, lhs) (_, rhs) ->
             let lsize = Sizeof.sizeof lhs in
             let rsize = Sizeof.sizeof rhs in
             compare rsize lsize)
      |> List.fold_left
           (fun (acc_map, acc_stack_variable) variable ->
             let open GreedyColoration.ColoredGraph in
             let node =
               GreedyColoration.ColoredGraph.find variable colored_graph
             in
             match node.color with
             | Some color ->
                 (* let () = Printf.printf "%s : %d\n" (fst variable) (Obj.magic color) in *)
                 let reg =
                   Location.loc_reg
                   @@ Register.according_register_variable color variable
                 in
                 (SanVariableMap.add variable reg acc_map, acc_stack_variable)
             | None -> (acc_map, variable :: acc_stack_variable))
           (SanVariableMap.empty, [])
    in

    let stack_variable_types = List.map snd stack_variable in

    let variable_map =
      stack_variable |> List.mapi Util.couple
      |> List.fold_left
           (fun (acc_address, acc_map) (index, variable) ->
             let offset =
               Sizeof.offset_of_tuple_index index stack_variable_types
             in
             let address =
               Location.increment_adress (Int64.of_int offset) base_address
             in
             let loc_adrress = Location.loc_addr address in
             (address, SanVariableMap.add variable loc_adrress acc_map))
           (base_address, variable_map)
      |> snd
    in

    let local_space = Sizeof.sizeof_tuple stack_variable_types in
    { local_space; variable_map }

  let prologue (function_decl : SanTyped.SanTyAst.ty_san_function) fd =
    let open LineInstruction in
    let open Instruction in
    let open Register in
    let open Line in
    let open Location in
    let stack_sub_size = Sizeof.align_16 (16 + fd.local_space) in
    let variable_frame_size = stack_sub_size - 16 in
    let base = stp_inst ~vframe:(Int64.of_int variable_frame_size) in
    let stack_sub_line =
      instruction @@ sub sp sp (`ILitteral (Int64.of_int stack_sub_size))
    in
    let alignx29_line =
      instruction
      @@ add_o ~destination:x29 ~operand1:sp
           (`ILitteral (Int64.of_int variable_frame_size))
    in

<<<<<<< HEAD
    let parameter_count = List.length Register.arguments_register in
    let register_parameters, _stack_parameters =
      function_decl.parameters |> List.mapi Util.couple
      |> List.partition_map (fun (index, variable) ->
             if index < parameter_count then Either.left variable
             else Either.right variable)
    in
=======
    let parameter_count = List.length (Register.arguments_register ()) in
    let register_parameters, _stack_parameters = function_decl.parameters |> List.mapi Util.couple |> List.partition_map (fun (index, variable) ->
      if index < parameter_count then Either.left variable else Either.right variable
    ) in
>>>>>>> fd18d3e ([San]: Mov Sancfg to SanCommon)

    let store_parameters_value =
      register_parameters
<<<<<<< HEAD
      |> Util.combine_safe Register.arguments_register
      |> List.filter_map (fun (register, variable) ->
             match location_of variable fd with
             | LocAddr address -> Some (variable, register, address)
             | LocReg _ -> None)
      |> List.map (fun (variable, register, address) ->
             str_instr
               ~data_size:(Condition_Code.data_size_of_variable variable)
               ~source:(Register.according_register_variable register variable)
               address)
=======
      |> Util.combine_safe (Register.arguments_register ())
      |> List.filter_map (fun (register, variable) -> 
        match location_of variable fd with
        | LocAddr address -> Some (variable, register, address)
        | LocReg _ -> None
      )
      |> List.map (fun (variable, register, address) -> 
        str_instr 
          ~data_size:(Condition_Code.data_size_of_variable variable)
          ~source:(Register.according_register_variable register variable)
          address
      )
>>>>>>> fd18d3e ([San]: Mov Sancfg to SanCommon)
      |> List.flatten
    in
    (stack_sub_line :: base) @ (alignx29_line :: store_parameters_value)

  let epilogue fd =
    let open Instruction in
    let open Register in
    let open Line in
    let stack_space = Sizeof.align_16 (16 + fd.local_space) in
    let vframe = Int64.of_int @@ (stack_space - 16) in
    let base = LineInstruction.ldp_instr ~vframe in
    let stack_add =
      instruction
      @@ add_o ~destination:sp ~operand1:sp
           (`ILitteral (Int64.of_int stack_space))
    in
    let return = instruction ret in

    base @ [ stack_add; return ]
end

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

module IdVar = KosuCommon.IdVar
module IdVarMap = KosuCommon.IdVarMap

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
      int16
    )
end

type adress_mode =
  | Immediat (* out = *intptr; *)
  | Prefix (* out = *(++intptr);*)
  | Postfix (* out = *(intptr++);*)

type data_size = B | SB | H | SH

module Condition_Code = struct
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

  (* Inverse of the op > == <= *)
  let cc_of_tac_bin_reversed ?(is_unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacOr | TacAnd ->
        None
    | TacEqual ->
        Some NE
    | TacDiff ->
        Some EQ
    | TacSup ->
        Some
          ( if is_unsigned then
              LS
            else
              LE
          )
    | TacSupEq ->
        Some
          ( if is_unsigned then
              CC
            else
              LT
          )
    | TacInfEq ->
        Some
          ( if is_unsigned then
              HI
            else
              GT
          )
    | TacInf ->
        Some
          ( if is_unsigned then
              CS
            else
              GE
          )

  let cc_of_tac_bin ?(is_unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacOr | TacAnd ->
        None
    | TacEqual ->
        Some EQ
    | TacDiff ->
        Some NE
    | TacSup ->
        Some
          ( if is_unsigned then
              HI
            else
              GT
          )
    | TacSupEq ->
        Some
          ( if is_unsigned then
              CS
            else
              GE
          )
    | TacInfEq ->
        Some
          ( if is_unsigned then
              LS
            else
              LE
          )
    | TacInf ->
        Some
          ( if is_unsigned then
              CC
            else
              LT
          )
end

module Register = struct
  type register_size = SReg32 | SReg64

  type float_register =
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

  type int_register =
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

  let floatreg_of_intreg = function
    | X0 ->
        D0
    | X1 ->
        D1
    | X2 ->
        D2
    | X3 ->
        D3
    | X4 ->
        D4
    | X5 ->
        D5
    | X6 ->
        D6
    | X7 ->
        D7
    | X8 ->
        D8
    | X9 ->
        D9
    | X10 ->
        D10
    | X11 ->
        D11
    | X12 ->
        D12
    | X13 ->
        D13
    | X14 ->
        D14
    | X15 ->
        D15
    | X16 ->
        D16
    | X29 ->
        D29
    | X30 ->
        D30
    | XZR ->
        DZR
    | SP ->
        failwith "SP has not float equivalent"

  let intreg_of_floatreg = function
    | D0 ->
        X0
    | D1 ->
        X1
    | D2 ->
        X2
    | D3 ->
        X3
    | D4 ->
        X4
    | D5 ->
        X5
    | D6 ->
        X6
    | D7 ->
        X7
    | D8 ->
        X8
    | D9 ->
        X9
    | D10 ->
        X10
    | D11 ->
        X11
    | D12 ->
        X12
    | D13 ->
        X13
    | D14 ->
        X14
    | D15 ->
        X15
    | D16 ->
        X16
    | D29 ->
        X29
    | D30 ->
        X30
    | DZR ->
        XZR

  type raw_register = FloatReg of float_register | IntegerReg of int_register
  type register = { register : raw_register; size : register_size }

  let resize64 reg = { reg with size = SReg64 }
  let resize32 reg = { reg with size = SReg32 }
  let resize_register size register = { register with size }

  let to_float64reg reg =
    match reg.register with
    | FloatReg _ ->
        resize64 reg
    | IntegerReg reg ->
        { register = FloatReg (floatreg_of_intreg reg); size = SReg64 }

  let to_int64reg reg =
    match reg.register with
    | IntegerReg _ ->
        resize64 reg
    | FloatReg reg ->
        { register = IntegerReg (intreg_of_floatreg reg); size = SReg64 }

  let to_int32reg reg =
    match reg.register with
    | IntegerReg _ ->
        resize64 reg
    | FloatReg reg ->
        { register = IntegerReg (intreg_of_floatreg reg); size = SReg32 }

  let to_float32reg reg =
    match reg.register with
    | FloatReg _ ->
        resize32 reg
    | IntegerReg reg ->
        { register = FloatReg (floatreg_of_intreg reg); size = SReg32 }

  let regsize_of_fsize = function
    | KosuFrontend.Ast.F32 ->
        SReg32
    | KosuFrontend.Ast.F64 ->
        SReg64

  let x0 = { register = IntegerReg X0; size = SReg64 }
  let x1 = { register = IntegerReg X1; size = SReg64 }
  let x2 = { register = IntegerReg X2; size = SReg64 }
  let x3 = { register = IntegerReg X3; size = SReg64 }
  let x4 = { register = IntegerReg X4; size = SReg64 }
  let x5 = { register = IntegerReg X5; size = SReg64 }
  let x6 = { register = IntegerReg X6; size = SReg64 }
  let x7 = { register = IntegerReg X7; size = SReg64 }
  let x8 = { register = IntegerReg X8; size = SReg64 }
  let w8 = resize32 x8
  let x9 = { register = IntegerReg X9; size = SReg64 }
  let w9 = resize32 x9
  let x10 = { register = IntegerReg X10; size = SReg64 }
  let w10 = resize32 x10
  let x11 = { register = IntegerReg X11; size = SReg64 }
  let w11 = resize32 x11
  let x15 = { register = IntegerReg X15; size = SReg64 }
  let xzr = { register = IntegerReg XZR; size = SReg64 }
  let wzr = { register = IntegerReg XZR; size = SReg32 }
  let x29 = { register = IntegerReg X29; size = SReg64 }
  let x30 = { register = IntegerReg X30; size = SReg64 }
  let sp = { register = IntegerReg SP; size = SReg64 }
  let d0 = { register = FloatReg D0; size = SReg64 }
  let d1 = { register = FloatReg D1; size = SReg64 }
  let d2 = { register = FloatReg D2; size = SReg64 }
  let d3 = { register = FloatReg D3; size = SReg64 }
  let d4 = { register = FloatReg D4; size = SReg64 }
  let d5 = { register = FloatReg D5; size = SReg64 }
  let d6 = { register = FloatReg D6; size = SReg64 }
  let d7 = { register = FloatReg D7; size = SReg64 }
  let d8 = { register = FloatReg D8; size = SReg64 }
  let d9 = { register = FloatReg D9; size = SReg64 }
  let s9 = { register = FloatReg D9; size = SReg32 }
  let d10 = { register = FloatReg D10; size = SReg64 }
  let d11 = { register = FloatReg D11; size = SReg64 }
  let x16 = { register = IntegerReg X16; size = SReg64 }

  let float64reg_of_64bitsreg = function
    | X0 ->
        D0
    | X1 ->
        D1
    | X2 ->
        D2
    | X3 ->
        D3
    | X4 ->
        D4
    | X5 ->
        D5
    | X6 ->
        D6
    | X7 ->
        D7
    | X8 ->
        D8
    | X9 ->
        D9
    | X10 ->
        D10
    | X11 ->
        D11
    | X12 ->
        D12
    | X13 ->
        D13
    | X14 ->
        D14
    | X15 ->
        D15
    | X16 ->
        D16
    | X29 ->
        D29
    | X30 ->
        D30
    | XZR ->
        DZR
    | SP ->
        failwith "SP has not float equivalent"

  let argument_registers = [ x0; x1; x2; x3; x4; x5; x6; x7 ]
  let float_arguments_register = [ d0; d1; d2; d3; d4; d5; d6; d7 ]
  let syscall_arguments_register = [ x0; x1; x2; x3; x4; x5 ]
  let frame_registers = [ x29; x30 ]

  let return_register_ktype ~ktype =
    let is_float = KosuIrTyped.Asttyhelper.RType.is_float ktype in
    function
    | 4L when is_float ->
        resize32 d0
    | 8L when is_float ->
        d0
    | _ when is_float ->
        failwith "Float impossible return type"
    | 1L | 2L | 4L ->
        resize32 x0
    | 8L ->
        x0
    | _ ->
        x8

  let is_f64_reg = function
    | { register = FloatReg _; size = SReg64 } ->
        true
    | _ ->
        false

  let is_f32_reg = function
    | { register = FloatReg _; size = SReg32 } ->
        true
    | _ ->
        false

  let is_float_reg = function
    | { register = FloatReg _; _ } ->
        true
    | _ ->
        false

  let size_of_ktype_size s =
    if s <= 4L then
      SReg32
    else
      SReg64

  let size_of_reg register = register.size

  (* let reg_of_size size reg =
     match size with SReg32 -> resize32 reg | SReg64 -> resize64 reg *)

  let xr = x8
  let ftmp64reg = d8
  let tmp64reg = x8
  let tmp32reg = w8
  let ftmp64reg_2 = d9
  let tmp64reg_2 = x9
  let tmp32reg_2 = w9
  let ftmp64reg_3 = d10
  let tmp32reg_3 = w10
  let tmp64reg_3 = x10
  let ftmp64reg_4 = d11
  let tmp32reg_4 = w11
  let tmp64reg_4 = x11

  let tmpreg_of_size size =
    if size <= 4L then
      tmp32reg
    else
      tmp64reg

  let tmpreg_of_size_2 size =
    if size <= 4L then
      tmp32reg_2
    else
      tmp64reg_2

  let tmpreg_of_size_3 size =
    if size <= 4L then
      tmp32reg_3
    else
      tmp64reg_3

  let tmpreg_of_size_4 size =
    if size <= 4L then
      tmp32reg_4
    else
      tmp64reg_4

  let regsize_of_ktype size =
    match size with 1L | 2L | 4L -> SReg32 | _ -> SReg64

  let reg_of_ktype rprogram ktype ~register =
    match ktype with
    | KosuIrTyped.Asttyped.RTFloat KosuFrontend.Ast.F32 ->
        to_float32reg register
    | RTFloat F64 ->
        to_float64reg register
    | _ ->
        let sizeof = KosuIrTyped.Sizeof.sizeof rprogram ktype in
        let size = size_of_ktype_size sizeof in
        resize_register size register

  let reg8_of_ktype rprogram ktype = reg_of_ktype rprogram ktype ~register:x8
  let reg0_of_ktype = reg_of_ktype ~register:x0
  let reg9_of_ktype = reg_of_ktype ~register:x9
  let reg10_of_ktype = reg_of_ktype ~register:x10
  let reg11_of_ktype = reg_of_ktype ~register:x11

  (* let tmpreg_of_ktype rprogram ktype =
     if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then ftmp64reg
     else
       let size = KosuIrTyped.Sizeof.sizeof rprogram ktype in
       tmpreg_of_size size *)

  (* let tmpreg_of_ktype_2 rprogram ktype =
     if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then ftmp64reg_2
     else
       let size = KosuIrTyped.Sizeof.sizeof rprogram ktype in
       tmpreg_of_size_2 size *)
  (*
     let tmpreg_of_ktype_3 rprogram ktype =
       if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then ftmp64reg_3
       else
         let size = KosuIrTyped.Sizeof.sizeof rprogram ktype in
         tmpreg_of_size_3 size *)

  (* let tmpreg_of_ktype_4 rprogram ktype =
     if KosuIrTyped.Asttyhelper.RType.is_64bits_float ktype then ftmp64reg_4
     else
       let size = KosuIrTyped.Sizeof.sizeof rprogram ktype in
       tmpreg_of_size_4 size *)
end

open Register

type src =
  [ `ILitteral of int64
  | `F64Litteral of float
  | `Register of register
  | `Label of string ]

type adress_offset = [ `ILitteral of int64 | `Register of register ]

let src_of_adress_offset (adress_offset : adress_offset) = (adress_offset :> src)

type address = { base : register; offset : adress_offset }

let create_adress ?(offset = 0L) base = { base; offset = `ILitteral offset }

let is_register_based_address address =
  match address with `ILitteral _ -> false | `Register _ -> true

let str_ldr_offset_range reg n =
  if n < 0L then
    -256L < n
  else
    match reg.size with
    | SReg32 ->
        n < 255L || (Int64.unsigned_rem n 4L = 0L && n < 16380L)
    | SReg64 ->
        n < 255L || (Int64.unsigned_rem n 8L = 0L && n < 32760L)

let is_offset_too_far reg address =
  match address with
  | `ILitteral i when not @@ str_ldr_offset_range reg i ->
      true
  | `ILitteral _ | `Register _ ->
      false

let increment_adress off adress =
  match adress.offset with
  | `ILitteral offset ->
      { adress with offset = `ILitteral (Int64.add offset off) }
  | `Register _reg ->
      failwith "Increment register based address"

let asm_const_name current_module const_name =
  Printf.sprintf "_%s_%s"
    (current_module
    |> String.map (fun c ->
           if c = ':' then
             '_'
           else
             c
       )
    )
    const_name

module Instruction = struct
  open Condition_Code

  type shift = SH16 | SH32 | SH48

  type instruction =
    | Mov of {
        destination : Register.register;
        (* Careful int max INT16 *)
        flexsec_operand : src;
      }
    | Movk of {
        destination : Register.register;
        operand : src;
        shift : shift option;
      }
    | Mvn of { destination : Register.register; operand : src }
    | Not of { destination : Register.register; source : src }
    | Neg of { destination : Register.register; source : Register.register }
    | FCVT of { into : Register.register; turn : Register.register }
      (* For float convert *)
    | FCVTZU of {
        int_register : Register.register;
        float_register : Register.register;
      }
      (* Float -> Uint *)
    | FCVTZS of {
        int_register : Register.register;
        float_register : Register.register;
      }
      (* Float -> Sint *)
    | SCVTF of {
        float_register : Register.register;
        int_register : Register.register;
      }
      (* int -> float *)
    | ADD of {
        destination : Register.register;
        operand1 : Register.register;
        (* Int12 litteral oprand*)
        operand2 : src;
        offset : bool;
      }
    | ADDS of {
        destination : Register.register;
        operand1 : Register.register;
        (* Int12 litteral oprand*)
        operand2 : src;
      }
    | MADD of {
        destination : Register.register;
        operand1_base : Register.register;
        operand2 : register;
        scale : register;
      }
    | MSUB of {
        destination : Register.register;
        operand1_base : Register.register;
        operand2 : register;
        scale : register;
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
    | LSL of {
        destination : Register.register;
        operand1 : Register.register;
        (* LIteral range [0-31] *)
        operand2 : src;
      }
    | LSR of {
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
        destination : Register.register;
        operand1 : Register.register;
        operand2 : Register.register;
        condition : condition_code;
      }
    | CMP of { operand1 : Register.register; operand2 : src }
    | CSET of { register : Register.register; cc : condition_code }
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
    | ROR of {
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
        x1 : Register.register;
        x2 : Register.register;
        address : address;
        adress_mode : adress_mode;
      }
    | ADRP of { dst : Register.register; label : string }
    | LDP of {
        x1 : Register.register;
        x2 : Register.register;
        address : address;
        adress_mode : adress_mode;
      }
    | B of { cc : condition_code option; label : string }
    | BL of { cc : condition_code option; label : string }
    | BR of { cc : condition_code option; reg : Register.register }
    | BLR of { cc : condition_code option; reg : Register.register }
    | SVC
    | RET

  type comment = Comment of string

  type line =
    | Instruction of instruction
    | Directive of string
    | Label of string
    | Line_Com of comment

  (* type line = raw_line * comment option *)

  let instruction i = Instruction i

  let mov_integer register n =
    let open Immediat in
    if is_direct_immediat n then
      Instruction
        (Mov { destination = register; flexsec_operand = `ILitteral n })
      :: []
    else
      let int64, int48, int32, int16 = split n in
      let base =
        [
          Instruction
            (Mov { destination = register; flexsec_operand = `ILitteral int16 });
        ]
      in
      base
      |> (fun l ->
           if int32 = 0L then
             l
           else
             l
             @ [
                 Instruction
                   (Movk
                      {
                        destination = register;
                        operand = `ILitteral int32;
                        shift = Some SH16;
                      }
                   );
               ]
         )
      |> (fun l ->
           if int48 = 0L then
             l
           else
             l
             @ [
                 Instruction
                   (Movk
                      {
                        destination = register;
                        operand = `ILitteral int48;
                        shift = Some SH32;
                      }
                   );
               ]
         )
      |> fun l ->
      if int64 = 0L then
        l
      else
        l
        @ [
            Instruction
              (Movk
                 {
                   destination = register;
                   operand = `ILitteral int32;
                   shift = Some SH48;
                 }
              );
          ]

  let str_instr ?(mode = Immediat) ~data_size ~source address =
    match address.offset with
    | `ILitteral i when not @@ str_ldr_offset_range source i ->
        let mov = mov_integer x15 i in
        let adress = { base = address.base; offset = `Register x15 } in
        let str =
          [
            instruction @@ STR { data_size; source; adress; adress_mode = mode };
          ]
        in
        mov @ str
    | `Register _ | `ILitteral _ ->
        [
          instruction
          @@ STR { data_size; source; adress = address; adress_mode = mode };
        ]

  let ldr_instr ?(mode = Immediat) ~data_size ~destination address =
    match address.offset with
    | `ILitteral i when not @@ str_ldr_offset_range destination i ->
        let mov = mov_integer x15 i in
        let adress = { base = address.base; offset = `Register x15 } in
        let str =
          [
            instruction
            @@ LDR
                 {
                   data_size;
                   destination;
                   adress_src = adress;
                   adress_mode = mode;
                 };
          ]
        in
        mov @ str
    | `Register _ | `ILitteral _ ->
        [
          instruction
          @@ LDR
               {
                 data_size;
                 destination;
                 adress_src = address;
                 adress_mode = mode;
               };
        ]

  let ins_madd ~destination ~operand1_base ~operand2 ~scale =
    [ instruction @@ MADD { destination; operand1_base; operand2; scale } ]

  let ins_msub ~destination ~operand1_base ~operand2 ~scale =
    [ instruction @@ MSUB { destination; operand1_base; operand2; scale } ]

  let ins_add ~destination ~operand1 ~operand2 =
    [
      instruction
      @@ ADD
           {
             destination;
             operand1 = resize_register destination.size operand1;
             operand2 = `Register (resize_register destination.size operand2);
             offset = false;
           };
    ]

  let ins_sub ~destination ~operand1 ~operand2 =
    [
      instruction
      @@ SUB { destination; operand1; operand2 = `Register operand2 };
    ]

  let ins_mult ~destination ~operand1 ~operand2 =
    [ instruction @@ MUL { destination; operand1; operand2 } ]

  let ins_unsigned_div ~destination ~operand1 ~operand2 =
    [ instruction @@ UDIV { destination; operand1; operand2 } ]

  let ins_signed_div ~destination ~operand1 ~operand2 =
    [ instruction @@ SDIV { destination; operand1; operand2 } ]

  let ins_bitwiseand ~destination ~operand1 ~operand2 =
    [
      instruction
      @@ AND { destination; operand1; operand2 = `Register operand2 };
    ]

  let ins_bitwiseor ~destination ~operand1 ~operand2 =
    [
      instruction
      @@ ORR { destination; operand1; operand2 = `Register operand2 };
    ]

  let ins_bitwisexor ~destination ~operand1 ~operand2 =
    [
      instruction
      @@ EOR { destination; operand1; operand2 = `Register operand2 };
    ]

  let ins_shift_left ~destination ~operand1 ~operand2 =
    [
      instruction
      @@ LSL { destination; operand1; operand2 = `Register operand2 };
    ]

  let ins_arith_shift_right ~destination ~operand1 ~operand2 =
    [
      instruction
      @@ ASR { destination; operand1; operand2 = `Register operand2 };
    ]

  let ins_logical_shift_right ~destination ~operand1 ~operand2 =
    [
      instruction
      @@ LSR { destination; operand1; operand2 = `Register operand2 };
    ]

  let promote_float register =
    if is_f32_reg register then
      [ Instruction (FCVT { turn = register; into = resize64 register }) ]
    else
      []

  let is_stp_range n = -512L <= n && n <= 504L

  let binop_instruction_of_tacself ?(unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacAdd ->
        ins_add
    | TacMinus ->
        ins_sub
    | TacBitwiseAnd ->
        ins_bitwiseand
    | TacBitwiseOr ->
        ins_bitwiseor
    | TacBitwiseXor ->
        ins_bitwisexor
    | TacShiftLeft ->
        ins_shift_left
    | TacShiftRight ->
        if unsigned then
          ins_logical_shift_right
        else
          ins_arith_shift_right
    | TacMult ->
        ins_mult
    | TacDiv ->
        if unsigned then
          ins_unsigned_div
        else
          ins_signed_div
    | _ ->
        failwith
          "Other binor cannot be facorised either instruction (Modulo) or type \
           (Mult/Div)"

  let mult_add_or_sub =
    let open KosuIrTAC.Asttac in
    function
    | TacAdd ->
        ins_madd
    | TacMinus ->
        ins_msub
    | _ ->
        failwith "Expected Add or Minus"

  let and_or_or_instruction =
    let open KosuIrTAC.Asttac in
    function
    | TacAnd ->
        ins_bitwiseand
    | TacOr ->
        ins_bitwiseor
    | _ ->
        failwith "Expected And or Or"

  let minstruction ?(_lcomm = "") instr = Instruction instr

  let load_label ?module_path label register =
    let label =
      module_path
      |> Option.map (fun mp -> asm_const_name mp label)
      |> Option.value ~default:label
    in
    let register = resize64 register in
    let load = Instruction (ADRP { dst = register; label }) in
    let add =
      Instruction
        (ADD
           {
             destination = register;
             operand1 = register;
             operand2 = `Label label;
             offset = true;
           }
        )
    in
    [ load; add ]

  (**
    [copy_large adress_str base_src_reg size] 
      Copy [size] bytes from value where the base address is held by the register [base_src_reg]
      at the adress [adress_str]

  *)
  let rec copy_large adress_str base_src_reg size =
    if size < 0L then
      failwith "Negive size to copy"
    else if size = 0L then
      []
    else
      let (data_size, offset) : data_size option * int64 =
        if size = 0L then
          (None, 0L)
        else if 1L <= size && size < 2L then
          (Some B, 1L)
        else if 2L <= size && size < 4L then
          (Some H, 2L)
        else if 4L <= size && size < 8L then
          (None, 4L)
        else
          (None, 8L)
      in
      let r10 =
        if offset >= 8L then
          x10
        else
          w10
      in
      let ldr_instructions =
        ldr_instr ~data_size ~mode:Postfix ~destination:r10
          (create_adress ~offset base_src_reg)
      in
      let str_instructions =
        str_instr ~data_size ~mode:Immediat ~source:r10 adress_str
      in
      let next_adresss_store = increment_adress offset adress_str in

      let next_size = Int64.sub size offset in

      let next_instructions =
        copy_large next_adresss_store base_src_reg next_size
      in

      ldr_instructions @ str_instructions @ next_instructions

  let is_register_size = function 1L | 2L | 4L | 8L -> true | _ -> false

  let compute_data_size ktype = function
    | 1L ->
        Some
          ( if not @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype
            then
              SB
            else
              B
          )
    | 2L ->
        Some
          ( if not @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype
            then
              SH
            else
              H
          )
    | _ ->
        None

  let copy_from_reg ?(variadic = false) register (address : address) ktype
      rprogram =
    let size = KosuIrTyped.Sizeof.sizeof rprogram ktype in
    let is_reg_size = is_register_size size in
    match is_reg_size with
    | false ->
        copy_large address register size
    | true ->
        let size =
          if variadic then
            max size 8L
          else
            size
        in
        let data_size = compute_data_size ktype size in
        let reg = resize_register (regsize_of_ktype size) register in
        let strs = str_instr ~data_size ~source:reg address in
        strs

  let load_register register (address : address) ktype ktype_size =
    match is_register_size ktype_size with
    | false ->
        []
    | true ->
        let data_size = compute_data_size ktype ktype_size in
        let reg = resize_register (regsize_of_ktype ktype_size) register in
        ldr_instr ~data_size ~destination:reg address
end

let unsigned_data_size = function SH -> H | SB -> B | t -> t

module FrameManager = struct
  open Instruction
  open Register
  open KosuIrTyped.Sizeof

  type frame_desc = {
    stack_param_count : int;
    locals_space : int64;
    need_xr : bool;
    stack_map : address IdVarMap.t;
    variadic_style : Aarch64AsmSpec.variadic_args_stye;
    discarded_values : (string * KosuIrTyped.Asttyped.rktype) list;
  }

  let indirect_return_var = "@xreturn"
  let indirect_return_type = KosuIrTyped.Asttyped.(RTPointer RTUnknow)
  let indirect_return_vt = (indirect_return_var, indirect_return_type)

  let align_16 size =
    let ( ** ) = Int64.mul in
    let ( ++ ) = Int64.add in
    let div = Int64.unsigned_div size 16L in
    let modulo =
      if Int64.unsigned_rem size 16L = 0L then
        0L
      else
        1L
    in
    16L ** (div ++ modulo)

  (* let frame_descriptor rprogram (tac_function: KosuIrTAC.Asttac.tac_function_decl) =
       let open KosuIrTAC.Asttac in
       let open Util.Args in
       let iparas, fparams, stack_parameters =
       Util.Args.consume_args
       ~fregs:Register.float_arguments_register
       ~iregs:Register.argument_registers
       ~fpstyle:(fun (_, kt) ->
         if KosuIrTyped.Asttyhelper.RType.is_float kt then
           Simple_Reg Float
         else
           Simple_Reg Other
       ) tac_function.rparameters
     in
       let need_xr =
         tac_function.return_type
         |> KosuIrTyped.Sizeof.sizeof rprogram
         |> is_register_size |> not
       in
       failwith "" *)
  let stack_args_passed_in_function ~variadic_style rprogram
      (fn_info_calls : KosuIrTyped.Asttyped.function_call_infos) =
    let open KosuIrTyped.Asttyped in
    let open Util.Args in
    fn_info_calls
    |> List.fold_left
         (fun acc { varia_index; parameters; return_type = _ } ->
           let fpstyle kt =
             if KosuIrTyped.Asttyhelper.RType.is_float kt then
               Simple_Reg Float
             else
               Simple_Reg Other
           in

           let _, _, stack_args, variadic_args =
             match variadic_style with
             | Aarch64AsmSpec.AbiDarwin ->
                 consume_args ?novariadic_args:varia_index
                   ~fregs:Register.float_arguments_register
                   ~iregs:Register.argument_registers ~fpstyle parameters
             | AbiSysV ->
                 let a, b, stack_args =
                   Util.Args.consume_args_sysv ~reversed_stack:true
                     ~fregs:Register.float_arguments_register
                     ~iregs:Register.argument_registers ~fpstyle parameters
                 in
                 (a, b, stack_args, [])
           in

           let stack_args_size =
             stack_args |> KosuIrTyped.Asttyhelper.RType.rtuple
             |> KosuIrTyped.Sizeof.sizeof rprogram
           in

           let variadic_size =
             variadic_args
             |> List.map (fun kt ->
                    KosuIrTyped.(
                      Sizeof.align_8 @@ KosuIrTyped.Sizeof.sizeof rprogram kt
                    )
                )
             |> List.fold_left Int64.add 0L
           in

           max (Int64.add stack_args_size variadic_size) acc
         )
         0L

  let frame_descriptor ~variadic_style
      (function_decl : KosuIrTAC.Asttac.tac_function_decl) rprogram =
    let open KosuIrTAC.Asttac in
    let open Util.Args in
    (* Since kosu function arent variadic, we can use sysV abi *)
    let iparas, fparams, stack_parameters =
      Util.Args.consume_args_sysv
        ~reversed_stack:
          ( match variadic_style with
          | Aarch64AsmSpec.AbiSysV ->
              true
          | AbiDarwin ->
              false
          )
        ~fregs:Register.float_arguments_register
        ~iregs:Register.argument_registers
        ~fpstyle:KosuCommon.Function.kosu_passing_style
        function_decl.rparameters
    in
    let locale_variables =
      function_decl.locale_var
      |> List.sort (fun lhs rhs ->
             let lsize = KosuIrTyped.Sizeof.sizeof_kt lhs.locale_ty in
             let rsize = KosuIrTyped.Sizeof.sizeof_kt rhs.locale_ty in
             compare rsize lsize
         )
      |> List.map
           KosuIrTAC.Asttachelper.LocaleVariable.variable_of_tac_locale_variable
    in
    let need_xr =
      function_decl.return_type
      |> KosuIrTyped.Sizeof.sizeof rprogram
      |> is_register_size |> not
    in
    let reg_parameters, _ =
      iparas |> ( @ ) fparams
      |> List.map (fun (variable, return_kind) ->
             match return_kind with
             | Simple_return reg ->
                 (variable, (variable, reg))
             | Double_return _ ->
                 failwith "Unreachable"
         )
      |> List.split
    in
    let stack_concat = reg_parameters @ locale_variables in
    let stack_concat =
      if need_xr then
        indirect_return_vt :: stack_concat
      else
        stack_concat
    in
    let fake_tuple = List.map snd stack_concat in
    let locals_space =
      fake_tuple |> KosuIrTyped.Asttyhelper.RType.rtuple
      |> KosuIrTyped.Sizeof.sizeof rprogram
    in
    let stack_future_call =
      stack_args_passed_in_function ~variadic_style rprogram
        function_decl.fn_call_infos
    in

    (* let () = Printf.printf "fn_name = %s, stack call = %Lu%!" function_decl.rfn_name stack_future_call in *)
    let locals_space = Int64.add locals_space stack_future_call in
    (* let () = Printf.printf "Locale space = %Lu\n" locals_space in *)
    let map =
      stack_concat
      |> List.mapi (fun index value -> (index, value))
      |> List.fold_left
           (fun acc (index, st) ->
             let offset = offset_of_tuple_index index fake_tuple rprogram in
             let x29_relative_address =
               locals_space |> Int64.neg |> Int64.add offset
             in
             let address =
               if x29_relative_address > -256L then
                 create_adress
                   ~offset:
                     (locals_space |> Int64.neg |> Int64.add offset
                     |> Int64.add stack_future_call
                     )
                   x29
               else
                 create_adress ~offset:(Int64.add stack_future_call offset) sp
             in
             (* let () = Printf.printf "-> %s : %s == [x29, %Ld] \n" (fst st) (KosuIrTyped.Asttypprint.string_of_rktype @@ snd @@ st) (offset) in *)
             IdVarMap.add st address acc
           )
           IdVarMap.empty
    in
    let stack_args_rktype = List.map snd stack_parameters in
    let stack_x29_offset = 16L in
    let map =
      stack_parameters |> List.mapi Util.couple
      |> List.fold_left
           (fun acc (index, st) ->
             let offset =
               offset_of_tuple_index index stack_args_rktype rprogram
             in
             let offset = Int64.add stack_x29_offset offset in
             let address = create_adress ~offset x29 in
             IdVarMap.add st address acc
           )
           map
    in
    {
      stack_param_count = Int64.to_int stack_future_call;
      locals_space;
      stack_map = map;
      discarded_values = function_decl.discarded_values;
      variadic_style;
      need_xr;
    }

  let address_of (variable, rktype) frame_desc =
    (* let () = Printf.printf "Lookup => %s : %s\n" (variable) (KosuIrTyped.Asttypprint.string_of_rktype rktype) in *)
    if List.mem (variable, rktype) frame_desc.discarded_values then
      None
    else
      match IdVarMap.find (variable, rktype) frame_desc.stack_map with
      | addres ->
          Some addres
      | exception Not_found ->
          failwith
            (Printf.sprintf "Not found: %s : %s" variable
               (KosuIrTyped.Asttypprint.string_of_rktype rktype)
            )

  let prologue_epilogue_stack_size framesize =
    if is_stp_range framesize then
      ([], { base = sp; offset = `ILitteral framesize })
    else
      ( mov_integer x15 framesize
        @ (instruction
          @@ SUB
               {
                 destination = sp;
                 operand1 = sp;
                 operand2 = `ILitteral (Int64.add 16L framesize);
               }
          )
          :: ins_add ~destination:x15 ~operand1:sp ~operand2:x15,
        { base = x15; offset = `ILitteral 0L }
      )

  let stp_inst ~vframe =
    if is_stp_range vframe then
      let address = { base = sp; offset = `ILitteral vframe } in
      [
        instruction
        @@ STP { x1 = x29; x2 = x30; address; adress_mode = Immediat };
      ]
    else
      let x29_address = create_adress ~offset:(Int64.add 8L vframe) sp in
      let x30_address = create_adress ~offset:vframe sp in
      [
        instruction
        @@ STR
             {
               data_size = None;
               source = x29;
               adress = x29_address;
               adress_mode = Immediat;
             };
        instruction
        @@ STR
             {
               data_size = None;
               source = x30;
               adress = x30_address;
               adress_mode = Immediat;
             };
      ]

  let ldp_instr ~vframe =
    if is_stp_range vframe then
      let address = { base = sp; offset = `ILitteral vframe } in
      [
        instruction
        @@ LDP { x1 = x29; x2 = x30; address; adress_mode = Immediat };
      ]
    else
      let x29_address = create_adress ~offset:(Int64.add 8L vframe) sp in
      let x30_address = create_adress ~offset:vframe sp in
      [
        instruction
        @@ LDR
             {
               data_size = None;
               destination = x29;
               adress_src = x29_address;
               adress_mode = Immediat;
             };
        instruction
        @@ LDR
             {
               data_size = None;
               destination = x30;
               adress_src = x30_address;
               adress_mode = Immediat;
             };
      ]

  let function_prologue (tac_function : KosuIrTAC.Asttac.tac_function_decl)
      rprogram fd =
    let open KosuIrTAC.Asttac in
    let open Util.Args in
    let stack_sub_size = align_16 (Int64.add 16L fd.locals_space) in
    let variable_frame_size = Int64.sub stack_sub_size 16L in
    (* let stp_instructions, address = prologue_epilogue_stack_size frame_register_offset in *)
    let base = stp_inst ~vframe:variable_frame_size in
    let stack_sub =
      Instruction
        (SUB
           {
             destination = sp;
             operand1 = sp;
             operand2 = `ILitteral stack_sub_size;
           }
        )
    in
    let alignx29 =
      Instruction
        (ADD
           {
             destination = x29;
             operand1 = sp;
             operand2 = `ILitteral variable_frame_size;
             offset = false;
           }
        )
    in
    let store_x8 =
      if fd.need_xr then
        [
          Instruction
            (STR
               {
                 data_size = None;
                 source = xr;
                 adress = address_of indirect_return_vt fd |> Option.get;
                 adress_mode = Immediat;
               }
            );
        ]
      else
        []
    in
    let iparas, fparams, _ =
      Util.Args.consume_args_sysv
        ~reversed_stack:
          ( match fd.variadic_style with
          | Aarch64AsmSpec.AbiSysV ->
              true
          | AbiDarwin ->
              false
          )
        ~fregs:Register.float_arguments_register
        ~iregs:Register.argument_registers
        ~fpstyle:KosuCommon.Function.kosu_passing_style tac_function.rparameters
    in
    let iparas =
      iparas
      |> List.map (fun (variable, return_kind) ->
             match return_kind with
             | Simple_return reg ->
                 (variable, reg)
             | Double_return _ ->
                 failwith "Unreachable"
         )
    in

    let fparams =
      fparams
      |> List.map (fun (variable, return_kind) ->
             match return_kind with
             | Simple_return reg ->
                 (variable, reg)
             | Double_return _ ->
                 failwith "Unreachable"
         )
    in

    let copy_instructions =
      iparas
      |> List.fold_left
           (fun acc ((name, kt), register) ->
             let whereis =
               address_of (name, kt) fd
               |> fun adr ->
               match adr with
               | Some a ->
                   a
               | None ->
                   failwith "From register setup null address"
             in
             acc @ copy_from_reg register whereis kt rprogram
           )
           []
    in

    let float_copy_instructions =
      fparams
      |> List.fold_left
           (fun acc ((name, kt), register) ->
             let whereis =
               address_of (name, kt) fd
               |> fun adr ->
               match adr with
               | Some a ->
                   a
               | None ->
                   failwith "From register setup null address"
             in
             acc @ copy_from_reg register whereis kt rprogram
           )
           []
    in

    (stack_sub :: base) @ [ alignx29 ] @ store_x8 @ copy_instructions
    @ float_copy_instructions

  let function_epilogue fd =
    let stack_space = align_16 (Int64.add 16L fd.locals_space) in
    let vframe = Int64.sub stack_space 16L in
    let base = ldp_instr ~vframe in
    let stack_add =
      Instruction
        (ADD
           {
             destination = sp;
             offset = false;
             operand1 = sp;
             operand2 = `ILitteral stack_space;
           }
        )
    in
    let return = Instruction RET in

    base @ [ stack_add; return ]

  let call_instruction ~origin =
    let call = Instruction (BL { cc = None; label = origin }) in
    [ call ]
end

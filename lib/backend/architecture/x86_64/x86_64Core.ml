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

module IdVar = KosuCommon.IdVar
module IdVarMap = KosuCommon.IdVarMap

type int_data_size = B | W | L | Q
type float_data_size = SS | SD
type data_size = IntSize of int_data_size | FloatSize of float_data_size

let intsize i = IntSize i
let floatsize f = FloatSize f
let iq = IntSize Q
let ib = IntSize B
let il = IntSize L
let iw = IntSize W
let fss = FloatSize SS
let fsd = FloatSize SD

let float_data_size_of_ktype = function
  | KosuIrTyped.Asttyped.RTFloat KosuFrontend.Ast.F32 ->
      FloatSize SS
  | RTFloat F64 ->
      FloatSize SD
  | _ ->
      failwith "expected float type"

let data_size_of_int64 = function
  | 1L ->
      Some B
  | 2L ->
      Some W
  | 4L ->
      Some L
  | 8L ->
      Some Q
  | _ ->
      None

let float_data_size_of_int64 = function
  | 4L ->
      Some SS
  | 8L ->
      Some SD
  | _ ->
      None

let need_long_promotion = function B | W -> true | _ -> false

let int_data_size_of_int64_def ?(default = Q) size =
  Option.value ~default @@ data_size_of_int64 size

let data_size_of_ktype ?(default = Q) rprogram ktype =
  match ktype with
  | KosuIrTyped.Asttyped.RTFloat KosuFrontend.Ast.F32 ->
      FloatSize SS
  | RTFloat F64 ->
      FloatSize SD
  | kt ->
      let size = KosuIrTyped.Sizeof.sizeof rprogram kt in
      IntSize (int_data_size_of_int64_def ~default size)

let data_size_of_ktype_opt rprogram ktype =
  match ktype with
  | KosuIrTyped.Asttyped.RTFloat KosuFrontend.Ast.F32 ->
      Some (FloatSize SS)
  | RTFloat F64 ->
      Some (FloatSize SD)
  | kt ->
      let size = KosuIrTyped.Sizeof.sizeof rprogram kt in
      size |> data_size_of_int64 |> Option.map (fun size -> IntSize size)

let data_size_min_l = function
  | IntSize s ->
      IntSize (match s with (Q | L) as s -> s | W | B -> L)
  | FloatSize _ as t ->
      t

let int64_of_data_size = function B -> 1L | W -> 2L | L -> 4L | Q -> 8L

let data_size_of_isize =
  let open KosuFrontend.Ast in
  function I8 -> B | I16 -> W | I32 -> L | I64 -> Q

let fdate_size_of_fsize =
  let open KosuFrontend.Ast in
  function F32 -> SS | F64 -> SD

let is_register_size = function 1L | 2L | 4L | 8L -> true | _ -> false

let is_register_size_ktype rprogram ktype =
  let size = KosuIrTyped.Sizeof.sizeof rprogram ktype in
  is_register_size size

module Register = struct
  type int_register =
    | RAX
    | RBX
    | RCX
    | RDX
    | RSI
    | RDI
    | RBP
    | RSP
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | RIP

  type float_register =
    | XMM0
    | XMM1
    | XMM2
    | XMM3
    | XMM4
    | XMM5
    | XMM6
    | XMM7
    | XMM8
    | XMM9
    | XMM10
    | XMM11
    | XMM12
    | XMM13
    | XMM14
    | XMM15

  type raw_register = IntegerReg of int_register | FloatReg of float_register

  type register = {
    register : raw_register;
    (* Since the instructions hold the size it's duplicated to have it
       also in the register expects if it's needed to be explicitly set
    *)
    register_size : int_data_size option;
  }

  let fregister floatreg =
    { register = FloatReg floatreg; register_size = None }

  let iregister ?size ireg =
    { register = IntegerReg ireg; register_size = size }

  let rxmm0 = FloatReg XMM0
  let rxmm1 = FloatReg XMM1
  let rxmm2 = FloatReg XMM2
  let rxmm3 = FloatReg XMM3
  let rxmm4 = FloatReg XMM4
  let rxmm5 = FloatReg XMM5
  let rxmm6 = FloatReg XMM6
  let rxmm7 = FloatReg XMM7
  let rxmm8 = FloatReg XMM8
  let rxmm9 = FloatReg XMM9
  let rxmm10 = FloatReg XMM10
  let rxmm11 = FloatReg XMM11
  let xmm0 = { register = rxmm0; register_size = None }
  let xmm1 = { register = rxmm1; register_size = None }
  let xmm2 = { register = rxmm2; register_size = None }
  let xmm3 = fregister XMM3
  let xmm4 = fregister XMM4
  let xmm5 = fregister XMM5
  let xmm6 = fregister XMM6
  let xmm7 = fregister XMM7
  let xmm8 = fregister XMM8
  let xmm9 = fregister XMM9
  let xmm10 = fregister XMM10
  let xmm11 = fregister XMM11
  let r9 = iregister R9
  let r10 = iregister R10
  let r11 = iregister R11
  let rbp = iregister RBP
  let rsp = iregister RSP
  let rdi = iregister RDI
  let rsi = iregister RSI
  let rip = iregister RIP
  let rdx = iregister RDX
  let rax = iregister RAX
  let rcx = iregister RCX
  let cl = iregister ~size:B RCX

  (* %rdi, %rsi, %rdx, %rcx, %r8, %r9 *)
  let argument_registers = [ RDI; RSI; RDX; RCX; R8; R9 ]
  let syscall_arguments_register = [ RDI; RSI; RDX; R10; R8; R9 ]

  let float_arguments_register =
    [ xmm0; xmm1; xmm2; xmm3; xmm4; xmm5; xmm6; xmm8; xmm7 ]

  let return_register rprogram ktype =
    if KosuIrTyped.Asttyhelper.RType.is_float ktype then
      Option.some xmm0
    else if is_register_size_ktype rprogram ktype then
      Some rax
    else
      None

  let passing_register_kt kt =
    let regs =
      List.map
        (fun reg -> { register = IntegerReg reg; register_size = None })
        argument_registers
    in
    let hold_in_register =
      is_register_size @@ KosuIrTyped.Sizeof.sizeof_kt kt
    in
    if hold_in_register then
      regs
    else
      List.tl regs

  (** RAX *)
  let tmp_rax_ktype ktype =
    if KosuIrTyped.Asttyhelper.RType.is_float ktype then
      xmm0
    else
      rax

  let tmp_rcx_ktype ktype =
    if KosuIrTyped.Asttyhelper.RType.is_float ktype then
      xmm1
    else
      rcx

  let tmp_r9_ktype ktype =
    if KosuIrTyped.Asttyhelper.RType.is_float ktype then
      xmm9
    else
      r9

  (** R10 *)
  let tmp_r10_ktype ktype =
    if KosuIrTyped.Asttyhelper.RType.is_float ktype then
      xmm10
    else
      r10

  let tmp_r11_ktype ktype =
    if KosuIrTyped.Asttyhelper.RType.is_float ktype then
      xmm11
    else
      r11

  let is_numerical_register = function
    | R9 | R10 | R11 | R12 | R13 | R14 | R15 ->
        true
    | _ ->
        false

  let full_letter_reg = function reg -> not @@ is_numerical_register reg
end

module Operande = struct
  type addr_offset = Addr_label of (string * int64) | Offset of int64

  type address = {
    offset : addr_offset;
    base : Register.register;
    index : Register.register option;
    scale : int;
  }

  type dst = [ `Register of Register.register | `Address of address ]

  type src =
    [ `ILitteral of int64 | `ULitteral of int64 | `Label of string | dst ]

  let src_of_dst : dst -> src = function
    | `Address addr ->
        `Address addr
    | `Register reg ->
        `Register reg

  let dummy_dst : dst = `Register Register.r10
  let is_adress = function `Address _ -> true | _ -> false
  let is_register = function `Register _ -> true | _ -> false

  let create_address_offset ?(offset = 0L) base =
    { offset = Offset offset; base; index = None; scale = 1 }

  let register_of_dst : dst -> Register.register = function
    | `Address _ ->
        raise (Invalid_argument "The destination is an address not a register")
    | `Register reg ->
        reg

  let create_address_label ~label ?(offset = 0L) base =
    { offset = Addr_label (label, offset); base; index = None; scale = 1 }

  let address_of_dst : dst -> address = function
    | `Address addr ->
        addr
    | `Register reg ->
        create_address_offset reg

  let increment_adress by address =
    {
      address with
      offset =
        ( match address.offset with
        | Offset o ->
            Offset (Int64.add by o)
        | Addr_label (s, o) ->
            Addr_label (s, Int64.add o by)
        );
    }

  (* let resize_dst data_size : dst -> dst = function
       | `Address _ as addr -> addr
       | `Register reg -> `Register (Register.resize_register data_size reg)

     let resize_src data_size : src -> src = function
       | #dst as dst -> dst |> resize_dst data_size |> src_of_dst
       | a -> a *)

  let increment_dst_address by : dst -> dst = function
    | `Register _ as reg ->
        reg
    | `Address addr ->
        `Address (increment_adress by addr)
end

module Condition_Code = struct
  type condition_code = E | NE | S | NS | G | GE | L | LE | A | AE | B | BE

  (* Float should use unsigned condition code *)
  let cc_of_tac_bin ?(is_unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacOr | TacAnd ->
        None
    | TacEqual ->
        Some E
    | TacDiff ->
        Some NE
    | TacSup ->
        Some
          ( if is_unsigned then
              B
            else
              L
          )
    | TacSupEq ->
        Some
          ( if is_unsigned then
              BE
            else
              LE
          )
    | TacInfEq ->
        Some
          ( if is_unsigned then
              AE
            else
              GE
          )
    | TacInf ->
        Some
          ( if is_unsigned then
              A
            else
              G
          )
end

module Instruction = struct
  open Register
  open Operande
  open Condition_Code

  type comment = Comment of string

  and line =
    | Instruction of instruction
    | Directive of string
    | Label of string
    | Line_Com of comment

  and instruction =
    | Mov of { size : data_size; source : src; destination : dst }
    | Movsl of { size : int_data_size; source : src; destination : dst }
    | Movzl of { size : int_data_size; source : src; destination : dst }
    | Set of {
        size : int_data_size;
        cc : condition_code;
        register : Register.register;
      }
    | Lea of { size : data_size; source : address; destination : register }
    | Neg of { size : data_size; source : register }
    | Not of { size : data_size; source : register }
    | Add of { size : data_size; destination : dst; source : src }
    | Sub of { size : data_size; destination : dst; source : src }
    | IMul of { size : data_size; destination : register option; source : src }
    | Mul of { size : data_size; destination : register option; source : src }
    | Xor of { size : int_data_size; destination : dst; source : src }
    | Or of { size : int_data_size; destination : dst; source : src }
    | And of { size : int_data_size; destination : dst; source : src }
    | Comi of { size : float_data_size; source : src; destination : src }
    | Ucomi of { size : float_data_size; source : src; destination : src }
    (* i to f*)
    | Cvts2s of {
        source_size : data_size;
        dst_size : data_size;
        source : src;
        destination : dst;
      }
    | Cvtts2s of {
        source_size : data_size;
        dst_size : data_size;
        source : src;
        destination : dst;
      }
    | Fdiv of {
        size : float_data_size;
        destination : float_register;
        source : src;
      }
    | IDivl of { (* l | q *)
                 size : int_data_size; divisor : src }
    | Div of { (* l | q *)
               size : int_data_size; divisor : src }
    (* Shift Left *)
    | Sal of { size : int_data_size; shift : src; destination : dst }
    (* Arithmetic right shift *)
    | Sar of { size : int_data_size; shift : src; destination : dst }
    (* Logical right shift *)
    | Shr of { size : int_data_size; shift : src; destination : dst }
    | Push of { size : int_data_size; source : src }
    | Pop of { size : int_data_size; destination : dst }
    | Cmp of { size : data_size; lhs : src; rhs : src }
    | Jmp of {
        cc : condition_code option;
        where : [ `Register of register | `Label of string ];
      }
    | Call of { what : [ `Register of register | `Label of string ] }
    | Syscall
    | Cltd
    | Cqto
    | Ret

  let division_split = function Q -> Cqto | _ -> Cltd

  let division_instruction ~unsigned size divisor =
    match (unsigned, size) with
    | false, Q ->
        Instruction (IDivl { size = Q; divisor })
    | true, Q ->
        Instruction (Div { size = Q; divisor })
    | false, _ ->
        Instruction (IDivl { size = L; divisor })
    | true, _ ->
        Instruction (Div { size = L; divisor })

  let mult_instruction data_size ~unsigned source destination =
    ignore unsigned;
    match data_size with
    | FloatSize _ ->
        Mul { size = data_size; source; destination }
    | IntSize _ ->
        IMul { size = data_size; source; destination }

  let neg_instruction size dst =
    match size with
    | IntSize _ as size ->
        [ Neg { size; source = dst } ]
    | FloatSize _ as size ->
        [
          Mov
            {
              size = il;
              source = `ULitteral (-1L);
              destination = `Register rax;
            };
          Cvts2s
            {
              source_size = il;
              dst_size = size;
              source = `Register rax;
              destination = `Register xmm1;
            };
          Mul { size; source = `Register xmm1; destination = Some dst };
        ]

  let cmp_instruction size ~lhs ~rhs =
    match size with
    | FloatSize fs ->
        Ucomi { size = fs; source = lhs; destination = rhs }
    | IntSize _ as size ->
        Cmp { size; lhs; rhs }

  let ins_add ~size ~destination ~source =
    [ Instruction (Add { size; destination; source }) ]

  let ins_sub ~size ~destination ~source =
    [ Instruction (Sub { size; destination; source }) ]

  let ins_bitwiseand ~size ~destination ~source =
    match size with
    | IntSize size ->
        [ Instruction (And { size; destination; source }) ]
    | FloatSize _ ->
        failwith "Invalid Bitwise and"

  let ins_bitwiseor ~size ~destination ~source =
    match size with
    | IntSize size ->
        [ Instruction (Or { size; destination; source }) ]
    | FloatSize _ ->
        failwith "Invalid Bitwize Xor"

  let ins_bitwisexor ~size ~destination ~source =
    match size with
    | IntSize size ->
        [ Instruction (Xor { size; destination; source }) ]
    | FloatSize _ ->
        failwith "Invalid Bitwexor float float"

  let ins_shiftleft ~size ~destination ~source =
    match size with
    | IntSize size ->
        [ Instruction (Sal { size; destination; shift = source }) ]
    | FloatSize _ ->
        failwith "Invalid ShiftLeft for float"

  let ins_shift_signed_right ~size ~destination ~source =
    match size with
    | IntSize size ->
        [ Instruction (Sar { size; destination; shift = source }) ]
    | FloatSize _ ->
        failwith "Invalid ins_shift_signed_righ for float"

  let ins_shift_unsigned_right ~size ~destination ~source =
    match size with
    | IntSize size ->
        [ Instruction (Shr { size; destination; shift = source }) ]
    | FloatSize _ ->
        failwith "Invalid ins_shift_unsigned_right for float"

  let ins_mult ~size ~destination ~source =
    let reg = register_of_dst destination in
    [ Instruction (IMul { size; destination = Some reg; source }) ]

  let binop_instruction_of_tacself ?(unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacAdd ->
        ins_add
    | TacMinus ->
        ins_sub
    | TacMult ->
        ins_mult
    | TacBitwiseAnd ->
        ins_bitwiseand
    | TacBitwiseOr ->
        ins_bitwiseor
    | TacBitwiseXor ->
        ins_bitwisexor
    | TacShiftLeft ->
        ins_shiftleft
    | TacShiftRight ->
        if unsigned then
          ins_shift_signed_right
        else
          ins_shift_unsigned_right
    | _ ->
        failwith "Binop cannot be factorised"

  let mov_promote_sign ~sign ~size ~src dst =
    match size with
    | (W | B) when is_register dst -> (
        match sign with
        | KosuFrontend.Ast.Signed ->
            Instruction (Movsl { size; destination = dst; source = src }) :: []
        | KosuFrontend.Ast.Unsigned ->
            Instruction (Movzl { size; destination = dst; source = src }) :: []
      )
    | _ ->
        Instruction
          (Mov { size = IntSize size; source = src; destination = dst })
        :: []
end

module LineInstruction = struct
  let instruction i = Instruction.Instruction i
end

let rec copy_large ~address_str ~base_address_reg size =
  let open Instruction in
  (* let () = Printf.printf "size = %Ld\n%!" size in *)
  if size < 0L then
    failwith "X86_64 : Negative size to copy"
  else if size = 0L then
    []
  else
    let (dsize, moved_size) : data_size * int64 =
      if 1L <= size && size < 2L then
        (ib, 1L)
      else if 2L <= size && size < 4L then
        (iw, 2L)
      else if 4L <= size && size < 8L then
        (il, 4L)
      else
        (iq, 8L)
    in
    [
      Instruction
        (Mov
           {
             size = dsize;
             source = `Address base_address_reg;
             destination = `Register Register.rax;
           }
        );
      Instruction
        (Mov
           {
             size = dsize;
             source = `Register Register.rax;
             destination = `Address address_str;
           }
        );
    ]
    @ copy_large
        ~address_str:(Operande.increment_adress moved_size address_str)
        ~base_address_reg:(Operande.increment_adress moved_size base_address_reg)
        (Int64.sub size moved_size)

(** 
    Copy the value in [register] at address [address]
    The function supposes that value in register is either the plain value if the value can be held in a register 
    or its address 
      
  *)
let copy_from_reg (register : Register.register) address ktype rprogram =
  let open Instruction in
  let size = KosuIrTyped.Sizeof.sizeof rprogram ktype in
  match size with
  | s when is_register_size s ->
      let data_size = data_size_of_ktype rprogram ktype in
      [
        Instruction
          (Mov
             {
               size = data_size;
               destination = `Address address;
               source = `Register register;
             }
          );
      ]
  | _ ->
      copy_large ~address_str:address
        ~base_address_reg:(Operande.create_address_offset register)
        size

let copy_from_reg_opt register address ktype rprogram =
  match register with
  | None ->
      []
  | Some reg ->
      copy_from_reg reg address ktype rprogram

let load_register register (address : Operande.address) ktype_size =
  let open Instruction in
  let data_size = ktype_size |> data_size_of_int64 |> Option.value ~default:Q in
  [
    Instruction
      (Mov
         {
           size = IntSize data_size;
           source = `Address address;
           destination = `Register register;
         }
      );
  ]

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

let address_of_const const_name =
  Operande.create_address_label ~label:const_name Register.rip

let load_label ?module_path label (dst : Operande.dst) =
  let open Instruction in
  let label =
    module_path
    |> Option.map (fun mp -> asm_const_name mp label)
    |> Option.value ~default:label
  in
  match dst with
  | `Register reg ->
      [
        Instruction
          (Lea
             {
               size = IntSize Q;
               destination = reg;
               source = address_of_const label;
             }
          );
      ]
  | `Address addr ->
      [
        Instruction
          (Lea
             {
               size = IntSize Q;
               destination = Register.rax;
               source = address_of_const label;
             }
          );
        Instruction
          (Mov
             {
               size = IntSize Q;
               destination = `Address addr;
               source = `Register Register.rax;
             }
          );
      ]

let load_float_label fsize labelname destination =
  let open Instruction in
  let address = address_of_const labelname in
  let source_f = `Address address in
  let fdata_size = FloatSize (fdate_size_of_fsize fsize) in
  match destination with
  | `Register _ ->
      [
        Instruction (Mov { size = fdata_size; source = source_f; destination });
      ]
  | `Address _ ->
      let tmp_xmm0 = `Register Register.xmm0 in
      [
        Instruction
          (Mov { size = fdata_size; source = source_f; destination = tmp_xmm0 });
        Instruction (Mov { size = fdata_size; source = tmp_xmm0; destination });
      ]

module FrameManager = struct
  (* open Instruction *)
  open Register
  open Operande
  open KosuIrTyped.Sizeof

  type frame_desc = {
    stack_param_count : int;
    locals_space : int64;
    need_result_ptr : bool;
    stack_map : address IdVarMap.t;
    discarded_values : (string * KosuIrTyped.Asttyped.rktype) list;
  }

  let stack_args_passed_in_function rprogram
      (fn_info_calls : KosuIrTyped.Asttyped.function_call_infos) =
    let open KosuIrTyped.Asttyped in
    let open Util.Args in
    fn_info_calls
    |> List.fold_left
         (fun acc { varia_index = _; parameters; return_type } ->
           let arguments_registers = passing_register_kt return_type in
           let _, _, stack_args =
             consume_args_sysv ~reversed_stack:true
               ~fregs:Register.float_arguments_register
               ~iregs:arguments_registers
               ~fpstyle:KosuCommon.Function.kosu_passing_style_kt parameters
           in

           stack_args |> KosuIrTyped.Asttyhelper.RType.rtuple
           |> KosuIrTyped.Sizeof.sizeof rprogram
           |> max acc
         )
         0L

  let indirect_return_var = "@xreturn"
  let indirect_return_type = KosuIrTyped.Asttyped.(RTPointer RTUnknow)
  let indirect_return_vt = (indirect_return_var, indirect_return_type)

  let frame_descriptor (function_decl : KosuIrTAC.Asttac.tac_function_decl)
      rprogram =
    let open KosuIrTAC.Asttac in
    let open Util.Args in
    let open Operande in
    let arguments_registers = passing_register_kt function_decl.return_type in
    let iparas, fparams, stack_parameters =
      Util.Args.consume_args_sysv ~reversed_stack:true
        ~fregs:Register.float_arguments_register ~iregs:arguments_registers
        ~fpstyle:KosuCommon.Function.kosu_passing_style
        function_decl.rparameters
    in
    let need_result_ptr =
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

    (* let () = Printf.printf "ktype : %s size = %Lu = need = %b\n"
         (KosuIrTyped.Asttypprint.string_of_rktype return_type)
         (sizeof rprogram return_type)
         (need_result_ptr)
       in *)
    let stack_concat = reg_parameters @ locale_variables in

    let stack_concat =
      if need_result_ptr then
        indirect_return_vt :: stack_concat
      else
        stack_concat
    in

    let fake_tuple = stack_concat |> List.map snd in
    let locals_space =
      fake_tuple |> KosuIrTyped.Asttyhelper.RType.rtuple
      |> KosuIrTyped.Sizeof.sizeof rprogram
    in
    let stack_future_call =
      stack_args_passed_in_function rprogram function_decl.fn_call_infos
    in
    let locals_space = Int64.add locals_space stack_future_call in

    let map =
      stack_concat
      |> List.mapi (fun index value -> (index, value))
      |> List.fold_left
           (fun acc (index, st) ->
             let offset = offset_of_tuple_index index fake_tuple rprogram in
             let rbp_relative_address =
               locals_space |> Int64.neg |> Int64.add offset
             in
             let address =
               create_address_offset ~offset:rbp_relative_address rbp
             in
             IdVarMap.add st address acc
           )
           IdVarMap.empty
    in
    let stack_args_rktype = List.map snd stack_parameters in
    let stack_rbp_offset = 16L in
    let map =
      stack_parameters |> List.mapi Util.couple
      |> List.fold_left
           (fun acc (index, st) ->
             let offset =
               offset_of_tuple_index index stack_args_rktype rprogram
             in
             let offset = Int64.add stack_rbp_offset offset in
             let address = create_address_offset ~offset rbp in
             IdVarMap.add st address acc
           )
           map
    in
    {
      stack_param_count = Int64.to_int stack_future_call;
      locals_space;
      stack_map = map;
      discarded_values = function_decl.discarded_values;
      need_result_ptr;
    }

  let address_of (variable, rktype) frame_desc =
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

  let call_instruction ~origin _stack_param (_fd : frame_desc) =
    let open Instruction in
    let call = Instruction (Call { what = origin }) in
    [ call ]

  (** 
        Assumption on [fn_register_params] 
          already containing [rdi] if return type cannot be contain in [rax] 
    *)
  let function_prologue (tac_function : KosuIrTAC.Asttac.tac_function_decl)
      rprogram fd =
    let open Instruction in
    let open KosuIrTAC.Asttac in
    let open Util.Args in
    let base = Instruction (Push { size = Q; source = `Register rbp }) in
    let sub_align = KosuCommon.OffsetHelper.align_16 fd.locals_space in
    let sp_sub =
      [
        Instruction
          (Mov
             { size = iq; source = `Register rsp; destination = `Register rbp }
          );
        Instruction
          (Sub
             {
               size = IntSize Q;
               destination = `Register rsp;
               source = `ILitteral sub_align;
             }
          );
      ]
    in
    let arguments_registers = passing_register_kt tac_function.return_type in
    let iparas, fparams, _ =
      Util.Args.consume_args_sysv ~reversed_stack:true
        ~fregs:Register.float_arguments_register ~iregs:arguments_registers
        ~fpstyle:KosuCommon.Function.kosu_passing_style tac_function.rparameters
    in

    let indirect_return =
      if fd.need_result_ptr then
        Some (indirect_return_vt, rdi)
      else
        None
    in

    let indirect_return_instructions =
      indirect_return
      |> Option.map (fun (indirect_return, reg) ->
             let whereis = Option.get @@ address_of indirect_return fd in
             copy_from_reg reg whereis (snd indirect_return) rprogram
         )
      |> Option.value ~default:[]
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

    let copy_reg_instructions =
      iparas @ fparams
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

    (base :: sp_sub) @ indirect_return_instructions @ copy_reg_instructions

  let function_epilogue _fd =
    let open Instruction in
    let base =
      Instruction
        (Mov { size = iq; destination = `Register rsp; source = `Register rbp })
    in
    let pop = Instruction (Pop { size = Q; destination = `Register rbp }) in
    let return = Instruction Ret in
    [ base; pop; return ]
end

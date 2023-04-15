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

module IdVar = Common.IdVar
module IdVarMap = Common.IdVarMap


type int_data_size = B | W | L | Q
type float_data_size = SS | SD
type _ data_size = 
  | IntSize : int_data_size -> int_data_size data_size
  | FloatSize : float_data_size -> float_data_size data_size

let iq = IntSize Q
let ib = IntSize B
let il = IntSize L
let iw = IntSize W

let float_data_size_of_ktype = function
| KosuIrTyped.Asttyped.RTFloat KosuFrontend.Ast.F32 -> FloatSize SS
| RTFloat F64 -> FloatSize SD
| _ -> failwith "expected float type"

let data_size_of_int64 = function
  | 1L -> Some B
  | 2L -> Some W
  | 4L -> Some L
  | 8L -> Some Q
  | _ -> None

let float_data_size_of_int64 = function
| 4L -> Some SS
| 8L -> Some SD 
| _ -> None

let need_long_promotion = function B | W -> true | _ -> false

let data_size_of_int64_def ?(default = Q) size =
  Option.value ~default @@ data_size_of_int64 size

let int64_of_data_size = function B -> 1L | W -> 2L | L -> 4L | Q -> 8L

let data_size_of_isize =
  let open KosuFrontend.Ast in
  function I8 -> B | I16 -> W | I32 -> L | I64 -> Q

let fdate_size_of_fsize = 
  let open KosuFrontend.Ast in
  function
  | F32 -> SS
  | F64 -> SD  

let is_register_size = function 1L | 2L | 4L | 8L -> true | _ -> false

module Register = struct
  type raw_register =
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

  type int_register = { size : int_data_size; reg : raw_register }

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

  let is_numerical_register = function
    | R9 | R10 | R11 | R12 | R13 | R14 | R15 -> true
    | _ -> false

  let full_letter_reg = function reg -> not @@ is_numerical_register reg

  type register = 
  | IntReg of int_register
  | FloatReg of float_register

  let to_float_register = function
  | FloatReg _ as fr -> fr
  | IntReg integ -> begin match integ.reg with
    | RAX -> FloatReg XMM0
    | RBX -> FloatReg XMM1
    | RCX -> FloatReg XMM2
    | RDX -> FloatReg XMM3
    | RSI -> FloatReg XMM4
    | RDI -> FloatReg XMM5
    | RBP -> FloatReg XMM6
    | RSP -> FloatReg XMM7
    | R8 ->  FloatReg XMM8
    | R9 ->  FloatReg XMM9
    | R10 -> FloatReg XMM10
    | R11 -> FloatReg XMM11
    | R12 -> FloatReg XMM12
    | R13 -> FloatReg XMM13
    | R14 -> FloatReg XMM14
    | R15 -> FloatReg XMM15
    | RIP -> failwith "RIP: doesnt have Float conterpart"
  end

  let resize_register size = function
  | IntReg register -> IntReg { register with size }
  | FloatReg _ as register -> register

  let reg_of_ktype rprogram ktype ~register = 
    match ktype with
    | KosuIrTyped.Asttyped.RTFloat _ -> to_float_register register
    | _ -> 
      let sizeof = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
      let size = data_size_of_int64_def ~default:Q sizeof in
      resize_register size register


  let size_of_reg_opt = function
  | IntReg r -> Some r.size
  | FloatReg _ -> None

  (* %rdi, %rsi, %rdx, %rcx, %r8, %r9 *)
  let argument_registers = [ RDI; RSI; RDX; RCX; R8; R9 ]
  let syscall_arguments_register = [ RDI; RSI; RDX; R10; R8; R9 ]

  let float_arguments_register = [
    FloatReg XMM0;
    FloatReg XMM1;
    FloatReg XMM2;
    FloatReg XMM3;
    FloatReg XMM4;
    FloatReg XMM5;
    FloatReg XMM6;
    FloatReg XMM7
  ]
  let sized_register size register = IntReg { size; reg = register }
  let r9q = IntReg { size = Q; reg = R9 }
  let rbpq = IntReg { size = Q; reg = RBP }
  let rspq = IntReg { size = Q; reg = RSP }
  let rdiq = IntReg { size = Q; reg = RDI }
  let rsiq = IntReg { size = Q; reg = RSI }
  let ripq = IntReg { size = Q; reg = RIP }
  let rdxq = IntReg { size = Q; reg = RDX }
  let raxq = IntReg { size = Q; reg = RAX }
  (* let is_aliased lhs rhs = lhs.reg = rhs.reg *)

  let return_register rprogram ktype  =
    if KosuIrTyped.Asttyhelper.RType.is_float ktype then Option.some @@ FloatReg XMM0 else
    let return_size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype  in
    return_size |> data_size_of_int64
    |> Option.map (fun size -> resize_register size raxq)

  (** Rax *)
  let tmp_rax size =
    let data_size = size |> data_size_of_int64 |> Option.value ~default:Q in
    sized_register data_size RAX

  let tmp_rax_ktype rpogram ktype =
    let size =
      data_size_of_int64_def
      @@ KosuIrTyped.Asttyconvert.Sizeof.sizeof rpogram ktype
    in
    sized_register size RAX

  let tmp_r9 size =
    let data_size = size |> data_size_of_int64 |> Option.value ~default:Q in
    sized_register data_size R9

  let tmp_r9_ktype rpogram ktype =
    let size =
      data_size_of_int64_def
      @@ KosuIrTyped.Asttyconvert.Sizeof.sizeof rpogram ktype
    in
    sized_register size R9

  (** R10 *)
  let tmp_r10 size =
    let data_size = size |> data_size_of_int64 |> Option.value ~default:Q in
    sized_register data_size R10

  let tmp_r10_ktype rpogram ktype =
    let size =
      data_size_of_int64_def
      @@ KosuIrTyped.Asttyconvert.Sizeof.sizeof rpogram ktype
    in
    sized_register size R10

  let tmp_r11 size =
    let data_size = size |> data_size_of_int64 |> Option.value ~default:Q in
    sized_register data_size R11

  let tmp_r11_ktype rpogram ktype =
    let size =
      data_size_of_int64_def
      @@ KosuIrTyped.Asttyconvert.Sizeof.sizeof rpogram ktype
    in
    sized_register size R11
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
    [ `ILitteral of int64 | `F64Litteral of float | `Label of string | dst ]

  let src_of_dst : dst -> src = function
    | `Address addr -> `Address addr
    | `Register reg -> `Register reg

  let dummy_dst : dst = `Register (IntReg { size = L; reg = R10 })
  let is_adress = function `Address _ -> true | _ -> false
  let is_register = function `Register _ -> true | _ -> false

  let create_address_offset ?(offset = 0L) base =
    { offset = Offset offset; base; index = None; scale = 1 }

  let register_of_dst : dst -> Register.register = function
    | `Address _ ->
        raise (Invalid_argument "The destination is an address not a register")
    | `Register reg -> reg

  let create_address_label ~label ?(offset = 0L) base =
    { offset = Addr_label (label, offset); base; index = None; scale = 1 }

  let address_of_dst : dst -> address = function
    | `Address addr -> addr
    | `Register reg -> create_address_offset reg

  let increment_adress by address =
    {
      address with
      offset =
        (match address.offset with
        | Offset o -> Offset (Int64.add by o)
        | Addr_label (s, o) -> Addr_label (s, Int64.add o by));
    }

  let resize_dst data_size : dst -> dst = function
    | `Address _ as addr -> addr
    | `Register reg -> `Register (Register.resize_register data_size reg)

  let resize_src data_size : src -> src = function
    | #dst as dst -> dst |> resize_dst data_size |> src_of_dst
    | a -> a

  let increment_dst_address by : dst -> dst = function
    | `Register _ as reg -> reg
    | `Address addr -> `Address (increment_adress by addr)
end

module Condition_Code = struct
  type condition_code = E | NE | S | NS | G | GE | L | LE | A | AE | B | BE

  let cc_of_tac_bin ?(is_unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacOr | TacAnd -> None
    | TacEqual -> Some E
    | TacDiff -> Some NE
    | TacSup -> Some (if is_unsigned then B else L)
    | TacSupEq -> Some (if is_unsigned then BE else LE)
    | TacInfEq -> Some (if is_unsigned then AE else GE)
    | TacInf -> Some (if is_unsigned then A else G)
end

module Instruction = struct
  open Register
  open Operande
  open Condition_Code

  type comment = Comment of string

  and raw_line =
    | Instruction of instruction
    | Directive of string
    | Label of string
    | Line_Com of comment

  and instruction =
    | Mov : { size : 'a data_size; source : src; destination : dst } -> instruction
    | Movsl of { size : int_data_size; source : src; destination : dst }
    | Movzl of { size : int_data_size; source : src; destination : dst }
    | Set : { cc : condition_code; register : Register.register } -> instruction
    | Lea : { size : 'a data_size; source : address; destination : register } -> instruction
    | Neg : { size : 'a data_size; source : register } -> instruction
    | Not : { size : 'a data_size; source : register } -> instruction
    | Add : { size : 'a data_size; destination : dst; source : src } -> instruction
    | Sub : { size : 'a data_size; destination : dst; source : src } -> instruction
    | IMul : { size : 'a data_size; destination : register; source : src } -> instruction
    | Xor of { size : int_data_size; destination : dst; source : src }
    | Or of { size :  int_data_size; destination : dst; source : src }
    | And of { size : int_data_size; destination : dst; source : src }
    | Fdiv of { size : float_data_size; destination : float_register; source: float_register }
    | IDivl of { (* l | q *)
                 size : int_data_size; divisor : src }
    | Div of { (* l | q *)
               size :  int_data_size; divisor : src }
    (* Shift Left *)
    | Sal of { size : int_data_size; shift : src; destination : dst }
    (* Arithmetic right shift *)
    | Sar of { size : int_data_size; shift : src; destination : dst }
    (* Logical right shift *)
    | Shr of { size : int_data_size; shift : src; destination : dst }
    | Push of  { size : int_data_size; source : src } 
    | Pop of { size : int_data_size; destination : dst } 
    | Cmp : { size : 'a data_size; lhs : src; rhs : src } -> instruction
    | Jmp of {
        cc : condition_code option;
        where : [ `Register of int_register | `Label of string ];
      }
    | Call of { what : [ `Register of int_register | `Label of string ] }
    | Syscall
    | Cltd
    | Cqto
    | Ret

  let division_split = function Q -> Cqto | _ -> Cltd

  let division_instruction ~unsigned size divisor =
    match (unsigned, size) with
    | false, Q -> Instruction (IDivl { size =  Q; divisor })
    | true, Q -> Instruction (Div { size = Q; divisor })
    | false, _ -> Instruction (IDivl { size = L; divisor })
    | true, _ -> Instruction (Div { size = L; divisor })

  let ins_add ~size ~destination ~source =
    [ Instruction (Add { size; destination; source }) ]

  let ins_sub ~size ~destination ~source =
    [ Instruction (Sub { size; destination; source }) ]

  let ins_bitwiseand: type a. size:a data_size -> destination:dst -> source:src -> raw_line list = fun ~size ~destination ~source -> 
    match size with
    | IntSize size ->
      [ Instruction (And { size; destination; source }) ]
    | FloatSize _ ->
      failwith "Invalid Bitwise and"

  let ins_bitwiseor: type a. size:a data_size -> destination:dst -> source:src -> raw_line list = fun ~size ~destination ~source -> 
    match size with
    | IntSize size ->
      [ Instruction (Or { size; destination; source }) ]
    | FloatSize _ ->
      failwith "Invalid Bitwize Xor"

  let ins_bitwisexor: type a. size:a data_size -> destination:dst -> source:src -> raw_line list = fun ~size ~destination ~source -> 
    match size with
    | IntSize size ->
      [ Instruction (Xor { size; destination; source }) ]
    | FloatSize _ ->
      failwith "Invalid Bitwexor float float"

  let ins_shiftleft: type a. size:a data_size -> destination:dst -> source:src -> raw_line list = fun ~size ~destination ~source -> 
    match size with
    | IntSize size ->
      [ Instruction (Sal { size; destination; shift = source }) ]
    | FloatSize _ -> 
      failwith "Invalid ShiftLeft for float"

  let ins_shift_signed_right: type a. size:a data_size -> destination:dst -> source:src -> raw_line list = fun ~size ~destination ~source -> 
    match size with
    | IntSize size -> [ Instruction (Sar { size; destination; shift = source }) ]
    | FloatSize _ ->
      failwith "Invalid ins_shift_signed_righ for float"

  let ins_shift_unsigned_right: type a. size:a data_size -> destination:dst -> source:src -> raw_line list = fun ~size ~destination ~source -> 
    match size with
    | IntSize size -> [ Instruction (Shr { size; destination; shift = source }) ]
    | FloatSize _ ->
      failwith "Invalid ins_shift_unsigned_right for float"

  let ins_mult ~size ~destination ~source =
    [
      (let reg = register_of_dst destination in
       Instruction (IMul { size; destination = reg; source }));
    ]

  let binop_instruction_of_tacself ?(unsigned = false) =
    let open KosuIrTAC.Asttac in
    function
    | TacAdd -> ins_add
    | TacMinus -> ins_sub
    | TacMult -> ins_mult
    | TacBitwiseAnd -> ins_bitwiseand
    | TacBitwiseOr -> ins_bitwiseor
    | TacBitwiseXor -> ins_bitwisexor
    | TacShiftLeft -> ins_shiftleft
    | TacShiftRight ->
        if unsigned then ins_shift_signed_right else ins_shift_unsigned_right
    | _ -> failwith "Binop cannot be factorised"

  let mov_promote_sign ~sign ~size ~src dst =
    match size with
    | (W | B) when is_register dst -> (
        match sign with
        | KosuFrontend.Ast.Signed ->
            Instruction
              (Movsl { size; destination = resize_dst L dst; source = src })
            :: []
        | KosuFrontend.Ast.Unsigned ->
            Instruction
              (Movzl { size; destination = resize_dst L dst; source = src })
            :: [])
    | _ -> Instruction (Mov { size = IntSize size; source = src; destination = dst }) :: []
end

let rec copy_large ~address_str ~base_address_reg size =
  let open Instruction in
  if size < 0L then failwith "X86_64 : Negative size to copy"
  else if size = 0L then []
  else
    let dsize = size |> data_size_of_int64 |> Option.value ~default:Q in
    let moved_size = int64_of_data_size dsize in
    let sized_rax = Register.tmp_rax size in
    [
      Instruction
        (Mov
           {
             size = IntSize dsize;
             source = `Address base_address_reg;
             destination = `Register sized_rax;
           });
      Instruction
        (Mov
           {
             size = IntSize dsize;
             source = `Register sized_rax;
             destination = `Address address_str;
           });
    ]
    @ copy_large
        ~address_str:(Operande.increment_adress moved_size address_str)
        ~base_address_reg:
          (Operande.increment_adress moved_size base_address_reg)
        (Int64.sub size moved_size)

(** 
    Copy the value in [register] at address [address]
    The function supposes that value in register is either the plain value if the value can be held in a register 
    or its address 
      
  *)
let copy_from_reg (register : Register.register) address ktype rprogram =
  let open Instruction in
  let size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
  match size with
  | s when is_register_size s ->
      let data_size = Option.get @@ data_size_of_int64 s in
      [
        Instruction
          (Mov
             {
               size = IntSize data_size;
               destination = `Address address;
               source = `Register (Register.resize_register data_size register);
             });
      ]
  | _ ->
      copy_large ~address_str:address
        ~base_address_reg:(Operande.create_address_offset register)
        size

let copy_from_reg_opt register address ktype rprogram =
  match register with
  | None -> []
  | Some reg -> copy_from_reg reg address ktype rprogram

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
         });
  ]

let asm_const_name current_module const_name =
  Printf.sprintf "_%s_%s"
    (current_module |> String.map (fun c -> if c = ':' then '_' else c))
    const_name

let address_of_const const_name =
  Operande.create_address_label ~label:const_name Register.ripq

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
          (Lea { size = IntSize Q; destination = reg; source = address_of_const label });
      ]
  | `Address addr ->
      [
        Instruction
          (Lea
             {
               size = IntSize Q;
               destination = Register.raxq;
               source = address_of_const label;
             });
        Instruction
          (Mov
             {
               size = IntSize Q;
               destination = `Address addr;
               source = `Register Register.raxq;
             });
      ]

  let load_float_label fsize labelname destination = 
    let open Instruction in
    let address = address_of_const labelname in
    let fdata_size = FloatSize (fdate_size_of_fsize fsize) in
    [
      Instruction (Mov {size = fdata_size; source = `Address address; destination;})
    ]



module FrameManager = struct
  (* open Instruction *)
  open Register
  open Operande
  open KosuIrTyped.Asttyconvert.Sizeof

  type frame_desc = {
    stack_param_count : int;
    locals_space : int64;
    need_result_ptr : bool;
    stack_map : address IdVarMap.t;
    discarded_values : (string * KosuIrTyped.Asttyped.rktype) list;
  }

  let indirect_return_var = "@xreturn"
  let indirect_return_type = KosuIrTyped.Asttyped.(RTPointer RTUnknow)
  let indirect_return_vt = (indirect_return_var, indirect_return_type)

  let frame_descriptor ?(stack_future_call = 0L)
      ~(fn_register_params : (string * KosuIrTyped.Asttyped.rktype) list)
      ~(stack_param : (string * KosuIrTyped.Asttyped.rktype) list) ~return_type
      ~locals_var ~discarded_values rprogram =
    let open Operande in
    let stack_param_count = stack_param |> List.length in
    let need_result_ptr =
      return_type
      |> KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram
      |> is_register_size |> not
    in

    (* let () = Printf.printf "ktype : %s size = %Lu = need = %b\n"
         (KosuIrTyped.Asttypprint.string_of_rktype return_type)
         (sizeof rprogram return_type)
         (need_result_ptr)
       in *)
    let stack_concat = fn_register_params @ stack_param @ locals_var in

    let fake_tuple = stack_concat |> List.map snd in
    let locals_space =
      fake_tuple |> KosuIrTyped.Asttyhelper.RType.rtuple
      |> KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram
    in
    let locals_space = Int64.add locals_space stack_future_call in

    let map =
      stack_concat
      |> List.mapi (fun index value -> (index, value))
      |> List.fold_left
           (fun acc (index, st) ->
             let offset =
               offset_of_tuple_index ~generics:(Hashtbl.create 0) index
                 fake_tuple rprogram
             in
             let rbp_relative_address =
               locals_space |> Int64.neg |> Int64.add offset
             in
             let address =
               create_address_offset ~offset:rbp_relative_address
                 rbpq
             in
             IdVarMap.add st address acc)
           IdVarMap.empty
    in
    {
      stack_param_count;
      locals_space;
      stack_map = map;
      discarded_values;
      need_result_ptr;
    }

  let address_of (variable, rktype) frame_desc =
    if List.mem (variable, rktype) frame_desc.discarded_values then None
    else
      match IdVarMap.find (variable, rktype) frame_desc.stack_map with
      | addres -> Some addres
      | exception Not_found ->
          failwith
            (Printf.sprintf "Not found: %s : %s" variable
               (KosuIrTyped.Asttypprint.string_of_rktype rktype))

  let call_instruction ~origin _stack_param (_fd : frame_desc) =
    let open Instruction in
    let call = Instruction (Call { what = origin }) in
    [ call ]

  (** 
        Assumption on [fn_register_params] 
          already containing [rdi] if return type cannot be contain in [rax] 
    *)
  let function_prologue ~fn_register_params ~stack_params rprogram fd =
    let open Instruction in
    let base = Instruction (Push { size = Q; source = `Register rbpq }) in
    let sub_align = Common.OffsetHelper.align_16 fd.locals_space in
    let sp_sub =
      [
        Instruction
          (Mov
             { size = IntSize Q; source = `Register rspq; destination = `Register rbpq });
        Instruction
          (Sub
             {
               size = IntSize Q;
               destination = `Register rspq;
               source = `ILitteral sub_align;
             });
      ]
    in
    let _is_indirect_return =
      match List.nth_opt fn_register_params 0 with
      | Some t when t = indirect_return_vt -> true
      | _ -> false
    in
    let copy_reg_instruction =
      fn_register_params
      |> Util.ListHelper.combine_safe argument_registers
      |> List.mapi Util.couple
      |> List.fold_left
           (fun acc (_, (register, (name, kt))) ->
             let whereis =
               match address_of (name, kt) fd with
               | Some a -> a
               | None -> failwith "X86_64: No stack allocated for this variable"
             in
             let data_size =
               data_size_of_int64_def
               @@ KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram kt
             in
             let sized_regiser = sized_register data_size register in
             acc @ copy_from_reg sized_regiser whereis kt rprogram)
           []
    in
    let stack_params_offset =
      stack_params
      |> List.map (fun (_, kt) ->
             if KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram kt > 8L then
               KosuIrTyped.Asttyhelper.RType.rpointer kt
             else kt)
    in
    let sp_address =
      Operande.create_address_offset ~offset:(Int64.add 16L sub_align) rspq
    in
    let copy_stack_params_instruction =
      stack_params
      |> List.mapi (fun index value -> (index, value))
      |> List.fold_left
           (fun acc (index, (name, kt)) ->
             let sizeofkt =
               KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram kt
             in
             let offset =
               offset_of_tuple_index index stack_params_offset rprogram
             in
             let future_address_location =
               address_of (name, kt) fd |> fun adr ->
               match adr with
               | Some a -> a
               | None -> failwith "On stack setup null address"
             in

             let tmprreg = tmp_rax sizeofkt in
             let param_stack_address = increment_adress offset sp_address in
             let load_instruction =
               load_register tmprreg param_stack_address sizeofkt
             in
             let str_instruction =
               copy_from_reg tmprreg future_address_location kt rprogram
             in
             acc @ str_instruction @ load_instruction)
           []
    in
    (base :: sp_sub) @ copy_reg_instruction @ copy_stack_params_instruction

  let function_epilogue _fd =
    let open Instruction in
    let base =
      Instruction
        (Mov { size = IntSize Q; destination = `Register rspq; source = `Register rbpq })
    in
    let pop = Instruction (Pop { size = Q; destination = `Register rbpq }) in
    let return = Instruction Ret in
    [ base; pop; return ]
end

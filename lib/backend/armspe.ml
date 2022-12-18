open KosuIrTAC.Asttac
open Common

module IdVar = struct
  type t = string * KosuIrTyped.Asttyped.rktype

  let compare = compare
end

module IdVarMap = Map.Make (IdVar)

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

  let return_register_ktype = function
  | 8L | 16L | 32L -> Register32 W0
  | 64L -> Register64 X0
  | _ -> Register64 X8

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

  let to_32bits = function
  | Register64 reg -> Register32 (reg32_of_64 reg)
  | (Register32 _) as t -> t

  let size_of_ktype_size s = if s > 32L then SReg32 else SReg64
  let size_of_reg = function
  | Register32 _ -> SReg32
  | Register64 _ -> SReg64

  let reg_of_size size reg =
    match size with
    | SReg32 -> to_32bits reg
    | SReg64 -> to_64bits reg

  let tmp64reg = Register64 X8
  let tmp32reg = Register32 W8

  let tmp64reg_2 = Register64 X9
  let tmp32reg_2 = Register32 W9

  let tmpreg_of_size = fun size -> if size <= 32L then tmp32reg else tmp64reg
  let tmpreg_of_size_2 = fun size -> if size <= 32L then tmp32reg_2 else tmp64reg_2

  let tmpreg_of_ktype rprogram ktype = 
    let size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
    tmpreg_of_size size

  let tmpreg_of_ktype_2 rprogram ktype = 
    let size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
    tmpreg_of_size_2 size
end

open Register
type src = [ `ILitteral of int64 | `Register of register | `Label of string]
type address = {
  base : register;
  offset : int64;
}

let create_adress ?(offset = 0L) base = 
  {base; offset}

let increment_adress off adress = {
  adress with offset = Int64.add adress.offset off
}

module Instruction = struct
  


  type instruction =
  | Mov of {
   
    destination : Register.register;
    (* Careful int max INT16 *)
    flexsec_operand : src;
  }
| Not of {
 
  destination : Register.register;
  source : Register.register
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
  }
| SUB of {
   
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
  base: Register.register;
  offset: int;
  adress_mode : adress_mode;
}
| B of { cc : condition_code option; label : Common.label }
| BL of { cc : condition_code option; label : Common.label }
| BR of { rcc : condition_code option; eg : Register.register }
| BLR of { cc : condition_code option; reg : Register.register }
| SVC of int64
| RET

type comment = 
| Comment of string

type raw_line =
| Instruction of instruction
| Directive of string
| Label of string
| Line_Com of comment

type line = (raw_line * comment option)



let load_label label register = 
  let register = to_64bits register in
  let load =  Instruction ( ADRP {dst = register; label}) in
  let add = Instruction ( ADD {destination = register; operand1 = register; operand2 = `Label label} ) in
  load::add::[]

end

open Instruction



let argument_registers = [ 
  Register64 X0; 
  Register64 X1; 
  Register64 X2; 
  Register64 X3; 
  Register64 X4; 
  Register64 X5; 
  Register64 X6; 
  Register64 X7; 
  Register64 X8 
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


let is_register_size = function
| 8L | 16L | 32L | 64L -> true
| _ -> false

let compute_data_size ktype = function
| 8L -> Some (if not @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then SB else B)
| 16L -> Some (if not @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then SH else H)
| _ -> None


module FrameManager = struct
  type frame_desc = {
  stack_param_count : int;
  locals_space : int64;
  stack_map : address IdVarMap.t;
}

let frame_descriptor ~fn_register_params ~(stack_param: (string * KosuIrTyped.Asttyped.rktype) list) ~locals_var ~rprogram =
  let stack_param_count = stack_param |> List.length in
  let stack_concat = fn_register_params @ locals_var in
  let fake_tuple = stack_concat |> List.map snd in
  let locals_space =
    fake_tuple |> KosuIrTyped.Asttyhelper.RType.rtuple
    |> KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram
  in
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
               (Register64 X30)
           in
           IdVarMap.add st adress acc)
         IdVarMap.empty
  in
  { stack_param_count; locals_space; stack_map = map }

  let call_instruction ~origin _stack_param (_fd: frame_desc) = 

    let call = Instruction ( Instruction.BL { cc = None; label = Label origin}) in
    [call]

let adress_of (variable,rktype)  frame_desc: address = 
  IdVarMap.find (variable, rktype) frame_desc.stack_map
end

module Codegen = struct

  let rec copy_large adress_str base_src_reg size = 
    if size < 0L
      then failwith "Negive size to copy"
  else if size = 0L
    then []
  else if size < 16L && size >= 8L
    then [
      Instruction( LDR {
        data_size = Some B;
        destination = reg_of_32 W9;
        adress_src = create_adress (base_src_reg);
        adress_mode = Immediat
      });
      Instruction (Instruction.STR {
        data_size = Some B;
        source = reg_of_32 W9;
        adress = adress_str;
        adress_mode = Immediat
      }) ;
      Instruction (Instruction.ADD {
        destination = base_src_reg;
        operand1 = base_src_reg;
        operand2 = `ILitteral 8L
      })
    ] @ (copy_large (increment_adress 16L adress_str) base_src_reg (Int64.sub size 8L))
  else if size < 32L && size >= 16L
    then [
      Instruction (Instruction.LDR {
        data_size = Some H;
        destination = reg_of_32 W9;
        adress_src = create_adress base_src_reg;
        adress_mode = Immediat
      });
      Instruction (Instruction.STR {
        data_size = Some H;
        source = reg_of_32 W9;
        adress = adress_str;
        adress_mode = Immediat
      }) ;
      Instruction (Instruction.ADD {
        destination = base_src_reg;
        operand1 = base_src_reg;
        operand2 = `ILitteral 16L
      })
    ] @ (copy_large (increment_adress 16L adress_str) base_src_reg (Int64.sub size 16L))
  else if size < 64L && size >= 32L
    then [
      Instruction (Instruction.LDR {
        data_size = None;
        destination = reg_of_32 W9;
        adress_src = create_adress  (base_src_reg);
        adress_mode = Immediat
      });
      Instruction (Instruction.STR {
        data_size = None;
        source = reg_of_32 W9;
        adress = adress_str;
        adress_mode = Immediat
      }) ;
      Instruction (Instruction.ADD {
        destination = base_src_reg;
        operand1 = base_src_reg;
        operand2 = `ILitteral 32L
      })
    ] @ (copy_large (increment_adress 32L adress_str) base_src_reg (Int64.sub size 32L))
      else (*size >= 64L*) 
        [
          Instruction (Instruction.LDR {
            data_size = None;
            destination = reg_of_64 X9;
            adress_src = create_adress base_src_reg;
            adress_mode = Immediat
          });
          Instruction (Instruction.STR {
            data_size = None;
            source = reg_of_32 W9;
            adress = adress_str;
            adress_mode = Immediat
          }) ;
          Instruction (Instruction.ADD {
            destination = base_src_reg;
            operand1 = base_src_reg;
            operand2 = `ILitteral 64L
          })
        ] @ (copy_large (increment_adress 64L adress_str) base_src_reg (Int64.sub size 64L))
  ;;


  let copy_from_reg register (adress: address) ktype rprogram =
    let size =  KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
    match size with
    | 8L ->
      let data_size =  Some (if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then (B: data_size) else SB) in
      [Instruction (STR {data_size; source = to_32bits register; adress; adress_mode = Immediat})]
    | 16L -> let data_size = Some( if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then H else SH) in
      [Instruction (STR {data_size; source = to_32bits register; adress; adress_mode = Immediat})]
    | 32L -> 
      [Instruction (STR {data_size = None; source = to_32bits register; adress; adress_mode = Immediat})]
    | 64L ->
      [Instruction (STR {data_size = None; source = to_64bits register; adress; adress_mode = Immediat})]
    | _ -> copy_large adress register size 
  (* let function_prologue ~fn_register_params rprogram fd = 
    let base = Instruction.STP {x1 = R29; x2 = R30; base = SP; offset = Int64.to_int (Int64.sub fd.locals_space 16L); adress_mode = Immediat} in
    let stack_sub = Instruction.isub (SP) ~srcl:(`Register SP) ~srcr:(`Litteral fd.locals_space) in
    let copy_instructions = fn_register_params |> Util.ListHelper.combine_safe Arm64ABI.argument_registers |> List.fold_left (fun acc (register , (name, kt)) -> 
      let whereis = adress_of (name, kt) fd in
      acc @ (copy_from_reg (register) whereis kt rprogram)
      ) [] in
    base::stack_sub @ copy_instructions *)

  let sizeofn = KosuIrTyped.Asttyconvert.Sizeof.sizeof

  let translate_tac_expression ~str_lit_map ?(target_reg = Register32 W9) rprogram (fd: FrameManager.frame_desc) = 
    function
    |({tac_expression = TEString s; expr_rktype = _}) -> 
      let reg64 = to_64bits target_reg in
      target_reg, load_label (Hashtbl.find str_lit_map s) reg64
    |({tac_expression = TEFalse | TEmpty; expr_rktype = _}) -> 
       to_32bits target_reg,  Instruction ( Mov {destination = to_32bits target_reg; flexsec_operand = `Register (Register32 WZR) })::[]
    |({tac_expression = TENullptr; expr_rktype = _}) -> 
      let r64 = to_64bits target_reg in
      r64,  Instruction ( Mov {destination = r64; flexsec_operand = `Register (Register64 XZR) })::[]
    |({tac_expression = TETrue; _}) -> 
      let s32 = to_32bits target_reg in
      s32, Instruction ( Mov { destination = s32; flexsec_operand = `ILitteral 1L})::[]
    |({tac_expression = TEInt (_, isize, int64); _}) -> 
      let rreg = match isize with I64 -> to_64bits target_reg | _ -> to_32bits target_reg in
      rreg, Instruction (Mov {destination = rreg; flexsec_operand = `ILitteral int64})::[]
    |({tac_expression = TEIdentifier id; expr_rktype}) -> 
      let adress = FrameManager.adress_of (id, expr_rktype) fd in
      let sizeof = sizeofn rprogram expr_rktype in
      let rreg = if sizeof > 32L then to_64bits target_reg else to_32bits target_reg in
      if is_register_size sizeof 
        then 
          rreg, [
            Instruction (LDR {data_size = compute_data_size expr_rktype sizeof; destination = rreg; adress_src = adress; adress_mode = Immediat})
          ]
      else 
        rreg, [
          Instruction (ADD {destination = rreg; operand1 = adress.base; operand2 = `ILitteral adress.offset})
        ]
    | ({tac_expression = TESizeof kt; _}) -> 
      let r64 = to_64bits target_reg in
      let sizeof = sizeofn rprogram kt in
      r64, [ 
        Line_Com (Comment (Printf.sprintf "sizeof %s" (KosuIrTyped.Asttypprint.string_of_rktype kt))); 
        Instruction (Mov {destination = r64; flexsec_operand = `ILitteral sizeof})
      ]
    | _ -> failwith ""

    let rec translate_tac_rvalue ~str_lit_map ~(where: address) current_module rprogram (fd: FrameManager.frame_desc) {rval_rktype; rvalue} =
      match rvalue with
      | RVUminus ttr -> 
        let last_reg, insts =  translate_tac_rvalue ~str_lit_map ~where current_module rprogram fd ttr in
        let neg_instruction = [Instruction (Neg {destination = last_reg; source = last_reg})] in
        last_reg, insts @ neg_instruction
      | RVNot ttr -> 
          let last_reg, insts =  translate_tac_rvalue ~str_lit_map ~where current_module rprogram fd ttr in
          let not_instruction = [Instruction (Not {destination = last_reg; source = last_reg})] in
          last_reg, insts @ not_instruction
      
      | RVExpression tac_typed_expression -> translate_tac_expression ~str_lit_map rprogram fd tac_typed_expression
      | RVStruct {module_path = _; struct_name = _; fields} -> begin 
        let struct_decl = 
          match KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye rval_rktype rprogram with
          | Some (RDecl_Struct s) -> s
          | Some (RDecl_Enum _) -> failwith "Expected to find a struct get an enum"
          | None -> failwith "Non type decl ??? my validation is very weak" in 
        let offset_list = fields 
          |> List.map (fun (field, _) -> offset_of_field field struct_decl rprogram)
          |> List.tl
          |> ( fun l -> l @ [0L] )
        in
        let tmpreg = Register64 X9 in
        tmpreg,  fields |> List.mapi (fun index value -> index, value) |> List.fold_left (fun (accumuled_adre, acc) (index, (_field, tte)) -> 
          let reg_texp, instructions = translate_tac_expression ~str_lit_map rprogram fd tte in
          increment_adress (List.nth offset_list index) accumuled_adre , acc @ instructions @ copy_from_reg reg_texp accumuled_adre tte.expr_rktype rprogram
        ) (where, []) |> snd
  
      end
      | RVFunction {module_path; fn_name; generics_resolver = _; tac_parameters} -> 
        let typed_parameters = tac_parameters |> List.map (fun {expr_rktype; _} -> expr_rktype) in
        let fn_module = (if module_path = "" then current_module else module_path) in
        let fn_decl = KosuIrTyped.Asttyhelper.RProgram.find_function_decl_of_name fn_module fn_name rprogram |> Option.get in
        begin match fn_decl with
        | RExternal_Decl external_func_decl -> 
          let fn_label = Printf.sprintf "_%s" (external_func_decl.c_name |> Option.value ~default:(external_func_decl.rsig_name)) in
          let _ = assert (tac_parameters |> List.length < 9) in
          let instructions, regs = tac_parameters |> Util.ListHelper.combine_safe argument_registers |> List.fold_left_map (fun acc (reg, tte) -> 
            let reg, instruction = translate_tac_expression ~str_lit_map ~target_reg:reg rprogram fd tte in 
            acc @ instruction, `Register reg
          ) [] in
      
          let call_instructions = FrameManager.call_instruction ~origin:fn_label regs fd in
          let return_size = sizeofn rprogram external_func_decl.return_type in
          let return_reg = return_register_ktype return_size in
          return_reg, instructions @ call_instructions @ copy_from_reg return_reg where external_func_decl.return_type rprogram
        | RSyscall_Decl syscall_decl -> 
          let _ = assert (tac_parameters |> List.length < 5) in
          let instructions, _regs = tac_parameters |> Util.ListHelper.combine_safe argument_registers |> List.fold_left_map (fun acc (reg, tte) -> 
            let reg, instruction = translate_tac_expression ~str_lit_map ~target_reg:reg rprogram fd tte in 
            acc @ instruction, reg
          ) [] in
          let return_size = sizeofn rprogram syscall_decl.return_type in
          let return_reg = return_register_ktype return_size in
          return_reg, instructions @ [
            Line_Com (Comment ("syscall " ^ syscall_decl.rsyscall_name));
            Instruction (SVC (syscall_decl.opcode))
            ] @ copy_from_reg return_reg where syscall_decl.return_type rprogram
        | RKosufn_Decl _ -> (
        let function_decl = rprogram |> KosuIrTyped.Asttyhelper.RProgram.find_function_decl_exact_param_types 
          ~module_name:fn_module
          ~fn_name
          ~ktypes: typed_parameters
          |> Option.get
      in
  
      let _ = assert (tac_parameters |> List.length < 9) in
      let fn_label = KosuIrTyped.Asttyhelper.Function.label_of_fn_name fn_module function_decl in
      let instructions, regs = tac_parameters |> Util.ListHelper.combine_safe argument_registers |> List.fold_left_map (fun acc (reg, tte) -> 
        let reg, instruction = translate_tac_expression ~str_lit_map ~target_reg:reg rprogram fd tte in 
        acc @ instruction, reg
      ) [] in
  
      let call_instructions = FrameManager.call_instruction ~origin:fn_label regs fd in
      let return_size = sizeofn rprogram function_decl.return_type in
      let return_reg = return_register_ktype return_size in
      return_reg, instructions @ call_instructions @ copy_from_reg return_reg where function_decl.return_type rprogram
    )
  end
      | RVTuple ttes -> 
        let ktlis = ttes |> List.map (fun {expr_rktype; _} -> expr_rktype) in
        let offset_list = ttes |> List.mapi (fun index _value -> 
          offset_of_tuple_index index ktlis rprogram
        )      
        |> List.tl
        |> ( fun l -> l @ [0L] ) in
        let last_reg = tmpreg_of_size (sizeofn rprogram rval_rktype) in
        last_reg, ttes 
          |> List.mapi (fun index value -> index, value) 
          |> List.fold_left (fun (accumuled_adre, acc) (index, tte) -> 
            let reg_texp, instructions = translate_tac_expression rprogram ~str_lit_map fd tte in
          increment_adress (List.nth offset_list index ) accumuled_adre, acc @ instructions @ copy_from_reg reg_texp accumuled_adre tte.expr_rktype rprogram
          ) (where, []) |> snd
      | RVFieldAcess {first_expr = {expr_rktype; tac_expression = TEIdentifier _}; field} -> 
        let struct_decl = 
          match KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye expr_rktype rprogram with
          | Some (RDecl_Struct s) -> s
          | Some (RDecl_Enum _) -> failwith "Expected to find a struct get an enum"
          | None -> failwith "Non type decl ??? my validation is very weak" in 
        let sizeof = sizeofn rprogram rval_rktype in
        let offset = offset_of_field field struct_decl rprogram in
        let adress = FrameManager.adress_of (field, rval_rktype) fd in
        let adress = increment_adress offset adress in
        let size = compute_data_size rval_rktype sizeof in
        let tmpreg = tmpreg_of_size sizeof in
        tmpreg, [Instruction (LDR {data_size = size; destination = tmpreg; adress_src = adress; adress_mode = Immediat})]
      | RVAdress id -> 
        let adress = FrameManager.adress_of (id, rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee) fd in
        tmp64reg, [Instruction (LDR {data_size = None; destination = tmp64reg; adress_src = adress; adress_mode = Immediat})]
      | RVDefer id ->
        let adress = FrameManager.adress_of (id, rval_rktype |> KosuIrTyped.Asttyhelper.RType.rpointer) fd in
        let load_instruction = [Instruction (LDR {data_size = None; destination = tmp64reg; adress_src = adress; adress_mode = Immediat})] in
        let sizeof = sizeofn rprogram rval_rktype in
        let last_reg = tmpreg_of_size sizeof in
        let data_size = compute_data_size rval_rktype sizeof in
        let load =   [Instruction (LDR {data_size; destination = last_reg; adress_src = (create_adress tmp64reg); adress_mode = Immediat})] in
       last_reg, load_instruction @ load
      | RVEnum {variant; assoc_tac_exprs; _} -> 
        let enum_decl = 
          match KosuIrTyped.Asttyhelper.RProgram.find_type_decl_from_rktye rval_rktype rprogram with
          | Some (RDecl_Struct _) -> failwith "Expected to find an enum get an struct"
          | Some (RDecl_Enum e) -> e
          | None -> failwith "Non type decl ??? my validation is very weak" in 
        let tag = KosuIrTyped.Asttyhelper.Renum.tag_of_variant variant enum_decl in
        let enum_tte_list = assoc_tac_exprs |> List.cons {expr_rktype = (RTInteger (Unsigned, I32)); tac_expression = (TEInt (Unsigned, I32, Int64.of_int32 tag))} in
        let enum_type_list = enum_tte_list |> List.map (fun {expr_rktype; _} -> expr_rktype) in
        let offset_list = enum_tte_list
        |> List.mapi (fun index {expr_rktype = _; _} -> offset_of_tuple_index index enum_type_list rprogram)
        |> List.tl
        |> ( fun l -> l @ [0L] )
      in
      let last_reg = tmpreg_of_size (sizeofn rprogram rval_rktype) in
        last_reg, enum_tte_list 
          |> List.mapi (fun index value -> index, value) 
          |> List.fold_left (fun (accumuled_adre, acc) (index, tte) -> 
            let reg_texp, instructions = translate_tac_expression rprogram ~str_lit_map fd tte in
          increment_adress (List.nth offset_list index ) accumuled_adre, acc @ instructions @ copy_from_reg reg_texp accumuled_adre tte.expr_rktype rprogram
          ) (where, []) |> snd
      | RVDiscard | RVLater -> tmp32reg, []
      | RVBuiltinBinop { binop = TacBool (TacOr); blhs; brhs} -> 
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in 
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r8 rprogram fd blhs in
        let or_instruction = [Instruction (ORR {destination = r8; operand1 = left_reg; operand2 = `Register right_reg})] in
        r8, rinstructions @ linstructions @ or_instruction
      | RVBuiltinBinop { binop = TacBool (TacAnd); blhs; brhs} -> 
        let r9 = tmpreg_of_ktype_2 rprogram brhs.expr_rktype in
        let r8 = tmpreg_of_ktype rprogram brhs.expr_rktype in 
        let right_reg, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:r9 rprogram fd brhs in
        let left_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:r8 rprogram fd blhs in
        let and_instruction = [Instruction (AND {destination = r8; operand1 = left_reg; operand2 = `Register right_reg})] in
        r8, rinstructions @ linstructions @ and_instruction
      (* | RVBuiltinBinop { binop = TacBool (TacEqual); blhs; brhs} -> 
        let _, rinstructions = translate_tac_expression ~str_lit_map ~target_reg:(second_prefered_tmp_reg) rprogram fd brhs in
        let last_reg, linstructions = translate_tac_expression ~str_lit_map ~target_reg:(tmpreg) rprogram fd blhs in
        let  *)
      | _ -> failwith "Mostly binop"
  
end
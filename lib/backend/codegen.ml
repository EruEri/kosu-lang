open KosuIrTyped.Asttyped
open KosuFrontend.Ast

module type ABI = sig
  type register
  type stack_parameter_order = Ordered | Reversed
  type register_mode = Value | Adress
  type adress_mode =
  | Immediat (* out = *intptr; *)
  | Prefix (* out = *(++intptr);*)
  | Postfix (* out = *(intptr++);*)

  type condition_code
  type dst
  type src
  type data_size

  type register_size = [ `x64 | `x32 ]

  type address = {
    offset : Common.imm option;
    base : register;
    idx : register option;
    scale : Common.scale;
  }

  val string_of_register : ?size:register_size -> register -> string
  val caller_save : register list
  val callee_save : register list
  val stack_pointer : register
  val frame_registers : register list
  val return_register : register
  val indirect_return : register
  val argument_registers : register list
  val syscall_arguments_register : register list 
  val prefered_tmp_reg : register
  val compute_data_size : KosuIrTyped.Asttyped.rktype -> int64 -> data_size option
  val second_prefered_tmp_reg : register 
  val return_register_ktype : int64 -> register
  val is_register_size: int64 -> bool
  val register_mode : int64 -> register_mode
  val stack_parameter_order : stack_parameter_order
  val stack_pointer_align : int
  val dst_of_register: register -> dst
  val src_of_register: register -> src
  val src_of_immediat: int64 -> src
  val src_of_label : string -> src

  val increment_adress: int64 -> address -> address

  val create_adress :
    ?offset:Common.imm ->
    base:register ->
    ?idx:register option ->
    ?scale:Common.scale ->
    unit ->
    address
end

module type Instruction = sig
  module ABI : sig
    include ABI
  end

  type instruction

  val imov :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    ABI.src ->
    instruction list

  val iadd :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val lea:
      ?cc: ABI.condition_code ->
      ABI.dst ->
      string -> 
      instruction list  

  val isub :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val imult :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val isdiv :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val inot:
   ?cc: ABI.condition_code ->
    ABI.dst ->
    ABI.src -> instruction list

  val ineg:
  ?cc: ABI.condition_code ->
    ABI.dst ->
    ABI.src -> instruction list

    val iudiv :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val iasl :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val iasr :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val icmp :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val iand :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val ior :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val ixor :
    ?cc:ABI.condition_code ->
    ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val ildr :
    ?cc:ABI.condition_code ->
    ?data_size:ABI.data_size option ->
    ABI.dst ->
    ABI.address -> 
    ABI.adress_mode ->
    instruction list

  val istr :
    ?cc:ABI.condition_code ->
    ?data_size:ABI.data_size option ->
    ABI.dst ->
    ABI.address -> 
    ABI.adress_mode ->
    instruction list

  val ijmplabel :
    ?cc:ABI.condition_code -> Common.label -> instruction list

  val ijumpreg : ?cc:ABI.condition_code -> ABI.register -> instruction list

  val icalllabel :
    ?cc:ABI.condition_code -> Common.label -> instruction list

  val icallreg : ?cc:ABI.condition_code -> ABI.register -> instruction list
  val syscall : ?code: int64 -> unit -> instruction
  val ret : instruction
end

(* module type AInstruction = functor (ABI : ABI) -> Instruction *)
module IdVarMap = Common.IdVarMap

module type FrameManager = sig
  module Instruction : sig
    include Instruction
  end

  type frame_desc = {
    stack_param_count : int;
    locals_space : int64;
    stack_map : Instruction.ABI.address IdVarMap.t;
  }

  val frame_descriptor :
    fn_register_params:(string * KosuIrTyped.Asttyped.rktype) list ->
    stack_param:(string * KosuIrTyped.Asttyped.rktype) list ->
    locals_var:(string * KosuIrTyped.Asttyped.rktype) list ->
    rprogram:KosuIrTyped.Asttyped.rprogram ->
    frame_desc

  val adress_of :
    string *  KosuIrTyped.Asttyped.rktype ->
    frame_desc ->
    Instruction.ABI.address

  val copy_from_reg:  
  Instruction.ABI.register ->
  Instruction.ABI.address ->
   KosuIrTyped.Asttyped.rktype -> KosuIrTyped.Asttyped.rprogram -> Instruction.instruction list

  val function_prologue : fn_register_params:(string * KosuIrTyped.Asttyped.rktype) list -> KosuIrTyped.Asttyped.rprogram -> frame_desc -> Instruction.instruction list
  val function_epilogue : frame_desc -> Instruction.instruction list

  val call_instruction :
    origin: string ->
    Instruction.ABI.src list ->
    frame_desc ->
    Instruction.instruction list
end

(**[ `Reg of Instruction.ABI.register | `Lab of string ] *)

module Make(FrameManager: FrameManager) = struct

  open KosuIrTAC.Asttac

  open FrameManager
  open Common
  open Instruction
  open ABI
  module RFD = KosuIrTyped.Asttyhelper.RFunction_Decl
  let tmpreg =  FrameManager.Instruction.ABI.prefered_tmp_reg
  let sizeofn = KosuIrTyped.Asttyconvert.Sizeof.sizeof

  let translate_tac_expression ~str_lit_map ?(target_reg = tmpreg) rprogram (fd: FrameManager.frame_desc) = 
    function
    |({tac_expression = TEString s; expr_rktype = _}) -> 
      target_reg, lea (dst_of_register tmpreg) (Hashtbl.find str_lit_map s)
    |({tac_expression = TEFalse | TENullptr | TEmpty; expr_rktype = _})  -> target_reg , FrameManager.Instruction.imov (dst_of_register target_reg) (src_of_immediat 0L)
    |({tac_expression = TETrue; _}) -> target_reg, imov (dst_of_register target_reg) (src_of_immediat 1L)
    |({tac_expression = TEInt (_, _, int64); _}) -> target_reg, imov (dst_of_register target_reg) (src_of_immediat int64)
    |({tac_expression = TEIdentifier id; expr_rktype}) -> 
      let adress = adress_of (id, expr_rktype) fd in
      let sizeof = sizeofn rprogram expr_rktype in
      if is_register_size sizeof 
        then target_reg, ildr ~data_size:(compute_data_size expr_rktype sizeof) (dst_of_register target_reg) adress Immediat
      else 
        target_reg, iadd (dst_of_register tmpreg) ~srcl:( adress.base |> src_of_register ) ~srcr:(adress.offset |> Option.map (fun s -> match s with Common.Lit (IntL s) -> s | _ -> 0L ) |> Option.value ~default:0L |> src_of_immediat) 
    | ({tac_expression = TESizeof kt; _}) -> 
      let sizeof = sizeofn rprogram kt in
      target_reg, imov (dst_of_register target_reg) (src_of_immediat sizeof)
    | _ -> failwith ""

  let rec translate_tac_rvalue ~str_lit_map ~(where: address) current_module rprogram (fd: FrameManager.frame_desc) {rval_rktype; rvalue} =
    match rvalue with
    | RVUminus ttr -> 
      let last_reg, insts =  translate_tac_rvalue ~str_lit_map ~where current_module rprogram fd ttr in
      let neg_instruction = ineg (dst_of_register last_reg) (src_of_register last_reg) in
      last_reg, insts @ neg_instruction
    | RVNot ttr -> 
        let last_reg, insts =  translate_tac_rvalue ~str_lit_map ~where current_module rprogram fd ttr in
        let not_instruction = inot (dst_of_register last_reg) (src_of_register last_reg) in
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
          acc @ instruction, src_of_register reg
        ) [] in
    
        let call_instructions = call_instruction ~origin:fn_label regs fd in
        let return_size = sizeofn rprogram external_func_decl.return_type in
        let return_reg = return_register_ktype return_size in
        return_reg, instructions @ call_instructions @ copy_from_reg return_reg where external_func_decl.return_type rprogram
      | RSyscall_Decl syscall_decl -> 
        let _ = assert (tac_parameters |> List.length < 5) in
        let instructions, _regs = tac_parameters |> Util.ListHelper.combine_safe argument_registers |> List.fold_left_map (fun acc (reg, tte) -> 
          let reg, instruction = translate_tac_expression ~str_lit_map ~target_reg:reg rprogram fd tte in 
          acc @ instruction, src_of_register reg
        ) [] in
        let return_size = sizeofn rprogram syscall_decl.return_type in
        let return_reg = return_register_ktype return_size in
        return_reg, instructions @ [syscall ~code:(syscall_decl.opcode) ()] @ copy_from_reg return_reg where syscall_decl.return_type rprogram
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
      acc @ instruction, src_of_register reg
    ) [] in

    let call_instructions = call_instruction ~origin:fn_label regs fd in
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
      tmpreg, ttes 
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
      let adress = adress_of (field, rval_rktype) fd in
      let adress = increment_adress offset adress in
      let size = compute_data_size rval_rktype sizeof in
      tmpreg, ildr ~data_size:(size) (dst_of_register tmpreg) adress Immediat
    | RVAdress id -> 
      let adress = adress_of (id, rval_rktype |> KosuIrTyped.Asttyhelper.RType.rtpointee) fd in
      tmpreg, ildr (dst_of_register tmpreg) adress Immediat
    | RVDefer id ->
      let adress = adress_of (id, rval_rktype |> KosuIrTyped.Asttyhelper.RType.rpointer) fd in
      let load_instruction = ildr (dst_of_register tmpreg) adress Immediat in
      let load = ildr (dst_of_register tmpreg) (create_adress ~base:(tmpreg) ~offset:(Lit (IntL (0L))) ()) Immediat in
     tmpreg, load_instruction @ load
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
      tmpreg, enum_tte_list 
        |> List.mapi (fun index value -> index, value) 
        |> List.fold_left (fun (accumuled_adre, acc) (index, tte) -> 
          let reg_texp, instructions = translate_tac_expression rprogram ~str_lit_map fd tte in
        increment_adress (List.nth offset_list index ) accumuled_adre, acc @ instructions @ copy_from_reg reg_texp accumuled_adre tte.expr_rktype rprogram
        ) (where, []) |> snd
    | RVDiscard | RVLater -> tmpreg, []
    | _ -> failwith ""
    
  let translate_tac_statement _current_module _rprogram (_fd: FrameManager.frame_desc) = failwith ""
end

module ARMCodegen = Make(Arm.Arm64FrameManager)
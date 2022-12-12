module type ABI = sig
  type register
  type stack_parameter_order = Ordered | Reversed
  type condition_code
  type dst
  type src
  type data_size
  type adress_mode
  type register_size = [ `x86 | `x32 ]

  type address = {
    offset : Common.imm option;
    base : register option;
    idx : register option;
    scale : Common.scale;
  }

  val string_of_register : ?size:register_size -> register -> string
  val caller_save : register list
  val callee_save : register list
  val stack_pointer : register
  val frame_registers : register list
  val return_register : register
  val argument_registers : register list
  val stack_parameter_order : stack_parameter_order
  val stack_pointer_align : int

  val create_adress :
    ?offset:Common.imm option ->
    ?base:register option ->
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
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    src:ABI.src ->
    instruction list

  val iadd :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val isub :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val imult :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val idiv :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val iasl :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val iasr :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val icmp :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val iand :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val ior :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val ixor :
    ?cc:ABI.condition_code option ->
    dst:ABI.dst ->
    srcl:ABI.src ->
    srcr:ABI.src ->
    instruction list

  val ildr :
    ?cc:ABI.condition_code option ->
    ?data_size:ABI.data_size option ->
    dst:ABI.dst ->
    src:ABI.src ->
    offset:int ->
    adress_mode:ABI.adress_mode ->
    instruction list

  val istr :
    ?cc:ABI.condition_code option ->
    ?data_size:ABI.data_size option ->
    dst:ABI.dst ->
    src:ABI.src ->
    offset:int ->
    adress_mode:ABI.adress_mode ->
    instruction list

  val ijmplabel :
    ?cc:ABI.condition_code option -> Common.label -> instruction list

  val ijumpreg : ?cc:ABI.condition_code -> ABI.register -> instruction list

  val icalllabel :
    ?cc:ABI.condition_code option -> Common.label -> instruction list

  val icallreg : ?cc:ABI.condition_code -> ABI.register -> instruction list
  val syscall : instruction
  val ret : instruction
end

(* module type AInstruction = functor (ABI : ABI) -> Instruction *)

module IdVar = struct
  type t = string * KosuIrTyped.Asttyped.rktype

  let compare = compare
end

module IdVarMap = Map.Make (IdVar)

module type FrameManager = sig
  module Instruction : sig
    include Instruction
  end

  type frame_desc = {
    stack_param_count : int;
    locals_space : int64;
    stack_map : (string * KosuIrTyped.Asttyped.rktype) IdVarMap.t;
  }

  val frame_descriptor :
    fn_register_params:(string * KosuIrTyped.Asttyped.rktype) list ->
    stack_param:(string * KosuIrTyped.Asttyped.rktype) list ->
    locals_var:(string * KosuIrTyped.Asttyped.rktype) list ->
    rprogram:KosuIrTyped.Asttyped.rprogram ->
    frame_desc

  val adress_of :
    rktype:KosuIrTyped.Asttyped.rktype ->
    string ->
    frame_desc ->
    Instruction.ABI.address

  val function_prologue : frame_desc -> Instruction.instruction list
  val function_epilogue : frame_desc -> Instruction.instruction list

  val call_instruction :
    frame_desc ->
    origin:[ `Reg | `Lab ] ->
    args:Instruction.ABI.src ->
    Instruction.instruction list
end

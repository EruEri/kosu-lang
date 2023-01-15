(* open Common
open KosuIrTyped.Asttyconvert.Sizeof
open KosuIrTAC.Asttachelper.StringLitteral
open KosuIrTAC.Asttac
open Util

module type Register = sig
  type register

  val argument_registers: register list

  val syscall_arguments_register: register list

  val indirect_return_reg: register

  (* val reg64_of_reg: register -> register
  val reg32_of_reg: register -> register
  val reg16_of_reg: register -> register
  val reg8_of_reg: register -> register *)

  val is_calee_saved: register
end

module type Instruction = sig
  type instruction
end

module type ABI = sig
    module Instruction : sig
      include Instruction
    end
end

module type Address = sig
  module Register : sig
    include Register
  end 

  type address

  val create_address: ?offset: int64 -> Register.register

  val increment_adress: int64 -> address -> address
end

module type FrameManager = sig

  module Address : sig
    include Address
  end

  module Instruction : sig 
    include Instruction
  end

  type frame_desc
  type instruction = Instruction.instruction
  type address = Address.address

  val frame_descriptor: ?stack_future_call:int64 
    -> fn_register_params:Common.IdVar.t list
    -> stack_param: Common.IdVar.t list
    -> return_ktype: KosuIrTyped.Asttyped.rktype
    -> locale_var:Common.IdVar.t list
    -> discarded_values:Common.IdVar.t
    -> KosuIrTyped.Asttyped.rprogram
    -> frame_desc

  val address_of: Common.IdVar.t -> frame_desc -> address option

  val fonction_prologue: fn_register_params: Common.IdVar.t list
    -> stack_params: Common.IdVar.t list
    -> KosuIrTyped.Asttyped.rprogram
    -> frame_desc
    -> instruction list

  val fonction_epilogue: frame_desc -> instruction list
end

module type InstrutionSelector = sig


  module Address : sig
    include Address
  end

  module Instruction : sig 
    include Instruction
  end

  
  type register = Address.Register.register
  type instruction = Instruction.instruction
  type address = Address.address
  type dst
  type src
  
  
  type condition_code

  val mov: src -> dst -> instruction list

  val mvn: src -> dst -> instruction list

  val not: src -> dst -> instruction list

  val neg: src -> dst -> instruction list

  val add: lhs:src -> rhs:src -> dst -> instruction list

  val adds: lhs:src -> rhs:src -> dst -> instruction list

  val madd: base:src -> operande:src -> scale:src -> dst -> instruction list

  val msub: base:src -> operande:src -> scale:src -> dst -> instruction list

  val sub: lhs:src -> rhs:src -> dst -> instruction list

  val subs: lhs:src -> rhs:src -> dst -> instruction list

  val mul: lhs:src -> rhs:src -> dst -> instruction list

  val udiv: lhs:src -> rhs:src -> dst -> instruction list

  val idiv: lhs:src -> rhs:src -> dst -> instruction list

  val losl: lhs:src -> rhs:src -> dst -> instruction list

  val losr: lhs:src -> rhs:src -> dst -> instruction list

  val aosr: lhs:src -> rhs:src -> dst -> instruction list

  val cmp: src -> src -> instruction list

  val loand: lhs:src -> rhs:src -> dst -> instruction list

  val loxor: lhs:src -> rhs:src -> dst -> instruction list 

  val loor: lhs:src -> rhs:src -> dst -> instruction list

  val b: condition_code option -> string -> instruction list

  val bl: condition_code option -> string -> instruction list

  val br: condition_code option -> register -> instruction list

  val blr: condition_code option -> register -> instruction list

  val ret: instruction list

  val syscall: int64 option -> instruction list

  val load_label: ?module_path:string -> string -> dst -> dst * instruction list

  val tmpreg: is_float:bool -> dst
  val tmpreg2: is_float:bool -> dst
  val tmpreg3: is_float:bool -> dst
  val tmpreg4: is_float:bool -> dst
 
end

module Make(IS: InstrutionSelector)(FM: FrameManager)(Register: Register) = struct
  
  let translate_tac_expression ~str_lit_map ?(target_reg = IS.tmpreg ~is_float:false) rprogram (fd: FM.frame_desc) = function
  | {tac_expression = TEString s; _} 
    ->     
    let (SLit str_labl) = Hashtbl.find str_lit_map s in
    IS.load_label str_labl target_reg    
end *)
open Common
open KosuIrTAC.Asttac
(* open Kosu_frontend.Ast *)

(* for i in $(seq 1 16); do echo "| R$i" >> lib/backend/arm.ml; done *)

(* type flexsec_operand = [ `Litteral of int | `Register of register ] *)

(*
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
   end *)

(* (**
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
            if does_it_hold_in_register program rktype then ()) *)

module Arm64ABI = struct
  type register =
    | R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8 (* XR *)
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | R16
    | R29
    | R30
    | SP

  type register_mode = Value | Adress

  type adress_mode =
    | Immediat (* out = *intptr; *)
    | Prefix (* out = *(++intptr);*)
    | Postfix (* out = *(intptr++);*)

  type data_size = B | SB | H | SH

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

  type dst = register
  type src = [ `Litteral of int64 | `Label of string | `Register of register ]
  type register_size = [ `x64 | `x32 ]
  type stack_parameter_order = Ordered | Reversed

  type address = {
    offset : Common.imm option;
    base : register;
    idx : register option;
    scale : Common.scale;
  }

  let stack_pointer_align = 16
  let stack_parameter_order = Ordered

  let string_of_register ?(size = (`x64: register_size)) register =
    let prefix = match size with `x64 -> 'x' | `x32 -> 'w' in
    match register with
    | R0 -> Printf.sprintf "%c0" prefix
    | R1 -> Printf.sprintf "%c1" prefix
    | R2 -> Printf.sprintf "%c2" prefix
    | R3 -> Printf.sprintf "%c3" prefix
    | R4 -> Printf.sprintf "%c4" prefix
    | R5 -> Printf.sprintf "%c5" prefix
    | R6 -> Printf.sprintf "%c6" prefix
    | R7 -> Printf.sprintf "%c7" prefix
    | R8 -> Printf.sprintf "%c8" prefix
    | R9 -> Printf.sprintf "%c9" prefix
    | R10 -> Printf.sprintf "%c10" prefix
    | R11 -> Printf.sprintf "%c11" prefix
    | R12 -> Printf.sprintf "%c12" prefix
    | R13 -> Printf.sprintf "%c13" prefix
    | R14 -> Printf.sprintf "%c14" prefix
    | R15 -> Printf.sprintf "%c15" prefix
    | R16 -> Printf.sprintf "%c16" prefix
    | R29 -> Printf.sprintf "%c29" prefix
    | R30 -> Printf.sprintf "%c30" prefix
    | SP -> Printf.sprintf "sp"

  let stack_pointer = SP
  let argument_registers = [ R0; R1; R2; R3; R4; R5; R6; R7; R8 ]

  let frame_registers = [R29; R30]
  let return_register = R0
  let indirect_return = R8

  let return_register_ktype = function
  | 8L | 16L | 32L | 64L -> return_register
  | _ -> indirect_return



  let is_register_size = function
  | 8L | 16L | 32L | 64L -> true
  | _ -> false

  let register_mode size = if is_register_size size then Value else Adress

  let compute_data_size ktype = function
  | 8L -> Some (if not @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then SB else B)
  | 16L -> Some (if not @@ KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then SH else H)
  | _ -> None
  let dst_of_register reg = (reg : dst)  
  let src_of_register register: src = `Register register
  let src_of_immediat int64: src = `Litteral int64
  let src_of_label string: src = `Label string 

  let prefered_tmp_reg = R8

  let second_prefered_tmp_reg = R9
  let caller_save =
    [ R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15 ]

  let callee_save = [ R30; R29; R16 ]

  let create_adress ?offset ~base ?(idx = None) ?(scale = `One) () =
    { offset; base; idx; scale }

  let increment_adress off adress = { 
    adress with offset = adress.offset |> Option.map (fun imm -> match imm with Lit (IntL x) -> Lit (IntL (Int64.add x off)) | t -> t)
  }
end

module Arm64Instruction = struct
  module ABI = struct
    include Arm64ABI
  end

  type instruction =
    | Mov of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        (* Careful int max INT16 *)
        flexsec_operand : Arm64ABI.src;
      }
    | Not of {
      cc : Arm64ABI.condition_code option;
      destination : Arm64ABI.register;
      source : Arm64ABI.register
    }
    | Neg of {
      cc : Arm64ABI.condition_code option;
      destination : Arm64ABI.register;
      source : Arm64ABI.register
    }
    | ADD of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        operand1 : Arm64ABI.register;
        (* Int12 litteral oprand*)
        operand2 : Arm64ABI.src;
      }
    | SUB of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        operand1 : Arm64ABI.register;
        (* Int12 litteral oprand*)
        operand2 : Arm64ABI.src;
      }
    | MUL of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        operand1 : Arm64ABI.register;
        operand2 : Arm64ABI.register;
      }
    | UDIV of {
      cc : Arm64ABI.condition_code option;
      destination : Arm64ABI.register;
      operand1 : Arm64ABI.register;
      operand2 : Arm64ABI.register;
    }
    | SDIV of {
      cc : Arm64ABI.condition_code option;
      destination : Arm64ABI.register;
      operand1 : Arm64ABI.register;
      operand2 : Arm64ABI.register;
    }
    | ASL of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        operand1 : Arm64ABI.register;
        (* LIteral range [0-31] *)
        operand2 : Arm64ABI.src;
      }
    | ASR of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        operand1 : Arm64ABI.register;
        (* LIteral range [0-31] *)
        operand2 : Arm64ABI.src;
      }
    | CMP of {
        cc : Arm64ABI.condition_code option;
        operand1 : Arm64ABI.register;
        operand2 : Arm64ABI.src;
      }
    (* Bitwise And*)
    | AND of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        operand1 : Arm64ABI.register;
        operand2 : Arm64ABI.src;
      }
    (* Bitwise OR*)
    | ORR of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        operand1 : Arm64ABI.register;
        operand2 : Arm64ABI.src;
      }
    (* Bitwise XOR*)
    | EOR of {
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        operand1 : Arm64ABI.register;
        operand2 : Arm64ABI.src;
      }
    | LDR of {
        data_size : Arm64ABI.data_size option;
        cc : Arm64ABI.condition_code option;
        destination : Arm64ABI.register;
        adress_src : Arm64ABI.address;
        adress_mode : Arm64ABI.adress_mode;
      }
    | STR of {
        data_size : Arm64ABI.data_size option;
        cc : Arm64ABI.condition_code option;
        source : Arm64ABI.register;
        adress : Arm64ABI.address;
        adress_mode : Arm64ABI.adress_mode;
      }
    | STP of {
      x1: Arm64ABI.register;
      x2: Arm64ABI.register;
      base: Arm64ABI.register;
      offset: int;
      adress_mode : Arm64ABI.adress_mode;
    }
    | ADRP of {
      dst: Arm64ABI.register;
      label: string;
    }
    | LDP of {
      x1: Arm64ABI.register;
      x2: Arm64ABI.register;
      base: Arm64ABI.register;
      offset: int;
      adress_mode : Arm64ABI.adress_mode;
    }
    | B of { cc : Arm64ABI.condition_code option; label : Common.label }
    | BL of { cc : Arm64ABI.condition_code option; label : Common.label }
    | BR of { cc : Arm64ABI.condition_code option; reg : Arm64ABI.register }
    | BLR of { cc : Arm64ABI.condition_code option; reg : Arm64ABI.register }
    | RET
    | SVC

  let imov ?cc dst src =
    [ Mov { cc; destination = dst; flexsec_operand = src } ]

  let inot ?cc dst src = 
    match src with
    | `Litteral _ -> 
      imov R8 src @ [
        Not {cc; destination = dst; source = R8}
      ]
    | `Register reg_dst  -> 
      [
        Not {cc; destination = dst; source = reg_dst}
      ]
    | _ -> failwith "Inot label"

    let ineg ?cc dst src = 
      match src with
      | `Litteral _ -> 
        imov R8 src @ [
          Neg {cc; destination = dst; source = R8}
        ]
      | `Register reg_dst  -> 
        [
          Neg {cc; destination = dst; source = reg_dst}
        ]
      | _ -> failwith "Inot label"

  let iadd ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let add =
          ADD { cc; destination = dst; operand1 = ABI.R9; operand2 = srcr }
        in
        imov ABI.R9 srcl @ (add :: [])
    | `Register reg, _ ->
        [ ADD { cc; destination = dst; operand1 = reg; operand2 = srcr } ]
    | _ -> failwith "Wrong add format"

  let isub ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let add =
          SUB { cc; destination = dst; operand1 = ABI.R9; operand2 = srcr }
        in
        imov ABI.R9 srcl @ (add :: [])
    | `Register reg, _ ->
        [ SUB { cc; destination = dst; operand1 = reg; operand2 = srcr } ]
    | _ -> failwith "Wrong add format"

  let imult ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let mov1 = imov ABI.R8 srcl in
        let mov2 = imov ABI.R9 srcr in
        mov1 @ mov2
        @ MUL { cc; destination = dst; operand1 = ABI.R8; operand2 = ABI.R9 }
          :: []
    | `Register reg1, `Register reg2 ->
        MUL { cc; destination = dst; operand1 = reg1; operand2 = reg2 } :: []
    | _ -> failwith "Wrong mult format"

  let iudiv ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
      let mov1 = imov ABI.R8 srcl in
      let mov2 = imov ABI.R9 srcr in
      mov1 @ mov2
      @ UDIV { cc; destination = dst; operand1 = ABI.R8; operand2 = ABI.R9 }
        :: []
  | `Register reg1, `Register reg2 ->
      UDIV { cc; destination = dst; operand1 = reg1; operand2 = reg2 } :: []
  | _ -> failwith "Wrong idv format"

  let isdiv ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
      let mov1 = imov ABI.R8 srcl in
      let mov2 = imov ABI.R9 srcr in
      mov1 @ mov2
      @ SDIV { cc; destination = dst; operand1 = ABI.R8; operand2 = ABI.R9 }
        :: []
  | `Register reg1, `Register reg2 ->
      SDIV { cc; destination = dst; operand1 = reg1; operand2 = reg2 } :: []
  | _ -> failwith "Wrong isdv format"
  let iasl ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let add =
          ASL { cc; destination = dst; operand1 = ABI.R9; operand2 = srcr }
        in
        imov ABI.R9 srcl @ (add :: [])
    | `Register reg, _ ->
        [ ASL { cc; destination = dst; operand1 = reg; operand2 = srcr } ]
    | _ -> failwith "Wrong add format"

  let iasr ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let add =
          ASR { cc; destination = dst; operand1 = ABI.R9; operand2 = srcr }
        in
        imov ABI.R9 srcl @ (add :: [])
    | `Register reg, _ ->
        [ ASR { cc; destination = dst; operand1 = reg; operand2 = srcr } ]
    | _ -> failwith "Wrong add format"

  let lea ?cc dst label = 
    [
      ADRP {dst; label};
      ADD {cc; destination = dst; operand1 = dst; operand2 = `Label label}
    ]
  let icmp ?cc (_dst: ABI.dst) ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let add = CMP { cc; operand1 = ABI.R9; operand2 = srcr } in
        imov ABI.R9 srcl @ (add :: [])
    | `Register reg, _ -> [ CMP { cc; operand1 = reg; operand2 = srcr } ]
    | _ -> failwith "Wrong add format"

  let iand ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let add =
          AND { cc; destination = dst; operand1 = ABI.R9; operand2 = srcr }
        in
        imov ABI.R9 srcl @ (add :: [])
    | `Register reg, _ ->
        [ AND { cc; destination = dst; operand1 = reg; operand2 = srcr } ]
    | _ -> failwith "Wrong add format"

  let ior ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let add =
          ORR { cc; destination = dst; operand1 = ABI.R9; operand2 = srcr }
        in
        imov ABI.R9 srcl @ (add :: [])
    | `Register reg, _ ->
        [ ORR { cc; destination = dst; operand1 = reg; operand2 = srcr } ]
    | _ -> failwith "Wrong add format"

  let ixor ?cc dst ~srcl ~srcr =
    match (srcl, srcr) with
    | `Litteral _, `Litteral _ ->
        let add =
          EOR { cc; destination = dst; operand1 = ABI.R9; operand2 = srcr }
        in
        imov ABI.R9 srcl @ (add :: [])
    | `Register reg, _ ->
        [ EOR { cc; destination = dst; operand1 = reg; operand2 = srcr } ]
    | _ -> failwith "Wrong add format"

  let ildr ?cc ?(data_size = None) dst adress adress_mode =
    LDR
    {
      cc;
      data_size;
      destination = dst;
      adress_src = adress;
      adress_mode;
    }::[]

  let istr ?cc ?(data_size = None) src adress adress_mode =
    STR
    {
      cc;
      data_size;
      source = src;
      adress = adress;
      adress_mode;
    }::[]

  let ijmplabel ?cc label = B { cc; label }::[]
  let ijumpreg ?cc reg = BR { cc; reg }::[]
  let icalllabel ?cc label = BL { cc; label }::[]
  let icallreg ?cc reg = BLR { cc; reg }::[]
  let syscall = SVC
  let ret = RET
end


module Arm64FrameManager = struct
  module Instruction = struct
    include Arm64Instruction
  end
  module IdVarMap = Common.IdVarMap

  type frame_desc = {
    stack_param_count : int;
    locals_space : int64;
    stack_map : Arm64Instruction.ABI.address IdVarMap.t;
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
               Instruction.ABI.create_adress
                 ~offset:
                   (Lit (IntL (locals_space |> Int64.neg |> Int64.add offset)))
                 ~base:Instruction.ABI.R30 ()
             in
             IdVarMap.add st adress acc)
           IdVarMap.empty
    in



    { stack_param_count; locals_space; stack_map = map }

    let adress_of (variable,rktype)  frame_desc: Instruction.ABI.address = 
      IdVarMap.find (variable, rktype) frame_desc.stack_map

    let rec copy_large adress_str base_src_reg size = 
      if size < 0L
        then failwith "Negive size to copy"
    else if size = 0L
      then []
    else if size < 16L && size >= 8L
      then [
        Instruction.LDR {
          data_size = Some Arm64ABI.B;
          cc = None;
          destination = R9;
          adress_src = Arm64ABI.create_adress ~offset:(Lit (IntL 0L)) ~base:(base_src_reg) ();
          adress_mode = Immediat
        };
        Instruction.STR {
          data_size = Some Arm64ABI.B;
          cc = None;
          source = R9;
          adress = adress_str;
          adress_mode = Immediat
        } ;
        Instruction.ADD {
          cc = None;
          destination = base_src_reg;
          operand1 = base_src_reg;
          operand2 = `Litteral 8L
        }
      ] @ (copy_large (Arm64ABI.increment_adress 16L adress_str) base_src_reg (Int64.sub size 16L))
    else if size < 32L && size >= 16L
      then [
        Instruction.LDR {
          data_size = Some Arm64ABI.H;
          cc = None;
          destination = R9;
          adress_src = Arm64ABI.create_adress ~offset:(Lit (IntL 0L)) ~base:(base_src_reg) ();
          adress_mode = Immediat
        };
        Instruction.STR {
          data_size = Some Arm64ABI.H;
          cc = None;
          source = R9;
          adress = adress_str;
          adress_mode = Immediat
        } ;
        Instruction.ADD {
          cc = None;
          destination = base_src_reg;
          operand1 = base_src_reg;
          operand2 = `Litteral 16L
        }
      ] @ (copy_large (Arm64ABI.increment_adress 16L adress_str) base_src_reg (Int64.sub size 16L))
    else if size < 64L && size >= 32L
      then [
        Instruction.LDR {
          data_size = None;
          cc = None;
          destination = R9;
          adress_src = Arm64ABI.create_adress ~offset:(Lit (IntL 0L)) ~base:(base_src_reg) ();
          adress_mode = Immediat
        };
        Instruction.STR {
          data_size = None;
          cc = None;
          source = R9;
          adress = adress_str;
          adress_mode = Immediat
        } ;
        Instruction.ADD {
          cc = None;
          destination = base_src_reg;
          operand1 = base_src_reg;
          operand2 = `Litteral 32L
        }
      ] @ (copy_large (Arm64ABI.increment_adress 32L adress_str) base_src_reg (Int64.sub size 32L))
        else (*size >= 64L*) 
          [
            Instruction.LDR {
              data_size = None;
              cc = None;
              destination = R9;
              adress_src = Arm64ABI.create_adress ~offset:(Lit (IntL 0L)) ~base:(base_src_reg) ();
              adress_mode = Immediat
            };
            Instruction.STR {
              data_size = None;
              cc = None;
              source = R9;
              adress = adress_str;
              adress_mode = Immediat
            } ;
            Instruction.ADD {
              cc = None;
              destination = base_src_reg;
              operand1 = base_src_reg;
              operand2 = `Litteral 64L
            }
          ] @ (copy_large (Arm64ABI.increment_adress 64L adress_str) base_src_reg (Int64.sub size 64L))






    ;;
    let copy_from_reg register (adress: Arm64ABI.address) ktype rprogram =
      let size =  KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
      match size with
      | 8L ->
        let data_size =  Some (if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then Arm64ABI.B else Arm64ABI.SB) in
        (Instruction.istr ~data_size register adress Immediat)
      | 16L -> let data_size = Some( if KosuIrTyped.Asttyhelper.RType.is_unsigned_integer ktype then Arm64ABI.H else Arm64ABI.SH) in
        (Instruction.istr ~data_size register adress Immediat)
      | 32L | 64L -> 
        (Instruction.istr register adress Immediat)
      | _ -> copy_large adress register size
    let function_prologue ~fn_register_params rprogram fd = 
      let base = Instruction.STP {x1 = R29; x2 = R30; base = SP; offset = Int64.to_int (Int64.sub fd.locals_space 16L); adress_mode = Immediat} in
      let stack_sub = Instruction.isub (SP) ~srcl:(`Register SP) ~srcr:(`Litteral fd.locals_space) in
      let copy_instructions = fn_register_params |> Util.ListHelper.combine_safe Arm64ABI.argument_registers |> List.fold_left (fun acc (register , (name, kt)) -> 
        let whereis = adress_of (name, kt) fd in
        acc @ (copy_from_reg (register) whereis kt rprogram)
        ) [] in
      base::stack_sub @ copy_instructions

    let function_epilogue fd = 
      let base = Instruction.LDP {x1 = R29; x2 = R30; base = SP; offset = Int64.to_int (Int64.sub fd.locals_space 16L); adress_mode = Immediat} in
      let stack_add = Instruction.iadd (SP) ~srcl:(`Register SP) ~srcr:(`Litteral fd.locals_space) in
      let return = Instruction.ret in

      base::stack_add @ return::[]

    let call_instruction ~origin args (_fd: frame_desc) = 
      let numbers_of_reg_params = Instruction.ABI.argument_registers |> List.length in
      let reg_params, _stack_param = args 
      |> List.mapi (fun index args -> index, args)
      |> List.partition_map ( fun (index, reg) -> if index < numbers_of_reg_params then Either.left reg else Either.right reg) in

      let assoc_args_reg_instructions = 
        reg_params 
        |> Util.ListHelper.combine_safe Instruction.ABI.argument_registers
        |> List.fold_left (fun acc (register, src) -> 
          acc @ (match src with 
          | `Register reg when reg = register -> []
          | _ -> Instruction.imov register src)
        ) []
      in
      let call = Instruction.BL { cc = None; label = Label origin} in
      assoc_args_reg_instructions @ [call]
end
let translate_tac_rvalue _current_module _rprogram (_fd: Arm64FrameManager.frame_desc) =

  let open Arm64FrameManager in
  let open Instruction in
  let open ABI in
  function
  | RVExpression ({tac_expression = TEFalse | TENullptr | TEmpty; _})  -> R8, imov R8 (src_of_immediat 0L)
  | RVExpression ({tac_expression = TETrue; _}) -> R8, imov R8 (src_of_immediat 1L)
  | RVExpression ({tac_expression = TEInt (_, _, int64); _}) -> R8, imov (dst_of_register R8) (src_of_immediat int64)
  | _ -> failwith ""
module IdVar = Common.IdVar
module IdVarMap = Common.IdVarMap



type data_size = 
| B
| W
| D
| Q

let data_size_of_int64 = function
| 1L -> Some B
| 2L -> Some W
| 4L -> Some D
| 8L -> Some Q
| _ -> None

let int64_of_data_size = function
| B -> 1L
| W -> 2L
| D -> 4L
| Q -> 8L

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

  type register = {
    size: data_size;
    reg: raw_register
  }


  (* %rdi, %rsi, %rdx, %rcx, %r8, %r9 *)
  let argument_registers = [
    RDI;
    RSI;
    RDX;
    RCX;
    R8;
    R9
  ]

  let sized_register size register = {
    size; reg = register
  }

  let rbpq = {
    size = Q; reg = RBP
  }

  let rspq = {
    size = Q;
    reg = RSP
  }

  let rdiq = {
    size = Q;
    reg = RDI
  }

  (** Rax *)
  let tmp_rax size = 
    let data_size = size |> data_size_of_int64 |> Option.value ~default:Q in
    sized_register data_size RAX

  let tmp_r9 size = 
    let data_size = size |> data_size_of_int64 |> Option.value ~default:Q in
    sized_register data_size R9
  
  (** R10 *)
  let tmp_r10 size = 
    let data_size = size |> data_size_of_int64 |> Option.value ~default:Q in
    sized_register data_size R10

  let tmp_r11 size =
      let data_size = size |> data_size_of_int64 |> Option.value ~default:Q in
      sized_register data_size R11
end

module Operande = struct
  type address = {
  offset: int64;
  base: Register.register;
  index: int64 option;
  scale: int;
  }

  type src = [
    `ILitteral of int64
    | `F64Litteral of float
    | `Register of Register.register
    | `Label of string
    | `Address of address
  ]

  type dst = [
    `Register of Register.register
    | `Address of address
  ]

  let is_adress = function
  | `Address _ -> true
  | _ -> false

  let create_address ?(offset = 0L) base = {offset; base; index = None; scale = 1}

  let increment_adress by address =  {
    address with offset = Int64.add address.offset by
  }

end



type condition_code = 
  | E
  | NE
  | S
  | NS
  | G
  | GE
  | L
  | LE
  | A
  | AE
  | B
  | BE

module Instruction = struct
  open Register
  open Operande
  
  type instruction = 
  | Mov of {
    size: data_size;
    source: src;
    destination: dst
  }
  | Lea of {
    size: data_size;
    source : dst;
    destination: register;
  }
  | Neg of {
    size: data_size;
    source: register;
  }
  | Not of {
    size: data_size;
    source: register;
  }
  | Add of {
    size: data_size;
    destination: dst;
    source: src;
  }
  | Sub of {
    size: data_size;
    destination: dst;
    source: src;
  }
  | IMul of {
    size: data_size;
    destination: register;
    source: src;
  }
  | Xor of {
    size: data_size;
    destination: dst;
    source: src;
  }
  | Or of {
    size: data_size;
    destination: dst;
    source: src;
  }  
  | And of {
    size: data_size;
    destination: dst;
    source: src;
  }
  | IDivl of {
    (* l | q *)
    size: data_size;
    divivor: src;
  }
  | Div of {
    (* l | q *)
    size: data_size;
    divivor: src;
  }
  (* Shift Left *)
  | Sal of {
    size: data_size;
    immediat: int;
    destination: dst;
  }
  (* Arithmetic right shift *)
  | Sar of {
    size: data_size;
    immediat: int;
    destination: dst;
  }
  (* Logical right shift *)
  | Shr of {
    size: data_size;
    immediat: int;
    destination: dst;
  }
  | Push of {
    size: data_size;
    source: src;
  }
  | Pop of {
    size: data_size;
    destination: dst;
  }
  | Jmp of {
    cc: condition_code option;
    where: [`Register of register | `Label of string ];
  }
  | Call of {
    what: [`Register of register | `Label of string ];
  }
  | Cltd
  | Cqto
  | Ret
end

type comment = Comment of string

type raw_line =
  | Instruction of Instruction.instruction
  | Directive of string
  | Label of string
  | Line_Com of comment

  let rec copy_large ~address_str ~base_address_reg size = 
    if size < 0L then failwith "X86_64 : Negative size to copy"
    else if size = 0L then []
    else 
      let dsize =  size |> data_size_of_int64 |> Option.value ~default:Q in
      let moved_size = int64_of_data_size dsize in
      let sized_rax = (Register.tmp_rax size) in
      [
        Instruction (Mov {size = dsize; source = `Address base_address_reg; destination = `Register sized_rax });
        Instruction (Mov {size = dsize; source = `Register sized_rax; destination =  `Address address_str})
      ] @ copy_large 
        ~address_str:(Operande.increment_adress moved_size address_str) 
        ~base_address_reg:(Operande.increment_adress moved_size base_address_reg)
        (Int64.sub size moved_size)
  
  (** 
    Copy the value in [register] at address [address]
    The function supposes that value in register is either the plain value if the value can be held in a register 
    or its address 
      
  *)
  let copy_from_reg (register: Register.register) address ktype rprogram =
    let open Instruction in 
  let size = KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram ktype in
  match size with
  | s when is_register_size s -> 
    let data_size = Option.get @@ data_size_of_int64 s in
    [
      Instruction (Mov {size = data_size; destination = `Address address; source = `Register register })
    ]
  | _ -> copy_large ~address_str:address ~base_address_reg:(Operande.create_address register) size

  let load_register register (address : Operande.address) ktype_size =
    let data_size = ktype_size |> data_size_of_int64 |> Option.value ~default:Q in
    [
      Instruction (Mov {size = data_size; source = `Address address; destination = `Register register})
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
    let need_result_ptr = return_type
    |> KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram
    |> is_register_size |> not
  in

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
           let rbp_relative_address = (locals_space |> Int64.neg |> Int64.add offset) in
           let address = create_address ~offset:rbp_relative_address ({size = Q; reg = RBP}) in
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
    (* let () = Printf.printf "Lookup => %s : %s\n" (variable) (KosuIrTyped.Asttypprint.string_of_rktype rktype) in *)
    if List.mem (variable, rktype) frame_desc.discarded_values then None
    else Some (IdVarMap.find (variable, rktype) frame_desc.stack_map)


    (** 
        Assumption on [fn_register_params] 
          already containing [rdi] if return type cannot be contain in [rax] 
    *)
    let function_prologue ~fn_register_params ~stack_params rprogram fd = 
      let stack_sub_size = Common.align_16 (Int64.add 16L fd.locals_space) in
      let base = Instruction (Push {size = Q; source = `Register rbpq}) in

      let sp_sub = [
        Instruction (Mov {size = Q; destination = `Register rspq; source = `Register rbpq});
        Instruction (Sub {size = Q; destination = `Register rspq; source = `ILitteral stack_sub_size })
      ] 
    in

    let copy_reg_instruction = 
      fn_register_params 
      |> Util.ListHelper.combine_safe argument_registers
      |> List.fold_left (fun acc (register, (name, kt)) -> 
        let whereis =  match address_of (name, kt) fd with
        | Some a -> a
        | None -> failwith "X86_64: No stack allocated for this variable"
        in
        acc @ copy_from_reg {size = Q; reg = register} whereis kt rprogram
      ) [] 
    in
    let stack_params_offset =
      stack_params
      |> List.map (fun (_, kt) ->
             if KosuIrTyped.Asttyconvert.Sizeof.sizeof rprogram kt > 8L then
               KosuIrTyped.Asttyhelper.RType.rpointer kt
             else kt)
    in
    let sp_address = Operande.create_address ~offset:(Int64.add 16L stack_sub_size) rspq in
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
      base::sp_sub @ copy_reg_instruction @ copy_stack_params_instruction
end

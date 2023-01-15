module IdVar = Common.IdVar
module IdVarMap = Common.IdVarMap



type data_size = 
| B
| W
| D
| Q

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

  let sized_rgister size register = {
    size; reg = register
  }
  
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
    lhs: dst;
    rhs: src;
  }
  | Sub of {
    size: data_size;
    lhs: dst;
    rhs: src;
  }
  | IMul of {
    size: data_size;
    lhs: register;
    rhs: src;
  }
  | Xor of {
    size: data_size;
    lhs: dst;
    rhs: src;
  }
  | Or of {
    size: data_size;
    lhs: dst;
    rhs: src;
  }  
  | And of {
    size: data_size;
    lhs: dst;
    rhs: src;
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

let is_register_size = function 1L | 2L | 4L | 8L -> true | _ -> false

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

  let stack_concat =
    if need_result_ptr then
      (indirect_return_var, indirect_return_type) :: stack_concat
    else stack_concat
  in

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


end
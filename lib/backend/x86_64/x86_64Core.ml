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
end

open Register

type address = {
  offset: int64 option;
  base: register;
  index: int64 option;
  scale: int option;
}

type src = [
  `ILitteral of int64
  | `F64Litteral of float
  | `Register of register
  | `Label of string
]

type dst = [
  `Register of register
  | `Address of address
]

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
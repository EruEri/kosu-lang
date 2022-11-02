open Kosu_frontend.Ast

type rswitch_case =
  | RSC_Enum_Identifier of { variant : string }
  | RSC_Enum_Identifier_Assoc of {
      variant : string ;
      assoc_ids : string option list;
    }

type rktype =
| RTParametric_identifier of {
    module_path : string;
    parametrics_type : rktype list;
    name : string;
  }
| RTType_Identifier of { module_path: string; name : string}
| RTInteger of (signedness * isize)
| RTPointer of rktype
| RTTuple of rktype list
| RTFunction of rktype list * rktype
| RTString_lit
| RTUnknow
| RTFloat
| RTBool
| RTUnit

type rkbody = rkastatement list * typed_expression
and typed_expression = {
  rktype: rktype;
  rexpression: rkexpression;
}
and rkastatement =
| RSDeclaration of {
  is_const: bool;
  variable_name: string;
  typed_expression: typed_expression;
}
| RSAffection of string * typed_expression
| RSDiscard of typed_expression
| RSDerefAffectation of string * typed_expression 

and rkexpression = 
| REmpty
| RTrue
| RFalse
| RENullptr
| REInteger of (signedness * isize * int64)
| REFloat of float
| RESizeof of { 
  rktype: rktype;
  size: int64; 
}
| REstring of string
| REAdress of string
| REDeference of int * (string)
| REIdentifier of { modules_path : string; identifier : string }
| REFieldAcces of { first_expr : typed_expression; fields : string list }
| REConst_Identifier of { modules_path : string; identifier : string }
| REStruct of {
  modules_path : string;
  struct_name : string;
  fields : (string * typed_expression) list;
}
| REEnum of {
  modules_path : string;
  enum_name : string option;
  variant : string;
  assoc_exprs : typed_expression list;
}
| RETuple of typed_expression list
| REBuiltin_Function_call of {
    fn_name : string;
    parameters : typed_expression list;
  }
| REFunction_call of {
    modules_path : string ;
    generics_resolver : ktype list option;
    fn_name : string ;
    parameters : typed_expression list;
  }
| REIf of (typed_expression) * rkbody * rkbody
| RECases of {
  cases : (typed_expression * rkbody) list;
  else_case : rkbody
}
| RESwitch of {
  rexpression: typed_expression;
  cases : (rswitch_case list * rkbody) list;
  wildcard_case : rkbody option;
}
| EBin_op of rkbin_op
| EUn_op of rkunary_op

and rkbin_op =
| RBAdd of typed_expression * typed_expression
| RBMinus of typed_expression * typed_expression
| RBMult of typed_expression * typed_expression
| RBDiv of typed_expression * typed_expression
| RBMod of typed_expression * typed_expression
| RBBitwiseOr of typed_expression * typed_expression
| RBBitwiseAnd of typed_expression * typed_expression
| RBBitwiseXor of typed_expression * typed_expression
| RBShiftLeft of typed_expression * typed_expression
| RBShiftRight of typed_expression * typed_expression
| RBAnd of typed_expression * typed_expression
| RBOr of typed_expression * typed_expression
| RBSup of typed_expression * typed_expression
| RBSupEq of typed_expression * typed_expression
| RBInf of typed_expression * typed_expression
| RBInfEq of typed_expression * typed_expression
| RBEqual of typed_expression * typed_expression
| RBDif of typed_expression * typed_expression

and rkunary_op = RUMinus of typed_expression | RUNot of typed_expression

type rstruct_decl = {
  rstruct_name: string;
  generics: string list;
  rfields : (string * rktype) list
}

type renum_decl = {
  renum_name : string;
  generics : string list;
  rvariants : ( string  * (rktype list) ) list;
}

type rfunction_decl = {
  rfn_name : string ;
  generics : string list;
  rparameters : (string * rktype) list;
  return_type : rktype;
  rbody : rkbody;
}

type roperator_decl =
  | RUnary of {
      op : parser_unary_op;
      rfield : (string * rktype);
      return_type : rktype;
      kbody : rkbody;
    }
  | RBinary of {
      op : parser_binary_op;
      rfields : (string * rktype) * (string * rktype);
      return_type : rktype;
      kbody : rkbody;
    }

type rsyscall_decl = {
  rsyscall_name : string;
  parameters : rktype list;
  return_type : rktype;
  opcode : int64;
}

type rexternal_func_decl = {
  rsig_name : string;
  fn_parameters : rktype list;
  return_type : rktype;
  is_variadic : bool;
  c_name : string option;
}

type rconst_decl = {
  rconst_name : string;
  value: typed_expression; 
}

type rmodule_node = 
| RNExternFunc of rexternal_func_decl
| RNFunction of rfunction_decl
| RNOperator of roperator_decl
| RNSyscall of rsyscall_decl
| RNStruct of rstruct_decl
| RNEnum of renum_decl
| RNConst of rconst_decl

type rmodule = RModule of rmodule_node list

type rmodule_path = {
  path: string;
  rmodule: rmodule
}

type named_rmodule_path = {
  filename: string;
  rmodule_path: rmodule_path
}

type rprogram = named_rmodule_path list
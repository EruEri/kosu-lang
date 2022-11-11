open KosuIrTyped.Asttyped
open KosuFrontend.Ast

type tac_binop_bool =
  | TacOr
  | TacSup
  | TacSupEq
  | TacInf
  | TacInfEq
  | TacEqual
  | TacDiff
  | TacAnd

type tac_binop_self =
  | TacAdd
  | TacMinus
  | TacMult
  | TacDiv
  | TacModulo
  | TacBitwiseOr
  | TacBitwiseAnd
  | TacBitwiseXor
  | TacShiftLeft
  | TacShiftRight
  

type tac_binop = TacSelf of tac_binop_self | TacBool of tac_binop_bool
type tac_unop = TacNot | TacUminus

type tac_expression =
  | TEFalse
  | TETrue
  | TEmpty
  | TENullptr
  | TEInt of (signedness * isize * int64)
  | TEFloat of float
  | TEIdentifier of string
  | TEString of string
  | TEConst of { module_path : string; name : string }
  | TESizeof of rktype

and tac_fncall = {
  module_path : string;
  fn_name : string;
  generics_resolver : rktype list option;
  tac_parameters : tac_expression list;
}

and binary = { binop : tac_binop; blhs : tac_expression; brhs : tac_expression }
and unary = { unop : tac_unop; expr : tac_expression }

and tac_rvalue =
  | RVUminus of tac_rvalue
  | RVNeg of tac_rvalue
  | RVExpression of tac_expression
  | RVFunction of tac_fncall
  | RVStruct of {
    module_path: string;
    struct_name: string;
    fields: (string*tac_expression) list
  }
  | RVEnum of {
    module_path: string;
    enum_name: string option;
    variant: string;
    assoc_tac_exprs: tac_expression list
    
  }
  | RVTuple of tac_expression list
  | RVFieldAcess of {
    first_expr: tac_expression;
    field: string
  }
  | RVAdress of string
  | RVDefer of string
  | RVBinop of binary
  | RVUnop of unary
  | RVLater
and tac_case = {
  statement_for_condition: tac_statement list;
  condition: tac_expression;
  goto: string;
  end_label: string;
  tac_body: tac_body;
}
and tac_statement =
  | STacDeclaration of { identifier : string; expression : tac_rvalue }
  | STacModification of { identifier : string; expression : tac_rvalue }
  | STDerefAffectation of { identifier : string; expression : tac_rvalue }
  | STIf of { 
    statement_for_bool: tac_statement list;
    condition_rvalue: tac_expression;
    goto: string;
    if_tac_body: tac_body;
    else_tac_body: tac_body;
}
  | SCases of {
    cases: tac_case list;
    else_tac_body: tac_body;
    exit_label: string;
  }

and tac_body = { label : string; body : tac_statement list * tac_expression }

let tac_rvalue_litteral_int sign isize value =
  RVExpression (TEInt (sign, isize, value))

let tac_rvalue_litteral_empty = RVExpression TEmpty
let tac_rvalue_litteral_false = RVExpression TEFalse
let tac_rvalue_litteral_true = RVExpression TETrue
let tac_rvalue_litteral_flaot float = RVExpression (TEFloat float)
let tac_rvalue_litteral_identifier var = RVExpression (TEIdentifier var)
let tac_rvalue_litteral_stringlit s = RVExpression (TEString s)

let tac_rvalue_litteral_const (module_path, name) =
  RVExpression (TEConst { module_path; name })

type tac_function_decl = {
    rfn_name : string;
    generics : string list;
    rparameters : (string * rktype) list;
    return_type : rktype;
    tac_body : tac_body;
}

type tac_operator_decl =
  | TacUnary of {
      op : parser_unary_op;
      rfield : string * rktype;
      return_type : rktype;
      tac_body : tac_body;
    }
  | TacBinary of {
      op : parser_binary_op;
      rfields : (string * rktype) * (string * rktype);
      return_type : rktype;
      tac_body : tac_body;
    }

type tac_module_node = 
| TNExternFunc of rexternal_func_decl
| TNSyscall of rsyscall_decl
| TNFunction of tac_function_decl
| TNOperator of tac_operator_decl
| TNStruct of rstruct_decl
| TNEnum of renum_decl
| TNConst of rconst_decl

type tac_module = TacModule of tac_module_node list

type tac_module_path = {
  path: string;
  tac_module: tac_module
}

type named_tacmodule_path = {
  filename: string;
  tac_module_path: tac_module_path
}

type tac_program = named_tacmodule_path list
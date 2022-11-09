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
| TacAnd
type tac_binop = 
| TacSelf of tac_binop_self
| TacBool of tac_binop_bool 

type tac_unop =
| TacNot
| TacUminus

type tac_expression =
| TEInt of (signedness * isize * int64)
| TEFloat of float
| TEIdentifier of string
| TEString of string
| TEConst of {module_path: string; name: string}
| TEUminus of tac_expression
| TENeg of tac_expression
and tac_fncall = 
| Function_call of {
  module_path: string;
  fn_name: string;
  generics_resolver: rktype list option;
  tac_parameters: tac_expression list
} 

and condition = {
  binbool: tac_binop_bool;
  clhs: tac_expression;
  crhs: tac_expression
}
and binary = {
  binop: tac_binop;
  blhs: tac_expression;
  brhs: tac_expression
}
and unary = {
  unop: tac_unop;
  expr: tac_expression
}
and tac_rvalue = 
| RLitteral of tac_expression
| RFunction of tac_fncall
| RSizeof of rktype
| RAdress of string
| RDefer of string
| RBinop of binary
| RUnop of unary
| RAffectation of string
| RIf of {
  statement_for_bool: tac_statement list;
  condition_rvalue: tac_rvalue;
  if_tac_body: tac_body;
  else_tac_body: tac_body;
}
and tac_statement = 
| STacDeclaration of {
  identifier: string;
  expression: tac_rvalue;

}
| STacModification of {
  identifier: string;
  expression: tac_rvalue
}
| STDerefAffectation of {
  identifier: string;
  expression: tac_rvalue;
}
and tac_body = {
  label: string;
  body: tac_statement list * tac_rvalue
}

let tac_rvalue_litteral_int sign isize value = 
  RLitteral ( TEInt(sign, isize, value) )

let tac_rvalue_litteral_flaot float = 
  RLitteral (TEFloat float)

let tac_rvalue_litteral_identifier var = 
  RLitteral (TEIdentifier var)

let tac_rvalue_litteral_stringlit s = 
  RLitteral (TEString s)

let tac_rvalue_litteral_const (module_path, name)  = 
  RLitteral (TEConst {module_path; name })
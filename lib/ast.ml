type signedness = Signed | Unsigned;;
type isize = I8 | I16 | I32 | I64;;

type switch_case =
| SC_Identifier of string
| SC_Integer_Literal of (signedness * isize * int64)
| SC_Enum_Identifier of {
  variant: string
}
| SC_Enum_Identifier_Assoc of {
  variant: string;
  assoc_ids: string option list
}





type ktype = 
| TParametric_identifier of (string * (ktype list) )
| TType_Identifier of string
| TInteger of (signedness * isize)
| TPointer of ktype
| TTuple of ktype list
| TFunction of (ktype list * ktype)
| TUnknow
| TFloat
| TBool
| TUnit
;;

type kstatement =
| SDeclaration of string * kexpression * bool
| SAffection of string * kexpression
| SExpression of kexpression
and kexpression = 
| Empty
| True
| False
| EInteger of (signedness * isize * int64)
| ESizeof of (ktype, kexpression) Either.t 
| EString of string
| EAdress of string
| EDeference of string
| EIdentifier of {
  modules_path: string list;
  identifier: string
}
| EStruct of {
  modules_path: string list;
  struct_name: string;
  fields: (string * kexpression) list
}
| EEnum of { 
  modules_path: string list;
  enum_name: string option;
  variant: string;
  assoc_exprs: kexpression list
}
| ETuple of kexpression list
| EFunction_call of {
  modules_path: string list;
  generics_resolver: ktype list option;
  fn_name: string;
  parameters: kexpression list;
}
| EIf of kexpression * (kstatement list) * (kstatement list)
| ECases of {
  cases: (kexpression list * kstatement list) list;
  else_case: kstatement list 
}
| ESwitch of {
  expression: kexpression;
  cases: (switch_case list * kstatement list) list;
  wildcard_case: kstatement list option
}
| EBin_op of kbin_op
| EUn_op of kunary_op
and kbin_op =
| BAdd of kexpression * kexpression
| BMinus of kexpression * kexpression
| BMult of kexpression * kexpression
| BDiv of kexpression * kexpression
| BMod of kexpression * kexpression
| BBitwiseOr of kexpression * kexpression
| BBitwiseAnd of kexpression * kexpression
| BBitwiseXor of kexpression * kexpression
| BShiftLeft of kexpression * kexpression
| BShiftRight of kexpression * kexpression
| BAnd of kexpression * kexpression
| BOr of kexpression * kexpression
| BSup of kexpression * kexpression
| BSupEq of kexpression * kexpression
| BInf of kexpression * kexpression
| BInfEq of kexpression * kexpression
| BEqual of kexpression * kexpression
| BDif of kexpression * kexpression
and kunary_op =
| UMinus of kexpression
| UNot of kexpression

type struct_decl = {
  struct_name: string;
  generics: string list;
  fields: (string * ktype) list
}

type enum_decl = {
  enum_name: string;
  generics: string list;
  variants: (string * (ktype list)) list
}

type function_decl = {
  fn_name: string;
  generics: string list;
  parameters: (string * ktype) list;
  return_type: ktype;
  body: kstatement list;
}

type external_func_decl = {
  sig_name: string;
  fn_parameters: (ktype) list;
  r_type: ktype;
  is_variadic: bool;
  c_name : string option;
}

type sig_decl = {
  sig_name: string;
  generics: string list;
  parameters: ktype list;
  return_type: ktype
}

type prog_node = 
| NExternFunc of external_func_decl
| NFunction of function_decl
| NStruct of struct_decl
| NEnum of enum_decl

type program = Prog of prog_node list

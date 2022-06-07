type signedness = Signed | Unsigned;;
type isize = I8 | I16 | I32 | I64;;

type kstatement =
| SDeclaration of string * kexpression * bool
| SAffection of string * kexpression
| SExpression of kexpression
and kexpression = 
| Empty
| True
| False
| EInteger of (signedness * isize * int64)
| ESizeof of kexpression
| EString of string
| EAdress of string
| EDeference of string
| EIdentifier of string * (string list)
| EStruct of string * ((string * kexpression) list)
| EEnum of (string option) * string * (kexpression list)
| ETuple of kexpression list
| EFunction_call of (string * (kexpression list)) * string list (* Module resolve*)
| EIf of kexpression * kstatement list * (kstatement list) option
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

type ktype = 
| TParametric_identifier of (string * (ktype list) )
| TType_Identifier of string
| TInteger of (signedness * isize)
| TPointer of ktype
| TTuple of ktype list
| TFunction of (ktype list * ktype)
| TBool
| TUnknow
| TUnit
| TFloat

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

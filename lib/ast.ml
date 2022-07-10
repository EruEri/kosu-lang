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
| TParametric_identifier of {
  module_path: string;
  parametrics_type: ktype list;
  name: string;
}
| TType_Identifier of {
  module_path: string;
  name: string
}
| TInteger of (signedness * isize)
| TPointer of ktype
| TTuple of  ktype list
| TFunction of ktype list * ktype
| TString_lit
| TUnknow
| TFloat
| TBool
| TUnit
;;

type kbody = kstatement list * kexpression
and kstatement =
| SDeclaration of  {
  is_const: bool;
  variable_name : string;
  expression: kexpression
}
| SAffection of string * kexpression
| SDiscard of kexpression
and kexpression = 
| Empty
| True
| False
| EInteger of (signedness * isize * int64)
| EFloat of float
| ESizeof of (ktype, kexpression) Either.t 
| EString of string
| EAdress of string
| EDeference of int*string
| EIdentifier of {
  modules_path: string;
  identifier: string
}
| EFieldAcces of {
  first_expr: kexpression;
  fields: string list
}
| EConst_Identifier of {
  modules_path: string;
  identifier: string
}
| EStruct of {
  modules_path: string;
  struct_name: string;
  fields: (string * kexpression) list
}
| EEnum of { 
  modules_path: string;
  enum_name: string option;
  variant: string;
  assoc_exprs: kexpression list
}
| ETuple of kexpression list
| EFunction_call of {
  modules_path: string;
  generics_resolver: ktype list option;
  fn_name: string;
  parameters: kexpression list;
}
| EIf of kexpression * kbody * kbody
| ECases of {
  cases: (kexpression list * kbody) list;
  else_case: kbody
}
| ESwitch of {
  expression: kexpression;
  cases: (switch_case list * kbody) list;
  wildcard_case: kbody option
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
  body: kbody;
}

type external_func_decl = {
  sig_name: string;
  fn_parameters: ktype list;
  r_type: ktype;
  is_variadic: bool;
  c_name : string option;
}

type const_decl = {
  const_name: string;
  explicit_type: ktype;
  value: kexpression
}

type sig_decl = {
  sig_name: string;
  generics: string list;
  parameters: ktype list;
  return_type: ktype
}

type module_node = 
| NExternFunc of external_func_decl
| NFunction of function_decl
| NSigFun of sig_decl
| NStruct of struct_decl
| NEnum of enum_decl
| NConst of const_decl

type _module = Mod of module_node list

type module_path = {
  path: string;
  _module: _module
}

type program = module_path list

module Env = struct

  type variable_info = {
    is_const: bool;
    ktype: ktype
  }

  type t = {
    contexts : ((string * variable_info) list) list
  }

  let flat_context env = env.contexts |> List.flatten

  let push_context (context: (string * variable_info) list) (env: t) = {
    contexts = context::env.contexts
  }
  let find_identifier_opt (identifier: string) (env: t) = 
    env |> flat_context |> List.assoc_opt identifier
  let add_variable couple (env: t) =
    match env.contexts with
    | [] -> env |> push_context (couple::[])
    | t::q -> {
      contexts = (couple::t)::q
    }
  let pop_context env = 
    match env.contexts with
    | [] -> env
    | _::q -> { contexts = q }
end

module Error = struct
  type struct_error = 
  | Unexpected_field of { expected: string ; found : string }
  | Unexisting_field of string
  | Wrong_field_count of { expected: int ; found : int }

  type enum_error = 
  | Wrong_length_assoc_type of { expected: int; found: int }
  | Uncompatible_type_in_variant of { variant_name: string }
  
  type ast_error = 
    | Bin_operator_Different_type
    | Undefined_Identifier of string
    | Undefined_Const of string
    | Undefined_Struct of string
    | Unbound_Module of string
    | Struct_Error of struct_error
    | Enum_Error of enum_error
    | Uncompatible_type of { expected: ktype; found : ktype }
    | Impossible_field_Access of ktype
    | Unvalid_Deference
  
  exception Ast_error of ast_error

  let ast_error e = Ast_error e
  let struct_error e = ast_error (Struct_Error e)
  let enum_error e = ast_error (Enum_Error e)
end

module Type = struct
  
  let rec are_compatible_type (lhs: ktype) (rhs: ktype) = 
    match lhs, rhs with
    | TParametric_identifier {module_path = mp1 ; parametrics_type = pt1; name = n1}
      , TParametric_identifier {module_path = mp2; parametrics_type = pt2; name = n2 } -> 
        n1 = n2 && mp1 = mp2 && (pt1 |> Util.are_same_lenght pt2) && (List.for_all2 are_compatible_type pt1 pt2)
    | TUnknow, _ | _ , TUnknow -> true
    | _, _ -> lhs =  rhs
end


module Type_Decl = struct
  type type_decl = 
  | Decl_Enum of enum_decl
  | Decl_Struct of struct_decl

  let decl_enum e = Decl_Enum e
  let decl_struct s = Decl_Struct s

  let is_enum = function Decl_Enum _ -> true | _ -> false
  let is_struct = function Decl_Struct _ -> true | _ -> false
end
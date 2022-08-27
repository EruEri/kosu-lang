type signedness = Signed | Unsigned;;
type isize = I8 | I16 | I32 | I64;;

type switch_case =
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
  explicit_type: ktype option;
  expression: kexpression
}
| SAffection of string * kexpression
| SDiscard of kexpression
and kexpression = 
| Empty
| True
| False
| ENullptr
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
| EBuiltin_Function_call of {
  fn_name: string;
  parameters: kexpression list
}
| EFunction_call of {
  modules_path: string;
  generics_resolver: ktype list option;
  fn_name: string;
  parameters: kexpression list;
}
| EIf of kexpression * kbody * kbody
| ECases of {
  cases: (kexpression * kbody) list;
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

type syscall_decl = {
  syscall_name: string;
  parameters: ktype list;
  return_type: ktype;
  opcode: int64
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
| NSyscall of syscall_decl
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

module OperatorFunction = struct
  type operator =
  | Add 
  | Minus 
  | Mult | Div 
  | Modulo | BitwiseOr 
  | BitwiseAnd | BitwiseXor 
  | ShiftLeft | ShiftRight 
  | And | Or 
  | Sup | SupEq
  | Inf | InfEq
  | Equal | Diff
  | Not | UMinus

  let name_of_operator = function
  | Add -> "add"
  | Minus -> "minus"
  | Mult -> "mult"
  | Div -> "div"
  | Modulo -> "modulo"
  | BitwiseAnd -> "bitwiseand"
  | BitwiseOr -> "bitwiseor"
  | BitwiseXor -> "bitwisexor"
  | ShiftLeft -> "shiftleft"
  | ShiftRight -> "shiftright"
  | And -> "and"
  | Or -> "or"
  | Sup -> "sup"
  | SupEq -> "supeq"
  | Inf -> "inf"
  | InfEq -> "infeq"
  | Equal -> "equal"
  | Diff -> "diff"
  | UMinus -> "uminus"
  | Not -> "not"

  let symbole_of_operator = function
  | Add -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Modulo -> "%"
  | BitwiseAnd -> "&"
  | BitwiseOr -> "|"
  | BitwiseXor -> "^"
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | And -> "&&"
  | Or -> "||"
  | Sup -> ">"
  | SupEq -> ">="
  | Inf -> "<"
  | InfEq -> "<="
  | Equal -> "=="
  | Diff -> "!="
  | Not -> "!"
  | UMinus -> "(-)"
end

module Error = struct
  type struct_error = 
  | Unexpected_field of { expected: string ; found : string }
  | Unexisting_field of string
  | Wrong_field_count of { expected: int ; found : int }

  type enum_error = 
  | Wrong_length_assoc_type of { expected: int; found: int }
  | Uncompatible_type_in_variant of { variant_name: string }

  type statement_error = 
  | Undefine_Identifier of { name: string }
  | Already_Define_Identifier of { name: string }
  | Reassign_Constante of { name: string }
  | Uncompatible_type_Assign of { expected: ktype; found: ktype}
  | Neead_explicit_type_declaration of { variable_name: string; infer_type: ktype }

  type func_error =
  | Unmatched_Parameters_length of { expected: int; found: int }
  | Unmatched_Generics_Resolver_length of { expected: int; found: int }
  | Uncompatible_type_for_C_Function of { external_func_decl: external_func_decl }
  | Uncompatible_type_for_Syscall of { syscall_decl: syscall_decl}
  | Mismatched_Parameters_Type of { expected : ktype; found : ktype }
  | Unknow_Function_Error

  type operator_error =
  | Invalid_pointer_arithmetic of ktype
  | No_built_in_op of {bin_op: OperatorFunction.operator; ktype: ktype}
  | Incompatible_Type of { bin_op: OperatorFunction.operator; lhs: ktype; rhs: ktype }
  | Operator_not_found of {bin_op: OperatorFunction.operator; ktype: ktype}
  | Too_many_operator_declaration of {bin_op: OperatorFunction.operator; ktype: ktype}
  | Not_Boolean_operand_in_And
  | Not_Boolean_operand_in_Or
  | Invalid_Uminus_for_Unsigned_integer of isize

  type builtin_func_error = 
  | Unknow_built_function of string
  | Wrong_parameters of { fn_name: string; expected: ktype; found: ktype }
  | Mismatched_Parameters_Length of { fn_name: string; expected: int; found: int }
  | Found_no_Integer of { fn_name: string; found: ktype}

  type switch_error =
  | Not_enum_type_in_switch_Expression of ktype
  | Not_all_cases_handled of (string* (ktype list)) list
  | Duplicated_case of string
  | Variant_not_found of {enum_decl: enum_decl; variant: string}
  | Mismatched_Assoc_length of { variant: string; expected: int; found: int}
  | Incompatible_Binding of ((string * ktype) list) * ((string * ktype) list)
  | Binded_identifier_already_exist of string


  type ast_error = 
    | Bin_operator_Different_type
    | Wrong_Assoc_Length_for_Parametrics of { expected: int; found: int; ktype: ktype} 
    | Undefined_Identifier of string
    | Undefined_Const of string
    | Undefined_Struct of string
    | Unbound_Module of string
    | Struct_Error of struct_error
    | Enum_Error of enum_error
    | Statement_Error of statement_error
    | Func_Error of func_error
    | Operator_Error of operator_error
    | Switch_error of switch_error
    | Builtin_Func_Error of builtin_func_error
    | Uncompatible_type of { expected: ktype; found : ktype }
    | Uncompatible_type_If_Else of { if_type: ktype; else_type: ktype }
    | Not_Boolean_Type_Condition of { found: ktype }
    | Impossible_field_Access of ktype
    | Enum_Access_field of {field: string; enum_decl: enum_decl}
    | Unvalid_Deference
  
  exception Ast_error of ast_error

  let ast_error e = Ast_error e
  let struct_error e = ast_error (Struct_Error e)
  let enum_error e = ast_error (Enum_Error e)
  let stmt_error e = ast_error (Statement_Error e)
  let func_error e = ast_error (Func_Error e)
  let operator_error e = ast_error (Operator_Error e)
  let switch_error e = ast_error (Switch_error e)
  let built_in_func_error e = ast_error (Builtin_Func_Error e)  
end

module Type = struct

  let ktuple kts = TTuple kts

  let is_any_ptr = function
  | TPointer _ -> true
  | _ -> false

  let is_unknown_ptr = function
  | TPointer TUnknow -> true
  | _ -> false

  let is_any_integer = function
  | TInteger _ -> true
  | _ -> false

  let is_string_litteral = function
  | TString_lit -> true
  | _ -> false

  let rec is_builtin_type = function
  | TParametric_identifier _ | TType_Identifier _ -> false
  | TTuple kts -> kts |> List.for_all is_builtin_type
  | _ -> true

  let is_parametric = function
  | TParametric_identifier _ -> true
  | _ -> false
  
  let rec is_type_full_known ktype = 
    match ktype with
    | TUnknow -> false
    | TParametric_identifier { module_path = _; parametrics_type = kts; name = _ } | TTuple (kts) -> kts |> List.for_all (is_type_full_known)
    | TPointer kt -> is_type_full_known kt
    | _ -> true

  let extract_parametrics_ktype ktype = 
    match ktype with
    | TParametric_identifier {module_path = _; parametrics_type; name = _ } -> parametrics_type
    | _ -> []

  let type_name_opt = function
  | TType_Identifier {module_path = _ ; name} | TParametric_identifier {module_path = _; parametrics_type = _; name} -> Some name
  | _ -> None

  let module_path_opt = function
  | TType_Identifier {module_path; name = _ } | TParametric_identifier {module_path; parametrics_type = _; name = _} -> Some module_path
  | _ -> None

  let rec are_compatible_type (lhs: ktype) (rhs: ktype) = 
    match lhs, rhs with
    | TParametric_identifier {module_path = mp1 ; parametrics_type = pt1; name = n1}
      , TParametric_identifier {module_path = mp2; parametrics_type = pt2; name = n2 } -> 
        n1 = n2 && mp1 = mp2 && (pt1 |> Util.are_same_lenght pt2) && (List.for_all2 are_compatible_type pt1 pt2)
    | TUnknow, _ | _ , TUnknow -> true
    | TPointer _, TPointer TUnknow -> true
    | TPointer TUnknow, TPointer _ -> true
    | _, _ -> lhs =  rhs

  let rec remap_generic_ktype (generics_table) (ktype) = 
    match ktype with
    | TType_Identifier { module_path = ""; name } as kt -> ( match Hashtbl.find_opt generics_table name with None -> kt | Some (_, typ) -> typ)
    | TParametric_identifier { module_path; parametrics_type; name } -> TParametric_identifier {
      module_path;
      parametrics_type = parametrics_type |> List.map ( remap_generic_ktype generics_table);
      name
    }
    | TTuple kts -> TTuple ( kts |> List.map (remap_generic_ktype generics_table))
    | TPointer kt -> TPointer (remap_generic_ktype generics_table kt)
    | _ as kt -> kt 

    let rec map_generics_type (generics_combined: (string*ktype) list) (primitive_generics: string list) ktype = 
      match ktype with
      (* | TType_Identifier {module_path = ""; name} when primitive_generics |> List.mem name -> ktype *)
      | TType_Identifier { module_path = ""; name} -> generics_combined |> List.assoc_opt name |> Option.value ~default:ktype
      | TParametric_identifier {module_path; parametrics_type; name} -> TParametric_identifier {
        module_path;
        parametrics_type = parametrics_type |> List.map (map_generics_type generics_combined primitive_generics);
        name
      }
      | TTuple kts -> TTuple (kts |> List.map (map_generics_type generics_combined primitive_generics))
      | TPointer kt -> TPointer(map_generics_type generics_combined primitive_generics kt)
      | _ -> ktype

  (**
  Returns the restricted version of the left type.
  this function returns the left type if [not (are_compatible_type to_restrict_type restrict_type)]
  *)
  let rec restrict_type (to_restrict_type: ktype) (restricted_type: ktype) = 
    if not (are_compatible_type to_restrict_type restricted_type) then to_restrict_type
    else
      match to_restrict_type, restricted_type with
      | TParametric_identifier {module_path = mp1 ; parametrics_type = pt1; name = n1}
      , TParametric_identifier {module_path = mp2; parametrics_type = pt2; name = n2 } -> 
        if (n1 <> n2 || mp1 <> mp2 || Util.are_diff_lenght pt1 pt2) then to_restrict_type else TParametric_identifier {
          module_path = mp1;
          parametrics_type = (List.combine pt1 pt2 |> List.map (fun (lhs, rhs) -> restrict_type lhs rhs));
          name = n1
        } 
      | TUnknow, t -> t
      | (TPointer _) as kt , TPointer TUnknow -> kt
      | _, _ -> to_restrict_type
end


module Type_Decl = struct
  type type_decl = 
  | Decl_Enum of enum_decl
  | Decl_Struct of struct_decl

  let decl_enum e = Decl_Enum e
  let decl_struct s = Decl_Struct s
end

module Function_Decl = struct
  type t = 
  | Decl_External of external_func_decl
  | Decl_Kosu_Function of function_decl
  | Decl_Syscall of syscall_decl

  let decl_external e = Decl_External e
  let decl_kosu_function e = Decl_Kosu_Function e
  let decl_syscall e = Decl_Syscall e
  let is_external = function Decl_External _ -> true | _ -> false
  let is_syscall = function Decl_Syscall _ -> true | _ -> false
  let is_kosu_func = function Decl_Kosu_Function _ -> true | _ -> false
end

module Builtin_Function = struct
  type functions = 
  | Tos8 
  | Tou8 
  | Tos16
  | Tou16
  | Tos32
  | Tou32
  | Tos64
  | Tou64
  | Stringl_ptr 
end

module Env = struct

  type variable_info = {
    is_const: bool;
    ktype: ktype
  }

  type t = {
    contexts : ((string * variable_info) list) list
  }

  let create_empty_env : t = {
    contexts = []
  }

  let create_env first_context = {
    contexts = first_context::[]
  }
  let flat_context env = env.contexts |> List.flatten

  let push_context (context: (string * variable_info) list) (env: t) = {
    contexts = context::env.contexts
  }
  let find_identifier_opt (identifier: string) (env: t) = 
    env |> flat_context |> List.assoc_opt identifier
  let is_identifier_exists identifier (env: t) = 
    env |> flat_context |> List.assoc_opt identifier |> Option.is_some

  let rec restrict_variable_type_context identifier ktype (context: (string * variable_info) list ) =
    match context with
    | [] -> []
    | t::q -> 
      let ctx_id, ctx_variable_info = t in
      if ctx_id <> identifier then t::(restrict_variable_type_context identifier ktype q)
      else 
        let { is_const = ctx_is_const; ktype = ctx_ktype } = ctx_variable_info in 
        let new_variable_info = { is_const = ctx_is_const; ktype = Type.restrict_type ctx_ktype ktype } in
        (ctx_id, new_variable_info)::q
      

  let restrict_variable_type identifier ktype (env: t) = 
    {
      contexts = env.contexts |> List.map (restrict_variable_type_context identifier ktype)
    }

  
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

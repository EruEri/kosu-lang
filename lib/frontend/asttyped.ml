open Ast
open Typecheck

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

type rkbody = rkastatement list * rkexpression
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
| SAffection of string * typed_expression
| SDiscard of typed_expression
| SDerefAffectation of string * kexpression 

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

module Convert = struct
  let rec from_ktype = function
  | TParametric_identifier {module_path; parametrics_type; name} -> RTParametric_identifier {
    module_path = module_path.v;
    parametrics_type = parametrics_type |> List.map (fun pkt -> pkt |> Position.value |> from_ktype);
    name = name.v
  }
  | TType_Identifier {module_path; name} -> RTType_Identifier {
    module_path = module_path.v;
    name = name.v
  }
  | TInteger (sign, size) -> RTInteger (sign, size)
  | TPointer kt -> RTPointer (kt.v |> from_ktype)
  | TTuple kts -> RTTuple (kts |> List.map (fun kt -> kt |> Position.value |> from_ktype)) 
  | TFunction (parameters, return_type) -> RTFunction (
    parameters |> List.map (fun kt -> kt |> Position.value |> from_ktype),
    return_type |> Position.value |> from_ktype
  )
  | TString_lit -> RTString_lit
  | TFloat -> RTFloat
  | TBool -> RTBool
  | TUnit -> RTUnit
  | TUnknow -> RTUnknow

  and typed_expression_of_kexpression ~generics_resolver (env : Env.t) (current_mod_name : string)
  (prog : module_path list) (expression : kexpression Position.location ) = {
    rktype = expression |> typeof ~generics_resolver env current_mod_name prog |> from_ktype;
    rexpression = failwith ""
  }
  (* and from_kexpression current_module program = let open Position in function
  | Empty -> REmpty
  | True -> RTrue
  | False -> RFalse
  | ENullptr -> RENullptr
  | EInteger(sign, size, value) -> REInteger (sign, size, value)
  | EFloat float -> REFloat float
  | ESizeof (either) -> (
    match either with
    | Either.Left ktype -> begin 
      let rktype = from_ktype ktype.v in
      let sizeof = Asthelper.Sizeof.sizeof current_module program ktype.v in
      RESizeof {
        rktype;
        size = sizeof;
      }
    end
    | Either.Right kexpression ->  begin 
      let type_of = type
    end
  ) *)
  and from_module_node current_module (prog : module_path list) = function
  | NConst const_decl -> 
    let generics_resolver = Hashtbl.create 0 in
      RNConst {
    rconst_name = const_decl.const_name.v;
    value = typed_expression_of_kexpression ~generics_resolver Env.create_empty_env current_module prog const_decl.value
  }
  | NExternFunc external_func_decl -> RNExternFunc {
    rsig_name = external_func_decl.sig_name.v;
    fn_parameters = external_func_decl.fn_parameters |> List.map (fun lkt -> lkt.v |> from_ktype);
    return_type = external_func_decl.r_type |> Position.value |> from_ktype;
    is_variadic = external_func_decl.is_variadic;
    c_name = external_func_decl.c_name;
  }
  | NSyscall syscall_decl -> RNSyscall {
    rsyscall_name = syscall_decl.syscall_name.v;
    parameters = syscall_decl.parameters |> List.map (fun ktl -> ktl.v |> from_ktype);
    return_type = syscall_decl.return_type.v |> from_ktype;
    opcode = syscall_decl.opcode.v 
  }
  | NSigFun _ -> failwith "To Delete in AST" 
  and from_module_path module_path_list {path; _module = Mod (module_nodes)} = 
   {
    path;
    rmodule = RModule (module_nodes |> List.map (fun mn -> from_module_node path module_path_list mn))
   }
  and from_program (program: Ast.program): rprogram = 
    program |> List.map (fun {filename; module_path} -> {
      filename; 
      rmodule_path = 
        from_module_path (program |> Asthelper.Program.to_module_path_list) module_path})
end
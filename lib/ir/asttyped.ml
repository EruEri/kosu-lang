open Kosu_frontend.Ast
open Kosu_frontend.Typecheck
open Kosu_frontend.Ast.Env
open Kosu_frontend

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

module Convert = struct
  let rec restrict_rktype to_restrict restrict = match to_restrict, restrict with
  | RTParametric_identifier {module_path = lmp; parametrics_type = lpt; name = lname} as lhs, RTParametric_identifier { module_path = rmp; parametrics_type = rpt; name = rname } -> 
    if lmp <> rmp || lname <> rname || Util.are_diff_lenght lpt rpt then lhs 
    else
      RTParametric_identifier {
      module_path = lmp;
      parametrics_type = List.map2 restrict_rktype lpt rpt;
      name = rname
    }
  | (RTPointer lhs, RTPointer rhs) -> RTPointer (restrict_rktype lhs rhs)
  | (RTUnknow, rtk) | (rtk, RTUnknow) -> rtk
  | (RTTuple rkts) as rkt, RTTuple lkts -> 
    if Util.are_diff_lenght rkts lkts then rkt
    else RTTuple (List.map2 restrict_rktype rkts lkts)
  | _ -> to_restrict 
  
  let restrict_typed_expression restrict typed_expression = {
      typed_expression with
      rktype = restrict_rktype typed_expression.rktype restrict
    }

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
  and from_switch_case = function
  | SC_Enum_Identifier { variant } -> RSC_Enum_Identifier { variant = variant.v}
  | SC_Enum_Identifier_Assoc { variant; assoc_ids} -> RSC_Enum_Identifier_Assoc {
    variant = variant.v;
    assoc_ids = assoc_ids |> List.map (Option.map Position.value)
  }
  and typed_expression_of_kexpression ~generics_resolver (env : Env.t) (current_mod_name : string)
  (prog : module_path list) (expression : kexpression Position.location ) = {
    rktype = expression |> typeof ~generics_resolver env current_mod_name prog |> from_ktype;
    rexpression = from_kexpression ~generics_resolver env current_mod_name prog expression.v
  }
  and rkbody_of_kbody 
  ~generics_resolver 
  (env: Env.t) 
  current_module 
  (program: module_path list) 
  ?(return_type = None) 
  ((kstatements: Ast.kstatement Position.location list), (kexpression: kexpression Position.location))
   = let open Position in 
   match kstatements with
   | kstatement::q -> (
    match kstatement.v with
    | SDiscard expr -> 
      let mapped = typed_expression_of_kexpression ~generics_resolver env current_module program expr in
      let stmts_remains, future_expr = (rkbody_of_kbody ~generics_resolver env current_module program ~return_type (q, kexpression)) in
      (RSDiscard mapped)::stmts_remains, future_expr
    | SDeclaration { is_const; variable_name; explicit_type; expression } -> 
      let typed_expression = typed_expression_of_kexpression ~generics_resolver env current_module program expression in
      let type_of_expression = typeof ~generics_resolver env current_module program expression in
      let variable_type = match explicit_type with
      | None -> type_of_expression
      | Some explicit_type -> explicit_type.v in
      let updated_env = env |> Env.add_variable (variable_name.v, {is_const; ktype = variable_type} ) in
      let stmts_remains, future_expr = (rkbody_of_kbody ~generics_resolver updated_env current_module program ~return_type (q, kexpression)) in
      (RSDeclaration {is_const; variable_name = variable_name.v; typed_expression})::stmts_remains, future_expr
    | SAffection (variable, expression) -> 
      let typed_expression = typed_expression_of_kexpression ~generics_resolver env current_module program expression in
      let stmts_remains, future_expr = (rkbody_of_kbody ~generics_resolver env current_module program ~return_type (q, kexpression)) in
      (RSAffection (variable.v, typed_expression))::stmts_remains, future_expr
    | SDerefAffectation (id, expression) -> 
      let {is_const = _; ktype} = env |> Env.find_identifier_opt (id.v) |> Option.get in
      let ktype = Type.restrict_type (Type.pointee_fail ktype) (expression |> typeof ~generics_resolver env current_module program) in
      let rktype = from_ktype ktype in
      let stmts_remains, future_expr = (rkbody_of_kbody ~generics_resolver env current_module program ~return_type (q, kexpression)) in
      (RSDerefAffectation (id.v, {
        rktype;
        rexpression = from_kexpression ~generics_resolver env current_module program expression.v;
      }))::stmts_remains, future_expr

      (* Latter use ktype of variable in env*)

   )
   | [] -> begin 
    let rktype = match return_type with Some kt -> kt |> from_ktype | None -> 
    kexpression |> typeof ~generics_resolver env current_module program |> from_ktype
    in
    let typed_ex = {
      rktype;
      rexpression = from_kexpression ~generics_resolver env current_module program kexpression.v
    } in
    [], typed_ex
  end 
  and from_kexpression ~generics_resolver (env: Env.t) current_module program = let open Position in function
  | Empty -> REmpty
  | True -> RTrue
  | False -> RFalse
  | ENullptr -> RENullptr
  | EInteger(sign, size, value) -> REInteger (sign, size, value)
  | EFloat float -> REFloat float
  | ESizeof either -> (
    let ktype =
    match either with
    | Either.Left ktype -> ktype.v
    | Either.Right kexpression -> 
      typeof ~generics_resolver env current_module program kexpression in
    let rktype = from_ktype ktype in
    let sizeof = Asthelper.Sizeof.sizeof current_module program ktype in
    RESizeof {
      rktype;
      size = sizeof;
    }
  )
  | EString s -> REstring s
  | EAdress e -> REAdress e.v
  | EDeference (c, id) -> REDeference (c, id.v)
  | EIdentifier {modules_path; identifier} -> REIdentifier {modules_path = modules_path.v; identifier = identifier.v}
  | EFieldAcces {first_expr; fields} -> 
    let typed_expression = typed_expression_of_kexpression ~generics_resolver env current_module program first_expr in
    REFieldAcces {
      first_expr = typed_expression;
      fields = fields |> List.map Position.value
    }
  | EConst_Identifier {modules_path; identifier} -> REConst_Identifier { modules_path = modules_path.v; identifier = identifier.v}
  | EStruct {modules_path; struct_name; fields} -> REStruct {
    modules_path = modules_path.v;
    struct_name = struct_name.v;
    fields = fields |> List.map (fun (field, expr) -> 
      field.v,
      typed_expression_of_kexpression ~generics_resolver env current_module program expr
    )
  }
  | EEnum {modules_path; enum_name; variant; assoc_exprs} -> REEnum {
      modules_path = modules_path.v;
      enum_name = enum_name |> Option.map Position.value;
      variant = variant.v;
      assoc_exprs = assoc_exprs |> List.map (typed_expression_of_kexpression ~generics_resolver env current_module program)
    }
  | ETuple exprs -> RETuple (exprs |> List.map (typed_expression_of_kexpression ~generics_resolver env current_module program))
  | EBuiltin_Function_call {fn_name; parameters} -> REBuiltin_Function_call {
    fn_name = fn_name.v;
    parameters = parameters |> List.map (typed_expression_of_kexpression ~generics_resolver env current_module program)
  }
  | EIf (condition, if_block, else_block) -> REIf ( 
    condition |> typed_expression_of_kexpression ~generics_resolver env current_module program,
    rkbody_of_kbody ~generics_resolver (env |> Env.push_context []) current_module program if_block,
    rkbody_of_kbody ~generics_resolver (env |> Env.push_context []) current_module program else_block
  )
  | ECases {cases; else_case} -> RECases {
    cases = cases |> List.map (fun (expr, kbody) -> 
      typed_expression_of_kexpression ~generics_resolver env current_module program expr,
      kbody |> rkbody_of_kbody ~generics_resolver (env |> Env.push_context []) current_module program
    );
    else_case = else_case |> rkbody_of_kbody ~generics_resolver (env |> Env.push_context []) current_module program
  }
  | ESwitch { expression; cases; wildcard_case } -> 
    let open Asthelper.Enum in
    let open Asthelper.Switch_case in
    let open Ast.Error in
    let variant_cases = cases |> List.map (fun (v, _) -> v |> List.map (function
    | SC_Enum_Identifier {variant} -> RSC_Enum_Identifier {variant = variant.v}
    | SC_Enum_Identifier_Assoc { variant; assoc_ids} -> RSC_Enum_Identifier_Assoc {
      variant = variant.v;
      assoc_ids = assoc_ids |> List.map (Option.map Position.value)
    }
    )) in
    let expr_type =
      typeof ~generics_resolver env current_module program expression
    in

    let module_path, name = expr_type |> Asthelper.module_path_of_ktype_opt |> Option.get in
    let enum_decl =
      match
        Asthelper.Program.find_type_decl_from_ktype
          ~ktype_def_path:module_path ~ktype_name:name
          ~current_module program
      with
      | Ok (Type_Decl.Decl_Enum e) -> e
      | _ -> failwith "Wierd it supposed to be an enum"
    in
    let generics_mapped =
      expr_type
      |> Ast.Type.extract_parametrics_ktype 
      |> List.combine
           (enum_decl.generics
           |> List.map (fun name ->
                  TType_Identifier
                    {
                      module_path = { v = ""; position = Position.dummy };
                      name;
                    }))
    in
    let rkbodys = ( cases
    |> List.map (fun (sc_list, kb) ->
           let combine_binding_type =
             sc_list
             |> List.map (fun sc ->
              let variant_name = (sc |> variant_name) in
                    let assoc_types =
                      extract_assoc_type_variant generics_mapped variant_name
                         enum_decl
                      |> Option.get
                    in
                    let assoc_binding = assoc_binding sc in
                    variant_name, assoc_types |> List.combine assoc_binding |> List.mapi (fun index (v, l) -> (index, v, l) )
             )
           in
           match combine_binding_type with
           | [] -> failwith "Unreachable case: empty case"
           | (first_variant, ass_bin) :: q ->
                 let new_conext = q
                 |> List.fold_left
                      (fun acc (variant_name, value) ->
                        let reduced_binding =
                          reduce_binded_variable_combine value
                        in
                        match Ast.Type.find_field_error acc reduced_binding with
                        | None -> 
                          begin 
                            acc 
                          end 
                        | Some (`diff_binding_name (lhs, rhs)) -> Incompatible_Binding_Name {
                          switch_expr = expression;
                          base_variant = first_variant;
                          base_bound_id = (lhs |> fst);

                          wrong_variant = variant_name;
                          wrong_bound_id = (rhs |> fst);
                        } |> switch_error |> raise
                        | Some (`diff_binding_ktype (lhs, rhs)) -> Incompatible_Binding_Ktype {
                          switch_expr = expression;
                          base_variant = first_variant;
                          base_bound_id = (lhs |> fst);
                          base_bound_ktype = (lhs |> snd);

                          wrong_variant = variant_name;
                          wrong_bound_id = (rhs |> fst);
                          wrong_bound_ktype = (rhs |> snd)
                        } |> switch_error |> raise
                        | Some (`diff_binding_index((base_index, base_bound_id), (wrong_index, wrong_bound_id))) -> Incompatible_Binding_Position {
                          base_index;
                          base_variant = first_variant;
                          base_bound_id;
                          wrong_index;
                          wrong_variant = variant_name;
                          wrong_bound_id;
                        } |> switch_error |> raise
                        )
                      (reduce_binded_variable_combine ass_bin)
                 |> List.map (fun (_, variable_name, ktype) -> 
                        ( variable_name,
                          ({ is_const = true; ktype = ktype.v }
                            : Env.variable_info) )) in
                (rkbody_of_kbody 
                  ~generics_resolver
                  (env |> Env.push_context (new_conext |> List.map Position.assoc_value_left))
                  current_module
                  program
                  kb
                )
    ))
               in 
    RESwitch {
    rexpression = typed_expression_of_kexpression ~generics_resolver env current_module program expression;
    cases = List.combine variant_cases rkbodys;
    wildcard_case = wildcard_case |> Option.map (
      rkbody_of_kbody ~generics_resolver (env |> Env.push_context []) current_module program 
    )
  }
  | EFunction_call {modules_path; generics_resolver = grc; fn_name; parameters} ->( 
      let fn_decl = Asthelper.Program.find_function_decl_from_fn_name modules_path fn_name current_module program |> Result.get_ok in
      
      match fn_decl with
      | Ast.Function_Decl.Decl_Syscall { syscall_name = _; parameters = sys_type_parameters; _ } ->
        let sys_rktype_parameters = sys_type_parameters |> List.map (fun stl -> stl |> Position.value |> from_ktype) in
        let typed_parameters = parameters |> List.map (typed_expression_of_kexpression ~generics_resolver env current_module program) in
        let mapped = List.map2 restrict_typed_expression sys_rktype_parameters typed_parameters in
        REFunction_call {
          modules_path = modules_path.v;
          generics_resolver = None;
          fn_name = fn_name.v;
          parameters = mapped
        }
      | Ast.Function_Decl.Decl_External { sig_name = _ ; fn_parameters; is_variadic; _ } -> 
        let mapped = if is_variadic then 
          let known_parameters_len = fn_parameters |> List.length in
          let external_rktype_parameters = fn_parameters |> List.map (fun stl -> stl |> Position.value |> from_ktype) in
          parameters 
            |> List.mapi (fun i expr -> i, typed_expression_of_kexpression ~generics_resolver env current_module program expr )
            |> List.partition (fun (index, _) -> index < known_parameters_len )
            |> fun (mappable, variadic) -> ( (List.map2 (fun rktype (i, typed_expr) -> i, restrict_typed_expression rktype typed_expr) external_rktype_parameters mappable) @ variadic )
            |> List.map snd
        else
          let external_rktype_parameters = fn_parameters |> List.map (fun stl -> stl |> Position.value |> from_ktype) in
          let typed_parameters = parameters |> List.map (typed_expression_of_kexpression ~generics_resolver env current_module program) in
          List.map2 restrict_typed_expression external_rktype_parameters typed_parameters in
        REFunction_call {
        modules_path = modules_path.v;
        generics_resolver = None;
        fn_name = fn_name.v;
         parameters = mapped 
        }
      | Ast.Function_Decl.Decl_Kosu_Function kosu_function -> 
        let new_map_generics = kosu_function.generics |> List.map (fun s -> s, ()) |> List.to_seq |> Hashtbl.of_seq in
        let typed_parameters = parameters |> List.map (typed_expression_of_kexpression ~generics_resolver:new_map_generics env current_module program) in
        let typed_parameters = kosu_function.parameters |> List.map (fun (_, { v = kt; _})-> from_ktype kt) |> List.map2 (fun typed_exp kt -> restrict_typed_expression kt typed_exp) typed_parameters in 
        REFunction_call {
          modules_path = modules_path.v;
          generics_resolver = grc |> Option.map (List.map Position.value);
          fn_name = fn_name.v;
          parameters = typed_parameters
        }
    )
  | _ -> failwith ""
  and from_module_node current_module (prog : module_path list) = let open Position in function
  | NStruct {struct_name; generics; fields} -> RNStruct {
    rstruct_name = struct_name.v;
    generics = generics |> List.map Position.value;
    rfields = fields |> List.map (fun (field, ktype) -> (field.v, ktype |> Position.value |> from_ktype))
  }
  | NEnum {enum_name; generics; variants} -> RNEnum {
    renum_name = enum_name.v;
    generics = generics |> List.map Position.value;
    rvariants = variants |> List.map ( fun (variant, assoc_ktype) -> 
      variant.v ,
      assoc_ktype |> List.map (fun lkt -> lkt |> Position.value |> from_ktype )
    )
  }
  | NOperator (Unary {op; field = (field, ktype); return_type; kbody}) -> 
    let empty_env = Env.create_empty_env in
    RNOperator(
    RUnary {
      op = op.v;
      rfield = (field.v, ktype |> Position.value |> from_ktype);
      return_type = return_type.v |> from_ktype;
      kbody = 
        rkbody_of_kbody 
        ~generics_resolver:(Hashtbl.create 0) 
        ( empty_env |> Env.add_fn_parameters ~const:true (field.v, ktype.v))
        current_module
        prog 
        ~return_type:(Some return_type.v)
        kbody
    }
  )
  | NOperator ( Binary {op; fields = (field1, ktype1), (field2, ktype2); return_type; kbody} ) -> 
      let env = Env.create_empty_env 
      |> Env.add_fn_parameters ~const:true (field1.v, ktype1.v)
      |> Env.add_fn_parameters ~const:true (field2.v, ktype2.v) in
    RNOperator (
        RBinary {
        op = op.v;
        rfields = (field1.v, from_ktype ktype1.v), (field2.v, from_ktype ktype2.v);
        return_type = return_type |> Position.value |> from_ktype;
        kbody = 
          rkbody_of_kbody 
          ~generics_resolver:(Hashtbl.create 0)
          env
          current_module
          prog
          ~return_type:(Some return_type.v)
          kbody
      }
      )
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
  | NFunction {fn_name; generics; parameters; return_type; body} ->
      let generics_resolver = generics |> List.map (fun g -> g, ()) |> List.to_seq |> Hashtbl.of_seq in
      let env = parameters |> List.fold_left (fun acc (para_name, ktype) -> 
        acc |> Env.add_fn_parameters ~const:false (para_name.v, ktype.v)
      ) Env.create_empty_env in 
      RNFunction {
        rfn_name = fn_name.v;
        generics = generics |> List.map Position.value;
        rparameters = parameters |> List.map (fun (lf, lkt) -> lf.v, lkt |> Position.value |> from_ktype);
      return_type = from_ktype return_type.v;
        rbody = 
          rkbody_of_kbody
          ~generics_resolver
          env
          current_module
          prog
          ~return_type:(Some return_type.v)
          body
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
open Ast
open Ast.Error

let find_struct_decl_from_name (current_module_name: string) (prog : program) (module_path: string) (struct_name: string) = 
  let structs_opt = (if module_path = "" then Some (prog |> Asthelper.Program.module_of_string current_module_name) else prog |> Asthelper.Program.module_of_string_opt current_module_name)
  |> Option.map Asthelper.Module.retrieve_struct_decl in
  
  match structs_opt with
  | None -> Error (Unbound_Module module_path)
  | Some structs ->
    structs |> List.find_opt (fun s -> s.struct_name = struct_name) |> Option.to_result ~none:(Undefined_Struct struct_name)
;;

 (**
  @raise Ast_error
  @raise Not_found : if a type declartion wasn't found
  @raise Too_Many_Occurence: if several type declarations matching was found
*)
 let rec validate_and_type_struct_initialisation ~env ~current_mod_name ~program ~struct_module_path ~fields ~struct_decl = 
  let parameters_length = fields |> List.length in
  let expected_length = struct_decl.fields |> List.length in
  if parameters_length <> expected_length then raise (Ast.Error.struct_error (Wrong_field_count { expected = expected_length; found = parameters_length }))
  else
    let zipped = List.combine fields struct_decl.fields in
    resolve_struct_type env current_mod_name program struct_module_path zipped struct_decl ~old_struct_decl:struct_decl
(*(**
  Check and validate the type of the struct initialisation
  @raise Ast_error
  @raise Not_found : if a type declartion wasn't found
  @raise Too_Many_Occurence: if several type declarations matching was found
*)*)
  and resolve_struct_type env (current_mod_name) (program) (struct_module_path) (fields_zipped) (struct_decl) ~old_struct_decl = 
  match fields_zipped with
  | [] -> Asthelper.Struct.to_ktype_help struct_module_path ~new_struct_decl:struct_decl old_struct_decl
  | ((init_field, init_expression), (struct_field, struct_ktype))::q -> 
    if init_field <> struct_field then raise (struct_error (Unexpected_field { expected = struct_field ; found = init_field }))
    else 
      let init_type = typeof env current_mod_name program init_expression in
      if not (Asthelper.Struct.is_type_compatible init_type struct_ktype old_struct_decl) then raise ( ast_error (Uncompatible_type { expected = struct_ktype; found = init_type}))
      else
        match Asthelper.Struct.retrieve_generic_name_from_field_opt struct_field old_struct_decl with
        | None -> resolve_struct_type env current_mod_name program struct_module_path q struct_decl ~old_struct_decl
        | Some generic_name -> resolve_struct_type env current_mod_name program struct_module_path q (Asthelper.Struct.bind_generic generic_name init_type struct_decl) ~old_struct_decl
(*(**
  Return the type of the code block expression
  @raise Ast_error
  @raise Not_found : if a type declartion wasn't found or a variant is not in enum variants
  @raise Too_Many_Occurence: if several type declarations matching was found
*)*)
and typeof_kbody (env: Env.t) (current_mod_name: string) (program: program) ?(return_type = None) (kbody: kbody) = 
  let statements, final_expr = kbody in
  match statements with
  | stamement::q -> begin 
    match stamement with
    | SDiscard expr -> ignore (typeof env current_mod_name program expr); typeof_kbody env current_mod_name program ~return_type (q, final_expr)
    | SDeclaration { is_const; variable_name; expression } -> 
      let type_init = typeof env current_mod_name program expression in
      if env |> Env.is_identifier_exists variable_name then raise (stmt_error (Ast.Error.Already_Define_Identifier { name = variable_name}))
      else
        typeof_kbody (env |> Env.add_variable ( variable_name , {is_const; ktype = type_init})) current_mod_name program ~return_type (q, final_expr) 
    | SAffection (variable, expr) -> (
      match env |> Env.find_identifier_opt variable with
      | None -> raise (stmt_error (Ast.Error.Undefine_Identifier { name = variable}))
      | Some { is_const; ktype } -> 
        if is_const then raise (stmt_error (Ast.Error.Reassign_Constante { name = variable}))
        else
          let new_type = typeof env current_mod_name program expr in
          if not (Ast.Type.are_compatible_type new_type ktype) then raise (stmt_error (Ast.Error.Uncompatible_type_Assign { expected = ktype; found = new_type}))
          else
            typeof_kbody (env |> Env.restrict_variable_type variable new_type) current_mod_name program ~return_type (q, final_expr)
    )
  end
  | [] -> 
    let final_expr_type = typeof env current_mod_name program final_expr in
    match return_type with
    | None -> final_expr_type
    | Some kt -> if not (Type.are_compatible_type kt final_expr_type) 
      then raise (stmt_error (Ast.Error.Uncompatible_type_Assign { expected = kt; found = final_expr_type}))
      else kt
and typeof (env : Env.t) (current_mod_name: string) (prog : program) (expression: kexpression) = 
  match expression with
  | Empty -> TUnit
  | True | False -> TBool
  | EInteger (sign, size, _ ) -> TInteger (sign, size) 
  | EFloat _ -> TFloat
  | ESizeof either -> begin 
    ignore (match either with
    | Left ktype -> ignore ( 
      match ktype with
      | TParametric_identifier { module_path; parametrics_type = _ ; name} | TType_Identifier { module_path; name } -> 
        ignore (Asthelper.Program.find_type_decl_from_ktype module_path name current_mod_name prog)
      | _ -> ignore ()
  )
    | Right expr -> ignore (typeof env current_mod_name prog expr));
    TInteger (Unsigned, I64)
  end 
  | EString _ ->  TString_lit
  | EAdress s -> env |> Env.flat_context |> List.assoc_opt s |> Option.map (fun (t: Env.variable_info) -> TPointer t.ktype) |> Option.fold ~none:(raise (ast_error (Undefined_Identifier s))) ~some:(Fun.id)
  | EDeference (indirection_count, id) -> begin
    let rec loop count ktype = 
      match count with
      | 0 ->  ktype
      | s -> match ktype with | Ast.TPointer t -> loop (s-1) t | _ -> raise (ast_error Unvalid_Deference)
    in
    match env |> Env.flat_context |> List.assoc_opt id with
    | None -> raise (ast_error (Undefined_Identifier id))
    | Some t -> loop indirection_count t.ktype
  end
  | EIdentifier { modules_path = _; identifier } ->
      env |> Env.flat_context 
      |> List.assoc_opt identifier 
      |> Option.map (fun (var_info: Env.variable_info) -> var_info.ktype)
      |> Option.fold ~none:(raise (ast_error (Undefined_Identifier identifier))) ~some:(Fun.id)
  | EConst_Identifier { modules_path ; identifier } -> begin
    let consts_opt = (if modules_path = "" then Some (prog |> Asthelper.Program.module_of_string current_mod_name) else prog |> Asthelper.Program.module_of_string_opt current_mod_name)
    |> Option.map Asthelper.Module.retrieve_const_decl in 

    match consts_opt with
    | None -> raise (ast_error (Unbound_Module modules_path))
    | Some consts -> 
      consts 
      |> List.find_map (fun c -> if c.const_name = identifier then Some c.explicit_type else None)
      |> Option.fold ~none: (raise (Ast_error (Unbound_Module modules_path))) ~some:(Fun.id)
    end
  | EFieldAcces { first_expr; fields } -> Asthelper.Struct.resolve_fields_access current_mod_name prog fields (typeof env current_mod_name prog first_expr)
  | EStruct { modules_path; struct_name; fields } -> 
    let struct_decl = match Asthelper.Program.find_struct_decl_opt current_mod_name modules_path struct_name prog with
    | Ok str -> str
    | Error e -> e |> ast_error |> raise in
  validate_and_type_struct_initialisation ~env ~current_mod_name ~program:prog ~struct_module_path:modules_path ~fields: fields ~struct_decl
  | EEnum { modules_path; enum_name; variant; assoc_exprs } -> begin 
    let enum_decl = match Asthelper.Program.find_enum_decl_opt current_mod_name modules_path enum_name variant assoc_exprs prog with
    | Error( Either.Right e ) -> raise e
    | Error (Left e) -> raise (Ast.Error.ast_error e)
    | Ok e -> e in
    let init_types = assoc_exprs |> List.map (typeof env current_mod_name prog) in
    let parametrics = enum_decl.variants 
    |> List.find_map (fun (var, assoc_types) -> if var = variant then Some assoc_types else None )
    |> Option.value ~default: (raise Not_found)
    |> fun assoc_types -> if Util.are_diff_lenght assoc_exprs assoc_types then raise (Ast.Error.enum_error (Ast.Error.Wrong_length_assoc_type { expected = assoc_types |> List.length; found = assoc_exprs |> List.length })) else assoc_types
    |> fun expected_types -> if not (Asthelper.Enum.is_valide_assoc_type_init ~init_types ~expected_types enum_decl) then raise (Ast.Error.enum_error (Ast.Error.Uncompatible_type_in_variant { variant_name = variant})) else expected_types
    |> fun expected_types -> Asthelper.Enum.infer_generics ~assoc_position: 0 (List.combine init_types expected_types) (enum_decl.generics |> List.map (fun s -> (TType_Identifier { module_path = ""; name = s}, false) )) enum_decl 
    |> List.map (fun (kt, true_type) -> if true_type then kt else TUnknow) in
    if not (Asthelper.Enum.contains_generics enum_decl) then TType_Identifier { module_path = if modules_path = "" then current_mod_name else modules_path; name = enum_decl.enum_name }
    else TParametric_identifier {
      module_path = if modules_path = "" then current_mod_name else modules_path;
      parametrics_type = parametrics;
      name = enum_decl.enum_name
  }
  end
  | ETuple expected_types -> TTuple (expected_types |> List.map (typeof env current_mod_name prog) )
  | EIf (expression, if_block, else_block ) -> 
    let if_condition = typeof env current_mod_name prog expression in
    if if_condition <> TBool then (raise (ast_error (Not_Boolean_Type_Condition { found = if_condition }) ))
    else
      let if_type = typeof_kbody (env |> Env.push_context []) current_mod_name prog if_block in
      let else_type = typeof_kbody (env |> Env.push_context []) current_mod_name prog else_block in
      if not (Type.are_compatible_type if_type else_type) then raise (ast_error (Ast.Error.Uncompatible_type_If_Else { if_type; else_type} ))
      else Type.restrict_type else_type if_type
  | ECases { cases; else_case } -> 
    cases 
    |> List.map (fun (expr, kbody) ->
      let expr_type = typeof env current_mod_name prog expr in
      if expr_type <> TBool then raise (ast_error (Not_Boolean_Type_Condition { found = expr_type }))
      else typeof_kbody (env |> Env.push_context []) current_mod_name prog kbody
    )
    |> List.fold_left (fun acc new_type -> 
      if not (Type.are_compatible_type acc new_type) then raise (ast_error (Uncompatible_type { expected = acc; found = new_type}))
      else Type.restrict_type acc new_type
    ) (typeof_kbody (env |> Env.push_context []) current_mod_name prog else_case)




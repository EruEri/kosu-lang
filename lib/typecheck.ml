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


(*(**
  Return the type of the code block expression
  @raise Ast_error
  @raise Not_found : if a type declartion wasn't found or a variant is not in enum variants
  @raise Too_Many_Occurence: if several type declarations matching was found
*)*)
let rec typeof_kbody ?(generics_resolver = None) (env: Env.t) (current_mod_name: string) (program: program) ?(return_type = None) (kbody: kbody) = 
let () = Printf.printf "env %s\n" ( Asthelper.string_of_env env) in
  let statements, final_expr = kbody in
  match statements with
  | stamement::q -> begin 
    match stamement with
    | SDiscard expr -> ignore (typeof ~generics_resolver env current_mod_name program expr); typeof_kbody  env current_mod_name program ~return_type (q, final_expr)
    | SDeclaration { is_const; variable_name; explicit_type ; expression } -> 
      let type_init = typeof env current_mod_name program expression in
      if env |> Env.is_identifier_exists variable_name then raise (stmt_error (Ast.Error.Already_Define_Identifier { name = variable_name}))
      else 
        if not (Type.are_compatible_type explicit_type type_init) then raise (Ast.Error.Uncompatible_type_Assign {expected = explicit_type; found = type_init } |> stmt_error |> raise )
        else
        typeof_kbody ~generics_resolver (env |> Env.add_variable ( variable_name , {is_const; ktype = explicit_type})) current_mod_name program ~return_type (q, final_expr) 
    | SAffection (variable, expr) -> (
      match env |> Env.find_identifier_opt variable with
      | None -> raise (stmt_error (Ast.Error.Undefine_Identifier { name = variable}))
      | Some { is_const; ktype } -> 
        if is_const then raise (stmt_error (Ast.Error.Reassign_Constante { name = variable}))
        else
          let new_type = typeof env current_mod_name program expr in
          if not (Ast.Type.are_compatible_type new_type ktype) then raise (stmt_error (Ast.Error.Uncompatible_type_Assign { expected = ktype; found = new_type}))
          else
            typeof_kbody ~generics_resolver (env |> Env.restrict_variable_type variable new_type) current_mod_name program ~return_type (q, final_expr)
    )
  end
  | [] -> 
    Printf.printf "Final expr\n"; 
    let final_expr_type = typeof env current_mod_name program final_expr in
    match return_type with
    | None -> final_expr_type
    | Some kt -> if not (Type.are_compatible_type kt final_expr_type) 
      then raise (stmt_error (Ast.Error.Uncompatible_type_Assign { expected = kt; found = final_expr_type}))
      else kt
and typeof ?(generics_resolver = None) (env: Env.t) (current_mod_name: string) (prog : program) (expression: kexpression) = 
  match expression with
  | Empty -> TUnit
  | True | False -> TBool
  | EInteger (sign, size, _ ) -> TInteger (sign, size) 
  | EFloat _ -> TFloat
  | ESizeof either -> begin 
    ignore (match either with
    | Left ktype -> ignore ( 
      match ktype with
      | TParametric_identifier { module_path; parametrics_type = _ ; name} | TType_Identifier { module_path; name } -> (
        try 
        ignore (Asthelper.Program.find_type_decl_from_ktype module_path name current_mod_name prog)
      with e -> begin 
        match generics_resolver with
        | None -> raise e
        | Some tbl -> ignore (Hashtbl.find_opt tbl name |> function | None -> raise e | Some s -> s)
      end
      )
      | _ -> ignore ()
  )
    | Right expr -> ignore (typeof env current_mod_name prog expr));
    TInteger (Unsigned, I64)
  end 
  | EString _ ->  TString_lit
  | EAdress s -> env |> Env.flat_context |> List.assoc_opt s |> Option.map (fun (t: Env.variable_info) -> TPointer t.ktype) |> ( function | None -> (raise (ast_error (Undefined_Identifier s))) | Some s -> s )
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
      |> (function | None -> (raise (ast_error (Undefined_Identifier identifier))) | Some s -> s)
  | EConst_Identifier { modules_path ; identifier } -> begin
    let consts_opt = (if modules_path = "" then Some (prog |> Asthelper.Program.module_of_string current_mod_name) else prog |> Asthelper.Program.module_of_string_opt current_mod_name)
    |> Option.map Asthelper.Module.retrieve_const_decl in 

    match consts_opt with
    | None -> raise (ast_error (Unbound_Module modules_path))
    | Some consts -> 
      consts 
      |> List.find_map (fun c -> if c.const_name = identifier then Some c.explicit_type else None)
      |> (function | None -> (raise (ast_error (Unbound_Module modules_path))) | Some s -> s)
    end
  | EFieldAcces { first_expr; fields } -> Asthelper.Struct.resolve_fields_access current_mod_name prog fields (typeof env current_mod_name prog first_expr)
  | EStruct { modules_path; struct_name; fields } -> begin
   
    let struct_decl = match Asthelper.Program.find_struct_decl_opt current_mod_name modules_path struct_name prog with
    | Ok str -> str
    | Error e -> e |> ast_error |> raise in

    let parameters_length = fields |> List.length in
    let expected_length = struct_decl.fields |> List.length in
    if parameters_length <> expected_length then raise (Ast.Error.struct_error (Wrong_field_count { expected = expected_length; found = parameters_length }));

    let generic_table = Hashtbl.create (struct_decl.generics |> List.length) in 
    let init_types = (fields |> List.map (fun (s, expr ) -> s, typeof env current_mod_name prog expr)) in
    List.combine init_types (struct_decl.fields)
    |> List.iter (fun ((init_field_name, init_type), (struct_field_name, expected_typed)) -> 
        if init_field_name <> struct_field_name then raise (struct_error (Unexpected_field { expected = struct_field_name ; found = init_field_name }));
        if (Asthelper.Struct.is_type_compatible_hashgen generic_table init_type expected_typed struct_decl) |> not then (Ast.Error.Uncompatible_type { expected = expected_typed; found = init_type } |> Ast.Error.ast_error |> raise);
      );

      Asthelper.Struct.to_ktype_hash generic_table modules_path struct_decl 
    end
  (* validate_and_type_struct_initialisation ~env ~current_mod_name ~program:prog ~struct_module_path:modules_path ~fields: fields ~struct_decl *)
  | EEnum { modules_path; enum_name; variant; assoc_exprs } -> begin 
    let enum_decl = match Asthelper.Program.find_enum_decl_opt current_mod_name modules_path enum_name variant assoc_exprs prog with
    | Error( Either.Right e ) -> raise e
    | Error (Left e) -> raise (Ast.Error.ast_error e)
    | Ok e -> e in

    let hashtbl = Hashtbl.create (enum_decl.generics |> List.length) in
    enum_decl.generics |> List.iteri (fun i generic_name -> Hashtbl.add hashtbl generic_name (i, TUnknow));
    let init_types = assoc_exprs |> List.map (typeof env current_mod_name prog) in
    let () = enum_decl.variants 
    |> List.find_map (fun (var, assoc_types) -> if var = variant then Some assoc_types else None )
    (* |> Option.map (fun k -> print_endline (k |> List.map Asthelper.string_of_ktype |> String.concat ", "); k ) *)
    (* |> function Some s -> s | None -> (raise Not_found) *)
    |> Option.get
    |> fun assoc_types -> if Util.are_diff_lenght init_types assoc_types then raise (Ast.Error.enum_error (Ast.Error.Wrong_length_assoc_type { expected = assoc_types |> List.length; found = assoc_exprs |> List.length })) else assoc_types
    |> List.combine init_types
    |> List.iter (fun (init, expected) -> 
      match Asthelper.Enum.is_type_compatible_hashgen hashtbl init expected enum_decl with
      | false -> Uncompatible_type { expected; found = init} |> ast_error |> raise
      | true -> ()
      ) in

      Asthelper.Enum.to_ktype_hash hashtbl modules_path enum_decl
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
  | EFunction_call { modules_path; generics_resolver = grc ; fn_name; parameters } -> begin
    let fn_decl = Asthelper.Program.find_function_decl_from_fn_name modules_path fn_name current_mod_name prog in
    match fn_decl with


    | Ast.Function_Decl.Decl_Kosu_Function e -> (
      if Util.are_diff_lenght (parameters) (e.parameters) then Unmatched_Parameters_length { expected = e.parameters |> List.length; found = parameters |> List.length } |> func_error |> raise;
      let init_type_parameters = parameters |> List.map ( typeof ~generics_resolver env current_mod_name prog ) in
      let hashtal = Hashtbl.create (e.generics |> List.length) in
      let () = match Asthelper.Function.does_need_generic_resolver e with
      | true -> 
        if Util.are_diff_lenght (grc |> Option.value ~default:[]) e.generics then Unmatched_Generics_Resolver_length { expected = e.generics |> List.length; found = (grc |> Option.value ~default:[] |> List.length) } |> func_error |> raise
        else ()
      | false -> begin 
        ()
      end in
    
      init_type_parameters
      |> List.combine e.parameters
      |> List.iter (fun ((_, para_type), init_type)  -> if e |> Asthelper.Function.is_type_compatible_hashgen hashtal init_type para_type |> not then Mismatched_Parameters_Type { expected = para_type; found = init_type}  |> func_error |> raise);

      Asthelper.Function.to_return_ktype_hashtab hashtal e
(*       
      let combine_generics = List.combine e.generics (grc |> Option.value ~default:[]) in
      let hashtal = Hashtbl.create (combine_generics |> List.length) in
      combine_generics |> List.iter ( fun ( name, assoc ) -> Hashtbl.add hashtal name assoc);

      typeof_kbody ~generics_resolver: (Some hashtal) (Env.create_env (e.parameters |> List.map (fun (name, ktype) -> (name, ({ is_const = true; ktype}: Env.variable_info ) )))) current_mod_name prog e.body *)
    )
    | Ast.Function_Decl.Decl_External external_func_decl -> begin
      if external_func_decl.is_variadic then 
        parameters 
      |> List.map ( typeof ~generics_resolver env current_mod_name prog )
      |> List.map (fun t -> if Asthelper.Program.is_c_type_from_ktype current_mod_name t prog then t else Ast.Error.Uncompatible_type_for_C_Function { external_func_decl } |> func_error |> raise)
      |> fun types -> if (types |> List.length) < (external_func_decl.fn_parameters |> List.length) then Unmatched_Parameters_length { expected = external_func_decl.fn_parameters |> List.length; found = parameters |> List.length } |> func_error |> raise else types
      |> List.mapi (fun i t -> (i,t))
      |> List.partition (fun (i,_) -> i <= (external_func_decl.fn_parameters |> List.length) )
      |> fun (lhs, _ ) -> lhs
      |> List.map (fun (_,t) -> t)
      |> List.combine external_func_decl.fn_parameters
      |> (List.for_all (fun (para_type, init_type) -> 
        match Asthelper.Program.is_c_type_from_ktype current_mod_name para_type prog, Asthelper.Program.is_c_type_from_ktype current_mod_name init_type prog with
        | true, true -> 
          if para_type <> init_type then Uncompatible_type_Assign { expected = para_type; found = init_type } |> stmt_error |> raise else true
        | _ -> Ast.Error.Uncompatible_type_for_C_Function { external_func_decl } |> func_error |> raise
      ) )
      |> fun b -> (if b then external_func_decl.r_type else Unknow_Function_Error |> func_error |> raise)
      else
        match (Util.are_same_lenght external_func_decl.fn_parameters parameters) with
        | false -> Unmatched_Parameters_length { expected = external_func_decl.fn_parameters |> List.length; found = parameters |> List.length } |> func_error |> raise
        | true -> 
          let mapped_type = parameters |> List.map ( typeof ~generics_resolver env current_mod_name prog ) in
          let zipped = List.combine external_func_decl.fn_parameters mapped_type in
          if (zipped |> List.for_all (fun (para_type, init_type) -> 
              match Asthelper.Program.is_c_type_from_ktype current_mod_name para_type prog, Asthelper.Program.is_c_type_from_ktype current_mod_name init_type prog with
              | true, true -> 
                if para_type <> init_type then Uncompatible_type_Assign { expected = para_type; found = init_type } |> stmt_error |> raise else true
              | _ -> Ast.Error.Uncompatible_type_for_C_Function { external_func_decl } |> func_error |> raise
            )) then external_func_decl.r_type
              else Unknow_Function_Error |> func_error |> raise
        end
      end
    | _ -> failwith ""




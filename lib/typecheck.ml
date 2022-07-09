open Ast
open Ast.Error
open Ast.Type

let find_struct_decl_from_name (current_module_name: string) (prog : program) (module_path: string) (struct_name: string) = 
  let structs_opt = (if module_path = "" then Some (prog |> Asthelper.Program.module_of_string current_module_name) else prog |> Asthelper.Program.module_of_string_opt current_module_name)
  |> Option.map Asthelper.Module.retrieve_struct_decl in
  
  match structs_opt with
  | None -> Error (Unbound_Module module_path)
  | Some structs ->
    structs |> List.find_opt (fun s -> s.struct_name = struct_name) |> Option.to_result ~none:(Undefined_Struct struct_name)
;;
let rec typeof_expected (env : Env.t) (current_mod_name: string) (prog : program) (expression: kexpression) = 
  match expression with
  | Empty -> Ok TUnit
  | True | False -> Ok TBool
  | EUn_op ( UMinus e | UNot e ) -> typeof_expected env current_mod_name prog e
  | EBin_op bin_op -> begin 
    match bin_op with
    | BAdd (lhs, rhs) | BMinus (lhs, rhs) | BMult (lhs, rhs) | BDiv (lhs, rhs) 
    | BMod (lhs, rhs) | BBitwiseOr (lhs, rhs) | BBitwiseAnd (lhs, rhs) | BBitwiseXor (lhs, rhs)
    | BShiftLeft (lhs, rhs) | BShiftRight (lhs, rhs) 
      -> if lhs = rhs then typeof_expected env current_mod_name prog lhs else Error Bin_operator_Different_type
    | BOr (lhs, rhs) | BAnd (lhs, rhs) | BEqual (lhs, rhs) | BDif (lhs, rhs) | BSup (lhs, rhs) 
    | BSupEq (lhs, rhs) | BInf (lhs, rhs) | BInfEq (lhs, rhs) -> if lhs = rhs then Ok TBool else Error Bin_operator_Different_type
  end
  | EInteger (sign, size, _ ) -> Ok (TInteger (sign, size) ) 
  | EFloat _ -> Ok TFloat
  | ESizeof _ -> Ok (TInteger (Unsigned, I64))
  | EString _ -> Ok TString_lit
  | EDeference (indirection_count, id) -> begin
    let rec loop count ktype = 
      match count with
      | 0 ->  Ok ktype
      | s -> match ktype with | Ast.TPointer t -> loop (s-1) t | _ -> Error Unvalid_Deference
    in
    match env |> Env.flat_context |> List.assoc_opt id with
    | None -> Error (Undefined_Identifier id)
    | Some t -> loop indirection_count t

  end
  | EAdress s -> begin 
    env |> Env.flat_context |> List.assoc_opt s |> Option.map (fun t -> TPointer t) |> Option.to_result ~none:(Undefined_Identifier s) 
  end
  | EConst_Identifier { modules_path ; identifier } -> begin
    let consts_opt = (if modules_path = "" then Some (prog |> Asthelper.Program.module_of_string current_mod_name) else prog |> Asthelper.Program.module_of_string_opt current_mod_name)
    |> Option.map Asthelper.Module.retrieve_const_decl in 

    match consts_opt with
    | None -> Error (Unbound_Module modules_path)
    | Some consts -> 
      consts 
      |> List.find_map (fun c -> if c.const_name = identifier then Some c.explicit_type else None)
      |> Option.to_result ~none: (Unbound_Module modules_path)
    end
  | EIdentifier { modules_path = _; identifier } ->
    env |> Env.flat_context |> List.assoc_opt identifier |> Option.to_result ~none: (Undefined_Identifier identifier)
  | ETuple exprs -> begin 
    let mapped = exprs |> List.map (typeof_expected env current_mod_name prog) in
    match mapped |> List.find_opt Result.is_error with
    | Some e -> e
    | None -> Ok (TTuple (mapped |> List.map Result.get_ok))
  end
  | EStruct { modules_path; struct_name; fields } -> begin
    let structs_opt = (if modules_path = "" then Some (prog |> Asthelper.Program.module_of_string current_mod_name) else prog |> Asthelper.Program.module_of_string_opt current_mod_name)
    |> Option.map Asthelper.Module.retrieve_struct_decl in
    
    match structs_opt with
    | None -> Error (Unbound_Module modules_path)
    | Some structs -> begin
      match structs |> List.find_opt (fun s -> s.struct_name = struct_name) with
      | None -> Error (Undefined_Struct struct_name)
      | Some _struct -> (
        _struct |> validate_struct_initialisation env current_mod_name prog modules_path fields 
      )
    end
  end
  | EFieldAcces { first_expr; fields } -> begin
    match typeof_expected env current_mod_name prog first_expr with
    | Error e -> Error e
    | Ok kt -> typeof_field_acces_not_generic env current_mod_name prog fields kt
  (* | EStruct { modules_path; struct_name; fields } -> begin

  end *)
  end
  | _ -> failwith ""
and validate_struct_initialisation env (current_mod_name: string) program struct_module_path (fields: (string * Ast.kexpression) list) struct_decl =         
  if not (struct_decl |> Asthelper.Struct.contains_generics) then Ok( TType_Identifier { module_path = struct_module_path;  name = struct_decl.struct_name} )
  else begin
    let parameters_lenght = fields |> List.length in
    let expected_lenght = struct_decl.fields |> List.length in
    if parameters_lenght <> expected_lenght
      then Error ( Struct_Error (Wrong_field_count { expected = expected_lenght; found = parameters_lenght }))
    else
      let zipped = List.combine fields struct_decl.fields in
      resolve_struct_type env current_mod_name program struct_module_path zipped struct_decl
end
and resolve_struct_type env (current_mod_name: string) program struct_module_path (fields_zipped: ((string*kexpression) * (string*ktype)) list) (struct_decl: struct_decl) = 
match fields_zipped with
| [] -> Ok (struct_decl |> Asthelper.Struct.to_ktype struct_module_path)
| ((init_field, init_expr), (decl_field, _))::q -> 
  if init_field <> decl_field then Error (Struct_Error (Unexpected_field { expected = decl_field ; found = init_field }))
  else 
    match typeof_expected env current_mod_name program init_expr with
    | Error e -> Error e
    | Ok t -> begin
      match validate_struct_type_initialisation decl_field t struct_decl with
      | Error e -> Error e
      | Ok str_decl -> resolve_struct_type env current_mod_name program struct_module_path q str_decl
    end
and validate_struct_type_initialisation (field_searched: string) (given_type: ktype) (struct_decl: struct_decl) = 
  let expected_type = struct_decl |> Asthelper.Struct.find_field_type field_searched in
  match struct_decl |> Asthelper.Struct.is_field_generic field_searched with
  | false -> if struct_decl |> Asthelper.Struct.is_type_compatible given_type expected_type then Ok struct_decl else Error (Uncompatible_type { expected = expected_type; found = given_type })
  | true -> Ok (
    let generic_name = struct_decl |> Asthelper.Struct.retrieve_generic_name_from_field_opt field_searched |> Option.get in
    struct_decl |> Asthelper.Struct.bind_generic generic_name given_type
    )
and typeof_field_acces_not_generic (env: Env.t) (current_mod_name: string) program (fields: string list) (ktype: ktype) =
  match fields with
  | [] -> failwith "Unreachable"
  | t::[] -> begin 
    match ktype with
    | TType_Identifier {module_path; name} -> (
      match find_struct_decl_from_name current_mod_name program module_path name with
      | Error e -> Error e
      | Ok struct_decl -> struct_decl.fields |> List.assoc_opt t |> Option.to_result ~none:(Struct_Error ( Unexisting_field t) )
    )
    | _ -> Error (Impossible_field_Access ktype)
  end
  | t::q -> begin 
    match ktype with
    | TType_Identifier {module_path; name} -> (
      match find_struct_decl_from_name current_mod_name program module_path name with
      | Error e -> Error e
      | Ok struct_decl -> (
        match struct_decl.fields |> List.assoc_opt t with
        | None -> Error (Struct_Error ( Unexisting_field t) )
        | Some kt -> typeof_field_acces_not_generic env current_mod_name program q kt
      )
    )
    | _ -> Error (Impossible_field_Access ktype)
  end

let rec is_valid_statements (env : Env.t) (current_mod_name: string) (prog : program) (statements: kstatement list) (last_expression: kexpression) (return_type: ktype) = 
  match statements with
  | [] -> is_valid_expresssion env current_mod_name prog last_expression
  | t::q -> 
    match t with
    | SDiscard expr -> is_valid_expresssion env current_mod_name prog expr && is_valid_statements env current_mod_name prog q last_expression return_type
    | _ -> failwith ""
and is_valid_expresssion (env : Env.t) (current_mod_name: string) (prog : program) (expression: kexpression) =
 match expression with
 | Empty | True | False | EInteger _ | EFloat _ | EString _ -> true
 | ESizeof either -> (match either with Either.Left _ -> true | Either.Right expr -> is_valid_expresssion env current_mod_name prog expr)
 | EUn_op (UMinus e | UNot e) -> is_valid_expresssion env current_mod_name prog e
 | EIdentifier { modules_path = _; identifier } | EAdress identifier -> begin env |> Env.find_identifier_opt identifier |> Option.fold ~none: false ~some:(fun _ -> true) end
 | EFieldAcces { first_expr; _ } -> is_valid_expresssion env current_mod_name prog first_expr && ( (typeof_expected env current_mod_name prog expression) |> Result.fold ~ok:(fun _ -> true) ~error:(fun _ -> false))
 | EConst_Identifier _ -> (typeof_expected env current_mod_name prog expression) |> Result.fold ~ok:(fun _ -> true) ~error:(fun _ -> false)
 | EStruct { modules_path; struct_name; fields } -> begin 
  match Asthelper.Program.find_struct_decl_opt current_mod_name modules_path struct_name prog with
  | Error _ -> false
  | Ok _struct -> (
    match _struct |> Asthelper.Struct.contains_generics with
    | false -> (
      if Util.are_same_lenght _struct.fields fields then false 
      else
        failwith ""
    )
    | true -> failwith ""
  )


end
 | _ -> failwith ""

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
  (**
  Check and validate the type of the struct initialisation
  @raise Ast_error
  @raise Not_found : if a type declartion wasn't found
  @raise Too_Many_Occurence: if several type declarations matching was found
*)
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
(**
  Return the type of the given expression
  @raise Ast_error
  @raise Not_found : if a type declartion wasn't found
  @raise Too_Many_Occurence: if several type declarations matching was found
*)
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
  
    



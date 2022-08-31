open Ast
open Ast.Error

(**
  Return the type of the code block expression by checking each expression in this one
  @raise Ast_error
  @raise No_Occurence : if a type declartion wasn't found or a variant is not in enum variants
  @raise Too_Many_Occurence: if several type declarations matching was found
*)
let rec typeof_kbody ?(generics_resolver = None) (env : Env.t)
    (current_mod_name : string) (program : program) ?(return_type = None)
    (kbody : kbody) =
  let () = Printf.printf "env %s\n" (Asthelper.string_of_env env) in
  let statements, final_expr = kbody in
  match statements with
  | stamement :: q -> (
      match stamement with
      | SDiscard expr ->
          ignore (typeof ~generics_resolver env current_mod_name program expr);
          typeof_kbody env current_mod_name program ~return_type (q, final_expr)
      | SDeclaration { is_const; variable_name; explicit_type; expression } ->
          let type_init = typeof env current_mod_name program expression in
          (* let () = Printf.printf "sizeof %s : %Lu\nalignement : %Lu\n" (Asthelper.string_of_ktype type_init) (Asthelper.Sizeof.sizeof current_mod_name program type_init) (Asthelper.Sizeof.alignmentof current_mod_name program type_init) in *)
          if env |> Env.is_identifier_exists variable_name then
            raise
              (stmt_error
                 (Ast.Error.Already_Define_Identifier { name = variable_name }))
          else
            let kt =
              match explicit_type with
              | None ->
                  if Ast.Type.is_type_full_known type_init |> not then
                    Neead_explicit_type_declaration
                      { variable_name; infer_type = type_init }
                    |> stmt_error |> raise
                  else type_init
              | Some explicit_type_sure ->
                  if not (Type.are_compatible_type explicit_type_sure type_init)
                  then
                    raise
                      (Ast.Error.Uncompatible_type_Assign
                         { expected = explicit_type_sure; found = type_init }
                      |> stmt_error |> raise)
                  else explicit_type_sure
            in
            typeof_kbody ~generics_resolver
              (env |> Env.add_variable (variable_name, { is_const; ktype = kt }))
              current_mod_name program ~return_type (q, final_expr)
      | SAffection (variable, expr) -> (
          match env |> Env.find_identifier_opt variable with
          | None ->
              raise
                (stmt_error (Ast.Error.Undefine_Identifier { name = variable }))
          | Some { is_const; ktype } ->
              if is_const then
                raise
                  (stmt_error
                     (Ast.Error.Reassign_Constante { name = variable }))
              else
                let new_type =
                  typeof ~generics_resolver env current_mod_name program expr
                in
                if not (Ast.Type.are_compatible_type new_type ktype) then
                  raise
                    (stmt_error
                       (Ast.Error.Uncompatible_type_Assign
                          { expected = ktype; found = new_type }))
                else
                  typeof_kbody ~generics_resolver
                    (env |> Env.restrict_variable_type variable new_type)
                    current_mod_name program ~return_type (q, final_expr)))
  | [] -> (
      Printf.printf "Final expr\n";
      let final_expr_type =
        typeof ~generics_resolver env current_mod_name program final_expr
      in
      match return_type with
      | None -> final_expr_type
      | Some kt ->
          if not (Type.are_compatible_type kt final_expr_type) then
            raise
              (ast_error
                 (Ast.Error.Uncompatible_type
                    { expected = kt; found = final_expr_type }))
          else kt)

(**
  Return the type of an expression
  @raise Ast_error
  @raise No_Occurence : if a type declartion wasn't found or a variant is not in enum variants
  @raise Too_Many_Occurence: if several type declarations matching was found
*)
and typeof ?(generics_resolver = None) (env : Env.t) (current_mod_name : string)
    (prog : program) (expression : kexpression) =
  match expression with
  | Empty -> TUnit
  | True | False -> TBool
  | ENullptr -> TPointer TUnknow
  | EInteger (sign, size, _) -> TInteger (sign, size)
  | EFloat _ -> TFloat
  | ESizeof either ->
      let () =
        match either with
        | Left ktype ->
            ignore
              (match ktype with
              | TParametric_identifier
                  { module_path; parametrics_type = _; name }
              | TType_Identifier { module_path; name } -> (
                  try
                    ignore
                      (Asthelper.Program.find_type_decl_from_ktype
                         ~ktype_def_path:module_path ~ktype_name:name
                         ~current_module:current_mod_name prog)
                  with e -> (
                    match generics_resolver with
                    | None -> raise e
                    | Some tbl ->
                        ignore
                          (Hashtbl.find_opt tbl name |> function
                           | None -> raise e
                           | Some s -> s)))
              | _ -> ignore ())
        | Right expr ->
            ignore (typeof ~generics_resolver env current_mod_name prog expr)
      in
      TInteger (Unsigned, I64)
  | EString _ -> TString_lit
  | EAdress s -> (
      env |> Env.flat_context |> List.assoc_opt s
      |> Option.map (fun (t : Env.variable_info) -> TPointer t.ktype)
      |> function
      | None -> raise (ast_error (Undefined_Identifier s))
      | Some s -> s)
  | EDeference (indirection_count, id) -> (
      let rec loop count ktype =
        match count with
        | 0 -> ktype
        | s -> (
            match ktype with
            | Ast.TPointer t -> loop (s - 1) t
            | _ -> raise (ast_error Unvalid_Deference))
      in
      match env |> Env.flat_context |> List.assoc_opt id with
      | None -> raise (ast_error (Undefined_Identifier id))
      | Some t -> loop indirection_count t.ktype)
  | EIdentifier { modules_path = _; identifier } -> (
      env |> Env.flat_context |> List.assoc_opt identifier
      |> Option.map (fun (var_info : Env.variable_info) -> var_info.ktype)
      |> function
      | None -> raise (ast_error (Undefined_Identifier identifier))
      | Some s -> s)
  | EConst_Identifier { modules_path; identifier } -> (
      let consts_opt =
        (if modules_path = "" then
         Some (prog |> Asthelper.Program.module_of_string current_mod_name)
        else prog |> Asthelper.Program.module_of_string_opt current_mod_name)
        |> Option.map Asthelper.Module.retrieve_const_decl
      in

      match consts_opt with
      | None -> raise (ast_error (Unbound_Module modules_path))
      | Some consts -> (
          consts
          |> List.find_map (fun c ->
                 if c.const_name = identifier then Some c.explicit_type
                 else None)
          |> function
          | None -> raise (ast_error (Unbound_Module modules_path))
          | Some s -> s))
  | EFieldAcces { first_expr; fields } ->
      let first_type =
        typeof ~generics_resolver env current_mod_name prog first_expr
      in
      let parametrics_types = Type.extract_parametrics_ktype first_type in
      let ktype_def_path = Type.module_path_opt first_type |> Option.get in
      let ktype_name = Type.type_name_opt first_type |> Option.get in
      let type_decl =
        Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name
          ~current_module:current_mod_name prog
      in
      Asthelper.Struct.resolve_fields_access_gen parametrics_types fields
        type_decl current_mod_name prog
  | EStruct { modules_path; struct_name; fields } ->
      let struct_decl =
        match
          Asthelper.Program.find_struct_decl_opt current_mod_name modules_path
            struct_name prog
        with
        | Ok str -> str
        | Error e -> e |> ast_error |> raise
      in

      let parameters_length = fields |> List.length in
      let expected_length = struct_decl.fields |> List.length in
      if parameters_length <> expected_length then
        raise
          (Ast.Error.struct_error
             (Wrong_field_count
                { expected = expected_length; found = parameters_length }));

      let generic_table =
        Hashtbl.create (struct_decl.generics |> List.length)
      in
      let init_types =
        fields
        |> List.map (fun (s, expr) ->
               (s, typeof ~generics_resolver env current_mod_name prog expr))
      in
      List.combine init_types struct_decl.fields
      |> List.iter
           (fun
             ((init_field_name, init_type), (struct_field_name, expected_typed))
           ->
             if init_field_name <> struct_field_name then
               raise
                 (struct_error
                    (Unexpected_field
                       { expected = struct_field_name; found = init_field_name }));
             if
               Asthelper.Struct.is_type_compatible_hashgen generic_table
                 init_type expected_typed struct_decl
               |> not
             then
               Ast.Error.Uncompatible_type
                 { expected = expected_typed; found = init_type }
               |> Ast.Error.ast_error |> raise);

      Asthelper.Struct.to_ktype_hash generic_table modules_path struct_decl
  (* validate_and_type_struct_initialisation ~env ~current_mod_name ~program:prog ~struct_module_path:modules_path ~fields: fields ~struct_decl *)
  | EEnum { modules_path; enum_name; variant; assoc_exprs } ->
      let enum_decl =
        match
          Asthelper.Program.find_enum_decl_opt current_mod_name modules_path
            enum_name variant assoc_exprs prog
        with
        | Error (Either.Right e) -> raise e
        | Error (Left e) -> raise (Ast.Error.ast_error e)
        | Ok e -> e
      in

      let hashtbl = Hashtbl.create (enum_decl.generics |> List.length) in
      enum_decl.generics
      |> List.iteri (fun i generic_name ->
             Hashtbl.add hashtbl generic_name (i, TUnknow));
      let init_types =
        assoc_exprs
        |> List.map (typeof ~generics_resolver env current_mod_name prog)
      in
      let () =
        enum_decl.variants
        |> List.find_map (fun (var, assoc_types) ->
               if var = variant then Some assoc_types else None)
        (* |> Option.map (fun k -> print_endline (k |> List.map Asthelper.string_of_ktype |> String.concat ", "); k ) *)
        (* |> function Some s -> s | None -> (raise Not_found) *)
        |> Option.get
        |> fun assoc_types ->
        if Util.are_diff_lenght init_types assoc_types then
          raise
            (Ast.Error.enum_error
               (Ast.Error.Wrong_length_assoc_type
                  {
                    expected = assoc_types |> List.length;
                    found = assoc_exprs |> List.length;
                  }))
        else
          assoc_types |> List.combine init_types
          |> List.iter (fun (init, expected) ->
                 match
                   Asthelper.Enum.is_type_compatible_hashgen hashtbl init
                     expected enum_decl
                 with
                 | false ->
                     Uncompatible_type { expected; found = init }
                     |> ast_error |> raise
                 | true -> ())
      in

      Asthelper.Enum.to_ktype_hash hashtbl modules_path enum_decl
  | ETuple expected_types ->
      TTuple
        (expected_types
        |> List.map (typeof ~generics_resolver env current_mod_name prog))
  | EIf (expression, if_block, else_block) ->
      let if_condition =
        typeof ~generics_resolver env current_mod_name prog expression
      in
      if if_condition <> TBool then
        raise (ast_error (Not_Boolean_Type_Condition { found = if_condition }))
      else
        let if_type =
          typeof_kbody ~generics_resolver
            (env |> Env.push_context [])
            current_mod_name prog if_block
        in
        let else_type =
          typeof_kbody ~generics_resolver
            (env |> Env.push_context [])
            current_mod_name prog else_block
        in
        if not (Type.are_compatible_type if_type else_type) then
          raise
            (ast_error
               (Ast.Error.Uncompatible_type_If_Else { if_type; else_type }))
        else Type.restrict_type else_type if_type
  | ECases { cases; else_case } ->
      cases
      |> List.map (fun (expr, kbody) ->
             let expr_type =
               typeof ~generics_resolver env current_mod_name prog expr
             in
             if expr_type <> TBool then
               raise
                 (ast_error (Not_Boolean_Type_Condition { found = expr_type }))
             else
               typeof_kbody ~generics_resolver
                 (env |> Env.push_context [])
                 current_mod_name prog kbody)
      |> List.fold_left
           (fun acc new_type ->
             if not (Type.are_compatible_type acc new_type) then
               raise
                 (ast_error
                    (Uncompatible_type { expected = acc; found = new_type }))
             else Type.restrict_type acc new_type)
           (typeof_kbody ~generics_resolver
              (env |> Env.push_context [])
              current_mod_name prog else_case)
  | EBuiltin_Function_call { fn_name; parameters } -> (
      let ( >>= ) = Result.bind in
      let parameters_type =
        parameters
        |> List.map (typeof ~generics_resolver env current_mod_name prog)
      in

      fn_name |> Asthelper.Builtin_Function.builtin_fn_of_fn_name
      >>= (fun builtin ->
            Asthelper.Builtin_Function.is_valide_parameters_type parameters_type
              builtin)
      |> Result.map Asthelper.Builtin_Function.builtin_return_type
      |> function
      | Ok kt -> kt
      | Error e -> e |> built_in_func_error |> raise)
  | EFunction_call
      { modules_path; generics_resolver = grc; fn_name; parameters } -> (
      let fn_decl =
        Asthelper.Program.find_function_decl_from_fn_name modules_path fn_name
          current_mod_name prog
      in
      match fn_decl with
      | Ast.Function_Decl.Decl_Kosu_Function e ->
          if Util.are_diff_lenght parameters e.parameters then
            Unmatched_Parameters_length
              {
                expected = e.parameters |> List.length;
                found = parameters |> List.length;
              }
            |> func_error |> raise;
          let new_map_generics =
            Hashtbl.of_seq
              (e.generics |> List.map (fun k -> (k, ())) |> List.to_seq)
          in
          let init_type_parameters =
            parameters
            |> List.map
                 (typeof ~generics_resolver:(Some new_map_generics) env
                    current_mod_name prog)
          in
          let hashtal = Hashtbl.create (e.generics |> List.length) in
          let () =
            match Asthelper.Function.does_need_generic_resolver e with
            | true ->
                if
                  Util.are_diff_lenght
                    (grc |> Option.value ~default:[])
                    e.generics
                then
                  Unmatched_Generics_Resolver_length
                    {
                      expected = e.generics |> List.length;
                      found = grc |> Option.value ~default:[] |> List.length;
                    }
                  |> func_error |> raise
                else ()
            | false -> ()
          in

          let () =
            match grc with
            | Some grc_safe ->
                List.combine e.generics grc_safe
                |> List.iteri (fun index (generic_name, field_ktype) ->
                       Hashtbl.add hashtal generic_name (index, field_ktype))
            | None -> ()
          in

          init_type_parameters |> List.combine e.parameters
          |> List.iter (fun ((_, para_type), init_type) ->
                 if
                   e
                   |> Asthelper.Function.is_type_compatible_hashgen hashtal
                        init_type para_type
                   |> not
                 then
                   Mismatched_Parameters_Type
                     { expected = para_type; found = init_type }
                   |> func_error |> raise);

          Asthelper.Function.to_return_ktype_hashtab hashtal e
      | Ast.Function_Decl.Decl_External external_func_decl -> (
          if external_func_decl.is_variadic then
            parameters
            |> List.map (typeof ~generics_resolver env current_mod_name prog)
            |> List.map (fun t ->
                   if
                     Asthelper.Program.is_c_type_from_ktype current_mod_name t
                       prog
                   then t
                   else
                     Ast.Error.Uncompatible_type_for_C_Function
                       { external_func_decl }
                     |> func_error |> raise)
            |> fun types ->
            if
              types |> List.length
              < (external_func_decl.fn_parameters |> List.length)
            then
              Unmatched_Parameters_length
                {
                  expected = external_func_decl.fn_parameters |> List.length;
                  found = parameters |> List.length;
                }
              |> func_error |> raise
            else
              types
              |> List.mapi (fun i t -> (i, t))
              |> List.partition (fun (i, _) ->
                     i < (external_func_decl.fn_parameters |> List.length))
              |> fun (lhs, _) ->
              lhs
              |> List.map (fun (_, t) -> t)
              |> List.combine external_func_decl.fn_parameters
              |> List.for_all (fun (para_type, init_type) ->
                     match
                       ( Asthelper.Program.is_c_type_from_ktype current_mod_name
                           para_type prog,
                         Asthelper.Program.is_c_type_from_ktype current_mod_name
                           init_type prog )
                     with
                     | true, true ->
                         if
                           not
                             (Ast.Type.are_compatible_type para_type init_type)
                         then
                           Uncompatible_type_Assign
                             { expected = para_type; found = init_type }
                           |> stmt_error |> raise
                         else true
                     | _ ->
                         Ast.Error.Uncompatible_type_for_C_Function
                           { external_func_decl }
                         |> func_error |> raise)
              |> fun b ->
              if b then external_func_decl.r_type
              else Unknow_Function_Error |> func_error |> raise
          else
            match
              Util.are_same_lenght external_func_decl.fn_parameters parameters
            with
            | false ->
                Unmatched_Parameters_length
                  {
                    expected = external_func_decl.fn_parameters |> List.length;
                    found = parameters |> List.length;
                  }
                |> func_error |> raise
            | true ->
                let mapped_type =
                  parameters
                  |> List.map
                       (typeof ~generics_resolver env current_mod_name prog)
                in
                let zipped =
                  List.combine external_func_decl.fn_parameters mapped_type
                in
                if
                  zipped
                  |> List.for_all (fun (para_type, init_type) ->
                         match
                           ( Asthelper.Program.is_c_type_from_ktype
                               current_mod_name para_type prog,
                             Asthelper.Program.is_c_type_from_ktype
                               current_mod_name init_type prog )
                         with
                         | true, true ->
                             if
                               not
                                 (Ast.Type.are_compatible_type para_type
                                    init_type)
                             then
                               Uncompatible_type_Assign
                                 { expected = para_type; found = init_type }
                               |> stmt_error |> raise
                             else true
                         | _ ->
                             Ast.Error.Uncompatible_type_for_C_Function
                               { external_func_decl }
                             |> func_error |> raise)
                then external_func_decl.r_type
                else Unknow_Function_Error |> func_error |> raise)
      | Ast.Function_Decl.Decl_Syscall syscall_decl -> (
          match Util.are_same_lenght syscall_decl.parameters parameters with
          | false ->
              Unmatched_Parameters_length
                {
                  expected = syscall_decl.parameters |> List.length;
                  found = parameters |> List.length;
                }
              |> func_error |> raise
          | true ->
              let mapped_type =
                parameters
                |> List.map
                     (typeof ~generics_resolver env current_mod_name prog)
              in
              let zipped = List.combine syscall_decl.parameters mapped_type in
              if
                zipped
                |> List.for_all (fun (para_type, init_type) ->
                       match
                         ( Asthelper.Program.is_c_type_from_ktype
                             current_mod_name para_type prog,
                           Asthelper.Program.is_c_type_from_ktype
                             current_mod_name init_type prog )
                       with
                       | true, true ->
                           if
                             not
                               (Ast.Type.are_compatible_type para_type init_type)
                           then
                             Uncompatible_type_Assign
                               { expected = para_type; found = init_type }
                             |> stmt_error |> raise
                           else true
                       | _ ->
                           Ast.Error.Uncompatible_type_for_Syscall
                             { syscall_decl }
                           |> func_error |> raise)
              then syscall_decl.return_type
              else Unknow_Function_Error |> func_error |> raise))
  | EBin_op (BAdd (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_add_operation l_type r_type prog with
      | `built_in_ptr_valid -> l_type
      | `invalid_add_pointer ->
          Invalid_pointer_arithmetic r_type |> operator_error |> raise
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.Add; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Add; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Add; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_add_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Add; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BMinus (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_minus_operation l_type r_type prog with
      | `built_in_ptr_valid -> l_type
      | `invalid_add_pointer ->
          Invalid_pointer_arithmetic r_type |> operator_error |> raise
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.Minus; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Minus; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Minus; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_minus_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Minus; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BMult (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_mult_operation l_type r_type prog with
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.Mult; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Mult; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Mult; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_mult_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Mult; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BDiv (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_div_operation l_type r_type prog with
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.Div; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Div; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Div; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_div_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Div; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BMod (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_mod_operation l_type r_type prog with
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.Modulo; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Modulo; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Modulo; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_mod_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.Modulo; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BBitwiseOr (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match
        Asthelper.Program.is_valid_bitwiseor_operation l_type r_type prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              bin_op = Ast.OperatorFunction.BitwiseOr;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.BitwiseOr; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.BitwiseOr; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_bitwiseor_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.BitwiseOr; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BBitwiseAnd (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match
        Asthelper.Program.is_valid_bitwiseand_operation l_type r_type prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              bin_op = Ast.OperatorFunction.BitwiseAnd;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.BitwiseAnd; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.BitwiseAnd; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_bitwiseand_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.BitwiseAnd; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BBitwiseXor (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match
        Asthelper.Program.is_valid_bitwisexor_operation l_type r_type prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              bin_op = Ast.OperatorFunction.BitwiseXor;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.BitwiseXor; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.BitwiseXor; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_bitwisexor_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.BitwiseXor; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BShiftLeft (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match
        Asthelper.Program.is_valid_shiftleft_operation l_type r_type prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              bin_op = Ast.OperatorFunction.ShiftLeft;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_shiftleft_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BShiftRight (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match
        Asthelper.Program.is_valid_shiftright_operation l_type r_type prog
      with
      | `diff_types ->
          Incompatible_Type
            {
              bin_op = Ast.OperatorFunction.ShiftLeft;
              lhs = l_type;
              rhs = r_type;
            }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_shiftright_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.ShiftLeft; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BAnd (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match (l_type, r_type) with
      | TBool, TBool -> TBool
      | _, _ -> Not_Boolean_operand_in_And |> operator_error |> raise)
  | EBin_op (BOr (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match (l_type, r_type) with
      | TBool, TBool -> TBool
      | _, _ -> Not_Boolean_operand_in_Or |> operator_error |> raise)
  | EBin_op (BEqual (lhs, rhs) | BDif (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_equal_operation l_type r_type prog with
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.Equal; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_equal_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BSup (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_sup_operation l_type r_type prog with
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.Sup; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Sup; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Sup; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_sup_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Sup; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BSupEq (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_supeq_operation l_type r_type prog with
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.SupEq; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.SupEq; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.SupEq; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_equal_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise
      | `no_sup_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Sup; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BInf (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_inf_operation l_type r_type prog with
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.Inf; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Inf; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Inf; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_inf_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Inf; ktype = l_type }
          |> operator_error |> raise)
  | EBin_op (BInfEq (lhs, rhs)) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      let r_type = typeof ~generics_resolver env current_mod_name prog rhs in
      match Asthelper.Program.is_valid_infeq_operation l_type r_type prog with
      | `diff_types ->
          Incompatible_Type
            { bin_op = Ast.OperatorFunction.InfEq; lhs = l_type; rhs = r_type }
          |> operator_error |> raise
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.InfEq; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> TBool
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.InfEq; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> TBool
      | `no_equal_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Equal; ktype = l_type }
          |> operator_error |> raise
      | `no_inf_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Inf; ktype = l_type }
          |> operator_error |> raise)
  | EUn_op (UNot lhs) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      match Asthelper.Program.is_valid_not_operation l_type prog with
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.Not; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.Not; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `no_not_for_built_in ->
          No_built_in_op { bin_op = Ast.OperatorFunction.Not; ktype = l_type }
          |> operator_error |> raise)
  | EUn_op (UMinus lhs) -> (
      let l_type = typeof ~generics_resolver env current_mod_name prog lhs in
      match Asthelper.Program.is_valid_uminus_operation l_type prog with
      | `no_function_found ->
          Operator_not_found
            { bin_op = Ast.OperatorFunction.UMinus; ktype = l_type }
          |> operator_error |> raise
      | `valid _ -> l_type
      | `to_many_declaration _ ->
          Too_many_operator_declaration
            { bin_op = Ast.OperatorFunction.UMinus; ktype = l_type }
          |> operator_error |> raise
      | `built_in_valid -> l_type
      | `invalid_unsigned_op size ->
          Invalid_Uminus_for_Unsigned_integer size |> operator_error |> raise
      | `no_uminus_for_built_in ->
          No_built_in_op
            { bin_op = Ast.OperatorFunction.UMinus; ktype = l_type }
          |> operator_error |> raise)
  | ESwitch { expression = expr; cases; wildcard_case } -> (
      let variant_cases = cases |> List.map (fun (v, _) -> v) |> List.flatten in
      let expr_type =
        typeof ~generics_resolver env current_mod_name prog expr
      in
      let module_path, name =
        expr_type |> Asthelper.module_path_of_ktype_opt |> function
        | None ->
            Not_enum_type_in_switch_Expression expr_type |> switch_error
            |> raise
        | Some s -> s
      in
      let enum_decl =
        match
          Asthelper.Program.find_type_decl_from_ktype
            ~ktype_def_path:module_path ~ktype_name:name
            ~current_module:current_mod_name prog
        with
        | Type_Decl.Decl_Enum e -> e
        | _ ->
            Not_enum_type_in_switch_Expression expr_type |> switch_error
            |> raise
      in

      let () =
        enum_decl.variants
        |> List.iter (fun (variant_name, _) ->
               if
                 Asthelper.Switch_case.is_cases_duplicated variant_name
                   variant_cases
               then
                 Ast.Error.Duplicated_case variant_name |> switch_error |> raise)
      in

      let generics_mapped =
        Ast.Type.extract_parametrics_ktype expr_type
        |> List.combine
             (enum_decl.generics
             |> List.map (fun name ->
                    TType_Identifier { module_path = ""; name }))
      in

      let open Asthelper.Enum in
      let open Asthelper.Switch_case in
      let () =
        if wildcard_case |> Option.is_none then
          match is_all_cases_handled variant_cases enum_decl with
          | Error e -> e |> switch_error |> raise
          | Ok _ -> ()
      in

      ( cases
      |> List.map (fun (sc_list, kb) ->
             let combine_binding_type =
               sc_list
               |> List.map (fun sc ->
                      let assoc_types =
                        extract_assoc_type_variant generics_mapped
                          (sc |> variant_name) enum_decl
                        |> Option.get
                      in
                      let assoc_binding = assoc_binding sc in
                      List.combine assoc_binding assoc_types)
             in
             match combine_binding_type with
             | [] -> failwith "Unreachable case: empty case"
             | ass_bin :: q ->
                 let new_context =
                   q
                   |> List.fold_left
                        (fun acc value ->
                          let reduced_binding =
                            reduce_binded_variable_combine value
                          in
                          if acc <> reduced_binding then
                            Incompatible_Binding (acc, reduced_binding)
                            |> switch_error |> raise
                          else acc)
                        (reduce_binded_variable_combine ass_bin)
                   |> List.map (fun (variable_name, ktype) ->
                          ( variable_name,
                            ({ is_const = true; ktype } : Env.variable_info) ))
                   |> List.map (fun (binding_name, var_info) ->
                          if env |> Env.is_identifier_exists binding_name then
                            Binded_identifier_already_exist binding_name
                            |> switch_error |> raise
                          else (binding_name, var_info))
                 in
                 typeof_kbody ~generics_resolver
                   (env |> Env.push_context new_context)
                   current_mod_name prog kb)
      |> fun l ->
        match wildcard_case with
        | None -> l
        | Some wild ->
            let wildcard_type =
              typeof_kbody ~generics_resolver env current_mod_name prog wild
            in
            wildcard_type :: l )
      |> function
      | [] -> failwith "unreachable case: empty kbody"
      | t :: q ->
          q
          |> List.fold_left
               (fun acc case_type ->
                 if not (Type.are_compatible_type acc case_type) then
                   Uncompatible_type { expected = acc; found = case_type }
                   |> ast_error |> raise
                 else Type.restrict_type acc case_type)
               t)

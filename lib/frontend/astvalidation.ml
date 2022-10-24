open Ast
open Position

module Error = struct
  type external_func_error =
    | Unit_parameter of external_func_decl
    | Not_C_compatible_type of external_func_decl * ktype location
    | Too_much_parameters of { external_func_decl: external_func_decl; limit : int; found : int }

  type syscall_error =
    | Syscall_Unit_parameter of syscall_decl
    | Syscall_Not_C_compatible_type of syscall_decl * ktype location
    | Syscall_Too_much_parameters of { syscall_decl: syscall_decl; limit : int; found : int }

  type struct_error =
    | SCyclic_Declaration of struct_decl
    | SDuplicated_field of {
      field: string location;
      struct_decl: struct_decl
      }

  type enum_error =
    | ECyclic_Declaration of enum_decl
    | EDuplicated_variant_name of {variant: string location; enum_decl: enum_decl}

  type operator_error =
    | Op_built_overload of ktype location
    | Op_binary_op_diff_type of {
      operator: string location;
      lhs: ktype;
      rhs: ktype
    }
    | Op_wrong_return_type_error of {
        op : string location;
        expected : ktype;
        found : ktype;
      }

  type function_error =
    | Wrong_signature_for_main of function_decl
    | Duplicated_parameters of {
      duplicatated_field: string location;
      function_decl: function_decl
    }
    | Function_Unit_parameter of {
      field: string location;
      function_decl: function_decl
      }

  type module_error =
    | Duplicate_function_declaration of {
      path: string;
      functions: Ast.Function_Decl.t list
    }
    | Duplicate_type_declaration of {
      path: string;
      types: Ast.Type_Decl.type_decl list
    }
    | Duplicate_const_declaration of {
      path: string;
      consts: Ast.const_decl list
    }
    | Duplicate_operator_declaration of {
        path: string;
        operators: Ast.operator_decl list;
    }
    | Main_no_kosu_function of [
      `syscall_decl of Ast.syscall_decl
      | `external_decl of Ast.external_func_decl
    ]

  type validation_error =
    | External_Func_Error of external_func_error
    | Syscall_Error of syscall_error
    | Struct_Error of struct_error
    | Enum_Error of enum_error
    | Operator_Error of operator_error
    | Function_Error of function_error
    | Module_Error of module_error
    (* | No_Type_decl_found of ktype location
    | Too_many_type_decl of ktype *)
    | Too_many_Main of (string * Ast.Function_Decl.t list) list
    | Ast_Error of Ast.Error.ast_error

  let external_func_error e = External_Func_Error e
  let syscall_error e = Syscall_Error e
  let struct_error e = Struct_Error e
  let enum_error e = Enum_Error e
  let operator_error e = Operator_Error e
  let function_error e = Function_Error e
  let module_error e = Module_Error e

  exception Validation_error of string*validation_error
end

module Help = struct
  (* let is_ktype_exist generics current_module program ktype =
    try
      let _ =
        Asthelper.Program.find_type_decl_from_true_ktype ktype current_module
          program
      in
      Ok ()
    with
    | Util.Occurence.No_Occurence ->
        Error.No_Type_decl_found ktype |> Result.error
    | Util.Occurence.Too_Many_Occurence ->
        Error.Too_many_type_decl ktype |> Result.error
    | Ast.Error.Ast_error e -> Error.Ast_Error e |> Result.error *)

  let is_ktype_exist_from_ktype generics current_module program ktype =
    try
      let _ =
        Asthelper.Program.find_type_decl_from_true_ktype ~generics ktype current_module
          program 
      in
      Ok ()
    with
    | Ast.Error.Ast_error e -> Error.Ast_Error e |> Result.error

  let rec does_ktype_contains_type_decl current_module program ktype
      ktype_type_decl_origin already_visited type_decl_to_check =
    (* already_visited |> List.map (Type_Decl.string_of_type_decl) |> List.iter (print_endline); *)
    match ktype with
    | TType_Identifier { module_path = ktype_def_path; name = ktype_name } -> (
        match
          Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path:ktype_def_path
            ~ktype_name:ktype_name ~current_module program
        with
        | Ok( Ast.Type_Decl.Decl_Struct struct_decl) ->
            does_contains_type_decl_struct current_module program struct_decl
              already_visited type_decl_to_check
        | Ok (Ast.Type_Decl.Decl_Enum enum_decl) ->
            does_contains_type_decl_enum current_module program enum_decl
              already_visited type_decl_to_check
        | Error e -> e |> Ast.Error.ast_error |> raise
      )
    | TParametric_identifier
        { module_path = ktype_def_path; parametrics_type; name = ktype_name }
      -> (
        let type_decl_found =
          Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path:ktype_def_path
            ~ktype_name:ktype_name ~current_module program
        in
        match type_decl_found with
        | Error e -> e |> Ast.Error.ast_error |> raise
        | Ok (Ast.Type_Decl.Decl_Struct struct_decl) ->
            let new_struct =
              Asthelper.Struct.bind_struct_decl (parametrics_type |> List.map Position.value)
                (ktype_type_decl_origin |> Asthelper.Type_Decl.generics)
                struct_decl
            in
            
            does_contains_type_decl_struct current_module program new_struct
              already_visited type_decl_to_check
        | Ok (Ast.Type_Decl.Decl_Enum enum_decl) ->
            let new_enum_decl =
              Asthelper.Enum.bind_enum_decl (parametrics_type |> List.map Position.value)
                (ktype_type_decl_origin |> Asthelper.Type_Decl.generics)
                enum_decl
            in
            does_contains_type_decl_enum current_module program new_enum_decl
              already_visited type_decl_to_check)
    | TTuple kts ->
        kts
        |> List.for_all (fun kt ->
               does_ktype_contains_type_decl current_module program kt.v
                 ktype_type_decl_origin already_visited type_decl_to_check)
    | _ -> false

  and does_contains_type_decl_struct current_module program struct_decl
      already_visited type_decl_to_check =
    let open Asthelper in
    if
      already_visited
      |> List.exists (fun ty_decl ->
             Type_Decl.are_same_type_decl ty_decl
               (Ast.Type_Decl.Decl_Struct struct_decl))
    then false
    else
      Ast.Type_Decl.decl_struct struct_decl = type_decl_to_check
      || struct_decl.fields
         |> List.exists (fun (_, kt) ->
                match kt.v with
                | TType_Identifier _ when Struct.is_type_generic kt.v struct_decl
                  ->
                    false
                | TType_Identifier
                    { module_path = ktype_def_path; name = ktype_name } ->
                      let t_decl =
                        match Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module
                          program with
                        | Error e -> e |> Ast.Error.ast_error |> raise
                        | Ok type_decl -> type_decl
                      in
                    t_decl = type_decl_to_check
                    || t_decl = Ast.Type_Decl.decl_struct struct_decl
                    || does_ktype_contains_type_decl current_module program kt.v
                         (Ast.Type_Decl.Decl_Struct struct_decl)
                         (Ast.Type_Decl.Decl_Struct struct_decl
                        :: already_visited)
                         type_decl_to_check
                | TParametric_identifier
                    {
                      module_path = ktype_def_path;
                      parametrics_type;
                      name = ktype_name;
                    } ->
                    let t_decl =
                      match Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module
                        program with
                      | Error e -> e |> Ast.Error.ast_error |> raise
                      | Ok type_decl -> type_decl
                    in
                    t_decl = type_decl_to_check
                    || parametrics_type
                       |> List.exists (fun ikt ->
                              if
                                struct_decl
                                |> Struct.is_ktype_generic_level_zero ikt.v
                              then false
                              else
                                does_ktype_contains_type_decl current_module
                                  program ikt.v
                                  (Ast.Type_Decl.Decl_Struct struct_decl)
                                  (Ast.Type_Decl.Decl_Struct struct_decl
                                 :: already_visited)
                                  type_decl_to_check)
                    || does_ktype_contains_type_decl current_module program kt.v
                         (Ast.Type_Decl.Decl_Struct struct_decl)
                         (Ast.Type_Decl.Decl_Struct struct_decl
                        :: already_visited)
                         type_decl_to_check
                | _ ->
                    does_ktype_contains_type_decl current_module program kt.v
                      (Ast.Type_Decl.Decl_Struct struct_decl) already_visited
                      type_decl_to_check)

  and does_contains_type_decl_enum current_module program
      (enum_decl : Ast.enum_decl) already_visited type_decl_to_check =
    let open Asthelper in
    if
      already_visited
      |> List.exists (fun ty_decl ->
             Type_Decl.are_same_type_decl ty_decl
               (Ast.Type_Decl.Decl_Enum enum_decl))
    then false
    else
      Ast.Type_Decl.decl_enum enum_decl = type_decl_to_check
      || enum_decl.variants
         |> List.exists (fun (_, kts) ->
                kts
                |> List.exists (fun kt ->
                  let kt = kt.v in
                       match kt with
                       | TType_Identifier _
                         when Enum.is_type_generic kt enum_decl ->
                           false
                       | TParametric_identifier
                           {
                             module_path = ktype_def_path;
                             parametrics_type;
                             name = ktype_name;
                           } ->
                            let t_decl =
                              match Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module
                                program with
                              | Error e -> e |> Ast.Error.ast_error |> raise
                              | Ok type_decl -> type_decl
                            in
                           t_decl = type_decl_to_check
                           || t_decl = Ast.Type_Decl.decl_enum enum_decl
                           || parametrics_type
                              |> List.exists (fun ikt ->
                                     if
                                       enum_decl
                                       |> Enum.is_ktype_generic_level_zero ikt.v
                                     then false
                                     else
                                       does_ktype_contains_type_decl
                                         current_module program ikt.v
                                         (Ast.Type_Decl.Decl_Enum enum_decl)
                                         (Ast.Type_Decl.Decl_Enum enum_decl
                                        :: already_visited)
                                         type_decl_to_check)
                           || does_ktype_contains_type_decl current_module
                                program kt (Ast.Type_Decl.Decl_Enum enum_decl)
                                (Ast.Type_Decl.Decl_Enum enum_decl
                               :: already_visited)
                                type_decl_to_check
                       | TType_Identifier
                           { module_path = ktype_def_path; name = ktype_name }
                         ->
                          let t_decl =
                            match Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module
                              program with
                            | Error e -> e |> Ast.Error.ast_error |> raise
                            | Ok type_decl -> type_decl
                          in
                           t_decl = type_decl_to_check
                           || t_decl = Ast.Type_Decl.decl_enum enum_decl
                           || does_ktype_contains_type_decl current_module
                                program kt (Ast.Type_Decl.Decl_Enum enum_decl)
                                (Ast.Type_Decl.Decl_Enum enum_decl
                               :: already_visited)
                                type_decl_to_check
                       | _ ->
                           does_ktype_contains_type_decl current_module program
                             kt (Ast.Type_Decl.Decl_Enum enum_decl)
                             already_visited type_decl_to_check))

  and is_cyclic_struct current_module program struct_decl =
    struct_decl.fields
    |> List.exists (fun (_, kt) ->
      let kt = kt.v in
           if Asthelper.Struct.is_ktype_generic_level_zero kt struct_decl then
             false
           else
             does_ktype_contains_type_decl current_module program kt
               (Ast.Type_Decl.decl_struct struct_decl)
               []
               (Ast.Type_Decl.decl_struct struct_decl))

  and is_cyclic_enum current_module program enum_decl =
    enum_decl.variants
    |> List.map (fun (_, kts) -> kts)
    |> List.flatten
    |> List.exists (fun kt ->
      let kt = kt.v in
           if Asthelper.Enum.is_ktype_generic_level_zero kt enum_decl then false
           else
             does_ktype_contains_type_decl current_module program kt
               (Ast.Type_Decl.decl_enum enum_decl)
               []
               (Ast.Type_Decl.decl_enum enum_decl))

  let program_remove_implicit_type_path program =
    program
    |> List.map (fun {filename; module_path = { path; _module = Mod _module }} ->
           let new_module =
             _module
             |> List.map (fun node ->
                    Asthelper.AstModif.module_node_remove_implicit_type_path
                      path node)
           in
           {filename; module_path =  { path; _module = Mod new_module } })
end

module ValidateExternalFunction = struct
  let does_parameters_contains_unit (external_func_decl : external_func_decl) =
    if external_func_decl.fn_parameters |> List.map Position.value |> List.mem TUnit then
      Error.Unit_parameter external_func_decl |> Error.external_func_error
      |> Result.error
    else Ok ()

  let does_contains_not_c_compatible_type program current_module
      (external_func_decl : external_func_decl) =
    external_func_decl.fn_parameters
    |> List.filter_map (fun kt ->
           try
             if
               Asthelper.Program.is_c_type_from_ktype current_module kt.v program
               |> not
             then
               Error.External_Func_Error
                 (Error.Not_C_compatible_type (external_func_decl, kt))
               |> Option.some
             else None
           with
           | Ast.Error.Ast_error e -> Error.Ast_Error e |> Option.some)
    |> function
    | [] -> Ok ()
    | t :: _ -> t |> Result.error

  let does_contains_too_much_parameters
      (external_func_decl : external_func_decl) =
    let length = external_func_decl.fn_parameters |> List.length in
    if length > 15 then
      Error.Too_much_parameters { external_func_decl; limit = 15; found = length } |> Result.error
    else Ok ()

  let is_valid_external_function_declaration (program)
      (current_module_name : string) (external_func_decl : external_func_decl) =
    let ( >>= ) = Result.bind in
    ( does_parameters_contains_unit external_func_decl >>= fun () ->
      does_contains_not_c_compatible_type program current_module_name
        external_func_decl )
    >>= fun () -> does_parameters_contains_unit external_func_decl
end

module ValidateSyscall = struct
  let does_parameters_contains_unit (syscall_decl : syscall_decl) =
    if syscall_decl.parameters |> List.map Position.value |> List.mem TUnit then
      Error.Syscall_Unit_parameter syscall_decl |> Error.syscall_error
      |> Result.error
    else Ok ()

  let does_contains_not_c_compatible_type program current_module
      (syscall_decl : syscall_decl) =
    syscall_decl.parameters
    |> List.filter_map (fun kt ->
           try
             if
               Asthelper.Program.is_c_type_from_ktype current_module kt.v program
               |> not
             then
               Error.Syscall_Not_C_compatible_type (syscall_decl, kt)
               |> Error.syscall_error |> Option.some
             else None
           with
           | Ast.Error.Ast_error e -> Error.Ast_Error e |> Option.some)
    |> function
    | [] -> Ok ()
    | t :: _ -> t |> Result.error

  let does_contains_too_much_parameters (syscall_decl : syscall_decl) =
    let length = syscall_decl.parameters |> List.length in
    if length > 15 then
      Error.Syscall_Too_much_parameters { syscall_decl; limit = 15; found = length }
      |> Result.error
    else Ok ()

  let is_valid_syscall_declaration (program)
      (current_module_name : string) (syscall_decl : syscall_decl) =
    let ( >>= ) = Result.bind in
    ( does_parameters_contains_unit syscall_decl >>= fun () ->
      does_contains_not_c_compatible_type program current_module_name
        syscall_decl )
    >>= fun () -> does_parameters_contains_unit syscall_decl
end

module ValidateStruct = struct
  let is_all_type_exist current_module program struct_decl =
    struct_decl.fields
    |> List.find_map (fun (_, kt) ->
           if Asthelper.Struct.is_ktype_generic_level_zero kt.v struct_decl then
             None
           else
             match Help.is_ktype_exist_from_ktype struct_decl.generics current_module program kt.v with
             | Ok () -> None
             | Error e -> Some e)

  let is_field_duplicate struct_decl =
    struct_decl.fields
    |> List.map (fun (field, _) -> field.v)
    |> Util.ListHelper.duplicate |> (
      function
      | [] -> None
      | t::_ -> Some t
    )

  let is_valid_struct_decl program
      (current_module_name : string) (struct_decl : struct_decl) =
    let ( >>= ) = Result.bind in
    ( (is_all_type_exist current_module_name program struct_decl |> function
       | None -> Ok ()
       | Some e -> Error e)
    >>= fun () ->
      if Help.is_cyclic_struct current_module_name program struct_decl then
        Error.SCyclic_Declaration struct_decl |> Error.struct_error
        |> Result.error
      else Ok () )
    >>= fun () ->
      
    match is_field_duplicate struct_decl with
    | None -> Ok ()
    | Some field -> 
      let located_field = struct_decl.fields |> List.find_all (fun (fie, _) -> fie.v = field ) |> List.rev |> List.hd |> fun (f, _) -> f  in
      Error.SDuplicated_field {field = located_field; struct_decl} |> Error.struct_error |> Result.error
end

module ValidateEnum = struct
  let is_all_type_exist current_module program (enum_decl : enum_decl) =
    enum_decl.variants
    |> List.map (fun (_, kts) -> kts)
    |> List.flatten
    |> List.find_map (fun kt ->
           if Asthelper.Enum.is_ktype_generic_level_zero kt.v enum_decl then None
           else
             match Help.is_ktype_exist_from_ktype enum_decl.generics current_module program kt.v with
             | Ok () -> None
             | Error e -> Some e)

  let is_variant_duplicate enum_decl =
    enum_decl.variants
    |> List.map (fun (s, _) -> s.v)
    |> Util.ListHelper.duplicate |> (
      function
      | [] -> None
      | t::_ -> Some t
    )

  let is_valid_enum_decl program (current_module_name : string)
      (enum_decl : enum_decl) =
    let ( >>= ) = Result.bind in
    ( (is_all_type_exist current_module_name program enum_decl |> function
       | None -> Ok ()
       | Some e -> Error e)
    >>= fun () ->
      if Help.is_cyclic_enum current_module_name program enum_decl then
        Error.ECyclic_Declaration enum_decl |> Error.enum_error |> Result.error
      else Ok () )
    >>= fun () ->
    match is_variant_duplicate enum_decl with
    | None -> Ok ()
    | Some vari -> 
      let located_variant = enum_decl.variants |>  List.find_all (fun (variant, _) -> variant.v = vari ) |> List.rev |> List.hd |> fst in
      Error.EDuplicated_variant_name {variant = located_variant; enum_decl} |> Error.enum_error |> Result.error
end

module ValidateFunction_Decl = struct
  let is_main_function function_decl = function_decl.fn_name.v = "main"

  let is_valid_main_sig function_decl =
    function_decl.fn_name.v = "main"
    && function_decl.return_type.v = TInteger (Signed, I32)
    && function_decl.parameters = []
    && function_decl.generics = []

  let check_parameters_duplicate (function_decl : function_decl) = 
    function_decl.parameters 
      |> List.map (fun (field, _ ) -> field.v)
      |> Util.ListHelper.duplicate 
      |> (function
      | [] -> Ok ()
      | t::_ -> let duplicate = function_decl.parameters |> List.find_all (fun (field, _) -> field.v = t) |> List.rev |> List.hd |> fst in
      Error.Duplicated_parameters {duplicatated_field = duplicate; function_decl} |> Error.function_error |> Result.error
      )
    (* if
      function_decl.parameters
      |> List.map (fun (field, _) -> field)
      |> Util.ListHelper.duplicate |> Util.are_diff_lenght []
    then
      Error.Duplicated_parameters function_decl |> Error.function_error
      |> Result.error
    else Ok () *)

  let check_unit_parameters (function_decl : function_decl) =
    function_decl.parameters |> List.find_map (fun (field, kt) -> if kt.v = TUnit then Some field else None)
    |> Option.fold ~none:(Ok ()) ~some:(fun field ->
       Error.Function_Unit_parameter {field; function_decl} 
       |> Error.function_error
       |> Result.error
    )

  let is_all_type_exist current_module program function_decl =
    function_decl.parameters
    |> List.find_map (fun (_, kt) ->
           if Asthelper.Function.is_ktype_generic_level_zero kt.v function_decl
           then None
           else
             match Help.is_ktype_exist_from_ktype function_decl.generics current_module program kt.v with
             | Ok () -> None
             | Error e -> Some e)

  let check_main_sig function_decl =
    if not (is_main_function function_decl) then Ok ()
    else if is_valid_main_sig function_decl then Ok ()
    else Wrong_signature_for_main function_decl |> Error.function_error |> Result.error

  let check_kbody current_module program (function_decl : function_decl) =
    let hashtbl =
      Hashtbl.of_seq
        (function_decl.generics |> List.map (fun k -> (k, ())) |> List.to_seq)
    in
    try
      let _ =
        (* Printf.printf "\n\nFunction \"%s\" body\n" (function_decl.fn_name.v); *)
        Typecheck.typeof_kbody ~generics_resolver:hashtbl
          (function_decl.parameters
          |> List.fold_left
               (fun acc_env para ->
                 acc_env |> Env.add_fn_parameters ~const:false (para |> Position.assocs_value))
               Env.create_empty_env)
          current_module program ~return_type:(Some function_decl.return_type.v)
          function_decl.body
      in
      Ok ()
    with Ast.Error.Ast_error e -> Error.Ast_Error e |> Result.error

  let check_function program current_module function_decl =
    let ( >>= ) = Result.bind in

    is_all_type_exist current_module program function_decl
    |> Option.fold ~none:(Ok ()) ~some:Result.error
    >>= fun () ->
    check_main_sig function_decl >>= fun () ->
    check_unit_parameters function_decl >>= fun () ->
    check_parameters_duplicate function_decl >>= fun () ->
    check_kbody current_module program function_decl
end

module ValidateOperator_Decl = struct
  let is_all_type_exist current_module program (operator_decl : operator_decl) =
    let ( >>= ) = Result.bind in

    let first_kt =
      Asthelper.ParserOperator.first_parameter_ktype operator_decl
    in
    Help.is_ktype_exist_from_ktype [] current_module program first_kt.v >>= fun () ->
    match Asthelper.ParserOperator.second_parameter_ktype operator_decl with
    | None -> Ok ()
    | Some kt -> Help.is_ktype_exist_from_ktype [] current_module program kt.v

  let check_parameters operator_decl =
    let open Ast.Type in
    match operator_decl with
    | Unary _ -> Ok ()
    | Binary binary ->
        let (_, kt1), (_, kt2) = binary.fields in
        if  kt1.v === kt2.v then
          if kt1.v |> Ast.Type.is_builtin_type then
            Error.Op_built_overload kt1 |> Error.operator_error |> Result.error
          else Ok ()
        else
          Error.Op_binary_op_diff_type {
            operator = operator_decl |> Asthelper.ParserOperator.operator;
            lhs = kt1.v;
            rhs = kt2.v
          }
          |> Error.operator_error |> Result.error

  let check_return_ktype operator_decl =
    let open Ast.Type in
    match operator_decl with
    | Unary u ->
        let expected =
          Asthelper.ParserOperator.expected_unary_return_type
            (Asthelper.ParserOperator.first_parameter_ktype operator_decl).v
            u.op.v
        in
        if u.return_type.v === expected then Ok ()
        else
          Op_wrong_return_type_error
            { op = operator_decl |> Asthelper.ParserOperator.operator; expected; found = u.return_type.v }
          |> Error.operator_error |> Result.error
    | Binary b ->
        let expected =
          Asthelper.ParserOperator.expected_binary_return_type
            (Asthelper.ParserOperator.first_parameter_ktype operator_decl).v
            b.op.v
        in
        if b.return_type.v === expected then Ok ()
        else
          Op_wrong_return_type_error
            { op = operator_decl |> Asthelper.ParserOperator.operator; expected; found = b.return_type.v }
          |> Error.operator_error |> Result.error

  let check_kbody current_module program operator_decl =
    let fields =
      match operator_decl with
      | Unary u -> [ u.field ]
      | Binary b ->
          let t1, t2 = b.fields in
          [ t1; t2 ]
    in
    try
      let _ =
        Typecheck.typeof_kbody ~generics_resolver:(Hashtbl.create 0)
          (fields
          |> List.fold_left
               (fun acc_env para ->
                 acc_env |> Env.add_fn_parameters ~const:false (para |> Position.assocs_value))
               Env.create_empty_env)
          current_module program
          ~return_type:
            (Some (Asthelper.ParserOperator.return_ktype operator_decl).v)
          (Asthelper.ParserOperator.kbody operator_decl)
      in
      Ok ()
    with Ast.Error.Ast_error e -> Error.Ast_Error e |> Result.error

  let is_valid_operator_decl current_module program operator_decl =
    let ( >>= ) = Result.bind in
    ( ( is_all_type_exist current_module program operator_decl >>= fun () ->
        check_parameters operator_decl )
    >>= fun () -> check_return_ktype operator_decl )
    >>= fun () -> check_kbody current_module program operator_decl
end

module ValidateModule = struct
  let check_duplicate_function { path; _module } =
    _module |> Asthelper.Module.retrieve_functions_decl
    |> Util.ListHelper.duplicated (fun lfn rfn -> 
      lfn |> Ast.Function_Decl.calling_name |> value = 
      (rfn |> Ast.Function_Decl.calling_name |> value)
    )
    |> (function
    | [] -> Ok ()
    | t :: _ -> Error.Duplicate_function_declaration {path; functions = t} |> Error.module_error |> Result.error
    )
    (* |> List.map Ast.Function_Decl.calling_name
    |> Util.ListHelper.duplicate
    |> function
    | [] -> Ok ()
    | t :: _ ->
        Error.Duplicate_function_name t.v |> Error.module_error |> Result.error *)

  let check_duplicate_operator { path; _module } = let open Ast.Type in
    _module |> Asthelper.Module.retrieve_operator_decl
    |> Util.ListHelper.duplicated (fun lop rop -> 
      match lop, rop with
      | Binary l, Binary r -> begin 
      l.op |> Position.value = (r.op |> Position.value)
      && (l.fields |> fst |> snd |> Position.value) === (r.fields |> fst |> snd |> Position.value)
      && (l.fields |> snd |> snd |> Position.value) === (r.fields |> snd |> snd |> Position.value)
      && (l.return_type |> Position.value) === (r.return_type |> Position.value)
      end
      | Unary l, Unary r -> begin 
        l.op |> Position.value = (r.op |> Position.value)
        && (l.field |> snd |> Position.value) === (r.field |> snd |> Position.value)
        && (l.return_type |> Position.value) === (r.return_type |> Position.value)
      end
      | _, _ -> false
      )
      |> function
    | [] -> Ok ()
    | t :: _ ->
        Error.Duplicate_operator_declaration {path; operators = t} |> Error.module_error |> Result.error

  let check_main_signature { path; _module } =
    let ( >>= ) = Result.bind in
    let function_decls = _module |> Asthelper.Module.retrieve_functions_decl
    |> List.filter (fun decl ->
           decl |> Ast.Function_Decl.calling_name |> Position.value |> ( = ) "main") in
    function_decls |> List.fold_left
         (fun acc value ->
           acc >>= fun found ->
           match value with
           | Ast.Function_Decl.Decl_External external_decl ->
               Main_no_kosu_function (`external_decl external_decl)|> Error.module_error |> Result.error
          | Ast.Function_Decl.Decl_Syscall syscall_decl -> 
              Main_no_kosu_function (`syscall_decl syscall_decl) |> Error.module_error |> Result.error 
           | Ast.Function_Decl.Decl_Kosu_Function function_decl -> (
               let is_main_valid_sig =
                 ValidateFunction_Decl.is_valid_main_sig function_decl
               in
               match found with
               | None ->
                   if is_main_valid_sig then () |> Option.some |> Result.ok
                   else
                     Wrong_signature_for_main function_decl |> Error.function_error
                     |> Result.error
               | Some _ ->
                   Duplicate_function_declaration { path; functions = function_decls}
                   |> Error.module_error |> Result.error))
         (Ok None)
    |> Result.map (fun _ -> ())

  let check_duplicate_type { path; _module } =
    let type_decls = _module |> Asthelper.Module.retrieve_type_decl in
    type_decls
    |> Util.ListHelper.duplicated (fun lhs rhs -> 
      lhs |> Asthelper.Type_Decl.type_name |> Position.value = 
      (rhs |> Asthelper.Type_Decl.type_name |> Position.value )
      )
    |> function
    | [] -> Ok ()
    | t :: _ ->
      Error.Duplicate_type_declaration {path; types = t} |> Error.module_error |> Result.error

  let check_duplicate_const_name { path; _module } =
    let consts_decl = _module |> Asthelper.Module.retrieve_const_decl in
    consts_decl |> Util.ListHelper.duplicated (fun lconst rconst -> lconst.const_name.v = rconst.const_name.v)
    |> function
    | [] -> Ok ()
    | t :: _ ->
      Error.Duplicate_const_declaration {path; consts = t} |> Error.module_error |> Result.error

  let check_validate_module _module =
    let ( >>= ) = Result.bind in
    check_duplicate_const_name _module >>= fun () ->
    check_duplicate_function _module >>= fun () ->
    check_duplicate_operator _module >>= fun () ->
    check_main_signature _module >>= fun () -> check_duplicate_type _module
end

module Validate_Program = struct
  let check_main_in_program (program : program) =
    let filtered_function_decl = program
    |> Asthelper.Program.to_module_path_list
    |> List.map (fun { path; _module } ->
           path, (_module 
           |> Asthelper.Module.retrieve_functions_decl
           |> List.filter (fun fn -> fn |> Ast.Function_Decl.calling_name |> Position.value = "main")) )

    |> List.filter (fun (_path, fn_decls) -> fn_decls |> List.length |> ( <= ) 1) in
    let count = filtered_function_decl 
    |> List.fold_left (fun acc (_, fn_decls) -> acc + (List.length fn_decls)) 0 in
    if count < 2 then Ok ()
    else
      Error.Too_many_Main filtered_function_decl |> Result.error
end

let validate_module_node (program) (current_module_name : string)
    (node : Ast.module_node) =
  match node with
  | NConst _ | NSigFun _ -> Ok ()
  | NExternFunc external_func_decl ->
      ValidateExternalFunction.is_valid_external_function_declaration program
        current_module_name external_func_decl
  | NSyscall syscall_decl ->
      ValidateSyscall.is_valid_syscall_declaration program current_module_name
        syscall_decl
  | NStruct struct_decl ->
      ValidateStruct.is_valid_struct_decl program current_module_name
        struct_decl
  | NEnum enum_decl ->
      ValidateEnum.is_valid_enum_decl program current_module_name enum_decl
  | NFunction function_decl ->
      ValidateFunction_Decl.check_function program current_module_name
        function_decl
  | NOperator operator_decl ->
      ValidateOperator_Decl.is_valid_operator_decl current_module_name program
        operator_decl

let validate_module (program)
    ( {filename; module_path = { path; _module = Mod _module } as package} ) =
  let ( >>= ) = Result.bind in
  filename, ValidateModule.check_validate_module package >>= fun () ->
  _module
  |> List.fold_left
       (fun acc value ->
         if acc |> Result.is_error then acc
         else validate_module_node program path value)
       (Ok ())

let valide_program (program : program) =
  (* let ( >>= ) = Result.bind in *)
  match program |> Validate_Program.check_main_in_program with
  | Error e -> "", Error e
  | Ok () ->
  let remove_program = program |> Help.program_remove_implicit_type_path in
  remove_program
  |> List.fold_left
       (fun acc named_module_path ->
        let _f_name, result = acc in
         if result |> Result.is_error then acc 
        else validate_module (remove_program |> Asthelper.Program.to_module_path_list) named_module_path
        ) ("", Ok ())

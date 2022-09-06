open Ast
open Asthelper

module Error = struct
  type external_func_error =
    | Unit_parameter of external_func_decl
    | Not_C_compatible_type of external_func_decl * ktype
    | Too_much_parameters of { limit : int; found : int }

  type syscall_error =
    | Syscall_Unit_parameter of syscall_decl
    | Syscall_Not_C_compatible_type of syscall_decl * ktype
    | Syscall_Too_much_parameters of { limit : int; found : int }

  type struct_error =
    | Unknown_type_for_field of string * ktype
    | SCyclic_Declaration of struct_decl
    | SDuplicated_field of struct_decl

  type enum_error =
    | ECyclic_Declaration of enum_decl
    | EDuplicated_variant_name of enum_decl

  type operator_error =
    | Op_built_overload of ktype
    | Op_binary_op_diff_type of (ktype * ktype)
    | Op_wrong_return_type_error of {
        op :
          [ `binary of Ast.parser_binary_op | `unary of Ast.parser_unary_op ];
        expected : ktype;
        found : ktype;
      }

  type function_error = 
    | Wrong_signature_for_main
    | Duplicated_parameters of function_decl
    | Function_Unit_parameter of function_decl

  type module_error =
    | Duplicate_function_name of string
    | Duplicate_type_name of string
    | Duplicate_const_name of string
    | Duplicate_Operator of
        [ `binary of Ast.parser_binary_op | `unary of Ast.parser_unary_op ]
    | Main_no_kosu_function

  type validation_error =
    | External_Func_Error of external_func_error
    | Syscall_Error of syscall_error
    | Struct_Error of struct_error
    | Enum_Error of enum_error
    | Operator_Error of operator_error
    | Function_Error of function_error
    | Module_Error of module_error
    | No_Type_decl_found of ktype
    | Too_many_type_decl of ktype
    | Too_many_Main of int
    | Ast_Error of Ast.Error.ast_error

  let external_func_error e = External_Func_Error e
  let syscall_error e = Syscall_Error e
  let struct_error e = Struct_Error e
  let enum_error e = Enum_Error e
  let operator_error e = Operator_Error e
  let function_error e = Function_Error e
  let module_error e = Module_Error e

  exception Validation_error of validation_error

  let string_of_external_func_error =
    let open Printf in
    function
    | Unit_parameter external_func_decl ->
        sprintf "Unit parameter in %s" external_func_decl.sig_name
    | Not_C_compatible_type (external_func_decl, ktype) ->
        sprintf "Not_C_compatible_type in %s -- %s --"
          external_func_decl.sig_name (string_of_ktype ktype)
    | Too_much_parameters record ->
        sprintf "Too_much_parameters -- limit: %d, found: %d --" record.limit
          record.found

  let string_of_sycall_error =
    let open Printf in
    function
    | Syscall_Unit_parameter syscall_decl ->
        sprintf "Unit parameter in %s" syscall_decl.syscall_name
    | Syscall_Not_C_compatible_type (syscall_decl, ktype) ->
        sprintf "Not_C_compatible_type in %s -- %s --" syscall_decl.syscall_name
          (string_of_ktype ktype)
    | Syscall_Too_much_parameters record ->
        sprintf "Too_much_parameters -- limit: %d, found: %d --" record.limit
          record.found

  let string_of_struct_error =
    let open Printf in
    function
    | Unknown_type_for_field (field, ktype) ->
        sprintf "Unknown_type_for_field : %s -> %s" field
          (string_of_ktype ktype)
    | SCyclic_Declaration struct_decl ->
        sprintf "Struct Cyclic_Declaration for %s" struct_decl.struct_name
    | SDuplicated_field struct_decl ->
        sprintf "Struct Duplicated_field for %s" struct_decl.struct_name

  let string_of_enum_error =
    let open Printf in
    function
    | ECyclic_Declaration enum_decl ->
        sprintf " Enum_Cyclic_Declaration for %s" enum_decl.enum_name
    | EDuplicated_variant_name enum_decl ->
        sprintf "Enum_Duplicated_variant_name for %s" enum_decl.enum_name

  let string_of_operator_error =
    let open Printf in
    function
    | Op_built_overload kt ->
        sprintf "Try to overload builtin type : %s" (string_of_ktype kt)
    | Op_binary_op_diff_type (lkt, rkt) ->
        sprintf "Binary operator different type : -- %s | %s --"
          (string_of_ktype lkt) (string_of_ktype rkt)
    | Op_wrong_return_type_error { op; expected; found } ->
        sprintf "Op_wrong_return_type_error for ( %s ) %s"
          (Asthelper.ParserOperator.string_of_parser_operator op)
          (Asthelper.string_of_expected_found
             (`ktype
               ( Asthelper.ParserOperator.expected_op_return_type expected op,
                 found )))

  let string_of_function_error =
    let open Printf in
    function 
    | Wrong_signature_for_main -> sprintf "Wrong_signature_for_main"
    | Duplicated_parameters function_decl -> sprintf "Duplicate name parameters for %s" function_decl.fn_name
    | Function_Unit_parameter function_decl -> sprintf "Function_Unit_parameter : %s" function_decl.fn_name

  let string_of_module_error =
    let open Printf in
    function
    | Duplicate_function_name name ->
        sprintf "Duplicate_function_name : %s" name
    | Duplicate_type_name name -> sprintf "Duplicate_type_name : %s" name
    | Duplicate_const_name name -> sprintf "Duplicate_const_name : %s" name
    | Duplicate_Operator op ->
        sprintf "Duplicate_Operator for -- %s --"
          (Asthelper.ParserOperator.string_of_parser_operator op)
    | Main_no_kosu_function -> "Main function should be a kosu function"

  let string_of_validation_error =
    let open Printf in
    function
    | External_Func_Error e -> string_of_external_func_error e
    | Syscall_Error e -> string_of_sycall_error e
    | Struct_Error e -> string_of_struct_error e
    | Enum_Error e -> string_of_enum_error e
    | Operator_Error e -> string_of_operator_error e
    | Function_Error e -> string_of_function_error e
    | Module_Error e -> string_of_module_error e
    | Too_many_Main count ->
        sprintf
          "Too many main found -- count : %d --\n\
           There should be at most 1 main across the program" count
    | No_Type_decl_found kt ->
        sprintf "No_Type_decl_found : %s" (string_of_ktype kt)
    | Too_many_type_decl kt ->
        sprintf "Too_many_type_decl : %s" (string_of_ktype kt)
    | Ast_Error e -> Asthelper.string_of_ast_error e
end

module Help = struct
  let is_ktype_exist current_module program ktype =
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
    | Ast.Error.Ast_error e -> Error.Ast_Error e |> Result.error

  let is_ktype_exist_from_ktype current_module program ktype =
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
    | Ast.Error.Ast_error e -> Error.Ast_Error e |> Result.error

  let rec does_ktype_contains_type_decl current_module program ktype
      ktype_type_decl_origin already_visited type_decl_to_check =
    (* already_visited |> List.map (Type_Decl.string_of_type_decl) |> List.iter (print_endline); *)
    match ktype with
    | TType_Identifier { module_path = ktype_def_path; name = ktype_name } -> (
        match
          Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path
            ~ktype_name ~current_module program
        with
        | Ast.Type_Decl.Decl_Struct struct_decl ->
            does_contains_type_decl_struct current_module program struct_decl
              already_visited type_decl_to_check
        | Ast.Type_Decl.Decl_Enum enum_decl ->
            does_contains_type_decl_enum current_module program enum_decl
              already_visited type_decl_to_check)
    | TParametric_identifier
        { module_path = ktype_def_path; parametrics_type; name = ktype_name }
      -> (
        let type_decl_found =
          Asthelper.Program.find_type_decl_from_ktype ~ktype_def_path
            ~ktype_name ~current_module program
        in
        match type_decl_found with
        | Ast.Type_Decl.Decl_Struct struct_decl ->
            let new_struct =
              Asthelper.Struct.bind_struct_decl parametrics_type
                (ktype_type_decl_origin |> Asthelper.Type_Decl.generics)
                struct_decl
            in
            print_endline (Struct.string_of_struct_decl new_struct);
            does_contains_type_decl_struct current_module program new_struct
              already_visited type_decl_to_check
        | Ast.Type_Decl.Decl_Enum enum_decl ->
            let new_enum_decl =
              Asthelper.Enum.bind_enum_decl parametrics_type
                (ktype_type_decl_origin |> Asthelper.Type_Decl.generics)
                enum_decl
            in
            does_contains_type_decl_enum current_module program new_enum_decl
              already_visited type_decl_to_check)
    | TTuple kts ->
        kts
        |> List.for_all (fun kt ->
               does_ktype_contains_type_decl current_module program kt
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
                match kt with
                | TType_Identifier _ when Struct.is_type_generic kt struct_decl
                  ->
                    false
                | TType_Identifier
                    { module_path = ktype_def_path; name = ktype_name } ->
                    let t_decl =
                      Program.find_type_decl_from_ktype ~ktype_def_path
                        ~ktype_name ~current_module program
                    in
                    t_decl = type_decl_to_check
                    || t_decl = Ast.Type_Decl.decl_struct struct_decl
                    || does_ktype_contains_type_decl current_module program kt
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
                      Program.find_type_decl_from_ktype ~ktype_def_path
                        ~ktype_name ~current_module program
                    in
                    t_decl = type_decl_to_check
                    || parametrics_type
                       |> List.exists (fun ikt ->
                              if
                                struct_decl
                                |> Struct.is_ktype_generic_level_zero ikt
                              then false
                              else
                                does_ktype_contains_type_decl current_module
                                  program ikt
                                  (Ast.Type_Decl.Decl_Struct struct_decl)
                                  (Ast.Type_Decl.Decl_Struct struct_decl
                                 :: already_visited)
                                  type_decl_to_check)
                    || does_ktype_contains_type_decl current_module program kt
                         (Ast.Type_Decl.Decl_Struct struct_decl)
                         (Ast.Type_Decl.Decl_Struct struct_decl
                        :: already_visited)
                         type_decl_to_check
                | _ ->
                    does_ktype_contains_type_decl current_module program kt
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
                             Program.find_type_decl_from_ktype ~ktype_def_path
                               ~ktype_name ~current_module program
                           in
                           t_decl = type_decl_to_check
                           || t_decl = Ast.Type_Decl.decl_enum enum_decl
                           || parametrics_type
                              |> List.exists (fun ikt ->
                                     if
                                       enum_decl
                                       |> Enum.is_ktype_generic_level_zero ikt
                                     then false
                                     else
                                       does_ktype_contains_type_decl
                                         current_module program ikt
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
                             Program.find_type_decl_from_ktype ~ktype_def_path
                               ~ktype_name ~current_module program
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
           if Asthelper.Enum.is_ktype_generic_level_zero kt enum_decl then false
           else
             does_ktype_contains_type_decl current_module program kt
               (Ast.Type_Decl.decl_enum enum_decl)
               []
               (Ast.Type_Decl.decl_enum enum_decl))

  let program_remove_implicit_type_path (program : program) =
    program
    |> List.map (fun { path; _module = Mod _module } ->
           let new_module =
             _module
             |> List.map (fun node ->
                    Asthelper.AstModif.module_node_remove_implicit_type_path
                      path node)
           in
           { path; _module = Mod new_module })
end

module ValidateExternalFunction = struct
  let does_parameters_contains_unit (external_func_decl : external_func_decl) =
    if external_func_decl.fn_parameters |> List.mem TUnit then
      Error.Unit_parameter external_func_decl |> Error.external_func_error
      |> Result.error
    else Ok ()

  let does_contains_not_c_compatible_type program current_module
      (external_func_decl : external_func_decl) =
    external_func_decl.fn_parameters
    |> List.filter_map (fun kt ->
           try
             if
               Asthelper.Program.is_c_type_from_ktype current_module kt program
               |> not
             then
               Error.External_Func_Error
                 (Error.Not_C_compatible_type (external_func_decl, kt))
               |> Option.some
             else None
           with
           | Util.Occurence.No_Occurence ->
               Error.No_Type_decl_found kt |> Option.some
           | Util.Occurence.Too_Many_Occurence ->
               Error.Too_many_type_decl kt |> Option.some)
    |> function
    | [] -> Ok ()
    | t :: _ -> t |> Result.error

  let does_contains_too_much_parameters
      (external_func_decl : external_func_decl) =
    let length = external_func_decl.fn_parameters |> List.length in
    if length > 15 then
      Error.Too_much_parameters { limit = 15; found = length } |> Result.error
    else Ok ()

  let is_valid_external_function_declaration (program : Ast.program)
      (current_module_name : string) (external_func_decl : external_func_decl) =
    let ( >>= ) = Result.bind in
    ( does_parameters_contains_unit external_func_decl >>= fun () ->
      does_contains_not_c_compatible_type program current_module_name
        external_func_decl )
    >>= fun () -> does_parameters_contains_unit external_func_decl
end

module ValidateSyscall = struct
  let does_parameters_contains_unit (syscall_decl : syscall_decl) =
    if syscall_decl.parameters |> List.mem TUnit then
      Error.Syscall_Unit_parameter syscall_decl |> Error.syscall_error
      |> Result.error
    else Ok ()

  let does_contains_not_c_compatible_type program current_module
      (syscall_decl : syscall_decl) =
    syscall_decl.parameters
    |> List.filter_map (fun kt ->
           try
             if
               Asthelper.Program.is_c_type_from_ktype current_module kt program
               |> not
             then
               Error.Syscall_Not_C_compatible_type (syscall_decl, kt)
               |> Error.syscall_error |> Option.some
             else None
           with
           | Util.Occurence.No_Occurence ->
               Error.No_Type_decl_found kt |> Option.some
           | Util.Occurence.Too_Many_Occurence ->
               Error.Too_many_type_decl kt |> Option.some)
    |> function
    | [] -> Ok ()
    | t :: _ -> t |> Result.error

  let does_contains_too_much_parameters (syscall_decl : syscall_decl) =
    let length = syscall_decl.parameters |> List.length in
    if length > 15 then
      Error.Syscall_Too_much_parameters { limit = 15; found = length }
      |> Result.error
    else Ok ()

  let is_valid_syscall_declaration (program : Ast.program)
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
           if Asthelper.Struct.is_ktype_generic_level_zero kt struct_decl then
             None
           else
             match Help.is_ktype_exist current_module program kt with
             | Ok () -> None
             | Error e -> Some e)

  let is_field_duplicate struct_decl =
    struct_decl.fields
    |> List.map (fun (field, _) -> field)
    |> Util.ListHelper.duplicate |> Util.are_diff_lenght []

  let is_valid_struct_decl (program : Ast.program)
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
    if is_field_duplicate struct_decl then
      Error.SDuplicated_field struct_decl |> Error.struct_error |> Result.error
    else Ok ()
end

module ValidateEnum = struct
  let is_all_type_exist current_module program (enum_decl : enum_decl) =
    enum_decl.variants
    |> List.map (fun (_, kts) -> kts)
    |> List.flatten
    |> List.find_map (fun kt ->
           if Asthelper.Enum.is_ktype_generic_level_zero kt enum_decl then None
           else
             match Help.is_ktype_exist current_module program kt with
             | Ok () -> None
             | Error e -> Some e)

  let is_variant_duplicate enum_decl =
    enum_decl.variants
    |> List.map (fun (s, _) -> s)
    |> Util.ListHelper.duplicate |> Util.are_diff_lenght []

  let is_valid_enum_decl (program : Ast.program) (current_module_name : string)
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
    if is_variant_duplicate enum_decl then
      Error.EDuplicated_variant_name enum_decl |> Error.enum_error
      |> Result.error
    else Ok ()
end

module ValidateFunction_Decl = struct

  let is_main_function function_decl = function_decl.fn_name = "main"

  let is_valid_main_sig function_decl =
    function_decl.fn_name = "main"
    && function_decl.return_type = TInteger (Signed, I32)
    && function_decl.parameters = []
    && function_decl.generics = []

  let check_parameters_duplicate (function_decl: function_decl) =
    if function_decl.parameters
    |> List.map (fun (field, _) -> field)
    |> Util.ListHelper.duplicate |> Util.are_diff_lenght [] then 
      Error.Duplicated_parameters function_decl |> Error.function_error |> Result.error
    else Ok ()
  let check_unit_parameters (function_decl: function_decl) = 
    if function_decl.parameters
    |> List.exists (fun (_, kt) -> kt = TUnit) then Error.Function_Unit_parameter function_decl |> Error.function_error |> Result.error
    else Ok ()

  let is_all_type_exist current_module program function_decl = 
    function_decl.parameters
    |> List.find_map (fun (_, kt) -> 
        if Asthelper.Function.is_ktype_generic_level_zero kt function_decl then None
        else
          match Help.is_ktype_exist current_module program kt with
          | Ok () -> None
          | Error e -> Some e
      )
    
  let check_main_sig function_decl = 
    if not (is_main_function function_decl) then Ok ()
    else if is_valid_main_sig function_decl then Ok ()
    else  Wrong_signature_for_main |> Error.function_error |> Result.error


  let check_kbody current_module program
      (function_decl : function_decl) =
    let hashtbl =
      Hashtbl.of_seq
        (function_decl.generics |> List.map (fun k -> (k, ())) |> List.to_seq)
    in
    try
      let _ =
        Typecheck.typeof_kbody ~generics_resolver:(Some hashtbl)
          (function_decl.parameters
          |> List.fold_left
               (fun acc_env para ->
                 acc_env |> Env.add_fn_parameters ~const:false para)
               Env.create_empty_env)
          current_module program ~return_type:(Some function_decl.return_type)
          function_decl.body
      in
      Ok ()
    with Ast.Error.Ast_error e -> Error.Ast_Error e |> Result.error


  let check_function program current_module  function_decl = 
    let ( >>= ) = Result.bind in

    (is_all_type_exist current_module program function_decl)
    |> ( Option.fold ~none:(Ok ()) ~some:(Result.error))
    >>= fun () -> check_main_sig function_decl
    >>= fun () -> check_unit_parameters function_decl
    >>= fun () -> check_parameters_duplicate function_decl
    >>= fun () -> check_kbody current_module program function_decl

end

module ValidateOperator_Decl = struct
  let is_all_type_exist current_module program (operator_decl : operator_decl) =
    let ( >>= ) = Result.bind in

    let first_kt =
      Asthelper.ParserOperator.first_parameter_ktype operator_decl
    in
    Help.is_ktype_exist_from_ktype current_module program first_kt >>= fun () ->
    match Asthelper.ParserOperator.second_parameter_ktype operator_decl with
    | None -> Ok ()
    | Some kt -> Help.is_ktype_exist_from_ktype current_module program kt

  let check_parameters operator_decl =
    match operator_decl with
    | Unary _ -> Ok ()
    | Binary binary ->
        let (_, kt1), (_, kt2) = binary.fields in
        if kt1 = kt2 then
          if kt1 |> Ast.Type.is_builtin_type then
            Error.Op_built_overload kt1 |> Error.operator_error |> Result.error
          else Ok ()
        else
          Error.Op_binary_op_diff_type (kt1, kt2)
          |> Error.operator_error |> Result.error

  let check_return_ktype operator_decl =
    match operator_decl with
    | Unary u ->
        let expected =
          Asthelper.ParserOperator.expected_unary_return_type
            (Asthelper.ParserOperator.first_parameter_ktype operator_decl)
            u.op
        in
        if u.return_type = expected then Ok ()
        else
          Op_wrong_return_type_error
            { op = `unary u.op; expected; found = u.return_type }
          |> Error.operator_error |> Result.error
    | Binary b ->
        let expected =
          Asthelper.ParserOperator.expected_binary_return_type
            (Asthelper.ParserOperator.first_parameter_ktype operator_decl)
            b.op
        in
        if b.return_type = expected then Ok ()
        else
          Op_wrong_return_type_error
            { op = `binary b.op; expected; found = b.return_type }
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
        Typecheck.typeof_kbody
          (fields
          |> List.fold_left
               (fun acc_env para ->
                 acc_env |> Env.add_fn_parameters ~const:false para)
               Env.create_empty_env)
          current_module program
          ~return_type:
            (Some (Asthelper.ParserOperator.return_ktype operator_decl))
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
  let check_duplicate_function { path = _; _module } =
    _module |> Asthelper.Module.retrieve_functions_decl
    |> List.map Ast.Function_Decl.calling_name
    |> Util.ListHelper.duplicate
    |> function
    | [] -> Ok ()
    | t :: _ ->
        Error.Duplicate_function_name t |> Error.module_error |> Result.error

  let check_duplicate_operator { path = _; _module } =
    _module |> Asthelper.Module.retrieve_operator_decl
    |> List.map Asthelper.ParserOperator.signature
    |> Util.ListHelper.duplicate
    |> function
    | [] -> Ok ()
    | (op, _, _) :: _ ->
        Error.Duplicate_Operator op |> Error.module_error |> Result.error

  let check_main_signature { path = _; _module } =
    let ( >>= ) = Result.bind in
    _module |> Asthelper.Module.retrieve_functions_decl
    |> List.filter (fun decl ->
           decl |> Ast.Function_Decl.calling_name |> ( = ) "main")
    |> List.fold_left
         (fun acc value ->
           acc >>= fun found ->
           match value with
           | Ast.Function_Decl.Decl_External _ | Function_Decl.Decl_Syscall _ ->
               Main_no_kosu_function |> Error.module_error |> Result.error
           | Ast.Function_Decl.Decl_Kosu_Function function_decl -> (
               let is_main_valid_sig =
                 ValidateFunction_Decl.is_valid_main_sig function_decl
               in
               match found with
               | None ->
                   if is_main_valid_sig then () |> Option.some |> Result.ok
                   else
                     Wrong_signature_for_main |> Error.function_error
                     |> Result.error
               | Some _ ->
                   Duplicate_function_name function_decl.fn_name
                   |> Error.module_error |> Result.error))
         (Ok None)
    |> Result.map (fun _ -> ())

  let check_duplicate_type { path = _; _module } =
    _module |> Asthelper.Module.retrieve_type_decl
    |> List.map Asthelper.Type_Decl.type_name
    |> Util.ListHelper.duplicate
    |> function
    | [] -> Ok ()
    | t :: _ ->
        Error.Duplicate_type_name t |> Error.module_error |> Result.error

  let check_duplicate_const_name { path = _; _module } =
    _module |> Asthelper.Module.retrieve_const_decl
    |> List.map (fun { const_name; _ } -> const_name)
    |> Util.ListHelper.duplicate
    |> function
    | [] -> Ok ()
    | t :: _ ->
        Error.Duplicate_const_name t |> Error.module_error |> Result.error

  let check_validate_module _module =
    let ( >>= ) = Result.bind in
    check_duplicate_const_name _module >>= fun () ->
    check_duplicate_function _module >>= fun () ->
    check_duplicate_operator _module >>= fun () ->
    check_main_signature _module >>= fun () -> check_duplicate_type _module
end

module Validate_Program = struct
  let check_main_in_program (program : program) =
    program
    |> List.map (fun { path = _; _module } ->
           _module |> Asthelper.Module.retrieve_functions_decl
           |> List.filter (fun fn ->
                  fn |> Ast.Function_Decl.calling_name = "main"))
    |> List.flatten
    |> function
    | [] | _ :: [] -> Ok ()
    | _ :: _ as l -> Error (Error.Too_many_Main (l |> List.length))
end

let validate_module_node (program : Ast.program) (current_module_name : string)
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
    ValidateFunction_Decl.check_function program current_module_name function_decl
  | NOperator operator_decl ->
      ValidateOperator_Decl.is_valid_operator_decl current_module_name program
        operator_decl

let validate_module (program : Ast.program)
    ({ path; _module = Mod _module } as package) =
  let ( >>= ) = Result.bind in
  (* Printf.printf "Module = %s\n" path; *)
  ValidateModule.check_validate_module package >>= fun () ->
  _module
  |> List.fold_left
       (fun acc value ->
         if acc |> Result.is_error then acc
         else validate_module_node program path value)
       (Ok ())

let valide_program (program : program) =
  let ( >>= ) = Result.bind in
  program |> Validate_Program.check_main_in_program 
  >>= fun () -> program |> Help.program_remove_implicit_type_path
  |> List.fold_left
       (fun acc value ->
         if acc |> Result.is_error then acc else validate_module program value)
       (Ok ())

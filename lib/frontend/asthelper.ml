open Ast
open Printf

let char_of_signedness = function Signed -> 's' | Unsigned -> 'u'

let string_of_isize = function
  | I8 -> "8"
  | I16 -> "16"
  | I32 -> "32"
  | I64 -> "64"

let size_of_isize = function I8 -> 8 | I16 -> 16 | I32 -> 32 | I64 -> 64
let f = sprintf "%Ld"

let rec string_of_ktype = function
  | TParametric_identifier { module_path; parametrics_type; name } ->
      sprintf "(%s)%s %s"
        (parametrics_type |> List.map string_of_ktype |> String.concat ", ")
        module_path name
  | TType_Identifier { module_path; name } ->
      sprintf "%s%s"
        (if module_path = "" then "" else sprintf "%s::" module_path)
        name
  | TInteger (sign, size) ->
      sprintf "%c%s" (char_of_signedness sign) (string_of_isize size)
  | TPointer ktype -> sprintf "*%s" (string_of_ktype ktype)
  | TTuple ktypes ->
      sprintf "(%s)" (ktypes |> List.map string_of_ktype |> String.concat ", ")
  | TFunction (parameters, r_type) ->
      sprintf "(%s) -> %s"
        (parameters |> List.map string_of_ktype |> String.concat ", ")
        (string_of_ktype r_type)
  | TString_lit -> "stringl"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TUnknow -> "unknow"
  | TFloat -> "f64"

let module_path_of_ktype_opt = function
  | TType_Identifier { module_path; name }
  | TParametric_identifier { module_path; parametrics_type = _; name } ->
      Some (module_path, name)
  | _ -> None

module Module = struct
  let retrieve_enum_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NEnum e -> Some e | _ -> None)

  let retrieve_struct_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NStruct s -> Some s | _ -> None)

  let retrieve_external_func_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NExternFunc s -> Some s | _ -> None)

  let retrieve_func_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NFunction s -> Some s | _ -> None)

  let retrieve_operator_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (function Ast.NOperator s -> Some s | _ -> None)

  let retrieve_syscall_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NSyscall s -> Some s | _ -> None)

  let retrieve_const_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NConst s -> Some s | _ -> None)

  let retrieve_sig_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with Ast.NSigFun s -> Some s | _ -> None)

  let retrieve_type_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with
               | Ast.NEnum e -> Some (Ast.Type_Decl.decl_enum e)
               | Ast.NStruct s -> Some (Ast.Type_Decl.decl_struct s)
               | _ -> None)

  let retrieve_functions_decl = function
    | Ast.Mod nodes ->
        nodes
        |> List.filter_map (fun node ->
               match node with
               | Ast.NExternFunc e -> Some (Ast.Function_Decl.Decl_External e)
               | Ast.NFunction e ->
                   Some (Ast.Function_Decl.decl_kosu_function e)
               | Ast.NSyscall e -> Some (Ast.Function_Decl.decl_syscall e)
               | _ -> None)

  let retrieve_function_decl_from_name_and_types fn_name parameters_types
      r_types _module =
    _module |> retrieve_func_decl
    |> Util.Occurence.find_occurence (fun fn_decl ->
           fn_decl.fn_name = fn_name
           && Ast.Type.are_compatible_type r_types fn_decl.return_type
           && Util.are_same_lenght fn_decl.parameters parameters_types
           && List.for_all2 Ast.Type.are_compatible_type
                (fn_decl.parameters |> List.map (fun (_, kt) -> kt))
                parameters_types)

  let type_decl_occurence (type_name : string) (module_definition : Ast._module)
      =
    module_definition |> retrieve_type_decl
    |> Util.Occurence.find_occurence (function
         | Ast.Type_Decl.Decl_Enum e -> e.enum_name = type_name
         | Ast.Type_Decl.Decl_Struct s -> s.struct_name = type_name)

  let function_decl_occurence (fn_name : string)
      (module_definition : Ast._module) =
    module_definition |> retrieve_functions_decl
    |> Util.Occurence.find_occurence (function
         | Ast.Function_Decl.Decl_External e -> fn_name = e.sig_name
         | Ast.Function_Decl.Decl_Kosu_Function e -> fn_name = e.fn_name
         | Ast.Function_Decl.Decl_Syscall e -> fn_name = e.syscall_name)
end

module Program = struct
  type t = Ast.program

  let module_of_string_opt mod_name (program : t) =
    program
    |> List.filter_map (fun (mp : Ast.module_path) ->
           if mod_name = mp.path then Some mp._module else None)
    |> function
    | [] -> None
    | t :: _ -> Some t

  let module_of_string mod_name (program : t) =
    program
    |> List.filter_map (fun (mp : Ast.module_path) ->
           if mod_name = mp.path then Some mp._module else None)
    |> List.hd

  let find_struct_decl_opt (current_module_name : string) (module_path : string)
      (struct_name : string) (program : t) =
    Result.bind
      ((if module_path = "" then
        Some (program |> module_of_string current_module_name)
       else program |> module_of_string_opt current_module_name)
      |> Option.map Module.retrieve_struct_decl
      |> Option.to_result ~none:(Ast.Error.Unbound_Module module_path))
      (fun structs ->
        structs
        |> List.find_opt (fun s -> s.struct_name = struct_name)
        |> Option.to_result ~none:(Ast.Error.Undefined_Struct struct_name))

  let find_enum_decl_opt current_module_name module_enum_path
      (enum_name_opt : string option) (variant : string)
      (assoc_exprs : kexpression list) (program : t) =
    match
      if module_enum_path = "" then
        Some (program |> module_of_string current_module_name)
      else program |> module_of_string_opt current_module_name
    with
    | None ->
        Ast.Error.Unbound_Module module_enum_path |> Either.left |> Result.error
    | Some _module -> (
        _module |> Module.retrieve_enum_decl
        |> Util.Occurence.find_occurence (fun enum_decl ->
               match enum_name_opt with
               | None ->
                   enum_decl.variants
                   |> List.exists (fun (variant_decl, assoc_type) ->
                          variant_decl = variant
                          && Util.are_same_lenght assoc_exprs assoc_type)
               | Some enum_name ->
                   enum_name = enum_decl.enum_name
                   && enum_decl.variants
                      |> List.exists (fun (variant_decl, assoc_type) ->
                             variant_decl = variant
                             && Util.are_same_lenght assoc_exprs assoc_type))
        |> function
        | Empty -> Not_found |> Either.right |> Result.error
        | Util.Occurence.Multiple _ ->
            Util.Occurence.Too_Many_Occurence |> Either.right |> Result.error
        | One enum_decl -> enum_decl |> Result.ok)

  let find_module_of_ktype ktype_def_path current_module program =
    match ktype_def_path with
    | "" ->
        program
        |> Util.Occurence.find_occurence (fun module_path ->
               module_path.path = current_module)
    | s ->
        program
        |> Util.Occurence.find_occurence (fun module_path ->
               module_path.path = s)

  let find_module_of_function fn_def_path current_module program =
    match fn_def_path with
    | "" ->
        program
        |> Util.Occurence.find_occurence (fun module_path ->
               module_path.path = current_module)
    | s ->
        program
        |> Util.Occurence.find_occurence (fun module_path ->
               module_path.path = s)

  (**
    Find type declaration from ktype
    @raise No_Occurence : if no type declaration was found
    @raise Too_Many_Occurence: if several type declaration matching was found
    *)
  let find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module
      program =
    find_module_of_ktype ktype_def_path current_module program
    |> Util.Occurence.one
    |> fun m ->
    m._module |> Module.type_decl_occurence ktype_name |> Util.Occurence.one

  (**
      Find type declaration from ktype
      @return type_decl if ktype came from a type declaration, [None] if ktype is a builtin type
      @raise No_Occurence : if no type declaration was found
      @raise Too_Many_Occurence: if several type declaration matching was found
      @raise Ast_error: if length of assoc_type is not the same as the length of the generic of the type declaration found
    *)
  let find_type_decl_from_true_ktype ktype current_module program =
    let type_generics = function
      | Ast.Type_Decl.Decl_Enum e -> e.generics
      | Ast.Type_Decl.Decl_Struct s -> s.generics
    in
    match ktype with
    | TType_Identifier { module_path = ktype_def_path; name = ktype_name } ->
        let type_decl =
          find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module
            program
        in
        let generics_len = type_decl |> type_generics |> List.length in
        if generics_len <> 0 then
          Wrong_Assoc_Length_for_Parametrics
            { expected = generics_len; found = 0; ktype }
          |> Ast.Error.ast_error |> raise
        else type_decl |> Option.some
    | TParametric_identifier
        { module_path = ktype_def_path; parametrics_type; name = ktype_name } ->
        let type_decl =
          find_type_decl_from_ktype ~ktype_def_path ~ktype_name ~current_module
            program
        in
        let generics_len = type_decl |> type_generics |> List.length in
        let assoc_type = parametrics_type |> List.length in
        if generics_len <> assoc_type then
          Wrong_Assoc_Length_for_Parametrics
            { expected = generics_len; found = assoc_type; ktype }
          |> Ast.Error.ast_error |> raise
        else type_decl |> Option.some
    | _ -> None

  (**
    Find function declaration from function name
    @raise No_Occurence : if no function declaration was found
    @raise Too_Many_Occurence: if several function declaration matching was found
    *)
  let find_function_decl_from_fn_name fn_def_path fn_name current_module program
      =
    program
    |> find_module_of_function fn_def_path current_module
    |> Util.Occurence.one
    |> fun m ->
    m._module |> Module.function_decl_occurence fn_name |> Util.Occurence.one

  let rec does_ktype_exist ktype current_module program =
    match ktype with
    | TType_Identifier { module_path = ktype_def_path; name = ktype_name } -> (
        try
          let _ =
            find_type_decl_from_ktype ~ktype_def_path ~ktype_name
              ~current_module program
          in
          `exist
        with
        | Util.Occurence.No_Occurence -> `not_exist
        | Util.Occurence.Too_Many_Occurence -> `not_unique)
    | TParametric_identifier
        { module_path = ktype_def_path; parametrics_type; name = ktype_name } ->
        let exist =
          try
            let _ =
              find_type_decl_from_ktype ~ktype_def_path ~ktype_name
                ~current_module program
            in
            `exist
          with
          | Util.Occurence.No_Occurence -> `not_exist
          | Util.Occurence.Too_Many_Occurence -> `not_unique
        in
        parametrics_type
        |> List.fold_left
             (fun acc value ->
               if acc <> `exist then acc
               else does_ktype_exist value current_module program)
             exist
    | _ -> `exist

  let rec is_c_type current_mod_name (type_decl : Ast.Type_Decl.type_decl)
      program =
    match type_decl with
    | Decl_Enum e ->
        e.generics = []
        && e.variants |> List.for_all (fun (_, assoc) -> assoc = [])
    | Decl_Struct s ->
        if s.generics = [] then false
        else
          s.fields
          |> List.for_all (fun (_, ktype) ->
                 match ktype with
                 | TParametric_identifier _ -> false
                 | TType_Identifier { module_path; name } -> (
                     try
                       let type_decl =
                         find_type_decl_from_ktype ~ktype_def_path:module_path
                           ~ktype_name:name ~current_module:current_mod_name
                           program
                       in
                       is_c_type current_mod_name type_decl program
                     with _ -> false)
                 | TFunction _ -> false
                 | TTuple _ -> false
                 | _ -> true)

  let is_c_type_from_ktype current_mod_name (ktype : ktype) program =
    match ktype with
    | TParametric_identifier _ -> false
    | TType_Identifier { module_path; name } -> (
        try
          let type_decl =
            find_type_decl_from_ktype ~ktype_def_path:module_path
              ~ktype_name:name ~current_module:current_mod_name program
          in
          is_c_type current_mod_name type_decl program
        with _ -> false)
    | TFunction _ -> false
    | TTuple _ -> false
    | _ -> true

  let find_binary_operator (op : Ast.parser_binary_op) (lhs, rhs) r_type program
      =
    program
    |> List.map (fun t -> Module.retrieve_operator_decl t._module)
    |> List.flatten
    |> List.filter (fun op_decl ->
           match op_decl with
           | Unary _ -> false
           | Binary record ->
               let (_, kt1), (_, kt2) = record.fields in
               record.return_type = r_type
               && kt1 = lhs && kt2 = rhs && record.op = op)

  let find_unary_operator (op : Ast.parser_unary_op) lhs r_type program =
    program
    |> List.map (fun t -> Module.retrieve_operator_decl t._module)
    |> List.flatten
    |> List.filter (fun op_decl ->
           match op_decl with
           | Binary _ -> false
           | Unary record ->
               let _, kt1 = record.field in
               record.return_type = r_type && kt1 = lhs && record.op = op)

  let find_function_exact fn_name ktypes_parameters return_type
      (program : module_path list) =
    let open Util.Occurence in
    program
    |> List.map (fun t -> t._module)
    |> List.map
         (Module.retrieve_function_decl_from_name_and_types fn_name
            ktypes_parameters return_type)
    |> List.filter (function One _ -> true | _ -> false)

  let is_valid_add_operation lhs rhs program =
    match lhs with
    | TPointer _ -> (
        match rhs with
        | TInteger _ -> `built_in_ptr_valid
        | _ -> `invalid_add_pointer)
    | _ -> (
        if lhs <> rhs then `diff_types
        else
          match lhs with
          | TType_Identifier _ as kt -> (
              match program |> find_binary_operator Ast.Add (kt, kt) kt with
              | [] -> `no_function_found
              | [ t ] -> `valid t
              | list -> `to_many_declaration list)
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_add_for_built_in)

  let is_valid_minus_operation lhs rhs program =
    match lhs with
    | TPointer _ -> (
        match rhs with
        | TInteger _ -> `built_in_ptr_valid
        | _ -> `invalid_add_pointer)
    | _ -> (
        if lhs <> rhs then `diff_types
        else
          match lhs with
          | TType_Identifier _ as kt -> (
              match program |> find_binary_operator Ast.Minus (kt, kt) kt with
              | [] -> `no_function_found
              | [ t ] -> `valid t
              | list -> `to_many_declaration list)
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_minus_for_built_in)

  let is_valid_mult_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.Mult (kt, kt) kt with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ | TFloat -> `built_in_valid
      | _ -> `no_mult_for_built_in

  let is_valid_div_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.Div (kt, kt) kt with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ | TFloat -> `built_in_valid
      | _ -> `no_div_for_built_in

  let is_valid_mod_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.Modulo (kt, kt) kt with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ -> `built_in_valid
      | _ -> `no_mod_for_built_in

  let is_valid_bitwiseor_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.BitwiseOr (kt, kt) kt with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ -> `built_in_valid
      | _ -> `no_bitwiseor_for_built_in

  let is_valid_bitwiseand_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.BitwiseAnd (kt, kt) kt with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ -> `built_in_valid
      | _ -> `no_bitwiseand_for_built_in

  let is_valid_bitwisexor_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.BitwiseXor (kt, kt) kt with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ -> `built_in_valid
      | _ -> `no_bitwisexor_for_built_in

  let is_valid_shiftleft_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.ShiftLeft (kt, kt) kt with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ -> `built_in_valid
      | _ -> `no_shiftleft_for_built_in

  let is_valid_shiftright_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.ShiftRight (kt, kt) kt with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ -> `built_in_valid
      | _ -> `no_shiftright_for_built_in

  let is_valid_equal_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.Equal (kt, kt) TBool with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ | TBool | TFloat | TPointer _ -> `built_in_valid
      | _ -> `no_equal_for_built_in (* Better handle the tuple *)

  let is_valid_sup_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.Sup (kt, kt) TBool with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ | TFloat -> `built_in_valid
      | _ -> `no_sup_for_built_in

  let is_valid_supeq_operation lhs rhs program =
    match is_valid_equal_operation lhs rhs program with
    | `built_in_valid | `valid _ -> is_valid_sup_operation lhs rhs program
    | _ as r -> r

  let is_valid_inf_operation lhs rhs program =
    if lhs <> rhs then `diff_types
    else
      match lhs with
      | TType_Identifier _ as kt -> (
          match program |> find_binary_operator Ast.Inf (kt, kt) TBool with
          | [] -> `no_function_found
          | [ t ] -> `valid t
          | list -> `to_many_declaration list)
      | TInteger _ | TFloat -> `built_in_valid
      | _ -> `no_inf_for_built_in

  let is_valid_infeq_operation lhs rhs program =
    match is_valid_equal_operation lhs rhs program with
    | `built_in_valid | `valid _ -> is_valid_inf_operation lhs rhs program
    | _ as r -> r

  let is_valid_not_operation ktype program =
    match ktype with
    | TType_Identifier _ as kt -> (
        match program |> find_unary_operator Ast.PNot kt kt with
        | [] -> `no_function_found
        | [ t ] -> `valid t
        | list -> `to_many_declaration list)
    | TInteger _ | TBool -> `built_in_valid
    | _ -> `no_not_for_built_in

  let is_valid_uminus_operation ktype program =
    match ktype with
    | TType_Identifier _ as kt -> (
        match program |> find_unary_operator Ast.PUMinus kt kt with
        | [] -> `no_function_found
        | [ t ] -> `valid t
        | list -> `to_many_declaration list)
    | TInteger (Signed, _) | TFloat -> `built_in_valid
    | TInteger (Unsigned, size) -> `invalid_unsigned_op size
    | _ -> `no_uminus_for_built_in
end

module Statement = struct
  let rec string_of_kbody = function
    | (statements : kstatement list), (expr : kexpression) ->
        sprintf "{\n  %s  %s\n}"
          (statements |> List.map string_of_kstatement |> String.concat "\n  ")
          (string_of_kexpression expr)

  and string_of_kstatement = function
    | SDeclaration { is_const; variable_name; explicit_type; expression } ->
        sprintf "%s %s : %s = %s;"
          (if is_const then "const" else "var")
          variable_name
          (explicit_type |> Option.map string_of_ktype
         |> Option.value ~default:"")
          (expression |> string_of_kexpression)
    | SAffection (id, expression) ->
        sprintf "%s = %s;" id (expression |> string_of_kexpression)
    | SDiscard expr -> sprintf "discard %s;" (string_of_kexpression expr)

  and string_of_kexpression = function
    | Empty -> "empty"
    | True -> "true"
    | False -> "false"
    | ENullptr -> "nullptr"
    | EInteger (sign, _, value) -> (
        match sign with
        | Signed -> sprintf "%Ld" value
        | Unsigned -> sprintf "%Lu" value)
    | EFloat f -> string_of_float f
    | EBin_op bin -> string_of_kbin_op bin
    | EUn_op un -> string_of_kunary_op un
    | ESizeof e ->
        let s =
          match e with
          | Either.Left t -> string_of_ktype t
          | Either.Right expr -> string_of_kexpression expr
        in
        sprintf "sizeof(%s)" s
    | EString s -> s
    | EAdress x -> sprintf "&%s" x
    | EDeference (indirection, id) ->
        sprintf "%s%s" (Util.string_of_chars indirection '*') id
    | EIdentifier { modules_path; identifier }
    | EConst_Identifier { modules_path; identifier } ->
        sprintf "%s%s" (Util.string_of_module_path modules_path) identifier
    | EFieldAcces { first_expr; fields } ->
        sprintf "(%s)->%s"
          (string_of_kexpression first_expr)
          (fields |> String.concat "->")
    | EStruct { modules_path; struct_name; fields } ->
        sprintf "%s%s { %s }"
          (if modules_path = "" then "" else sprintf "%s::" modules_path)
          struct_name
          (fields
          |> List.map (fun (id, expr) ->
                 sprintf "%s: %s" id (string_of_kexpression expr))
          |> String.concat ",")
    | EEnum { modules_path; enum_name; variant; assoc_exprs } ->
        sprintf "%s%s.%s%s"
          (Util.string_of_module_path modules_path)
          (enum_name |> Option.value ~default:" ")
          variant
          (if assoc_exprs = [] then ""
          else
            sprintf "(%s)"
              (assoc_exprs
              |> List.map string_of_kexpression
              |> String.concat ", "))
    | ETuple exprs ->
        sprintf "(%s)"
          (exprs |> List.map string_of_kexpression |> String.concat ", ")
    | EFunction_call { modules_path; generics_resolver; fn_name; parameters } ->
        sprintf "%s%s%s(%s)"
          (Util.string_of_module_path modules_path)
          fn_name
          (generics_resolver
          |> Option.map (fun kts ->
                 sprintf "::<%s>"
                   (kts |> List.map string_of_ktype |> String.concat ", "))
          |> Option.value ~default:"")
          (parameters |> List.map string_of_kexpression |> String.concat ", ")
    | EIf (expression, if_body, else_body) ->
        sprintf "if %s %s else %s"
          (string_of_kexpression expression)
          (string_of_kbody if_body)
          (string_of_kbody else_body)
    | ECases { cases; else_case } ->
        sprintf "cases {\n %s else => %s}"
          (cases
          |> List.map (fun (expr, kbody) ->
                 sprintf "%s => %s"
                   (expr |> string_of_kexpression)
                   (string_of_kbody kbody))
          |> String.concat "\n")
          (string_of_kbody else_case)
    | ESwitch { expression; cases; wildcard_case } ->
        sprintf "switch %s {%s\n%s}"
          (string_of_kexpression expression)
          (cases
          |> List.map (fun (sc, kbody) ->
                 sprintf "%s => %s"
                   (sc |> List.map string_of_switch_case |> String.concat ", ")
                   (string_of_kbody kbody))
          |> String.concat "\n")
          (match wildcard_case with
          | None -> ""
          | Some kbody -> sprintf "_ => %s" (string_of_kbody kbody))
    | EBuiltin_Function_call { fn_name; parameters } ->
        sprintf "@%s(%s)" fn_name
          (parameters |> List.map string_of_kexpression |> String.concat ", ")

  and string_of_kbin_op = function
    | BAdd (lhs, rhs) ->
        sprintf "(%s + %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BMinus (lhs, rhs) ->
        sprintf "(%s - %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BMult (lhs, rhs) ->
        sprintf "(%s * %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BDiv (lhs, rhs) ->
        sprintf "(%s / %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BMod (lhs, rhs) ->
        sprintf "(%s %% %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BBitwiseOr (lhs, rhs) ->
        sprintf "(%s | %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BBitwiseXor (lhs, rhs) ->
        sprintf "(%s ^ %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BBitwiseAnd (lhs, rhs) ->
        sprintf "(%s & %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BShiftLeft (lhs, rhs) ->
        sprintf "(%s << %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BShiftRight (lhs, rhs) ->
        sprintf "(%s >> %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BAnd (lhs, rhs) ->
        sprintf "(%s && %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BOr (lhs, rhs) ->
        sprintf "(%s || %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BInf (lhs, rhs) ->
        sprintf "(%s < %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BInfEq (lhs, rhs) ->
        sprintf "(%s <= %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BSup (lhs, rhs) ->
        sprintf "(%s > %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BSupEq (lhs, rhs) ->
        sprintf "(%s >= %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BEqual (lhs, rhs) ->
        sprintf "(%s == %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)
    | BDif (lhs, rhs) ->
        sprintf "(%s != %s)"
          (string_of_kexpression lhs)
          (string_of_kexpression rhs)

  and string_of_kunary_op = function
    | UMinus expr -> sprintf "-(%s)" (string_of_kexpression expr)
    | UNot expr -> sprintf "!(%s)" (string_of_kexpression expr)

  and string_of_switch_case = function
    | SC_Enum_Identifier { variant } -> "." ^ variant
    | SC_Enum_Identifier_Assoc { variant; assoc_ids } ->
        sprintf "%s(%s)" variant
          (assoc_ids
          |> List.map (Option.value ~default:"_")
          |> String.concat ",")
end

module Switch_case = struct
  type t = switch_case

  let variant_name = function
    | SC_Enum_Identifier { variant }
    | SC_Enum_Identifier_Assoc { variant; assoc_ids = _ } ->
        variant

  let assoc_binding = function
    | SC_Enum_Identifier _ -> []
    | SC_Enum_Identifier_Assoc e -> e.assoc_ids

  let is_case_matched (variant, (assoc_types : ktype list)) (switch_case : t) =
    match switch_case with
    | SC_Enum_Identifier { variant = matched_variant } ->
        matched_variant = variant && assoc_types |> List.length = 0
    | SC_Enum_Identifier_Assoc { variant = matched_variant; assoc_ids } ->
        matched_variant = variant && Util.are_same_lenght assoc_ids assoc_types

  let is_cases_matched variant (switch_cases : t list) =
    switch_cases |> List.exists (is_case_matched variant)

  let is_cases_duplicated variant (switch_cases : t list) =
    let open Util.Occurence in
    switch_cases
    |> find_occurence (fun sc -> sc |> variant_name |> ( = ) variant)
    |> function
    | Multiple _ -> true
    | _ -> false
end

module Enum = struct
  type t = enum_decl

  let string_of_enum_variant (variant : string * Ast.ktype list) =
    let name, types = variant in
    sprintf "%s (%s)" name
      (types |> List.map string_of_ktype |> String.concat ", ")

  let string_of_enum_decl (enum_decl : t) =
    sprintf "enum (%s) %s := { %s }"
      (enum_decl.generics |> String.concat ", ")
      enum_decl.enum_name
      (enum_decl.variants
      |> List.map string_of_enum_variant
      |> String.concat ", ")

  let rec are_type_compatible (init_type : ktype) (expected_type : ktype)
      (enum_decl : t) =
    (* let () = Printf.printf "expected : %s :: found : %s\n" (string_of_ktype expected_type) (string_of_ktype init_type) in *)
    match (init_type, expected_type) with
    | ( TType_Identifier { module_path = init_path; name = init_name },
        TType_Identifier { module_path = exp_path; name = exp_name } ) ->
        enum_decl.generics |> List.mem exp_name
        || (init_path = exp_path && init_name = exp_name)
    | ( TParametric_identifier
          {
            module_path = init_path;
            parametrics_type = init_pt;
            name = init_name;
          },
        TParametric_identifier
          { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
      ) ->
        if
          init_path <> exp_path || init_name <> exp_name
          || List.compare_lengths init_pt exp_pt <> 0
        then false
        else
          List.combine init_pt exp_pt
          |> List.for_all (fun (i, e) -> are_type_compatible i e enum_decl)
    | TPointer l_type, TPointer r_type ->
        are_type_compatible l_type r_type enum_decl
    | TTuple lhs, TTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.combine lhs rhs
           |> List.for_all (fun (lhs_type, rhs_type) ->
                  are_type_compatible lhs_type rhs_type enum_decl)
    | TUnknow, _ -> true
    | _, TType_Identifier { module_path; name }
      when module_path = "" && enum_decl.generics |> List.mem name ->
        true
    | lhs, rhs -> lhs = rhs

  let rec is_type_compatible_hashgen generic_table (init_type : ktype)
      (expected_type : ktype) (enum_decl : t) =
    match (init_type, expected_type) with
    | kt, TType_Identifier { module_path = ""; name }
      when match Hashtbl.find_opt generic_table name with
           | None -> false
           | Some (_, find_kt) ->
               if find_kt = TUnknow then
                 let () =
                   Hashtbl.replace generic_table name
                     ( enum_decl.generics
                       |> Util.ListHelper.index_of (( = ) name),
                       kt )
                 in
                 true
               else false ->
        true
    | ( TType_Identifier { module_path = init_path; name = init_name },
        TType_Identifier { module_path = exp_path; name = exp_name } ) ->
        enum_decl.generics |> List.mem exp_name
        || (init_path = exp_path && init_name = exp_name)
    | ( TParametric_identifier
          {
            module_path = init_path;
            parametrics_type = init_pt;
            name = init_name;
          },
        TParametric_identifier
          { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
      ) ->
        if
          init_path <> exp_path || init_name <> exp_name
          || List.compare_lengths init_pt exp_pt <> 0
        then false
        else
          List.combine init_pt exp_pt
          |> List.for_all (fun (i, e) ->
                 is_type_compatible_hashgen generic_table i e enum_decl)
    | TUnknow, _ -> true
    | TPointer l_type, TPointer r_type ->
        is_type_compatible_hashgen generic_table l_type r_type enum_decl
    | TTuple lhs, TTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.combine lhs rhs
           |> List.for_all (fun (lhs_type, rhs_type) ->
                  is_type_compatible_hashgen generic_table lhs_type rhs_type
                    enum_decl)
    | lhs, rhs -> lhs = rhs

  let to_ktype_hash generics module_def_path (enum_decl : t) =
    if enum_decl.generics = [] then
      TType_Identifier
        { module_path = module_def_path; name = enum_decl.enum_name }
    else
      TParametric_identifier
        {
          module_path = module_def_path;
          parametrics_type =
            generics |> Hashtbl.to_seq |> List.of_seq
            |> List.sort (fun (_, (i, _)) (_, (b, _)) -> compare i b)
            |> List.map (fun (_, (_, kt)) -> kt);
          name = enum_decl.enum_name;
        }

  let is_valide_assoc_type_init ~init_types ~expected_types enum_decl =
    Util.are_same_lenght init_types expected_types
    && List.combine init_types expected_types
       |> List.for_all (fun (init_type, expected_type) ->
              are_type_compatible init_type expected_type enum_decl)

  let is_simple_enum (enum_decl : t) =
    enum_decl.variants |> List.for_all (fun (_, assoc) -> assoc = [])

  let is_tagged_union (enum_decl : t) =
    enum_decl.variants |> List.exists (fun (_, assoc_type) -> assoc_type <> [])

  let contains_generics (enum_decl : t) = enum_decl.generics <> []

  let is_valid_case_match (switch_case : switch_case) (enum_decl : t) =
    let open Ast.Error in
    let ( >>= ) = Result.bind in
    let variant, given_len =
      match switch_case with
      | SC_Enum_Identifier { variant } -> (variant, 0)
      | SC_Enum_Identifier_Assoc { variant; assoc_ids } ->
          (variant, assoc_ids |> List.length)
    in
    enum_decl.variants
    |> List.find_map (fun (variant_enum, assoc_types) ->
           if variant = variant_enum then Some (variant, assoc_types) else None)
    |> Option.to_result ~none:(Variant_not_found { enum_decl; variant })
    >>= fun (_, assoc_types) ->
    let assoc_len = assoc_types |> List.length in
    if assoc_len = given_len then Ok ()
    else
      Error
        (Mismatched_Assoc_length
           { variant; expected = assoc_len; found = given_len })

  let is_all_cases_handled (switch_cases : switch_case list) (enum_decl : t) =
    let open Ast.Error in
    let _, missing =
      enum_decl.variants
      |> List.partition_map (fun enum_variant ->
             if Switch_case.is_cases_matched enum_variant switch_cases then
               Either.left enum_variant
             else Either.right enum_variant)
    in

    match missing with
    | [] ->
        switch_cases
        |> List.fold_left
             (fun acc sc ->
               if acc |> Result.is_error then acc
               else is_valid_case_match sc enum_decl)
             (Result.ok ())
    | t -> Error (Not_all_cases_handled t)

  let rec is_type_generic ktype (enum_decl : t) =
    match ktype with
    | TType_Identifier { module_path = ""; name } ->
        enum_decl.generics |> List.mem name
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } ->
        parametrics_type |> List.exists (fun kt -> is_type_generic kt enum_decl)
    | TPointer kt -> is_type_generic kt enum_decl
    | TTuple kts -> kts |> List.exists (fun kt -> is_type_generic kt enum_decl)
    | _ -> false

  let extract_assoc_type_variant generics variant (enum_decl : t) =
    enum_decl.variants
    |> List.find_map (fun (case, assoc_type) ->
           if case = variant then Some assoc_type else None)
    |> Option.map (fun assoc_ktypes ->
           assoc_ktypes
           |> List.map (fun kt ->
                  generics |> List.assoc_opt kt |> Option.value ~default:kt))

  let is_ktype_generic_level_zero ktype (enum_decl : t) =
    match ktype with
    | TType_Identifier { module_path = ""; name } ->
        enum_decl.generics |> List.mem name
    | _ -> false

  let remove_level_zero_genenics ktypes (enum_decl : t) =
    ktypes
    |> List.filter (fun kt -> is_ktype_generic_level_zero kt enum_decl |> not)

  let reduce_binded_variable (assoc_binded : string option list)
      (ktype_list : ktype list) =
    ktype_list |> List.combine assoc_binded
    |> List.filter_map (fun (name, ktypes) ->
           match name with None -> None | Some s -> Some (s, ktypes))

  let reduce_binded_variable_combine assoc =
    assoc
    |> List.filter_map (fun (name, (ktype : ktype)) ->
           match (name : string option) with
           | None -> None
           | Some s -> Some (s, ktype))

  let bind_enum_decl (ktypes : ktype list) primitive_generics (enum_decl : t) =
    let combined = List.combine enum_decl.generics ktypes in
    {
      enum_decl with
      generics = primitive_generics;
      variants =
        enum_decl.variants
        |> List.map (fun (name, kts) ->
               ( name,
                 kts
                 |> List.map
                      (Ast.Type.map_generics_type combined primitive_generics)
               ));
    }
end

module Struct = struct
  type t = struct_decl

  let string_of_struct_decl (struct_decl : t) =
    sprintf "struct (%s) %s := { %s }"
      (struct_decl.generics |> String.concat ", ")
      struct_decl.struct_name
      (struct_decl.fields
      |> List.map (fun (field, t) ->
             sprintf "%s : %s" field (string_of_ktype t))
      |> String.concat ", ")

  let bind_struct_decl (ktypes : ktype list) primitive_generics
      (struct_decl : t) =
    let combined = List.combine struct_decl.generics ktypes in
    {
      struct_decl with
      generics = primitive_generics;
      fields =
        struct_decl.fields
        |> List.map (fun (name, kt) ->
               (name, Ast.Type.map_generics_type combined primitive_generics kt));
    }

  let contains_generics (struct_decl : t) = struct_decl.generics <> []

  let rec is_type_generic ktype (struct_decl : t) =
    match ktype with
    | TType_Identifier { module_path = ""; name } ->
        struct_decl.generics |> List.mem name
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } ->
        parametrics_type
        |> List.exists (fun kt -> is_type_generic kt struct_decl)
    | TPointer kt -> is_type_generic kt struct_decl
    | TTuple kts ->
        kts |> List.exists (fun kt -> is_type_generic kt struct_decl)
    | _ -> false

  let to_ktype module_def_path (struct_decl : t) =
    if not (struct_decl |> contains_generics) then
      TType_Identifier
        { module_path = module_def_path; name = struct_decl.struct_name }
    else
      TParametric_identifier
        {
          module_path = module_def_path;
          parametrics_type = struct_decl.generics |> List.map (fun _ -> TUnknow);
          name = struct_decl.struct_name;
        }

  let ktype_of_field_gen (parametrics_types : ktype list) (field : string)
      (struct_decl : t) =
    let list_len = parametrics_types |> List.length in
    let dummy_list = List.init list_len (fun _ -> ()) in
    let dummy_parametrics = List.combine dummy_list parametrics_types in
    let generics_mapped = List.combine struct_decl.generics dummy_parametrics in
    let hashtbl = Hashtbl.of_seq (generics_mapped |> List.to_seq) in
    struct_decl.fields |> List.assoc_opt field
    |> Option.map (fun kt ->
           if not (is_type_generic kt struct_decl) then kt
           else Type.remap_generic_ktype hashtbl kt)

  let rec resolve_fields_access_gen (parametrics_types : ktype list)
      (fields : string list) (type_decl : Ast.Type_Decl.type_decl)
      (current_mod_name : string) (program : program) =
    let open Ast.Type_Decl in
    let open Ast.Error in
    match fields with
    | [] -> failwith "Unreachable: Empty field access"
    | [ t ] -> (
        match type_decl with
        | Decl_Enum enum_decl ->
            Enum_Access_field { field = t; enum_decl } |> ast_error |> raise
        | Decl_Struct struct_decl -> (
            match ktype_of_field_gen parametrics_types t struct_decl with
            | None ->
                Impossible_field_Access (to_ktype current_mod_name struct_decl)
                |> ast_error |> raise
            | Some kt -> kt))
    | t :: q -> (
        match type_decl with
        | Decl_Enum enum_decl ->
            Enum_Access_field { field = t; enum_decl } |> ast_error |> raise
        | Decl_Struct struct_decl -> (
            match ktype_of_field_gen parametrics_types t struct_decl with
            | None ->
                Impossible_field_Access (to_ktype current_mod_name struct_decl)
                |> ast_error |> raise
            | Some kt ->
                let parametrics_types_two = Type.extract_parametrics_ktype kt in
                let ktype_def_path = Type.module_path_opt kt |> Option.get in
                let ktype_name = Type.type_name_opt kt |> Option.get in
                let type_decl_two =
                  Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name
                    ~current_module:current_mod_name program
                in
                resolve_fields_access_gen parametrics_types_two q type_decl_two
                  current_mod_name program))

  let is_ktype_generic_level_zero ktype (struct_decl : t) =
    match ktype with
    | TType_Identifier { module_path = ""; name } ->
        struct_decl.generics |> List.mem name
    | _ -> false

  let remove_level_zero_genenics ktypes (struct_decl : t) =
    ktypes
    |> List.filter (fun kt -> is_ktype_generic_level_zero kt struct_decl |> not)

  let rec is_type_compatible_hashgen generic_table (init_type : ktype)
      (expected_type : ktype) (struct_decl : t) =
    match (init_type, expected_type) with
    | kt, TType_Identifier { module_path; name }
      when match Hashtbl.find_opt generic_table name with
           | None ->
               if module_path = "" && struct_decl.generics |> List.mem name then
                 let () =
                   Hashtbl.replace generic_table name
                     ( struct_decl.generics
                       |> Util.ListHelper.index_of (( = ) name),
                       kt )
                 in
                 true
               else false
           | Some (_, find_kt) -> find_kt = kt ->
        true
    | ( TType_Identifier { module_path = init_path; name = init_name },
        TType_Identifier { module_path = exp_path; name = exp_name } ) ->
        struct_decl.generics |> List.mem exp_name
        || (init_path = exp_path && init_name = exp_name)
    | ( TParametric_identifier
          {
            module_path = init_path;
            parametrics_type = init_pt;
            name = init_name;
          },
        TParametric_identifier
          { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
      ) ->
        if
          init_path <> exp_path || init_name <> exp_name
          || List.compare_lengths init_pt exp_pt <> 0
        then false
        else
          List.combine init_pt exp_pt
          |> List.for_all (fun (i, e) ->
                 is_type_compatible_hashgen generic_table i e struct_decl)
    | TPointer lhs, TPointer rhs ->
        is_type_compatible_hashgen generic_table lhs rhs struct_decl
    | TTuple lhs, TTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.for_all2
             (fun init exptected ->
               is_type_compatible_hashgen generic_table init exptected
                 struct_decl)
             lhs rhs
    | TUnknow, _ -> true
    | lhs, rhs -> lhs = rhs

  let to_ktype_hash generics module_def_path (struct_decl : t) =
    if struct_decl.generics = [] then
      TType_Identifier
        { module_path = module_def_path; name = struct_decl.struct_name }
    else
      TParametric_identifier
        {
          module_path = module_def_path;
          parametrics_type =
            generics |> Hashtbl.to_seq |> List.of_seq
            |> List.sort (fun (_, (i, _)) (_, (b, _)) -> compare i b)
            |> List.map (fun (_, (_, kt)) -> kt);
          name = struct_decl.struct_name;
        }
end

module Type_Decl = struct
  type t = Ast.Type_Decl.type_decl

  let is_enum = function Ast.Type_Decl.Decl_Enum _ -> true | _ -> false
  let is_struct = function Ast.Type_Decl.Decl_Struct _ -> true | _ -> false

  let type_name = function
    | Ast.Type_Decl.Decl_Enum e -> e.enum_name
    | Ast.Type_Decl.Decl_Struct s -> s.struct_name

  let generics = function
    | Ast.Type_Decl.Decl_Enum e -> e.generics
    | Ast.Type_Decl.Decl_Struct s -> s.generics

  let is_ktype_generic kt = function
    | Ast.Type_Decl.Decl_Enum e -> e |> Enum.is_type_generic kt
    | Ast.Type_Decl.Decl_Struct s -> s |> Struct.is_type_generic kt

  let is_ktype_generic_level_zero kt = function
    | Ast.Type_Decl.Decl_Enum e -> e |> Enum.is_ktype_generic_level_zero kt
    | Ast.Type_Decl.Decl_Struct s -> s |> Struct.is_ktype_generic_level_zero kt

  let remove_level_zero_genenics kts = function
    | Ast.Type_Decl.Decl_Enum e -> e |> Enum.remove_level_zero_genenics kts
    | Ast.Type_Decl.Decl_Struct s -> s |> Struct.remove_level_zero_genenics kts

  let string_of_type_decl = function
    | Ast.Type_Decl.Decl_Enum e -> Enum.string_of_enum_decl e
    | Ast.Type_Decl.Decl_Struct s -> Struct.string_of_struct_decl s

  let are_same_type_decl lhs rhs = type_name lhs = type_name rhs
end

module ExternalFunc = struct
  type t = external_func_decl

  let string_of_external_func_decl (efucn_decl : t) =
    sprintf "external %s(%s%s) %s %s" efucn_decl.sig_name
      (efucn_decl.fn_parameters |> List.map string_of_ktype
     |> String.concat ", ")
      (if efucn_decl.is_variadic then ";..." else "")
      (efucn_decl.r_type |> string_of_ktype)
      (efucn_decl.c_name
      |> Option.map (fun s -> sprintf " = %s" s)
      |> Option.value ~default:"")
end

module Syscall = struct
  type t = syscall_decl

  let string_of_syscall (syscall_decl : t) =
    sprintf "syscall %s(%s) %s" syscall_decl.syscall_name
      (syscall_decl.parameters |> List.map string_of_ktype |> String.concat ", ")
      (syscall_decl.return_type |> string_of_ktype)
end

module Builtin_Function = struct
  type t = Ast.Builtin_Function.functions

  let fn_name_of_built_in_fn =
    let open Ast.Builtin_Function in
    function
    | Tos8 -> "tos8"
    | Tou8 -> "tou8"
    | Tos16 -> "tos16"
    | Tou16 -> "tou16"
    | Tos32 -> "tos32"
    | Tou32 -> "tou32"
    | Tos64 -> "tos64"
    | Tou64 -> "tou64"
    | Stringl_ptr -> "stringlptr"

  let builtin_fn_of_fn_name =
    let open Ast.Builtin_Function in
    function
    | "tos8" -> Tos8 |> Result.ok
    | "tou8" -> Tou8 |> Result.ok
    | "tos16" -> Tos16 |> Result.ok
    | "tou16" -> Tou16 |> Result.ok
    | "tos32" -> Tos32 |> Result.ok
    | "tou32" -> Tou32 |> Result.ok
    | "tos64" -> Tos64 |> Result.ok
    | "tou64" -> Tou64 |> Result.ok
    | "stringlptr" -> Stringl_ptr |> Result.ok
    | _ as fn_name -> Ast.Error.Unknow_built_function fn_name |> Result.error

  let is_valide_parameters_type parameters =
    let open Ast.Builtin_Function in
    function
    | (Tos8 | Tou8 | Tos16 | Tou16 | Tos32 | Tou32 | Tos64 | Tou64) as fn -> (
        match parameters with
        | [ t ] ->
            if t |> Type.is_any_integer then Result.ok fn
            else
              Ast.Error.Found_no_Integer
                { fn_name = fn |> fn_name_of_built_in_fn; found = t }
              |> Result.error
        | list ->
            Ast.Error.Mismatched_Parameters_Length
              {
                fn_name = fn |> fn_name_of_built_in_fn;
                expected = 1;
                found = list |> List.length;
              }
            |> Result.error)
    | Stringl_ptr -> (
        match parameters with
        | [ t ] ->
            if t |> Type.is_string_litteral then Result.ok Stringl_ptr
            else
              Ast.Error.Wrong_parameters
                {
                  fn_name = Stringl_ptr |> fn_name_of_built_in_fn;
                  expected = TString_lit;
                  found = t;
                }
              |> Result.error
        | list ->
            Ast.Error.Mismatched_Parameters_Length
              {
                fn_name = Stringl_ptr |> fn_name_of_built_in_fn;
                expected = 1;
                found = list |> List.length;
              }
            |> Result.error)

  let builtin_return_type =
    let open Ast.Builtin_Function in
    function
    | Stringl_ptr -> TPointer (TInteger (Signed, I8))
    | Tos8 -> TInteger (Signed, I8)
    | Tou8 -> TInteger (Unsigned, I8)
    | Tos16 -> TInteger (Signed, I16)
    | Tou16 -> TInteger (Unsigned, I16)
    | Tos32 -> TInteger (Signed, I32)
    | Tou32 -> TInteger (Unsigned, I32)
    | Tos64 -> TInteger (Signed, I64)
    | Tou64 -> TInteger (Unsigned, I64)
end

module Function = struct
  type t = function_decl

  let string_of_func_decl (function_decl : t) =
    sprintf "fn %s%s(%s)%s%s" function_decl.fn_name
      (if function_decl.generics = [] then ""
      else sprintf "<%s>" (function_decl.generics |> String.concat ", "))
      (function_decl.parameters
      |> List.map (fun (id, ktype) ->
             sprintf "%s: %s" id (string_of_ktype ktype))
      |> String.concat ", ")
      (function_decl.return_type |> string_of_ktype)
      (function_decl.body |> Statement.string_of_kbody)

  let rec is_ktype_generic ktype (fn_decl : t) =
    match ktype with
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } ->
        parametrics_type |> List.exists (fun kt -> is_ktype_generic kt fn_decl)
    | TType_Identifier { module_path = ""; name } ->
        fn_decl.generics |> List.mem name
    | _ -> false

  (**
    @return true if the generics is the immediat type and not nested into a parametric type
  *)
  let is_ktype_generic_level_zero ktype (fn_decl : t) =
    match ktype with
    | TType_Identifier { module_path = ""; name } ->
        fn_decl.generics |> List.mem name
    | _ -> false

  let does_need_generic_resolver (function_decl : t) =
    if function_decl.generics = [] then false
    else if function_decl.parameters |> List.length = 0 then true
    else function_decl |> is_ktype_generic_level_zero function_decl.return_type

  (* (**
       @return : Returns [Some name] if the associated type with the field is generics else [None] if not or the field name doesn't exist
     *)
     let assoc_generics_name_of_field field (fn_decl: function_decl) =
       let (>>:) = Option.bind in
       fn_decl.parameters
       |> List.find_map (fun (field_name, ktype ) -> if field_name = field then Some ktype else None)
       >>: (Type.type_name_opt)
       >>: (fun s -> if fn_decl.generics |> List.mem s then Some s else None) *)

  let rec is_type_compatible_hashgen generic_table (init_type : ktype)
      (expected_type : ktype) (function_decl : t) =
    match (init_type, expected_type) with
    | kt, TType_Identifier { module_path = ""; name }
      when match Hashtbl.find_opt generic_table name with
           | None ->
               if function_decl.generics |> List.mem name then
                 let () =
                   Hashtbl.replace generic_table name
                     ( function_decl.generics
                       |> Util.ListHelper.index_of (( = ) name),
                       kt )
                 in
                 true
               else false
           | Some (_, find_kt) -> find_kt = kt ->
        true
    | ( TType_Identifier { module_path = init_path; name = init_name },
        TType_Identifier { module_path = exp_path; name = exp_name } ) ->
        function_decl.generics |> List.mem exp_name
        || (init_path = exp_path && init_name = exp_name)
    | ( TParametric_identifier
          {
            module_path = init_path;
            parametrics_type = init_pt;
            name = init_name;
          },
        TParametric_identifier
          { module_path = exp_path; parametrics_type = exp_pt; name = exp_name }
      ) ->
        if
          init_path <> exp_path || init_name <> exp_name
          || List.compare_lengths init_pt exp_pt <> 0
        then false
        else
          List.combine init_pt exp_pt
          |> List.for_all (fun (i, e) ->
                 is_type_compatible_hashgen generic_table i e function_decl)
    | TUnknow, _ -> true
    | TPointer _, TPointer TUnknow -> true
    | TPointer lhs, TPointer rhs ->
        is_type_compatible_hashgen generic_table lhs rhs function_decl
    | TTuple lhs, TTuple rhs ->
        Util.are_same_lenght lhs rhs
        && List.for_all2
             (fun lkt rkt ->
               is_type_compatible_hashgen generic_table lkt rkt function_decl)
             lhs rhs
    | lhs, rhs -> lhs = rhs

  let to_return_ktype_hashtab generic_table (function_decl : t) =
    if
      function_decl.generics = []
      || function_decl |> is_ktype_generic function_decl.return_type |> not
    then function_decl.return_type
    else Ast.Type.remap_generic_ktype generic_table function_decl.return_type

  let iter_statement fn (function_decl : t) =
    let statements, _ = function_decl.body in
    statements |> List.iter fn
end

module ParserOperator = struct
  let string_of_parser_unary = function PNot -> "(!)" | PUMinus -> "(.-)"

  let string_of_parser_binary = function
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
    | Sup -> ">"
    | Inf -> "<"
    | Equal -> "=="

  let string_of_parser_operator = function
    | `unary u -> string_of_parser_unary u
    | `binary b -> string_of_parser_binary b

  let first_parameter_ktype operator_decl =
    match operator_decl with
    | Unary u ->
        let _, kt1 = u.field in
        kt1
    | Binary b ->
        let (_, kt1), (_, _) = b.fields in
        kt1

  let second_parameter_ktype operator_decl =
    match operator_decl with
    | Unary _ -> None
    | Binary b ->
        let (_, _), (_, kt2) = b.fields in
        Some kt2

  let return_ktype operator_decl =
    match operator_decl with
    | Unary u -> u.return_type
    | Binary b -> b.return_type

  let expected_unary_return_type (ktype : ktype) op =
    match op with Ast.PNot | Ast.PUMinus -> ktype

  let kbody = function
    | Unary { op = _; field = _; return_type = _; kbody }
    | Binary { op = _; fields = _; return_type = _; kbody } ->
        kbody

  let expected_binary_return_type (ktype : ktype) op =
    match op with
    | Add | Minus | Mult | Div | Modulo | BitwiseOr | BitwiseAnd | BitwiseXor
    | ShiftLeft | ShiftRight ->
        ktype
    | Sup | Inf | Equal -> TBool

  let expected_op_return_type ktype op =
    match op with
    | `unary u -> u |> expected_unary_return_type ktype
    | `binary b -> b |> expected_binary_return_type ktype
end

module Sizeof = struct
  let ( ++ ) = Int64.add
  let ( -- ) = Int64.sub

  let rec size calcul current_module (program : Program.t) ktype =
    match ktype with
    | TUnit | TBool | TUnknow -> 1L
    | TInteger (_, isize) -> size_of_isize isize / 8 |> Int64.of_int
    | TFloat | TPointer _ | TString_lit | TFunction _ -> 8L
    | TTuple kts -> (
        kts |> function
        | list -> (
            let size, align, _packed_size =
              list
              |> List.fold_left
                   (fun (acc_size, acc_align, acc_packed_size) kt ->
                     let comming_size =
                       kt |> size `size current_module program
                     in
                     let comming_align =
                       kt |> size `align current_module program
                     in
                     let quotient = Int64.unsigned_div acc_size comming_align in
                     let reminder = Int64.unsigned_rem acc_size comming_align in
                     let new_pacced_size = comming_size ++ acc_packed_size in

                     let add_size =
                       if new_pacced_size < acc_size then 0L
                       else if comming_size < acc_align then acc_align
                       else comming_size
                     in

                     let padded_size =
                       if reminder = 0L || acc_size = 0L then acc_size
                       else Int64.mul comming_align (quotient ++ 1L)
                     in
                     ( padded_size ++ add_size,
                       max comming_align acc_align,
                       new_pacced_size ))
                   (0L, 0L, 0L)
            in
            match calcul with `size -> size | `align -> align))
    | kt -> (
        let ktype_def_path = Type.module_path_opt kt |> Option.get in
        let ktype_name = Type.type_name_opt kt |> Option.get in
        let type_decl =
          Program.find_type_decl_from_ktype ~ktype_def_path ~ktype_name
            ~current_module program
        in
        match type_decl with
        | Ast.Type_Decl.Decl_Enum enum_decl ->
            size_enum calcul current_module program
              (Util.dummy_generic_map enum_decl.generics
                 (Type.extract_parametrics_ktype kt))
              enum_decl
        | Ast.Type_Decl.Decl_Struct struct_decl ->
            size_struct calcul current_module program
              (Util.dummy_generic_map struct_decl.generics
                 (Type.extract_parametrics_ktype kt))
              struct_decl)

  and size_struct calcul current_module program generics struct_decl =
    struct_decl.fields
    |> List.map (fun (_, kt) -> Ast.Type.remap_generic_ktype generics kt)
    |> function
    | list -> (
        let size, align, _packed_size =
          list
          |> List.fold_left
               (fun (acc_size, acc_align, acc_packed_size) kt ->
                 let comming_size = kt |> size `size current_module program in
                 let comming_align = kt |> size `align current_module program in
                 let quotient = Int64.unsigned_div acc_size comming_align in
                 let reminder = Int64.unsigned_rem acc_size comming_align in
                 let new_pacced_size = comming_size ++ acc_packed_size in

                 let add_size =
                   if new_pacced_size < acc_size then 0L
                   else if comming_size < acc_align then acc_align
                   else comming_size
                 in

                 let padded_size =
                   if reminder = 0L || acc_size = 0L then acc_size
                   else Int64.mul comming_align (quotient ++ 1L)
                 in
                 ( padded_size ++ add_size,
                   max comming_align acc_align,
                   new_pacced_size ))
               (0L, 0L, 0L)
        in
        match calcul with `size -> size | `align -> align)

  and size_enum calcul current_module program generics enum_decl =
    enum_decl.variants
    |> List.map (fun (_, kts) ->
           kts
           |> List.map (Type.remap_generic_ktype generics)
           |> List.cons (TInteger (Unsigned, I32))
           |> Type.ktuple
           |> size calcul current_module program)
    |> List.fold_left max 0L

  let sizeof current_module program ktype =
    size `size current_module program ktype

  let alignmentof current_module program ktype =
    size `align current_module program ktype
end

let string_of_variable_info (variable_info : Env.variable_info) =
  Printf.sprintf "(%s, %s)"
    (if variable_info.is_const then "const" else "var")
    (string_of_ktype variable_info.ktype)

let string_of_env (env : Env.t) =
  let open Printf in
  sprintf "[ %s ]"
    (env.contexts
    |> List.map (fun context ->
           sprintf "{ %s }"
             (context
             |> List.map (fun (variable_name, variable_info) ->
                    sprintf "%s = %s" variable_name
                      (string_of_variable_info variable_info))
             |> String.concat ", "))
    |> String.concat " ; ")

let string_of_expected_found = function
  | `int (expected, found) ->
      Printf.sprintf "-- expected : %d, found : %d --" expected found
  | `str (expected, found) ->
      Printf.sprintf "-- expected : %s, found : %s --" expected found
  | `ktype (expected, found) ->
      Printf.sprintf "-- expected : %s, found : %s --"
        (expected |> string_of_ktype)
        (found |> string_of_ktype)

let string_of_struct_error =
  let open Ast.Error in
  let open Printf in
  function
  | Unexpected_field { expected; found } ->
      sprintf "Unexpected_field -- expected : %s, found : %s --" expected found
  | Unexisting_field s -> sprintf "Unexisting_field %s" s
  | Wrong_field_count record ->
      sprintf "Wrong_field_count -- expected : %d, found : %d" record.expected
        record.found

let string_of_enum_error =
  let open Ast.Error in
  let open Printf in
  function
  | Wrong_length_assoc_type record ->
      sprintf "Wrong_length_assoc_type -- expected : %d, found : %d --"
        record.expected record.found
  | Uncompatible_type_in_variant { variant_name } ->
      sprintf "Uncompatible_type_in_variant : %s" variant_name

let string_of_statement_error =
  let open Ast.Error in
  let open Printf in
  function
  | Undefine_Identifier s -> sprintf "Undefine_Identifier : %s" s.name
  | Already_Define_Identifier s ->
      sprintf "Already_Define_Identifier : %s" s.name
  | Reassign_Constante s -> sprintf "Reassign_Constante : %s" s.name
  | Uncompatible_type_Assign s ->
      sprintf "Uncompatible_type_Assign -- expected: %s, found: %s --"
        (s.expected |> string_of_ktype)
        (s.found |> string_of_ktype)
  | Neead_explicit_type_declaration s ->
      sprintf "Neead_explicit_type_declaration for : %s : type = %s"
        s.variable_name
        (string_of_ktype s.infer_type)

let string_of_function_error =
  let open Ast.Error in
  let open Printf in
  function
  | Unmatched_Parameters_length record ->
      sprintf "Unmatched_Parameters_length %s "
        (string_of_expected_found (`int (record.expected, record.found)))
  | Unmatched_Generics_Resolver_length record ->
      sprintf "Unmatched_Generics_Resolver_length : %s"
        (string_of_expected_found (`int (record.expected, record.found)))
  | Uncompatible_type_for_C_Function recod ->
      sprintf "Uncompatible_type_for_C_Function for %s "
        (ExternalFunc.string_of_external_func_decl recod.external_func_decl)
  | Uncompatible_type_for_Syscall record ->
      sprintf "Uncompatible_type_for_Syscall for %s"
        (Syscall.string_of_syscall record.syscall_decl)
  | Mismatched_Parameters_Type record ->
      sprintf "Mismatched_Parameters_Type : %s"
        (string_of_expected_found (`ktype (record.expected, record.found)))
  | Unknow_Function_Error -> "Unknow_Function_Error"

let string_of_operator_error =
  let open Ast.Error in
  let open Printf in
  let open Ast.OperatorFunction in
  function
  | Invalid_pointer_arithmetic kt ->
      sprintf "Invalid_pointer_arithmetic with %s" (string_of_ktype kt)
  | No_built_in_op record ->
      sprintf "No_built \" %s \" for -- %s --"
        (name_of_operator record.bin_op)
        (record.ktype |> string_of_ktype)
  | Incompatible_Type record ->
      sprintf "Incompatible_Type for \" %s \" -- lhs = %s : rhs = %s"
        (record.bin_op |> name_of_operator)
        (record.lhs |> string_of_ktype)
        (record.rhs |> string_of_ktype)
  | Operator_not_found record ->
      sprintf "No operator \" %s \" for -- %s --"
        (name_of_operator record.bin_op)
        (record.ktype |> string_of_ktype)
  | Too_many_operator_declaration record ->
      sprintf "Too many \" %s \" declaration for %s "
        (name_of_operator record.bin_op)
        (record.ktype |> string_of_ktype)
  | Not_Boolean_operand_in_And -> "Not_Boolean_operand_in_And"
  | Not_Boolean_operand_in_Or -> "Not_Boolean_operand_in_Or"
  | Invalid_Uminus_for_Unsigned_integer size ->
      sprintf "Invalid_Uminus_for_Unsigned_integer for u%s"
        (string_of_isize size)

let string_of_switch_error =
  let open Ast.Error in
  let open Printf in
  function
  | Duplicated_case name -> sprintf "Duplicated_case -- variant : %s --" name
  | Not_enum_type_in_switch_Expression e ->
      sprintf "Not_enum_type_in_switch_Expression -- found : %s --"
        (string_of_ktype e)
  | Not_all_cases_handled missing_cases ->
      sprintf "Not_all_cases_handled : missing cases :\n  %s"
        (missing_cases
        |> List.map (fun (variant, kts) ->
               sprintf "%s(%s)" variant
                 (kts |> List.map string_of_ktype |> String.concat ", "))
        |> String.concat "\n  ")
  | Variant_not_found { enum_decl; variant } ->
      sprintf "Variant_not_found %s in %s" variant enum_decl.enum_name
  | Mismatched_Assoc_length { variant; expected; found } ->
      sprintf "Mismatched_Assoc_length variant %s %s" variant
        (string_of_expected_found (`int (expected, found)))
  | Incompatible_Binding (lhs, rhs) ->
      sprintf "Incompatible_Binding between: \n-> %s\n-> %s"
        (lhs
        |> List.map (fun (id, ktype) ->
               sprintf "%s: %s" id (string_of_ktype ktype))
        |> String.concat ", ")
        (rhs
        |> List.map (fun (id, ktype) ->
               sprintf "%s: %s" id (string_of_ktype ktype))
        |> String.concat ", ")
  | Binded_identifier_already_exist s ->
      sprintf "Binded_identifier_already_exist_in_env : %s" s

let string_of_built_in_func_error =
  let open Ast.Error in
  let open Printf in
  function
  | Unknow_built_function fn_name ->
      sprintf "Unknow_built_function : %s" fn_name
  | Wrong_parameters { fn_name; expected; found } ->
      sprintf "Wrong_parameters for %s : %s" fn_name
        (string_of_expected_found (`ktype (expected, found)))
  | Mismatched_Parameters_Length { fn_name; expected; found } ->
      sprintf "Mismatched_Parameters_Length for %s : %s" fn_name
        (string_of_expected_found (`int (expected, found)))
  | Found_no_Integer { fn_name; found } ->
      sprintf "Found_no_Integer for %s : expected -- any Integer : found %s --"
        fn_name (string_of_ktype found)

let string_of_ast_error =
  let open Ast.Error in
  let open Printf in
  function
  | Bin_operator_Different_type -> "Bin_operator_Different_type"
  | Wrong_Assoc_Length_for_Parametrics record ->
      sprintf "Wrong_Assoc_Length_for_Parametrics for %s -> %s"
        (string_of_ktype record.ktype)
        (string_of_expected_found (`int (record.expected, record.found)))
  | Undefined_Identifier s -> sprintf "Undefined_Identifier : %s" s
  | Undefined_Const s -> sprintf "Undefined_Const : %s" s
  | Undefined_Struct s -> sprintf "Undefined_Struct : %s" s
  | Unbound_Module s -> sprintf "Unbound_Module : %s" s
  | Struct_Error s -> string_of_struct_error s
  | Enum_Error e -> string_of_enum_error e
  | Statement_Error e -> string_of_statement_error e
  | Func_Error e -> string_of_function_error e
  | Operator_Error e -> string_of_operator_error e
  | Switch_error e -> string_of_switch_error e
  | Builtin_Func_Error e -> string_of_built_in_func_error e
  | Uncompatible_type e ->
      sprintf "Uncompatible_type %s"
        (string_of_expected_found (`ktype (e.expected, e.found)))
  | Uncompatible_type_If_Else e ->
      sprintf "Uncompatible_type_If_Else %s"
        (string_of_expected_found (`ktype (e.if_type, e.else_type)))
  | Not_Boolean_Type_Condition e ->
      sprintf "Not_Boolean_Type_Condition -- found : %s : expected : bool --"
        (string_of_ktype e.found)
  | Impossible_field_Access e ->
      sprintf "Impossible_field_Access : %s" (string_of_ktype e)
  | Enum_Access_field record ->
      sprintf "Enum doesn't have field : %s for enum : %s" record.field
        record.enum_decl.enum_name
  | Unvalid_Deference -> sprintf "Unvalid_Deference"

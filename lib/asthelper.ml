open Ast;;
open Printf;;

let char_of_signedness = function
| Signed -> 's'
| Unsigned -> 'u'
;;

let string_of_isize = function
| I8 -> "8"
| I16 -> "16"
| I32 -> "32"
| I64 -> "64"

let f = sprintf "%Ld"
let rec string_of_ktype = function
| TParametric_identifier {module_path; parametrics_type; name} -> sprintf "(%s)%s %s" (parametrics_type |> List.map string_of_ktype |> String.concat ", ") (module_path) (name)
| TType_Identifier {module_path; name} -> sprintf "%s%s" (if module_path = "" then "" else sprintf "%s::" module_path) name
| TInteger (sign, size) -> sprintf "%c%s" (char_of_signedness sign) (string_of_isize size)
| TPointer ktype -> sprintf "*%s" (string_of_ktype ktype)
| TTuple ktypes -> sprintf "(%s)" (ktypes |> List.map string_of_ktype |> String.concat ", ")
| TFunction (parameters, r_type) -> sprintf "(%s) -> %s" (parameters |> List.map string_of_ktype |> String.concat ", ") (string_of_ktype r_type)
| TString_lit -> "stringl"
| TBool -> "bool"
| TUnit -> "unit"
| TUnknow -> "unknow"
| TFloat -> "f64"

module Module = struct

  let retrieve_enum_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NEnum e -> Some e | _ -> None)

  let retrieve_struct_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NStruct s -> Some s | _ -> None)

  let retrieve_external_func_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NExternFunc s -> Some s | _ -> None)

  let retrieve_func_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NFunction s -> Some s | _ -> None)

  let retrieve_const_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NConst s -> Some s | _ -> None)

  let retrieve_sig_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NSigFun s -> Some s | _ -> None)

  let retrieve_type_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NEnum e -> Some (Ast.Type_Decl.decl_enum e) | Ast.NStruct s -> Some (Ast.Type_Decl.decl_struct s) | _ -> None)

  let retrieve_functions_decl = function
  | Ast.Mod nodes -> nodes |> List.filter_map (fun node -> match node with Ast.NExternFunc e -> Some (Ast.Function_Decl.Decl_External e) | Ast.NFunction e -> Some (Ast.Function_Decl.decl_kosu_function e) | _ -> None)

  let retrieve_function_decl_from_name_and_types fn_name parameters_types r_types _module = 
  _module
  |> retrieve_func_decl
  |> Util.Occurence.find_occurence (fun fn_decl -> 
    fn_decl.fn_name = fn_name && Ast.Type.are_compatible_type r_types fn_decl.return_type && Util.are_same_lenght fn_decl.parameters parameters_types &&
    List.for_all2 (Ast.Type.are_compatible_type) (fn_decl.parameters |> List.map (fun (_, kt) -> kt) ) parameters_types
  )

  let type_decl_occurence (type_name: string) (module_definition: Ast._module) = 
    module_definition |> retrieve_type_decl |> Util.Occurence.find_occurence (function
    | Ast.Type_Decl.Decl_Enum e -> e.enum_name = type_name
    | Ast.Type_Decl.Decl_Struct s -> s.struct_name = type_name
    )

  let function_decl_occurence (fn_name: string) (module_definition: Ast._module) = 
    module_definition |> retrieve_functions_decl |> Util.Occurence.find_occurence (function
    | Ast.Function_Decl.Decl_External e -> fn_name = e.sig_name
    | Ast.Function_Decl.Decl_Kosu_Function e -> fn_name = e.fn_name
    )
end

module Program = struct
  type t = Ast.program

  let module_of_string_opt mod_name (program: t) = 
    program 
    |> List.filter_map ( fun (mp: Ast.module_path) -> if mod_name = mp.path then Some mp._module else None) 
    |> function [] -> None | t::_ -> Some t
  let module_of_string mod_name (program: t) = 
    program 
    |> List.filter_map ( fun (mp: Ast.module_path) -> if mod_name = mp.path then Some mp._module else None) 
    |> List.hd

  let find_struct_decl_opt (current_module_name: string) (module_path: string) (struct_name: string) (program: t) = 
    Result.bind (
    (if module_path = "" then Some (program |> module_of_string current_module_name) else program |> module_of_string_opt current_module_name)
    |> Option.map Module.retrieve_struct_decl 
    |> Option.to_result ~none:(Ast.Error.Unbound_Module module_path)
    )
    (fun structs -> 
      structs |> List.find_opt (fun s -> s.struct_name = struct_name) |> Option.to_result ~none:(Ast.Error.Undefined_Struct struct_name)
    )
  
  let find_enum_decl_opt (current_module_name) (module_enum_path) (enum_name_opt: string option) (variant: string) (assoc_exprs: kexpression list) (program: t) =
    match (if module_enum_path = "" then Some (program |> module_of_string current_module_name) else program |> module_of_string_opt current_module_name) with
    | None -> (Ast.Error.Unbound_Module module_enum_path) |> Either.left |> Result.error
    | Some _module -> 
      _module 
      |> Module.retrieve_enum_decl
      |> Util.Occurence.find_occurence (fun enum_decl -> 
        match enum_name_opt with
        | None -> enum_decl.variants |> List.exists (fun (variant_decl, assoc_type) -> variant_decl = variant && Util.are_same_lenght assoc_exprs assoc_type)
        | Some enum_name -> enum_name = enum_decl.enum_name && enum_decl.variants |> List.exists (fun (variant_decl, assoc_type) -> variant_decl = variant && Util.are_same_lenght assoc_exprs assoc_type)
        )
      |> function
      | Empty -> Not_found |> Either.right |> Result.error
      | Util.Occurence.Multiple _ -> Util.Occurence.Too_Many_Occurence |> Either.right |> Result.error
      | One enum_decl -> enum_decl |> Result.ok

    let find_module_of_ktype ktype_def_path current_module program = 
      match ktype_def_path with
      | "" -> program |> Util.Occurence.find_occurence (fun module_path -> module_path.path = current_module)
      | s  -> program |> Util.Occurence.find_occurence (fun module_path -> module_path.path = s)

    let find_module_of_function fn_def_path current_module program = 
      match fn_def_path with
      | "" -> program |> Util.Occurence.find_occurence (fun module_path -> module_path.path = current_module)
      | s  -> program |> Util.Occurence.find_occurence (fun module_path -> module_path.path = s)

    (**
    Find type declaration from ktype
    @raise Not_found : if no type declaration was found
    @raise Too_Many_Occurence: if several type declaration matching was found
    *)
    let find_type_decl_from_ktype ktype_def_path ktype_name current_module program = 
      find_module_of_ktype ktype_def_path current_module program
      |> Util.Occurence.one 
      |> fun m -> m._module
      |> Module.type_decl_occurence ktype_name
      |> Util.Occurence.one

    (**
    Find function declaration from function name
    @raise Not_found : if no function declaration was found
    @raise Too_Many_Occurence: if several function declaration matching was found
    *)
    let find_function_decl_from_fn_name fn_def_path fn_name current_module program =
      program 
      |> find_module_of_function fn_def_path current_module
      |> Util.Occurence.one
      |> fun m -> m._module
      |> Module.function_decl_occurence fn_name
      |> Util.Occurence.one

    let rec is_c_type (current_mod_name) (type_decl: Ast.Type_Decl.type_decl) program = 
      match type_decl with
      | Decl_Enum e -> e.generics = [] && e.variants |> List.for_all (fun (_, assoc) -> assoc = [] )
      | Decl_Struct s -> 
        if s.generics = [] then false 
        else s.fields |> List.for_all (fun (_, ktype) -> 
          match ktype with
          | TParametric_identifier _ -> false
          | TType_Identifier { module_path; name } -> begin
            try 
              let type_decl =  (find_type_decl_from_ktype module_path name current_mod_name program) in 
              is_c_type current_mod_name type_decl program
            with _ -> false
          end
          | TFunction _ -> false
          | TTuple _ -> false
          | _ -> true
        )
  
    let is_c_type_from_ktype (current_mod_name) (ktype: ktype) program = 
      match ktype with
      | TParametric_identifier _ -> false
      | TType_Identifier { module_path; name } -> begin
        try 
          let type_decl =  (find_type_decl_from_ktype module_path name current_mod_name program) in 
          is_c_type current_mod_name type_decl program
        with _ -> false
      end
      | TFunction _ -> false
      | TTuple _ -> false
      | _ -> true

      let find_function_exact fn_name ktypes_parameters return_type (program: module_path list) = 
        let open Util.Occurence in
        program
        |> List.map (fun t -> t._module)
        |> List.map (Module.retrieve_function_decl_from_name_and_types fn_name ktypes_parameters return_type)
        |> List.filter (function | One _ -> true | _ -> false)

    let is_valid_add_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      match lhs with
      | TPointer _ -> (match rhs with TInteger _ -> `built_in_ptr_valid | _ -> `invalid_add_pointer)
      | _ -> begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "add" [kt;kt] kt with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_add_for_built_in
      end

    let is_valid_minus_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      match lhs with
      | TPointer _ -> (match rhs with TInteger _ -> `built_in_ptr_valid | _ -> `invalid_add_pointer)
      | _ -> begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "minus" [kt;kt] kt with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_minus_for_built_in
      end

    let is_valid_mult_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "mult" [kt;kt] kt with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_mult_for_built_in
      end

    let is_valid_div_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "div" [kt;kt] kt with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_div_for_built_in
      end

  let is_valid_mod_operation (lhs) (rhs) program = 
    let open Util.Occurence in
    begin 
      if lhs <> rhs then `diff_types
      else match lhs with
        | TType_Identifier _ as kt -> (
          match program |> find_function_exact "mod" [kt;kt] kt with
          [] -> `no_function_found
          | t::[] -> `valid (t |> one)
          | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
        )
        | TInteger _ -> `built_in_valid
        | _ -> `no_mod_for_built_in
    end

    let is_valid_bitwiseor_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "bitwiseor" [kt;kt] kt with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ -> `built_in_valid
          | _ -> `no_bitwiseor_for_built_in
      end
    let is_valid_bitwiseand_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "bitwiseand" [kt;kt] kt with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ -> `built_in_valid
          | _ -> `no_bitwiseand_for_built_in
      end
    let is_valid_bitwisexor_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "bitwisexor" [kt;kt] kt with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ -> `built_in_valid
          | _ -> `no_bitwisexor_for_built_in
      end

    let is_valid_shiftleft_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "shiftleft" [kt;kt] kt with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ -> `built_in_valid
          | _ -> `no_shiftleft_for_built_in
      end

  let is_valid_shiftright_operation (lhs) (rhs) program = 
    let open Util.Occurence in
    begin 
      if lhs <> rhs then `diff_types
      else match lhs with
        | TType_Identifier _ as kt -> (
          match program |> find_function_exact "shiftright" [kt;kt] kt with
          [] -> `no_function_found
          | t::[] -> `valid (t |> one)
          | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
        )
        | TInteger _ -> `built_in_valid
        | _ -> `no_shiftright_for_built_in
    end
  let is_valid_equal_operation (lhs) (rhs) program = 
    let open Util.Occurence in
    begin 
      if lhs <> rhs then `diff_types
      else match lhs with
        | TType_Identifier _ as kt -> (
          match program |> find_function_exact "equal" [kt;kt] TBool with
          [] -> `no_function_found
          | t::[] -> `valid (t |> one)
          | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
        )
        | TInteger _ | TBool | TFloat | TPointer _ -> `built_in_valid
        | _ -> `no_equal_for_built_in
        (* Better handle the tuple *)
    end

    let is_valid_sup_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "sup" [kt;kt] TBool with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_sup_for_built_in
      end

    let is_valid_supeq_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "sup_eq" [kt;kt] TBool with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_supeq_for_built_in
      end

    let is_valid_inf_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "inf" [kt;kt] TBool with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_inf_for_built_in
      end

    let is_valid_infeq_operation (lhs) (rhs) program = 
      let open Util.Occurence in
      begin 
        if lhs <> rhs then `diff_types
        else match lhs with
          | TType_Identifier _ as kt -> (
            match program |> find_function_exact "inf_eq" [kt;kt] TBool with
            [] -> `no_function_found
            | t::[] -> `valid (t |> one)
            | list -> `to_many_declaration (list |> List.filter_map (function | Multiple s -> Some s | _ -> None))
          )
          | TInteger _ | TFloat -> `built_in_valid
          | _ -> `no_infeq_for_built_in
      end
      

end

module Statement = struct

  let rec string_of_kbody = function
  | (statements: kstatement list), (expr: kexpression) -> begin 
  sprintf "{\n  %s  %s\n}"
  (statements |> List.map string_of_kstatement |> String.concat "\n  ")
  (string_of_kexpression expr)
  end
  and string_of_kstatement = function
  | SDeclaration { is_const; variable_name; explicit_type ; expression } -> 
    sprintf "%s %s : %s = %s;"
    (if is_const then "const" else "var")
    (variable_name)
    (explicit_type |> Option.map (string_of_ktype) |> Option.value ~default: "")
    (expression |> string_of_kexpression)
  | SAffection (id, expression) -> sprintf "%s = %s;" (id) (expression |> string_of_kexpression)
  | SDiscard (expr) -> sprintf "discard %s;" (string_of_kexpression expr)
  and string_of_kexpression = function
  | Empty -> "empty"
  | True -> "true"
  | False -> "false"
  | EInteger (sign, _, value) -> (match sign with Signed -> sprintf "%Ld" value | Unsigned -> sprintf "%Lu" value)
  | EFloat f -> string_of_float f
  | EBin_op bin -> string_of_kbin_op bin
  | EUn_op un -> string_of_kunary_op un
  | ESizeof e -> let s = begin match e with Either.Left t -> string_of_ktype t | Either.Right expr -> string_of_kexpression expr end in
  sprintf "sizeof(%s)" s
  | EString s -> s
  | EAdress x -> sprintf "&%s" x
  | EDeference (indirection, id) -> sprintf "%s%s" (Util.string_of_chars indirection '*') id
  | EIdentifier { modules_path; identifier } | EConst_Identifier {modules_path; identifier}  -> sprintf "%s%s" (Util.string_of_module_path modules_path) identifier
  | EFieldAcces {first_expr; fields} -> sprintf "(%s)->%s" (string_of_kexpression first_expr) (fields |> String.concat "->")
  | EStruct { modules_path; struct_name; fields } -> 
    sprintf "%s%s { %s }" 
    (if modules_path = "" then "" else sprintf "%s::" modules_path) 
    struct_name 
    (fields |> List.map (fun (id, expr) -> sprintf "%s: %s" (id) (string_of_kexpression expr)) |> String.concat ",")
  | EEnum {modules_path; enum_name; variant; assoc_exprs} -> 
    sprintf "%s%s.%s%s"
    (Util.string_of_module_path modules_path)
    (enum_name |> Option.value ~default: " ")
    (variant)
    (if assoc_exprs = [] then "" else sprintf "(%s)" (assoc_exprs |> List.map string_of_kexpression |> String.concat ", "))
  | ETuple exprs -> sprintf "(%s)" (exprs |> List.map string_of_kexpression |> String.concat ", ")
  | EFunction_call {modules_path; generics_resolver; fn_name; parameters} -> 
    sprintf "%s%s%s(%s)"
    (Util.string_of_module_path modules_path)
    (fn_name)
    (generics_resolver |> Option.map (fun kts -> sprintf "::<%s>" (kts |> List.map string_of_ktype |> String.concat ", ")) |> Option.value ~default: "" )
    (parameters |> List.map string_of_kexpression |> String.concat ", ")
  | EIf (expression, if_body, else_body) -> 
    sprintf "if %s %s else %s"
    (string_of_kexpression expression)
    (string_of_kbody if_body)
    (string_of_kbody else_body)  
  | ECases { cases ; else_case } -> 
    sprintf "cases {\n %s else => %s}"
    ( cases |> List.map (fun (expr, kbody) -> sprintf "%s => %s" (expr |> string_of_kexpression) (string_of_kbody kbody)) |> String.concat "\n")
    (string_of_kbody else_case)
  | ESwitch { expression; cases; wildcard_case } -> 
    sprintf "switch %s {%s\n%s}"
    (string_of_kexpression expression)
    (cases |> List.map (fun (sc, kbody) -> sprintf "%s => %s" (sc |> List.map string_of_switch_case |> String.concat ", ") (string_of_kbody kbody)) |> String.concat "\n")
    (match wildcard_case with None -> "" | Some kbody -> sprintf "_ => %s" (string_of_kbody kbody))
  and string_of_kbin_op = function
  | BAdd (lhs, rhs) -> sprintf "(%s + %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BMinus (lhs, rhs) -> sprintf "(%s - %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BMult (lhs, rhs) -> sprintf "(%s * %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BDiv (lhs, rhs) -> sprintf "(%s / %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BMod (lhs, rhs) -> sprintf "(%s %% %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BBitwiseOr (lhs, rhs) -> sprintf "(%s | %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BBitwiseXor (lhs, rhs) -> sprintf "(%s ^ %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BBitwiseAnd (lhs, rhs) -> sprintf "(%s & %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BShiftLeft (lhs, rhs) -> sprintf "(%s << %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BShiftRight (lhs, rhs) -> sprintf "(%s >> %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BAnd (lhs, rhs) -> sprintf "(%s && %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BOr (lhs, rhs) -> sprintf "(%s || %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BInf (lhs, rhs) -> sprintf "(%s < %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BInfEq (lhs, rhs) -> sprintf "(%s <= %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BSup (lhs, rhs) -> sprintf "(%s > %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BSupEq (lhs, rhs) -> sprintf "(%s >= %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BEqual (lhs, rhs) -> sprintf "(%s == %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  | BDif (lhs, rhs) -> sprintf "(%s != %s)" (string_of_kexpression lhs) (string_of_kexpression rhs)
  and string_of_kunary_op = function
  | UMinus expr -> sprintf "-(%s)" (string_of_kexpression expr)
  | UNot expr -> sprintf "!(%s)" (string_of_kexpression expr)
  and string_of_switch_case = function
  | SC_Identifier s -> s
  | SC_Integer_Literal (sign, size, value) -> string_of_kexpression (EInteger (sign, size, value))
  | SC_Enum_Identifier { variant } -> "."^variant
  | SC_Enum_Identifier_Assoc { variant; assoc_ids } -> 
    sprintf "%s(%s)"
    variant
    (assoc_ids |> List.map (Option.value ~default: "_") |> String.concat "," )

  module BinOp = struct
    type t = kbin_op
    let is_boolean = function
    | BAnd _  | BOr _ | BInf _ | BInfEq _ | BSup _ | BSupEq _ | BEqual _ | BDif _ -> true
    | _ -> false

    let is_arithmetic bin = bin |> is_boolean |> not
  end
end 
module Enum = struct
  type t = enum_decl

  let string_of_enum_variant (variant: (string * Ast.ktype list)) = 
    let name, types = variant in
    sprintf "%s (%s)" name (types |> List.map string_of_ktype |> String.concat ", ")

  let string_of_enum_decl (enum_decl: t) = 
    sprintf "enum (%s) %s := { %s }"
    (enum_decl.generics |> String.concat ", ")
    (enum_decl.enum_name)
    (enum_decl.variants |> List.map string_of_enum_variant |> String.concat ", ")

  let rec is_assoc_type_contains_generic (ktype: ktype) (enum_decl: t) = 
    enum_decl.generics |> List.exists (fun g -> match ktype with
    | TType_Identifier { module_path; name } -> module_path = "" && name = g
    | TParametric_identifier { module_path = _ ; parametrics_type; name = _ } -> parametrics_type |> List.exists (fun kt -> is_assoc_type_contains_generic kt enum_decl)
    | TPointer kt -> is_assoc_type_contains_generic kt enum_decl
    | TTuple kts -> kts |> List.exists (fun kt -> is_assoc_type_contains_generic kt enum_decl)
    | _ -> false
    )
  let rec are_type_compatible (init_type: ktype) (expected_type: ktype) (enum_decl: t) = 
    (* let () = Printf.printf "expected : %s :: found : %s\n" (string_of_ktype expected_type) (string_of_ktype init_type) in *)
    match init_type, expected_type with
    | TType_Identifier {module_path = init_path; name = init_name}, TType_Identifier {module_path = exp_path; name = exp_name } -> begin
      enum_decl.generics |> List.mem exp_name || (init_path = exp_path && init_name = exp_name)
    end
    | TParametric_identifier {module_path = init_path; parametrics_type = init_pt; name = init_name}, 
      TParametric_identifier {module_path = exp_path; parametrics_type = exp_pt; name = exp_name} -> 
        if init_path <> exp_path || init_name <> exp_name || List.compare_lengths init_pt exp_pt <> 0 then false
        else begin
          List.combine init_pt exp_pt |> List.for_all (fun (i,e) -> are_type_compatible i e enum_decl)
        end
    | TPointer (l_type), TPointer (r_type) -> are_type_compatible l_type r_type enum_decl
    | TTuple (lhs), TTuple (rhs) -> Util.are_same_lenght lhs rhs && (List.combine lhs rhs |> List.for_all (fun (lhs_type, rhs_type) -> are_type_compatible lhs_type rhs_type enum_decl))
    | TUnknow, _ -> true
    | _ , TType_Identifier { module_path; name } when module_path = "" && enum_decl.generics |> List.mem name ->  true
    | lhs, rhs -> lhs = rhs

  let rec bind_generic_ktype generic_name new_type expected_type (enum_decl: t) = 
    match expected_type with
    | TType_Identifier { module_path; name } -> if module_path = "" && generic_name = name && enum_decl.generics |> List.mem generic_name then new_type else expected_type
    | TParametric_identifier { module_path; parametrics_type; name } -> TParametric_identifier {
      module_path;
      parametrics_type = parametrics_type |> List.map (fun kt -> bind_generic_ktype generic_name new_type kt enum_decl);
      name
    }
    | _ as t -> t

  let bind_single_generic (generic_name) (new_type) (enum_decl: t) = 
    {
      enum_name = enum_decl.enum_name;
      generics = enum_decl.generics |> List.filter ((<>) generic_name);
      variants = enum_decl.variants |> List.map (fun (variant, assoc_type) -> 
        (variant, 
        assoc_type |> List.map (fun kt -> bind_generic_ktype generic_name new_type kt enum_decl)
        )
      )
    }

    let rec is_type_compatible_hashgen (generic_table) (init_type: ktype) (expected_type: ktype) (enum_decl: t) = 
      match init_type, expected_type with
      | kt , TType_Identifier { module_path = "" ; name } when begin   
      match Hashtbl.find_opt generic_table name with
      | None -> false
      | Some (_, find_kt) -> 
          if find_kt = TUnknow then let () = Hashtbl.replace generic_table name (enum_decl.generics |> Util.ListHelper.index_of (( = ) name ), kt ) in true
          else false
      end -> true
      
      | TType_Identifier {module_path = init_path; name = init_name}, TType_Identifier {module_path = exp_path; name = exp_name } -> begin
        enum_decl.generics |> List.mem exp_name || (init_path = exp_path && init_name = exp_name)
      end
      | TParametric_identifier {module_path = init_path; parametrics_type = init_pt; name = init_name}, 
        TParametric_identifier {module_path = exp_path; parametrics_type = exp_pt; name = exp_name} -> 
          if init_path <> exp_path || init_name <> exp_name || List.compare_lengths init_pt exp_pt <> 0 then false
          else begin
            List.combine init_pt exp_pt |> List.for_all (fun (i,e) -> is_type_compatible_hashgen generic_table i e enum_decl)
          end
      | TUnknow, _ -> true
      | TPointer (l_type), TPointer (r_type) -> is_type_compatible_hashgen generic_table l_type r_type enum_decl
      | TTuple (lhs), TTuple (rhs) -> Util.are_same_lenght lhs rhs && (List.combine lhs rhs |> List.for_all (fun (lhs_type, rhs_type) -> is_type_compatible_hashgen generic_table lhs_type rhs_type enum_decl))
      | lhs, rhs -> lhs = rhs

    let to_ktype_hash generics module_def_path (enum_decl: t) = 
      if enum_decl.generics = [] then TType_Identifier { module_path = module_def_path; name = enum_decl.enum_name }
      else TParametric_identifier {
        module_path = module_def_path;
        parametrics_type = (
          generics |> Hashtbl.to_seq |> List.of_seq |> List.sort ( fun ( _, (i, _)) ( _, (b, _)) -> compare i b) |> List.map (fun ( _, (_, kt)) -> kt)
        );
        name = enum_decl.enum_name
      }

  let rec find_generic_name_from_ktype ~generics_result ktype (enum_decl: t) =
    match ktype with
    | TType_Identifier { module_path; name } -> if module_path = "" && enum_decl.generics |> List.mem name then name::generics_result else generics_result
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } -> 
      parametrics_type 
      |> List.map (fun kt -> find_generic_name_from_ktype ~generics_result kt enum_decl)
      |> List.concat
      |> List.fold_left (fun acc value -> if acc |> List.mem value then acc else value::acc) []
    | _ -> generics_result

  (**
    Returns the generic name for the associated type of an variant at a given position
    Returns [None] if [variant_name] not in the list of enum variant or [assoc_position > lenght of variant assoc type] or type found isn't generic
  *)
  let find_generic_name_from_positon ~assoc_position (variant_name: string) (enum_decl: t) = 
    enum_decl.variants
    |> List.find_map (fun (v, assoc_types) -> if v = variant_name then Some assoc_types else None)
    |> function
    | None -> None
    | Some assoc_types -> assoc_position |> List.nth_opt assoc_types
    |> function
    | None -> None
    | Some kt -> Some (find_generic_name_from_ktype ~generics_result:[] kt enum_decl) 
  
  let rec infer_generics ~assoc_position (variant_zipped: (ktype * ktype) list) (generics: (ktype * bool) list) enum_decl = 
    match variant_zipped with
    | [] -> generics
    | (init_ktype, expected_ktype)::q -> 
      if is_assoc_type_contains_generic expected_ktype enum_decl then 
        if are_type_compatible init_ktype expected_ktype enum_decl then
        enum_decl
        |> infer_generics ~assoc_position:(assoc_position + 1) q ( generics |> List.map (fun (kt, true_type) -> 
          match kt with
          | TType_Identifier { module_path = _; name } -> if enum_decl.generics |> List.mem name && not true_type
            then (bind_generic_ktype name init_ktype expected_ktype enum_decl, true) 
            else (kt, true_type)
          | _ -> raise (Failure "Generics cannot have a different type than TIdentifier" )
          ))
        else failwith ""
      else
      infer_generics ~assoc_position:(assoc_position + 1) q generics enum_decl

  let is_valide_assoc_type_init ~init_types ~expected_types enum_decl = 
    Util.are_same_lenght init_types expected_types 
    && (List.combine init_types expected_types |> List.for_all (fun (init_type, expected_type) -> are_type_compatible init_type expected_type enum_decl))

  let is_simple_enum (enum_decl: t) = 
    enum_decl.variants |> List.for_all (fun (_, assoc) -> assoc = [])
  let is_tagged_union (enum_decl: t) = 
    enum_decl.variants |> List.exists (fun (_, assoc_type) -> assoc_type <> [] )
  let contains_generics (enum_decl: t) = enum_decl.generics <> []
end

module Struct = struct
  type t = struct_decl

  let find_struct_decl_from_name (current_module_name: string) (prog : program) (module_path: string) (struct_name: string) = 
    let structs_opt = (if module_path = "" then Some (prog |> Program.module_of_string current_module_name) else prog |> Program.module_of_string_opt current_module_name)
    |> Option.map Module.retrieve_struct_decl in
    
    match structs_opt with
    | None -> Error (Ast.Error.Unbound_Module module_path)
    | Some structs ->
      structs |> List.find_opt (fun s -> s.struct_name = struct_name) |> Option.to_result ~none:(Ast.Error.Undefined_Struct struct_name)
  ;;

  let string_of_struct_decl (struct_decl: t) = 
    sprintf "struct (%s) %s := { %s }" 
    (struct_decl.generics |> String.concat ", ")
    (struct_decl.struct_name)
    (struct_decl.fields |> List.map (fun (field, t) -> sprintf "%s : %s" (field) (string_of_ktype t)) |> String.concat ", " )
  let contains_generics (struct_decl: t) = struct_decl.generics <> []

  (**
  @raise Type_Error: 
  @raise Not_found: raised if field or struct declaration not found
  *)
  let rec resolve_fields_access (current_mod_name: string) (program: program) (fields: string list) (ktype: ktype) = 
    match fields with
    | [] -> failwith "Not supposed to be reached"
    | t::[] -> begin 
      match ktype with
      | TType_Identifier { module_path; name } -> (
        match find_struct_decl_from_name current_mod_name program module_path name with
        | Error e -> e |> Error.ast_error |> raise
        | Ok _struct -> _struct.fields |> List.assoc t
        )
      | _ -> raise (Ast.Error.ast_error (Impossible_field_Access ktype))
    end
    | t::q -> begin 
      match ktype with
      | TType_Identifier { module_path; name } ->  begin
        match find_struct_decl_from_name current_mod_name program module_path name with
        | Error e -> e |> Error.ast_error |> raise
        | Ok _struct -> resolve_fields_access current_mod_name program q (_struct.fields |> List.assoc t)
      end
      | _ -> raise (Ast.Error.ast_error (Impossible_field_Access ktype))
    end

  
  

  let find_field_type_opt (field: string) (struct_decl: t) = struct_decl.fields |> List.assoc_opt field

  (** 
  @raise Not_found    
  *)
  let find_field_type (field: string) (struct_decl: t) = struct_decl.fields |> List.assoc field

  let rec resolve_single_generics (generic_name: string) (mapped_type: ktype) (expected_type: ktype) =
    match expected_type with
    | TType_Identifier { module_path = _ ; name} -> if name = generic_name then mapped_type else expected_type
    | TParametric_identifier { module_path; parametrics_type; name } -> begin 
      TParametric_identifier { 
        module_path; 
        parametrics_type = parametrics_type |> List.map (resolve_single_generics generic_name mapped_type); 
        name 
      }
    end
    | t -> t
  let rec is_type_generic ktype (struct_decl: t) = 
    match ktype with
    | TType_Identifier { module_path ; name} ->  module_path = "" && struct_decl.generics |> List.mem name
    | TParametric_identifier { module_path = _; parametrics_type ; name = _ } -> parametrics_type |> List.exists ( fun kt -> is_type_generic kt struct_decl)
    | _ -> false
  let is_field_generic_opt field (struct_decl : t) =
    struct_decl.fields |> List.assoc_opt field |> Option.map ( fun kt -> is_type_generic kt struct_decl)

  (**
  @raise Not_found 
  *)
  let is_field_generic field (struct_decl : t) =
    struct_decl.fields |> List.assoc field |> fun kt -> is_type_generic kt struct_decl
  let bind_generic (generic_name) (new_type: ktype) (struct_decl: t) = 
    {
      struct_name = struct_decl.struct_name;
      generics = struct_decl.generics |> List.filter ( (<>) generic_name ) ;
      fields = struct_decl.fields |> List.map ( fun (f,t) -> (f, resolve_single_generics generic_name new_type t))    ;
    }

  let rec is_type_compatible_hashgen (generic_table) (init_type: ktype) (expected_type: ktype) (struct_decl: t) = 
    match init_type, expected_type with
    | kt , TType_Identifier { module_path; name } when begin   
    match Hashtbl.find_opt generic_table name with
    | None -> if module_path = "" && struct_decl.generics |> List.mem name 
        then let () = Hashtbl.replace generic_table name (struct_decl.generics |> Util.ListHelper.index_of (( = ) name ), kt ) in true
        else false 
    | Some (_, find_kt) -> find_kt = kt
    end -> true
    
    | TType_Identifier {module_path = init_path; name = init_name}, TType_Identifier {module_path = exp_path; name = exp_name } -> begin
      struct_decl.generics |> List.mem exp_name || (init_path = exp_path && init_name = exp_name)
    end
    | TParametric_identifier {module_path = init_path; parametrics_type = init_pt; name = init_name}, 
      TParametric_identifier {module_path = exp_path; parametrics_type = exp_pt; name = exp_name} -> 
        if init_path <> exp_path || init_name <> exp_name || List.compare_lengths init_pt exp_pt <> 0 then false
        else begin
          List.combine init_pt exp_pt |> List.for_all (fun (i,e) -> is_type_compatible_hashgen generic_table i e struct_decl)
        end
    | TPointer lhs, TPointer rhs -> is_type_compatible_hashgen generic_table lhs rhs struct_decl
    | TTuple lhs, TTuple rhs -> Util.are_same_lenght lhs rhs && List.for_all2 (fun init exptected -> is_type_compatible_hashgen generic_table init exptected struct_decl) lhs rhs
    | TUnknow, _ -> true
    | lhs, rhs -> lhs = rhs

  let rec is_type_compatible (init_type: ktype) (expected_type: ktype) (struct_decl: t) = 
    match init_type, expected_type with
    | _ , TType_Identifier { module_path; name } when module_path = "" && struct_decl.generics |> List.mem name -> true
    | TType_Identifier {module_path = init_path; name = init_name}, TType_Identifier {module_path = exp_path; name = exp_name } -> begin
      struct_decl.generics |> List.mem exp_name || (init_path = exp_path && init_name = exp_name)
    end
    | TParametric_identifier {module_path = init_path; parametrics_type = init_pt; name = init_name}, 
      TParametric_identifier {module_path = exp_path; parametrics_type = exp_pt; name = exp_name} -> 
        if init_path <> exp_path || init_name <> exp_name || List.compare_lengths init_pt exp_pt <> 0 then false
        else begin
          List.combine init_pt exp_pt |> List.for_all (fun (i,e) -> is_type_compatible i e struct_decl)
        end
    | TUnknow, _ -> true
    | lhs, rhs -> lhs = rhs
  let rec find_generic_name_from_ktype (ktype: ktype) (struct_decl: t) = 
    match ktype with
    | TType_Identifier { module_path = _ ; name } as tti -> if struct_decl |> is_type_generic tti then Some name else None
    | TParametric_identifier { module_path = _; parametrics_type; name = _ } -> begin 
      parametrics_type |> List.filter_map (fun k -> find_generic_name_from_ktype k struct_decl) |> function [] -> None | t::_ -> Some t
    end
    | _ -> None
  let retrieve_generic_name_from_field_opt (field: string) (struct_decl: t) =
    match struct_decl.fields |> List.assoc_opt field with None -> None | Some kt -> find_generic_name_from_ktype kt struct_decl
  let to_ktype module_def_path (struct_decl: t) = 
    if not (struct_decl |> contains_generics) then TType_Identifier { module_path = module_def_path; name = struct_decl.struct_name }
    else begin
    TParametric_identifier {
      module_path = module_def_path;
      parametrics_type = struct_decl.generics |> List.map (fun _ -> TUnknow);
      name = struct_decl.struct_name
    }
    end
  let rec is_cyclic_aux ktype_to_test module_def_path (struct_decl: t) =
    match ktype_to_test with
    | TType_Identifier _ as tti -> tti = (struct_decl |> to_ktype module_def_path)
    | TParametric_identifier { module_path; parametrics_type; name } -> (module_path = module_def_path && name = struct_decl.struct_name) || parametrics_type |> List.exists (fun k -> is_cyclic_aux k module_def_path struct_decl)
    | _ -> false
  let is_cyclic module_def_path (struct_decl: t) = 
    struct_decl.fields |> List.exists ( fun (_, ktype) -> is_cyclic_aux ktype module_def_path struct_decl )

  let to_ktype_hash generics module_def_path (struct_decl: t) = 
    if struct_decl.generics = [] then TType_Identifier { module_path = module_def_path; name = struct_decl.struct_name }
    else TParametric_identifier {
      module_path = module_def_path;
      parametrics_type = (
        generics |> Hashtbl.to_seq |> List.of_seq |> List.sort ( fun ( _, (i, _)) ( _, (b, _)) -> compare i b) |> List.map (fun ( _, (_, kt)) -> kt)
      );
      name = struct_decl.struct_name
    }

end

module ExternalFunc = struct 
  type t = external_func_decl

  let string_of_external_func_decl (efucn_decl: t) = 
    sprintf "external %s(%s%s) %s %s"
    (efucn_decl.sig_name)
    (efucn_decl.fn_parameters |> List.map string_of_ktype |> String.concat ",")
    (if efucn_decl.is_variadic then ";..." else "")
    (efucn_decl.r_type |> string_of_ktype)
    (efucn_decl.c_name |> Option.map (fun s -> sprintf " = %s" s) |> Option.value ~default: "")
end

module Function = struct
  type t = function_decl

  let string_of_func_decl (function_decl: t) = 
    sprintf "fn %s%s(%s)%s%s"
    (function_decl.fn_name)
    (if function_decl.generics = [] then "" else sprintf "<%s>" (function_decl.generics |> String.concat ", "))
    (function_decl.parameters |> List.map (fun (id, ktype) -> sprintf "%s: %s" (id) (string_of_ktype ktype)) |> String.concat ", ")
    (function_decl.return_type |> string_of_ktype)
    (function_decl.body |> Statement.string_of_kbody)



  let rec is_ktype_generic ktype (fn_decl: t) = 
    match ktype with
    | TParametric_identifier { module_path = _; parametrics_type ; name = _ } -> parametrics_type |> List.exists (fun kt -> is_ktype_generic kt fn_decl)
    | TType_Identifier { module_path = "" ; name } -> fn_decl.generics |> List.mem name
    | _ -> false

    let does_need_generic_resolver (function_decl : t) = 
      if function_decl.generics = [] then false
      else if function_decl.parameters |> List.length = 0 then true
      else function_decl |> is_ktype_generic function_decl.return_type

  (**
    @return true if parameter contains generics, false if not and None if paramater name doesn't exist
  *)
  let is_parameter_generic (para_name: string) (fn_decl: t) = 
    fn_decl.parameters
    |> List.assoc_opt para_name
    |> Option.map ( fun kt -> is_ktype_generic kt fn_decl)

  let is_no_nested_generic ktype (fn_decl: t) = 
    match ktype with
    | TType_Identifier { module_path = _; name } -> fn_decl.generics |> List.mem name
    | _ -> false

  let rec is_type_compatible_hashgen (generic_table) (init_type: ktype) (expected_type: ktype) (function_decl: t) = 
    match init_type, expected_type with
    | kt , TType_Identifier { module_path = ""; name } when begin   
    match Hashtbl.find_opt generic_table name with
    | None -> if function_decl.generics |> List.mem name 
        then let () = Hashtbl.replace generic_table name (function_decl.generics |> Util.ListHelper.index_of (( = ) name ), kt ) in true
        else false 
    | Some (_, find_kt) -> find_kt = kt
    end -> true
    | TType_Identifier {module_path = init_path; name = init_name}, TType_Identifier {module_path = exp_path; name = exp_name } -> begin
      function_decl.generics |> List.mem exp_name || (init_path = exp_path && init_name = exp_name)
    end
    | TParametric_identifier {module_path = init_path; parametrics_type = init_pt; name = init_name}, 
      TParametric_identifier {module_path = exp_path; parametrics_type = exp_pt; name = exp_name} -> 
        if init_path <> exp_path || init_name <> exp_name || List.compare_lengths init_pt exp_pt <> 0 then false
        else begin
          List.combine init_pt exp_pt |> List.for_all (fun (i,e) -> is_type_compatible_hashgen generic_table i e function_decl)
        end
    | TUnknow, _ -> true
    | lhs, rhs -> lhs = rhs



    let to_return_ktype_hashtab (generic_table) (function_decl: t) = 
      if function_decl.generics = []  || function_decl |> is_ktype_generic function_decl.return_type |> not then function_decl.return_type
      else Ast.Type.remap_generic_ktype generic_table
       function_decl.return_type

  let rec are_ktypes_compatible ~para_type ~init_type (fn_decl: t) =
    if is_no_nested_generic para_type fn_decl then true 
    else match para_type, init_type with 
    | TType_Identifier {module_path = l_module_path; name = lname}, TType_Identifier {module_path = r_module_path; name = r_name} -> l_module_path = r_module_path && lname = r_name
    | TParametric_identifier { module_path = lmp; parametrics_type = lpt ; name = ln }, TParametric_identifier { module_path = rmp ; parametrics_type = rpt; name = rn } -> 
      rn = ln && lmp = rmp && (Util.are_same_lenght lpt rpt) && (List.combine lpt rpt |> List.for_all (fun (l, r) -> are_ktypes_compatible ~para_type: l ~init_type: r fn_decl))
    | lhs, rhs -> lhs = rhs


  let iter_statement fn (function_decl: t) = let statements, _  = function_decl.body in statements |> List.iter fn
end

let string_of_variable_info (variable_info: Env.variable_info) = 
  Printf.sprintf "(%s, %s)" (if variable_info.is_const then "const" else "var") (string_of_ktype variable_info.ktype)
let string_of_env (env: Env.t) = let open Printf in
sprintf "[ %s ]" (env.contexts |> List.map (fun context -> 
  sprintf "{ %s }" (context |> List.map (fun (variable_name, variable_info) -> 
    sprintf "%s = %s" (variable_name) (string_of_variable_info variable_info)) |> String.concat ", ") 
  ) |> String.concat " ; "
  )

let string_of_found_expected = function
| `int(expected, found ) -> Printf.sprintf "-- expected : %d, found : %d --" expected found
| `str(expected, found) -> Printf.sprintf "-- expected : %s, found : %s --" expected found
| `ktype(expected, found ) -> Printf.sprintf "-- expected : %s, found : %s --" (expected |> string_of_ktype) (found  |> string_of_ktype)

let string_of_struct_error = let open Ast.Error in let open Printf in function
| Unexpected_field { expected; found } -> sprintf "Unexpected_field -- expected : %s, found : %s --" expected found
| Unexisting_field s -> sprintf "Unexisting_field %s" s
| Wrong_field_count record -> sprintf "Wrong_field_count -- expected : %d, found : %d" record.expected record.found

let string_of_enum_error = let open Ast.Error in let open Printf in function
| Wrong_length_assoc_type record -> sprintf "Wrong_length_assoc_type -- expected : %d, found : %d --" record.expected record.found
| Uncompatible_type_in_variant { variant_name } -> sprintf "Uncompatible_type_in_variant : %s" variant_name

let string_of_statement_error = let open Ast.Error in let open Printf in function
| Undefine_Identifier s -> sprintf "Undefine_Identifier : %s" s.name
| Already_Define_Identifier s -> sprintf "Already_Define_Identifier : %s" s.name
| Reassign_Constante s -> sprintf "Reassign_Constante : %s" s.name
| Uncompatible_type_Assign s -> sprintf "Uncompatible_type_Assign -- expected: %s, found: %s --" (s.expected |> string_of_ktype)  (s.found |> string_of_ktype)
| Neead_explicit_type_declaration s -> sprintf "Neead_explicit_type_declaration for : %s : type = %s" s.variable_name (string_of_ktype s.infer_type)

let string_of_function_error = let open Ast.Error in let open Printf in function
| Unmatched_Parameters_length record -> sprintf "Unmatched_Parameters_length %s " (string_of_found_expected (`int(record.expected, record.found)))
| Unmatched_Generics_Resolver_length record -> sprintf "Unmatched_Generics_Resolver_length : %s" (string_of_found_expected (`int(record.expected, record.expected)) )
| Uncompatible_type_for_C_Function recod -> sprintf "Uncompatible_type_for_C_Function for %s " (ExternalFunc.string_of_external_func_decl recod.external_func_decl)
| Mismatched_Parameters_Type record -> sprintf "Mismatched_Parameters_Type : %s" (string_of_found_expected (`ktype(record.expected, record.found)))
| Unknow_Function_Error -> "Unknow_Function_Error"

let string_of_operator_error = let open Ast.Error in let open Printf in let open Ast.OperatorFunction in function
| Invalid_pointer_arithmetic kt -> sprintf "Invalid_pointer_arithmetic with %s" (string_of_ktype kt)
| No_built_in_op record -> sprintf "No_built \" %s \" for -- %s --" (name_of_bin_op record.bin_op ) (record.ktype |> string_of_ktype)
| Incompatible_Type record -> sprintf "Incompatible_Type for \" %s \" -- lhs = %s : rhs = %s" (record.bin_op |> name_of_bin_op) (record.lhs |> string_of_ktype) (record.rhs |> string_of_ktype)
| Operator_not_found record -> sprintf "No operator \" %s \" for -- %s --" (name_of_bin_op record.bin_op ) (record.ktype |> string_of_ktype)
| Too_many_operator_declaration record -> sprintf "Too many \" %s \" declaration for %s " (name_of_bin_op record.bin_op ) (record.ktype |> string_of_ktype)
| Not_Boolean_operand_in_And -> sprintf "Not_Boolean_operand_in_And"
| Not_Boolean_operand_in_Or -> sprintf "Not_Boolean_operand_in_Or"


let string_of_ast_error = let open Ast.Error in let open Printf in function
| Bin_operator_Different_type -> "Bin_operator_Different_type"
| Undefined_Identifier s -> sprintf "Undefined_Identifier : %s" s
| Undefined_Const s -> sprintf "Undefined_Const : %s" s
| Undefined_Struct s -> sprintf "Undefined_Struct : %s" s
| Unbound_Module s -> sprintf "Unbound_Module : %s" s
| Struct_Error s -> string_of_struct_error s
| Enum_Error e -> string_of_enum_error e
| Statement_Error e -> string_of_statement_error e
| Func_Error e -> string_of_function_error e
| Operator_Error e -> string_of_operator_error e
| Uncompatible_type e -> sprintf "Uncompatible_type %s" (string_of_found_expected (`ktype(e.expected, e.found)))
| Uncompatible_type_If_Else e -> sprintf "Uncompatible_type_If_Else %s" (string_of_found_expected (`ktype(e.if_type, e.else_type)))
| Not_Boolean_Type_Condition e -> sprintf "Not_Boolean_Type_Condition -- found : %s : expected : bool --" (string_of_ktype e.found)
| Impossible_field_Access e -> sprintf "Impossible_field_Access : %s" (string_of_ktype e)
| Unvalid_Deference -> sprintf "Unvalid_Deference"
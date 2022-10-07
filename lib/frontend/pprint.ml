open Position
open Printf
open Ast
open Astvalidation.Error


let char_of_signedness = function Signed -> 's' | Unsigned -> 'u'

let string_of_isize = function
  | I8 -> "8"
  | I16 -> "16"
  | I32 -> "32"
  | I64 -> "64"

let string_of_position_error {start_position; end_position} = 
  let start_position_line, start_position_column = line_column_of_position start_position in
  let end_position_line, end_position_column = line_column_of_position end_position in
  let column_string = Printf.sprintf "%d%s" (start_position_column) (if start_position_column = end_position_column then "" else " - "^(string_of_int end_position_column)) in 
  if start_position_line = end_position_line then Printf.sprintf "Line %d, Characters %s" start_position_line column_string
  else
    Printf.sprintf "Lines %d-%d, Characters %d-%d" start_position_line end_position_line start_position_column end_position_column

let string_of_located_error a b = 
  Printf.sprintf "%s : %s" (string_of_position_error a.position) (b)

  let rec string_of_ktype = function
  | TParametric_identifier { module_path; parametrics_type; name } ->
      sprintf "(%s)%s %s"
        (parametrics_type |> List.map (fun s -> string_of_ktype s.v) |> String.concat ", ")
        module_path.v name.v
  | TType_Identifier { module_path; name } ->
      sprintf "%s%s"
        (if module_path.v = "" then "" else sprintf "%s::" module_path.v)
        name.v
  | TInteger (sign, size) ->
      sprintf "%c%s" (char_of_signedness sign) (string_of_isize size)
  | TPointer ktype -> sprintf "*%s" (string_of_ktype ktype.v)
  | TTuple ktypes ->
      sprintf "(%s)" (ktypes |> List.map (fun s -> string_of_ktype s.v) |> String.concat ", ")
  | TFunction (parameters, r_type) ->
      sprintf "(%s) -> %s"
        (parameters |> List.map (fun s -> string_of_ktype s.v) |> String.concat ", ")
        (string_of_ktype r_type.v)
  | TString_lit -> "stringl"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TUnknow -> "unknow"
  | TFloat -> "f64"

  let rec string_of_kbody = function
  | (statements : kstatement location list), (expr : kexpression location) ->
      sprintf "{\n  %s  %s\n}"
        (statements |> List.map (fun s -> s |> value |> string_of_kstatement) |> String.concat "\n  ")
        (string_of_kexpression expr.v)

and string_of_kstatement = function
  | SDeclaration { is_const; variable_name; explicit_type; expression } ->
      sprintf "%s %s : %s = %s;"
        (if is_const then "const" else "var")
        variable_name.v
        (explicit_type |> Option.map (fun kt ->  string_of_ktype kt.v)
       |> Option.value ~default:"")
        (expression.v |> string_of_kexpression)
  | SAffection (id, expression) ->
      sprintf "%s = %s;" id.v (expression.v |> string_of_kexpression)
  | SDiscard expr -> sprintf "discard %s;" (string_of_kexpression expr.v)

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
        | Either.Left t -> string_of_ktype t.v
        | Either.Right expr -> string_of_kexpression expr.v
      in
      sprintf "sizeof(%s)" s
  | EString s -> s
  | EAdress x -> sprintf "&%s" x.v
  | EDeference (indirection, id) ->
      sprintf "%s%s" (Util.string_of_chars indirection '*') id.v
  | EIdentifier { modules_path; identifier }
  | EConst_Identifier { modules_path; identifier } ->
      sprintf "%s%s" (Util.string_of_module_path modules_path.v) identifier.v
  | EFieldAcces { first_expr; fields } ->
      sprintf "(%s)->%s"
        (string_of_kexpression first_expr.v)
        (fields |> List.map Position.value |> String.concat "->")
  | EStruct { modules_path; struct_name; fields } ->
      sprintf "%s%s { %s }"
        (if modules_path.v = "" then "" else sprintf "%s::" modules_path.v)
        struct_name.v
        (fields
        |> List.map (fun (id, expr) ->
               sprintf "%s: %s" id.v (string_of_kexpression expr.v))
        |> String.concat ",")
  | EEnum { modules_path; enum_name; variant; assoc_exprs } ->
      sprintf "%s%s.%s%s"
        (Util.string_of_module_path modules_path.v)
        (enum_name |>  Option.fold ~none: " " ~some:(Position.value))
        variant.v
        (if assoc_exprs = [] then ""
        else
          sprintf "(%s)"
            (assoc_exprs
            |> List.map Position.value
            |> List.map string_of_kexpression
            |> String.concat ", "))
  | ETuple exprs ->
      sprintf "(%s)"
        (exprs |> List.map Position.value |> List.map string_of_kexpression  |> String.concat ", ")
  | EFunction_call { modules_path; generics_resolver; fn_name; parameters } ->
      sprintf "%s%s%s(%s)"
        (Util.string_of_module_path modules_path.v)
        fn_name.v
        (generics_resolver
        |> Option.map (fun kts ->
               sprintf "::<%s>"
                 (kts |> List.map (fun gkt -> gkt |> value |> string_of_ktype) |> String.concat ", "))
        |> Option.value ~default:"")
        (parameters |> List.map Position.value |> List.map string_of_kexpression |> String.concat ", ")
  | EIf (expression, if_body, else_body) ->
      sprintf "if %s %s else %s"
        (string_of_kexpression expression.v)
        (string_of_kbody if_body)
        (string_of_kbody else_body)
  | ECases { cases; else_case } ->
      sprintf "cases {\n %s else => %s}"
        (cases
        |> List.map (fun (expr, kbody) ->
               sprintf "%s => %s"
                 (expr.v |> string_of_kexpression)
                 (string_of_kbody kbody))
        |> String.concat "\n")
        (string_of_kbody else_case)
  | ESwitch { expression; cases; wildcard_case } ->
      sprintf "switch %s {%s\n%s}"
        (string_of_kexpression expression.v)
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
      sprintf "@%s(%s)" fn_name.v
        (parameters |> List.map Position.value |> List.map string_of_kexpression |> String.concat ", ")

and string_of_kbin_op = function
  | BAdd (lhs, rhs) ->
      sprintf "(%s + %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BMinus (lhs, rhs) ->
      sprintf "(%s - %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BMult (lhs, rhs) ->
      sprintf "(%s * %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BDiv (lhs, rhs) ->
      sprintf "(%s / %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BMod (lhs, rhs) ->
      sprintf "(%s %% %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BBitwiseOr (lhs, rhs) ->
      sprintf "(%s | %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BBitwiseXor (lhs, rhs) ->
      sprintf "(%s ^ %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BBitwiseAnd (lhs, rhs) ->
      sprintf "(%s & %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BShiftLeft (lhs, rhs) ->
      sprintf "(%s << %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BShiftRight (lhs, rhs) ->
      sprintf "(%s >> %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BAnd (lhs, rhs) ->
      sprintf "(%s && %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BOr (lhs, rhs) ->
      sprintf "(%s || %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BInf (lhs, rhs) ->
      sprintf "(%s < %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BInfEq (lhs, rhs) ->
      sprintf "(%s <= %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BSup (lhs, rhs) ->
      sprintf "(%s > %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BSupEq (lhs, rhs) ->
      sprintf "(%s >= %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BEqual (lhs, rhs) ->
      sprintf "(%s == %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)
  | BDif (lhs, rhs) ->
      sprintf "(%s != %s)"
        (string_of_kexpression lhs.v)
        (string_of_kexpression rhs.v)

and string_of_kunary_op = function
  | UMinus expr -> sprintf "-(%s)" (string_of_kexpression expr.v)
  | UNot expr -> sprintf "!(%s)" (string_of_kexpression expr.v)

and string_of_switch_case = function
  | SC_Enum_Identifier { variant } -> "." ^ variant.v
  | SC_Enum_Identifier_Assoc { variant; assoc_ids } ->
      sprintf "%s(%s)" variant.v
        (assoc_ids
        |> List.map ( Option.fold ~none:"_" ~some:(Position.value))
        |> String.concat ",")

let string_of_enum_variant (variant : string * Ast.ktype list) =
  let name, types = variant in
  sprintf "%s (%s)" name
    (types |> List.map string_of_ktype |> String.concat ", ")

let string_of_enum_decl (enum_decl : enum_decl) =
  sprintf "enum (%s) %s := { %s }"
    (enum_decl.generics |> List.map Position.value |> String.concat ", ")
    enum_decl.enum_name.v
    (enum_decl.variants
    |> List.map (fun (sl, kl) -> sl.v, kl |> List.map Position.value)
    |> List.map string_of_enum_variant
    |> String.concat ", ")

let string_of_struct_decl (struct_decl : struct_decl) =
  sprintf "struct (%s) %s := { %s }"
    (struct_decl.generics |> List.map value |> String.concat ", ")
    struct_decl.struct_name.v
    (struct_decl.fields
    |> List.map (fun (field, t) ->
            sprintf "%s : %s" field.v (string_of_ktype t.v))
    |> String.concat ", ")


let string_of_type_decl = function
  | Ast.Type_Decl.Decl_Enum e -> string_of_enum_decl e
  | Ast.Type_Decl.Decl_Struct s -> string_of_struct_decl s

let string_of_external_func_decl (efucn_decl : external_func_decl) =
  sprintf "external %s(%s%s) %s %s" efucn_decl.sig_name.v
    (efucn_decl.fn_parameters |> List.map (Position.value) |> List.map string_of_ktype
    |> String.concat ", ")
    (if efucn_decl.is_variadic then ";..." else "")
    (efucn_decl.r_type.v |> string_of_ktype)
    (efucn_decl.c_name
    |> Option.map (fun s -> sprintf " = %s" s)
    |> Option.value ~default:"")

let string_of_syscall (syscall_decl : syscall_decl) =
  sprintf "syscall %s(%s) %s" syscall_decl.syscall_name.v
    (syscall_decl.parameters |> List.map (Position.value) |> List.map string_of_ktype |> String.concat ", ")
    (syscall_decl.return_type.v |> string_of_ktype)

let string_of_func_decl (function_decl : function_decl) =
  sprintf "fn %s%s(%s)%s%s" function_decl.fn_name.v
    (if function_decl.generics = [] then ""
    else sprintf "<%s>" (function_decl.generics |> List.map Position.value |> String.concat ", "))
    (function_decl.parameters
    |> List.map (fun (id, ktype) ->
            sprintf "%s: %s" id.v (string_of_ktype ktype.v))
    |> String.concat ", ")
    (function_decl.return_type.v |> string_of_ktype)
    (function_decl.body |> string_of_kbody)

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
      sprintf "Unexpected_field -- expected : %s, found : %s --" expected.v found.v
  | Unexisting_field s -> sprintf "Unexisting_field %s" s
  | Wrong_field_count record ->
      string_of_located_error 
      {v = true; position = record.location} 
      (sprintf "Wrong number of fields -- expected : %d, found : %d --" record.expected record.found)
      

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
  | Undefine_Identifier s -> sprintf "%s : Undefine Identifier : %s" (string_of_position_error s.name.position) (s.name.v)
  | Already_Define_Identifier s ->
      string_of_located_error s.name (sprintf "Identifier already defined : \"%s\"" s.name.v)
  | Reassign_Constante s ->  string_of_located_error s.name (sprintf "Reassign a constant : %s" s.name.v)
  | Uncompatible_type_Assign s ->
    string_of_located_error s.found 
    (
      sprintf "incompatible type between \"%s\" and \"%s\""
    (s.expected |> string_of_ktype)
    (s.found.v |> string_of_ktype)
    )
  | Need_explicit_type_declaration s ->
    string_of_located_error s.variable_name (
      sprintf "Need explicit type declaration for identifier \"%s\" ::=> type = %s"
        s.variable_name.v
        (string_of_ktype s.infer_type))

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
        (string_of_external_func_decl recod.external_func_decl)
  | Uncompatible_type_for_Syscall record ->
      sprintf "Uncompatible_type_for_Syscall for %s"
        (string_of_syscall record.syscall_decl)
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
      sprintf "Incompatible_Type for \" %s \" -- lhs = %s : rhs = %s --"
        (record.bin_op |> name_of_operator)
        (record.lhs |> string_of_ktype)
        (record.rhs |> string_of_ktype)
  | Operator_not_found record ->
      sprintf "No operator \" %s \" for -- %s --"
        (symbole_of_operator record.bin_op)
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
  | Not_fully_known_ktype ktype ->
      sprintf "Type of switch expr cannot be fully infered -- %s --"
        (string_of_ktype ktype)
  | Not_enum_type_in_switch_Expression e ->
      sprintf "Not_enum_type_in_switch_Expression -- found : %s --"
        (string_of_ktype e)
  | Not_all_cases_handled missing_cases ->
      sprintf "Not_all_cases_handled : missing cases :\n  %s"
        (missing_cases
        |> List.map (fun (variant, assoc) -> variant.v, assoc |> List.map value)
        |> List.map (fun (variant, kts) ->
                sprintf "%s(%s)" variant
                  (kts |> List.map string_of_ktype |> String.concat ", "))
        |> String.concat "\n  ")
  | Variant_not_found { enum_decl; variant } ->
      sprintf "Variant_not_found %s in %s" variant enum_decl.enum_name.v
  | Mismatched_Assoc_length { variant; expected; found } ->
      sprintf "Mismatched_Assoc_length variant %s %s" variant
        (string_of_expected_found (`int (expected, found)))
  | Incompatible_Binding (lhs, rhs) ->
      sprintf "Incompatible_Binding between: \n-> %s\n-> %s"
        (lhs
        |> List.map (fun (id, ktype) ->
                sprintf "%s: %s" id.v (string_of_ktype ktype.v))
        |> String.concat ", ")
        (rhs
        |> List.map (fun (id, ktype) ->
                sprintf "%s: %s" id.v (string_of_ktype ktype.v))
        |> String.concat ", ")
  | Binded_identifier_already_exist s ->
      sprintf "Binded_identifier_already_exist_in_env : %s" s

let string_of_built_in_func_error =
  let open Ast.Error in
  let open Printf in
  function
  | Unknow_built_function fn_name ->
      sprintf "Unknow_built_function : %s" fn_name.v
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
  | No_Occurence_found s -> sprintf "No Occurence found for %s" s
  | Too_Many_occurence_found s -> sprintf "Too_Many_occurence_found : %s" s
  | Undefined_Identifier s -> sprintf "%s : Undefined Identifier \"%s\"" (string_of_position_error s.position) s.v
  | Undefined_Const s -> sprintf "Undefined_Const : %s" s
  | Undefined_Struct s ->  string_of_located_error s (sprintf "Undefined Struct \"%s\"" s.v)
  | Unbound_Module s -> string_of_located_error s (sprintf "Unbound Module \"%s\"" s.v) 
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
    string_of_located_error 
    e.position 
    (
      sprintf "If block has the type -- %s -- but else block has type -- %s --, -- %s -- and -- %s -- aren't compatible" 
      (string_of_ktype e.if_type)
      (string_of_ktype e.else_type)
      (string_of_ktype e.if_type)
      (string_of_ktype e.else_type)
    )

  | Not_Boolean_Type_Condition e ->
    string_of_located_error e.found 
    (
      sprintf "Not Boolean Type Condition -- found : %s : expected : bool --" 
    (string_of_ktype e.found.v)
    )
      
        
  | Impossible_field_Access e ->
      sprintf "Impossible_field_Access : %s" (string_of_ktype e)
  | Enum_Access_field record ->
      sprintf "%s Enum doesn't have field : %s for enum : %s" (string_of_position_error record.field.position) record.field.v
        record.enum_decl.enum_name.v
  | Unvalid_Deference identifier -> 
    string_of_located_error identifier (sprintf "Invalid Deference for \"%s\"" identifier.v)
    
let string_of_external_func_error =
  let open Printf in
  function
  | Unit_parameter external_func_decl ->
      sprintf "Unit parameter in %s" external_func_decl.sig_name.v
  | Not_C_compatible_type (external_func_decl, ktype) ->
      sprintf "Not_C_compatible_type in %s -- %s --"
        external_func_decl.sig_name.v (string_of_ktype ktype)
  | Too_much_parameters record ->
      sprintf "Too_much_parameters -- limit: %d, found: %d --" record.limit
        record.found

let string_of_sycall_error =
  let open Printf in
  function
  | Syscall_Unit_parameter syscall_decl ->
      sprintf "Unit parameter in %s" syscall_decl.syscall_name.v
  | Syscall_Not_C_compatible_type (syscall_decl, ktype) ->
      sprintf "Not_C_compatible_type in %s -- %s --" syscall_decl.syscall_name.v
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

    string_of_located_error struct_decl.struct_name (sprintf "Cyclic struct declaration for %s" struct_decl.struct_name.v)
    (* string_of_located_error (struct_decl.struct_name) (x) *)
      (* sprintf  *)
  | SDuplicated_field struct_decl ->
      sprintf "Struct Duplicated_field for %s" struct_decl.struct_name.v

let string_of_enum_error =
  let open Printf in
  function
  | ECyclic_Declaration enum_decl ->
      sprintf " Enum_Cyclic_Declaration for %s" enum_decl.enum_name.v
  | EDuplicated_variant_name enum_decl ->
      sprintf "Enum_Duplicated_variant_name for %s" enum_decl.enum_name.v

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
        (string_of_expected_found
            (`ktype
              ( Asthelper.ParserOperator.expected_op_return_type expected op,
                found )))

let string_of_function_error =
  let open Printf in
  function
  | Wrong_signature_for_main -> sprintf "Wrong_signature_for_main"
  | Duplicated_parameters function_decl ->
      sprintf "Duplicate name parameters for %s" function_decl.fn_name.v
  | Function_Unit_parameter function_decl ->
      sprintf "Function_Unit_parameter : %s" function_decl.fn_name.v

let string_of_module_error =
  let open Printf in
  function
  | Duplicate_function_name name ->
      sprintf "Duplicate_function_name : %s" name
  | Duplicate_type_name name -> sprintf "Duplicate_type_name : %s" name
  | Duplicate_const_name name -> sprintf "Duplicate_const_name : %s" name
  | Duplicate_Operator op ->
      sprintf "Duplicate_Operator for -- %s --"
        (Asthelper.ParserOperator.string_of_parser_operator (op |> fun op -> match op with `unary o -> `unary o.v | `binary b -> `binary b.v))
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
  | Ast_Error e -> string_of_ast_error e
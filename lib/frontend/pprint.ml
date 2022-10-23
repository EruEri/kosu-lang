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

let located_symbole_of_operator = function
  | Unary {op; _} -> op |> Position.map (function
      | PNot -> "!"
      | PUMinus -> "(-.)"
  )
  | Binary {op; _} -> op |> Position.map (function
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
  )
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

let string_of_signature fn_name parameters return_type = 
  sprintf "%s(%s)%s"
  (fn_name.v)
  (
    parameters
    |> List.map (fun ktl -> ktl |> Position.value |> string_of_ktype)
    |> String.concat ", "
  )
  (return_type |> Position.value |> string_of_ktype)


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
  | Wrong_field_count record ->
      string_of_located_error
      record.struct_name
      (sprintf "struct \"%s\" expects %d field%s but %d was expected" 
      record.struct_name.v
      record.expected
      (if record.expected > 1 then "s" else "")
      record.found
      )
      

let string_of_enum_error =
  let open Ast.Error in
  let open Printf in
  function
  | Wrong_length_assoc_type record ->
    string_of_located_error record.variant (
      sprintf "variant \"%s\" expects %d associated value%s but %d %s provided"
      record.variant.v
      record.expected
      (if record.expected > 1 then "s" else "")
      record.found
      (if record.expected > 1 then "were" else "was")
    )
  | No_variant_found_for_enum {variant} -> 
    string_of_located_error variant (
      sprintf "No enum declaration found containing the variant \"%s\""
      variant.v
    )
  | Conflict_variant_multiple_decl {module_path; variant; enum_decls} -> 
    string_of_located_error variant (
      sprintf "Need explicit enum name. This variant \"%s\" appears in multiple declarations:\n\t%s"
      variant.v
      (
        enum_decls
        |> List.map (fun enum_decl -> 
          Printf.sprintf "%s::%s -> %s"
          (module_path)
          (enum_decl.enum_name.v)
          (string_of_position_error enum_decl.enum_name.position)
          )
        |> String.concat "\n\t"
      )
    ) 
let string_of_statement_error =
  let open Ast.Error in
  let open Printf in
  function
  | Undefine_Identifier s -> sprintf "%s : Undefine Identifier \"%s\"" (string_of_position_error s.name.position) (s.name.v)
  | Already_Define_Identifier s ->
      string_of_located_error s.name (sprintf "Identifier already defined : \"%s\"" s.name.v)
  | Reassign_Constante s ->  string_of_located_error s.name (sprintf "Reassign a constant \"%s\"" s.name.v)
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
      string_of_located_error 
      record.fn_name (sprintf "Function \"%s\" expects %d parameter%s but %d %s provided"
      (record.fn_name.v)
      (record.expected)
      (if record.expected > 1 then "s" else "")
      (record.found)
      (if record.found >= 2 then "were" else "was")
      )
  | Unmatched_Generics_Resolver_length record ->
    string_of_located_error record.fn_name
    (
      sprintf "Function \"%s\" expects %d generics resolver but %d %s provided"
      (record.fn_name.v)
      (record.expected)
      (record.found)
      (if record.found >= 2 then "were" else "was")
    )
  | Uncompatible_type_for_C_Function {fn_name; ktype} ->
    string_of_located_error ktype 
    (sprintf "Function \"%s\", this expression has the type \"%s\" but is not compatible with a C type"
    (fn_name.v)
    (string_of_ktype ktype.v)
    )
  | Uncompatible_type_for_Syscall {index; syscall_decl} ->
    let wrong_type = index |> Option.map (List.nth syscall_decl.parameters) |> Option.value ~default:syscall_decl.return_type in
    string_of_located_error wrong_type (
      sprintf "\"%s\" is not compatible with a C type and cannot be used in a signature of a system call"
      (string_of_ktype wrong_type.v)
      )
  | Mismatched_Parameters_Type {fn_name; expected; found} ->
    string_of_located_error found 
    (sprintf "Function \"%s\", this expression has the type \"%s\" but an expression of the type \"%s\" was expected. \"%s\" and \"%s\" aren't compatible"
    fn_name
    (string_of_ktype expected)
    (string_of_ktype found.v)
    (string_of_ktype found.v)
    (string_of_ktype expected)
    )
  | Unknow_Function_Error -> "Unknow_Function_Error"

let string_of_operator_error =
  let open Ast.Error in
  let open Printf in
  let open Ast.OperatorFunction in
  function
  | Invalid_pointer_arithmetic kt ->
    string_of_located_error kt (
      sprintf "this expression has the type \"%s\", which cannot be used in pointer arithmetic"
      (string_of_ktype kt.v)
    )
  | No_built_in_op record ->
      string_of_located_error record.ktype (
        sprintf "Builtin type \"%s\" doesn't implement the operator \"%s\""
        (string_of_ktype record.ktype.v)
        (symbole_of_operator record.bin_op)
      )
  | Incompatible_Type {bin_op; expr_loc; lhs; rhs} ->

    string_of_located_error expr_loc 
    (
      sprintf "left operande has the type \"%s\" but the right one has the type \"%s\". \"%s\" and \"%s\" aren't compatible for operator \"%s\""
      (string_of_ktype lhs.v)
      (string_of_ktype rhs.v)
      (string_of_ktype lhs.v)
      (string_of_ktype rhs.v)
      (symbole_of_operator bin_op)
    )
  | Operator_not_found record ->
    string_of_located_error record.ktype (
      sprintf "Type \"%s\" doesn't implement the operator \"%s\""
      (string_of_ktype record.ktype.v)
      (symbole_of_operator record.bin_op)
    )
  | Too_many_operator_declaration {operator_decls; bin_op; ktype} ->
    string_of_located_error ktype (
      sprintf "Type \"%s\" defines too many times the operator \"%s\". Redefinition occures here:\n\n%s\n"
      (string_of_ktype ktype.v)
      (symbole_of_operator bin_op)
      (operator_decls 
      |> List.map ( fun (path, op_decls) -> let open Asthelper.ParserOperator in
        sprintf "\tModule \"%s\":\n\t%s\n"
        (path)
        (op_decls |> List.map (fun op -> 
          let symbole = op |> backticked_operator in
          sprintf "\t-%s, %s"
          (symbole |> position |> string_of_position_error)
          (string_of_signature (symbole) (op |> parameters) (op |> return_ktype) )
          )
          |> String.concat "\n\t"
          )
        )
      |> String.concat ", "
      ) 
    )
  | Not_Boolean_operand_in_And kt -> 
    string_of_located_error kt (
      sprintf "this expression has the type \"%s\", but an expression of type \"%s\" was expected in operator \"%s\""
      (string_of_ktype kt.v)
      (string_of_ktype TBool)
      (symbole_of_operator And)
    )
  | Not_Boolean_operand_in_Or kt ->     
    string_of_located_error kt (
    sprintf "this expression has the type \"%s\", but an expression of type \"%s\" was expected in operator \"%s\""
    (string_of_ktype kt.v)
    (string_of_ktype TBool)
    (symbole_of_operator Or)
  )
  | Invalid_Uminus_for_Unsigned_integer size ->
    string_of_located_error size (
      sprintf "Cannot use unary minus for unsigned integer: \"%s\""
      (string_of_ktype (TInteger (Ast.Unsigned, size.v)))
    )

let string_of_switch_error =
  let open Ast.Error in
  function
  | Duplicated_case name -> 
    string_of_located_error name 
    (
      sprintf "case \"%s\" is duplicated"
      name.v
    )
  | Not_fully_known_ktype ktype ->
    string_of_located_error ktype
    (
      sprintf "this expression has the type \"%s\" which is not fully unknown"
      (string_of_ktype ktype.v)
    )
  | Not_enum_type_in_switch_Expression e ->
    string_of_located_error e (
      sprintf "This expression has the type \"%s\" but \"%s\" is not an enum"
      (string_of_ktype e.v)
      (string_of_ktype e.v)
    )
  | Not_all_cases_handled {expression_loc; missing_variant = (variant, assotype)} ->
    string_of_located_error expression_loc (
      sprintf "Not all cases are handled, miss at least: %s"
      (sprintf "\"%s(%s)\"" 
      variant.v
      (assotype |> List.map (fun ktl -> ktl |> Position.value |> string_of_ktype) |> String.concat ", ")
      )
    )
  | Variant_not_found { enum_decl; variant } ->
    string_of_located_error variant 
    (
      sprintf "enum \"%s\" doesn't contain the case \"%s\""
      enum_decl.enum_name.v
      variant.v
    )
  | Mismatched_Assoc_length { variant; expected; found } ->
    string_of_located_error variant 
    (sprintf "case \"%s\" expects %d associated values but %d %s provided"
    variant.v
    expected
    found
    (if found > 1 then "were" else "was")
    )

  | Incompatible_Binding_Ktype {
    switch_expr = _;
    base_variant;
    base_bound_id;
    base_bound_ktype;
    wrong_variant;
    wrong_bound_id;
    wrong_bound_ktype;
  } ->
    string_of_located_error wrong_bound_id (
      sprintf "Variable bound \"%s\" in the case \"%s\" has the type \"%s\" but due to the binding of \"%s\" in the case \"%s\" the expected type of \"%s\" is \"%s\""
      (wrong_bound_id.v)
      (wrong_variant.v)
      (string_of_ktype wrong_bound_ktype.v)
      (base_bound_id.v)
      (base_variant.v)
      (base_bound_id.v)
      (string_of_ktype base_bound_ktype.v)
    )
  | Incompatible_Binding_Name {
    switch_expr = _;
    base_variant;
    base_bound_id;
    wrong_variant;
    wrong_bound_id;
  } ->
      string_of_located_error wrong_bound_id (
        sprintf "Variable bound \"%s\" in the case \"%s\" is different from the bound variable \"%s\" in the case \"%s\""
        (wrong_bound_id.v)
        (wrong_variant.v)
        (base_bound_id.v)
        (base_variant.v)
  )
    
      (* sprintf "Incompatible_Binding between: \n-> %s\n-> %s"
        (lhs
        |> List.map (fun (id, ktype) ->
                sprintf "%s: %s" id.v (string_of_ktype ktype.v))
        |> String.concat ", ")
        (rhs
        |> List.map (fun (id, ktype) ->
                sprintf "%s: %s" id.v (string_of_ktype ktype.v))
        |> String.concat ", ") *)
  | Identifier_already_Bound s ->
    string_of_located_error s 
    (
      sprintf "variable \"%s\" is already bound"
      s.v
    )

let string_of_built_in_func_error =
  let open Ast.Error in
  let open Printf in
  function
  | Unknow_built_function fn_name ->
    string_of_located_error fn_name (
      sprintf "Unknown builtin function \"%s\"" fn_name.v
    )
  | Wrong_parameters { fn_name; expected; found } ->
    string_of_located_error found (
      sprintf "Builtin function \"%s\", this expression has the type \"%s\ but an expression of type \"%s\" was expected"
      fn_name
      (string_of_ktype found.v)
      (string_of_ktype expected)
    )
  | Mismatched_Parameters_Length { fn_name; expected; found } ->
    string_of_located_error fn_name (
      sprintf "Builtin function \"%s\" expects %d argument%s but %d %s provided"
      (fn_name.v)
      (expected)
      (if expected > 1 then "s" else "")
      (found)
      (if expected > 1 then "were" else "was")
    )
  | Found_no_Integer { fn_name; found } ->
    string_of_located_error found (
      sprintf "Builtin function \"%s\" expects an integer but an expression of type \"%s\" was provided"
      (fn_name)
      (string_of_ktype found.v)
    )
let string_of_ast_error =
  let open Ast.Error in
  function
  | Wrong_Assoc_Length_for_Parametrics record ->
    string_of_located_error record.type_name (
      sprintf "type \"%s\" expects %d parametric%s but %d %s given"
      (record.type_name.v)
      (record.expected)
      (if record.expected > 1 then "s" else "")
      (record.found)
      (if record.expected > 1 then "was" else "were")
    )
  (* | No_Occurence_found s -> sprintf "No Occurence found for %s" s
  | Too_Many_occurence_found s -> sprintf "Too_Many_occurence_found : %s" s *)
  | Undefined_Identifier s ->  string_of_located_error s (sprintf "Undefined Identifier \"%s\"" s.v)
  | Undefine_function s -> string_of_located_error s (sprintf "Undefined Function \"%s\"" s.v)
  | Undefined_Const s -> string_of_located_error s (sprintf "Undefined Constant \"%s\"" s.v)
  | Undefined_Struct s ->  string_of_located_error s (sprintf "Undefined Struct \"%s\"" s.v)
  | Unbound_Module s -> string_of_located_error s (sprintf "Unbound Module \"%s\"" s.v)
  | Undefine_Type s -> string_of_located_error s (sprintf "Undefined Type \"%s\"" s.v)
  | Struct_Error s -> string_of_struct_error s
  | Enum_Error e -> string_of_enum_error e
  | Statement_Error e -> string_of_statement_error e
  | Func_Error e -> string_of_function_error e
  | Operator_Error e -> string_of_operator_error e
  | Switch_error e -> string_of_switch_error e
  | Builtin_Func_Error e -> string_of_built_in_func_error e
  | Uncompatible_type {expected; found } ->
    string_of_located_error found (
      sprintf "this expression has the type \"%s\" but an expression of type \"%s\" was expected, \"%s\" and \"%s\" aren't compatible"
      (string_of_ktype found.v)
      (string_of_ktype expected)
      (string_of_ktype expected)
      (string_of_ktype found.v)
    )
  | Uncompatible_type_If_Else e ->
    string_of_located_error 
    e.position 
    (
      sprintf "If block has the type \"%s\" but else block has type \"%s\", \"%s\" and \"%s\" aren't compatible" 
      (string_of_ktype e.if_type)
      (string_of_ktype e.else_type)
      (string_of_ktype e.if_type)
      (string_of_ktype e.else_type)
    )

  | Not_Boolean_Type_Condition e ->
    string_of_located_error e.found 
    (
      sprintf "this expression has the type \"%s\" but an expression of type \"%s\" was expected" 
    (string_of_ktype e.found.v)
    (string_of_ktype TBool)
    )
      
        
  | Impossible_field_Access {field; struct_decl} ->
      string_of_located_error field 
      (sprintf "Struct \"%s\" doesn't contain a field named \"%s\""
      struct_decl.struct_name.v
      field.v
      )
      
  | Enum_Access_field record ->
      sprintf "%s Enum doesn't have field : \"%s\" for enum : \"%s\"" (string_of_position_error record.field.position) record.field.v
        record.enum_decl.enum_name.v
  | Unvalid_Deference identifier -> 
    string_of_located_error identifier (sprintf "Invalid Deference for \"%s\"" identifier.v)
  | Conflicting_function_declaration { fn_def_path; fn_name; fn_decls} -> 
    string_of_located_error fn_name (
      sprintf "Function \"%s\" has multiple declarations. Declarations occures here [%s]"
      fn_name.v
      (
        fn_decls
        |> List.map (fun fn_decl -> 
          sprintf "%s, %s::%s" 
          (fn_decl |> Ast.Function_Decl.calling_name |> Position.position |> string_of_position_error ) 
          fn_def_path 
          (string_of_signature (fn_decl |> Ast.Function_Decl.calling_name) (fn_decl |> Ast.Function_Decl.parameters) (fn_decl |> Ast.Function_Decl.return_type))
        )
        |> String.concat ", "
      )
    )
  | Conflicting_type_declaration {path; ktype_name; type_decls} -> 
    string_of_located_error ktype_name (
      sprintf "Type \"%s\" has multiple declarations. Declarations occures here [%s]"
      ktype_name.v
      (
        type_decls
        |> List.map (fun type_decl -> sprintf "%s, %s::%s" (type_decl |> Asthelper.Type_Decl.type_name |> Position.position |> string_of_position_error) path (type_decl |> Asthelper.Type_Decl.type_name |> Position.value))
        |> String.concat ", "
      )
    )
  | Conflicting_module_path_declaration {module_path; _} -> 
    string_of_located_error module_path (
      sprintf "Multiple modules have the name \"%s\""
      module_path.v
    )
    
    
let string_of_external_func_error =
  let open Printf in
  function
  | Unit_parameter external_func_decl ->
    string_of_located_error external_func_decl.sig_name (
      sprintf "External function \"%s\" has a unit/void parameter, which is not valid"
      external_func_decl.sig_name.v
    )
  | Not_C_compatible_type (external_func_decl, ktype) ->
    string_of_located_error ktype (
      sprintf "External function \"%s\", Type \"%s\" is not a C compatible type"
      (external_func_decl.sig_name.v)
      (string_of_ktype ktype.v)
    )
  | Too_much_parameters {external_func_decl; limit; found } ->
    string_of_located_error external_func_decl.sig_name (
      sprintf "External function \"%s\" has %d parameters but the limit of parameters is %d"
      (external_func_decl.sig_name.v)
      found
      limit
    )

let string_of_sycall_error =
  let open Printf in
  function
  | Syscall_Unit_parameter syscall_decl ->
    string_of_located_error syscall_decl.syscall_name (
      sprintf "System call \"%s\" has a unit/void parameter, which is not valid"
      syscall_decl.syscall_name.v
    )
  | Syscall_Not_C_compatible_type (syscall_decl, ktype) ->
    string_of_located_error ktype (
      sprintf "System call \"%s\", Type \"%s\" is not a C compatible type"
      (syscall_decl.syscall_name.v)
      (string_of_ktype ktype.v)
    )
  | Syscall_Too_much_parameters {syscall_decl; limit; found} ->
    string_of_located_error syscall_decl.syscall_name (
      sprintf "System call \"%s\" has %d parameters but the limit of parameters is %d"
      (syscall_decl.syscall_name.v)
      found
      limit
    )

let string_of_struct_error =
  let open Printf in
  function
  | SCyclic_Declaration struct_decl ->

    string_of_located_error struct_decl.struct_name (
      sprintf "Struct \"%s\" has the cyclic declaration" 
      struct_decl.struct_name.v
    )
  | SDuplicated_field {field; struct_decl} ->
      string_of_located_error field 
      (
        sprintf "field \"%s\" appears multiple type in \"%s\" declaration"
        field.v
        struct_decl.struct_name.v
      )

let string_of_enum_error =
  let open Printf in
  function
  | ECyclic_Declaration enum_decl ->
    string_of_located_error enum_decl.enum_name (
      sprintf "Enum \"%s\" has the cyclic declaration" 
      enum_decl.enum_name.v
    )
  | EDuplicated_variant_name {variant; enum_decl} ->
    string_of_located_error variant 
    (
      sprintf "variant \"%s\" appears multiple type in \"%s\" declaration"
      variant.v
      enum_decl.enum_name.v
    )

let string_of_operator_error =
  let open Printf in
  function
  | Op_built_overload kt ->
    string_of_located_error kt (
      sprintf "Try to overload builtin type \"%s\"" (string_of_ktype kt.v)
    )
  | Op_binary_op_diff_type {operator; lhs; rhs} ->
    string_of_located_error operator (
      sprintf "Operator \"%s\", left operande has the type \"%s\" but the right one has the type \"%s\". In operator override, left operande and right one must have the same type"
      (operator.v)
      (string_of_ktype lhs)
      (string_of_ktype rhs)
    )
  | Op_wrong_return_type_error { op; expected; found } ->
    string_of_located_error op (
      sprintf "Operator \"%s\" has the return type \"%s\" but \"%s\" type was expected"
      (op.v)
      (found |> string_of_ktype)
      (expected |> string_of_ktype)
    )

let string_of_function_error =
  let open Printf in
  function
  | Wrong_signature_for_main function_decl -> 
    string_of_located_error function_decl.fn_name (
      sprintf "Function \"%s\", this function doesn't have a valid signature for a main function"
      function_decl.fn_name.v
    )
  | Duplicated_parameters {duplicatated_field; function_decl} ->
    string_of_located_error duplicatated_field 
    (sprintf "Function \"%s\", parameter \"%s\" is duplicated"
      function_decl.fn_name.v
      duplicatated_field.v
    )
  | Function_Unit_parameter {field; function_decl} ->
    string_of_located_error field
    (sprintf 
      "Function \"%s\",  parameter \"%s\" has the type \"%s\", which is forbidden"
      function_decl.fn_name.v
      field.v
      (string_of_ktype TUnit)
    )

let string_of_module_error =
  let open Printf in
  function
  | Duplicate_function_declaration {path; functions} ->
   let calling_name = functions |> List.hd |> Function_Decl.calling_name in
      sprintf "Conflicting function declarations for \"%s\" between:\n\t-%s\n"
      calling_name.v
      (
        functions
        |> List.map (fun fn_decl -> 
          sprintf "%s, %s::%s"
          (fn_decl |> Function_Decl.calling_name |> Position.position |> string_of_position_error)
          path
          (string_of_signature (calling_name) (fn_decl |> Ast.Function_Decl.parameters) (fn_decl |> Ast.Function_Decl.return_type))
          )
        |> String.concat "\n\t-" 
      )
  | Duplicate_type_declaration {path; types} -> 
    let type_name = types |> List.hd |> Asthelper.Type_Decl.type_name in
    sprintf "Conflicting type declarations for \"%s\" between:\n\t-%s\n"
    type_name.v
    (
      types
      |> List.map (fun type_decl -> 
      sprintf "%s, %s::%s"
      (type_decl |> Asthelper.Type_Decl.type_name |> position |> string_of_position_error)
      path
      (string_of_type_decl type_decl)  
      )
      |> String.concat "\n\t-"
    )
  | Duplicate_const_declaration {path; consts} -> 
    let const_name = consts |> List.hd in
    sprintf "Conflicting constants declaration for \"%s\" between:\n\t-%s\n"
    const_name.const_name.v
    (
      consts
      |> List.map (fun const_decl -> 
        sprintf "%s, %s::%s"
        (const_decl.const_name |> position |> string_of_position_error)
        path
        (const_decl.const_name.v)
      )
      |> String.concat "\n\t-"
    )
  | Duplicate_operator_declaration {path; operators} -> let open Asthelper.ParserOperator in
      let operator = operators |> List.hd |> Asthelper.ParserOperator.operator in
      string_of_located_error operator (
        sprintf "Conflicting operator declaration for \"%s\" between:\n\t-%s\n"
        operator.v
        (
          operators
          |> List.map (fun operator_decl -> 
            let op = operator_decl |> Asthelper.ParserOperator.operator in
            sprintf "%s, %s::%s"
            (op |> Position.position |> string_of_position_error)
            (path)
            (string_of_signature op (operator_decl |> parameters) (operator_decl |> return_ktype) )
          )
          |> String.concat "\n\t-"
        )

      )

  | Main_no_kosu_function decl  -> 
    let position, message = match decl with 
    `syscall_decl syscall_decl -> syscall_decl.syscall_name, "A system call"
    | `external_decl external_func_decl -> external_func_decl.sig_name, "An external function" in
    string_of_located_error position (
      sprintf "%s cannot be used as a main function"
      message
    )
    


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
  | Too_many_Main list ->
    sprintf "Conflicting \"main\" function declaration between:\n\n%s\n"
    (
      list
      |> List.map (fun (path, fn_decls) -> 
        sprintf "\tModule \"%s\":\n\t%s"
        path
        (fn_decls |> List.map (fun fn_decl -> 
          sprintf "\t-%s, %s"
          (fn_decl |> Ast.Function_Decl.calling_name |> Position.position |> string_of_position_error ) 
          (string_of_signature (fn_decl |> Ast.Function_Decl.calling_name) (fn_decl |> Ast.Function_Decl.parameters) (fn_decl |> Ast.Function_Decl.return_type))
          )
          |> String.concat "\n\t" 
        )
        )
      |> String.concat "\n\n"
    )
  | Ast_Error e -> string_of_ast_error e
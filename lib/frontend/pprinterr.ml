(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms            *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kosu.         *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module Make
    (VRule : Astvalidation.KosuValidationRule)
    (TyRule : Typecheck.TypeCheckerRuleS) =
struct
  module Astvalidation = Astvalidation.Make (VRule) (TyRule)
  open Position
  open Printf
  open Ast
  open Pprint
  open Astvalidation

  let string_of_struct_error =
    let open Ast.Error in
    let open Printf in
    function
    | Unexpected_field { expected; found } ->
        sprintf "Unexpected_field -- expected : %s, found : %s --" expected.v
          found.v
    | Wrong_field_count record ->
        string_of_located_error record.struct_name
          (sprintf "struct \"%s\" expects %d field%s but %d was expected"
             record.struct_name.v record.expected
             ( if record.expected > 1 then
                 "s"
               else
                 ""
             )
             record.found
          )

  let string_of_enum_error =
    let open Ast.Error in
    let open Printf in
    function
    | Wrong_length_assoc_type record ->
        string_of_located_error record.variant
          (sprintf
             "variant \"%s\" expects %d associated value%s but %d %s provided"
             record.variant.v record.expected
             ( if record.expected > 1 then
                 "s"
               else
                 ""
             )
             record.found
             ( if record.expected > 1 then
                 "were"
               else
                 "was"
             )
          )
    | No_variant_found_for_enum { variant } ->
        string_of_located_error variant
          (sprintf "No enum declaration found containing the variant \"%s\""
             variant.v
          )
    | Conflict_variant_multiple_decl { module_path; variant; enum_decls } ->
        string_of_located_error variant
          (sprintf
             "Need explicit enum name. This variant \"%s\" appears in multiple \
              declarations:\n\
              \t%s"
             variant.v
             (enum_decls
             |> List.map (fun enum_decl ->
                    Printf.sprintf "%s::%s -> %s" module_path
                      enum_decl.enum_name.v
                      (string_of_position_error enum_decl.enum_name.position)
                )
             |> String.concat "\n\t"
             )
          )

  let string_of_statement_error =
    let open Ast.Error in
    let open Printf in
    function
    | Undefine_Identifier s ->
        sprintf "%s : Undefine Identifier \"%s\""
          (string_of_position_error s.name.position)
          s.name.v
    | Reassign_Constante_Struct_field { name } ->
        string_of_located_error name
          (sprintf
             "variable \"%s\" is declared as const. Can't reassign his fields"
             name.v
          )
    | Already_Define_Identifier s ->
        string_of_located_error s.name
          (sprintf "Identifier already defined : \"%s\"" s.name.v)
    | Reassign_Constante s ->
        string_of_located_error s.name
          (sprintf "Reassign a constant \"%s\"" s.name.v)
    | Dereference_No_pointer { name; ktype } ->
        string_of_located_error name
          (sprintf
             "Identifier \"%s\" has the type \"%s\" which is not a pointer. \
              Therefore, it can't be deferenced"
             name.v (string_of_ktype ktype)
          )
    | Dereference_Wrong_type { identifier; expected; found } ->
        string_of_located_error found
          (sprintf
             "This expression has the type \"%s\" but the deferenced variable \
              \"%s\" has the type \"%s\". \"%s\" and \"%s\" aren't compatible"
             (string_of_ktype found.v) identifier.v (string_of_ktype expected)
             (string_of_ktype found.v) (string_of_ktype expected)
          )
    | Uncompatible_type_Assign s ->
        string_of_located_error s.found
          (sprintf "incompatible type between \"%s\" and \"%s\""
             (s.expected |> string_of_ktype)
             (s.found.v |> string_of_ktype)
          )
    | Need_explicit_type_declaration s ->
        string_of_located_error s.variable_name
          (sprintf
             "Need explicit type declaration for identifier \"%s\" ::=> type = \
              %s"
             s.variable_name.v
             (string_of_ktype s.infer_type)
          )

  let string_of_function_error =
    let open Ast.Error in
    let open Printf in
    function
    | Unmatched_Parameters_length record ->
        string_of_located_error record.fn_name
          (sprintf "Function \"%s\" expects %d parameter%s but %d %s provided"
             record.fn_name.v record.expected
             ( if record.expected > 1 then
                 "s"
               else
                 ""
             )
             record.found
             ( if record.found >= 2 then
                 "were"
               else
                 "was"
             )
          )
    | Unmatched_Generics_Resolver_length record ->
        string_of_located_error record.fn_name
          (sprintf
             "Function \"%s\" expects %d generics resolver but %d %s provided"
             record.fn_name.v record.expected record.found
             ( if record.found >= 2 then
                 "were"
               else
                 "was"
             )
          )
    | Uncompatible_type_for_C_Function { fn_name; ktype } ->
        string_of_located_error ktype
          (sprintf
             "Function \"%s\", this expression has the type \"%s\" but is not \
              compatible with a C type"
             fn_name.v (string_of_ktype ktype.v)
          )
    | Uncompatible_type_for_Syscall { index; syscall_decl } ->
        let wrong_type =
          index
          |> Option.map (List.nth syscall_decl.parameters)
          |> Option.value ~default:syscall_decl.return_type
        in
        string_of_located_error wrong_type
          (sprintf
             "\"%s\" is not compatible with a C type and cannot be used in a \
              signature of a system call"
             (string_of_ktype wrong_type.v)
          )
    | Mismatched_Parameters_Type { fn_name; expected; found } ->
        string_of_located_error found
          (sprintf
             "Function \"%s\", this expression has the type \"%s\" but an \
              expression of the type \"%s\" was expected. \"%s\" and \"%s\" \
              aren't compatible"
             fn_name (string_of_ktype found.v) (string_of_ktype expected)
             (string_of_ktype found.v) (string_of_ktype expected)
          )
    | Unknow_Function_Error ->
        "Unknow_Function_Error"

  let string_of_operator_error =
    let open Ast.Error in
    let open Printf in
    let open Ast.OperatorFunction in
    function
    | Invalid_pointer_arithmetic kt ->
        string_of_located_error kt
          (sprintf
             "this expression has the type \"%s\", which cannot be used in \
              pointer arithmetic"
             (string_of_ktype kt.v)
          )
    | No_built_in_op record ->
        string_of_located_error record.ktype
          (sprintf "Builtin type \"%s\" doesn't implement the operator \"%s\""
             (string_of_ktype record.ktype.v)
             (symbole_of_operator record.bin_op)
          )
    | Incompatible_Type { bin_op; expr_loc; lhs; rhs } ->
        string_of_located_error expr_loc
          (sprintf
             "left operande has the type \"%s\" but the right one has the type \
              \"%s\". \"%s\" and \"%s\" aren't compatible for operator \"%s\""
             (string_of_ktype lhs.v) (string_of_ktype rhs.v)
             (string_of_ktype lhs.v) (string_of_ktype rhs.v)
             (symbole_of_operator bin_op)
          )
    | Operator_not_found record ->
        string_of_located_error record.ktype
          (sprintf "Type \"%s\" doesn't implement the operator \"%s\""
             (string_of_ktype record.ktype.v)
             (symbole_of_operator record.bin_op)
          )
    | Too_many_operator_declaration { operator_decls; bin_op; ktype } ->
        string_of_located_error ktype
          (sprintf
             "Type \"%s\" defines too many times the operator \"%s\". \
              Redefinition occures here:\n\n\
              %s\n"
             (string_of_ktype ktype.v)
             (symbole_of_operator bin_op)
             (operator_decls
             |> List.map (fun (path, op_decls) ->
                    let open Asthelper.ParserOperator in
                    sprintf "\tModule \"%s\":\n\t%s\n" path
                      (op_decls
                      |> List.map (fun op ->
                             let symbole = op |> backticked_operator in
                             sprintf "\t-%s, %s"
                               (symbole |> position |> string_of_position_error)
                               (string_of_signature symbole (op |> parameters)
                                  (op |> return_ktype)
                               )
                         )
                      |> String.concat "\n\t"
                      )
                )
             |> String.concat ", "
             )
          )
    | Not_Boolean_operand_in_And kt ->
        string_of_located_error kt
          (sprintf
             "this expression has the type \"%s\", but an expression of type \
              \"%s\" was expected in operator \"%s\""
             (string_of_ktype kt.v) (string_of_ktype TBool)
             (symbole_of_operator And)
          )
    | Not_Boolean_operand_in_Or kt ->
        string_of_located_error kt
          (sprintf
             "this expression has the type \"%s\", but an expression of type \
              \"%s\" was expected in operator \"%s\""
             (string_of_ktype kt.v) (string_of_ktype TBool)
             (symbole_of_operator Or)
          )
    | Invalid_Uminus_for_Unsigned_integer size ->
        string_of_located_error size
          (sprintf "Cannot use unary minus for unsigned integer: \"%s\""
             (string_of_ktype (TInteger (Ast.Unsigned, size.v)))
          )

  let string_of_switch_error =
    let open Ast.Error in
    function
    | Duplicated_case name ->
        string_of_located_error name (sprintf "case \"%s\" is duplicated" name.v)
    | Not_fully_known_ktype ktype ->
        string_of_located_error ktype
          (sprintf
             "this expression has the type \"%s\" which is not fully unknown"
             (string_of_ktype ktype.v)
          )
    | Not_enum_type_in_switch_Expression e ->
        string_of_located_error e
          (sprintf
             "This expression has the type \"%s\" but \"%s\" is not an enum"
             (string_of_ktype e.v) (string_of_ktype e.v)
          )
    | Not_all_cases_handled
        { expression_loc; missing_variant = variant, assotype } ->
        string_of_located_error expression_loc
          (sprintf "Not all cases are handled, miss at least: %s"
             (sprintf "\"%s(%s)\"" variant.v
                (assotype
                |> List.map (fun ktl -> ktl |> Position.value |> string_of_ktype)
                |> String.concat ", "
                )
             )
          )
    | Variant_not_found { enum_decl; variant } ->
        string_of_located_error variant
          (sprintf "enum \"%s\" doesn't contain the case \"%s\""
             enum_decl.enum_name.v variant.v
          )
    | Mismatched_Assoc_length { variant; expected; found } ->
        string_of_located_error variant
          (sprintf "case \"%s\" expects %d associated values but %d %s provided"
             variant.v expected found
             ( if found > 1 then
                 "were"
               else
                 "was"
             )
          )
    | Incompatible_Binding_Ktype
        {
          switch_expr = _;
          base_variant;
          base_bound_id;
          base_bound_ktype;
          wrong_variant;
          wrong_bound_id;
          wrong_bound_ktype;
        } ->
        string_of_located_error wrong_bound_id
          (sprintf
             "Variable bound \"%s\" in the case \"%s\" has the type \"%s\" but \
              due to the binding of \"%s\" in the case \"%s\" the expected \
              type of \"%s\" is \"%s\""
             wrong_bound_id.v wrong_variant.v
             (string_of_ktype wrong_bound_ktype.v)
             base_bound_id.v base_variant.v base_bound_id.v
             (string_of_ktype base_bound_ktype.v)
          )
    | Incompatible_Binding_Name
        {
          switch_expr = _;
          base_variant;
          base_bound_id;
          wrong_variant;
          wrong_bound_id;
        } ->
        string_of_located_error wrong_bound_id
          (sprintf
             "Variable bound \"%s\" in the case \"%s\" is different from the \
              bound variable \"%s\" in the case \"%s\""
             wrong_bound_id.v wrong_variant.v base_bound_id.v base_variant.v
          )
    | Incompatible_Binding_Position
        {
          base_index;
          base_variant;
          base_bound_id;
          wrong_index;
          wrong_variant;
          wrong_bound_id;
        } ->
        string_of_located_error wrong_bound_id
          (sprintf
             "Variable bound \"%s\" at index %d in the case \"%s\" but due to \
              the binding of \"%s\" in the case \"%s\" the expected index of \
              \"%s\" is %d"
             wrong_bound_id.v wrong_index wrong_variant.v base_bound_id.v
             base_variant.v wrong_bound_id.v base_index
          )
    | Identifier_already_Bound s ->
        string_of_located_error s
          (sprintf "variable \"%s\" is already bound" s.v)

  let string_of_built_in_func_error =
    let open Ast.Error in
    let open Printf in
    function
    | Unknow_built_function fn_name ->
        string_of_located_error fn_name
          (sprintf "Unknown builtin function \"%s\"" fn_name.v)
    | Wrong_parameters { fn_name; expected; found } ->
        string_of_located_error found
          (sprintf
             "Builtin function \"%s\", this expression has the type \"%s\" but \
              an expression of type \"%s\" was expected"
             fn_name (string_of_ktype found.v) (string_of_ktype expected)
          )
    | Mismatched_Parameters_Length { fn_name; expected; found } ->
        string_of_located_error fn_name
          (sprintf
             "Builtin function \"%s\" expects %d argument%s but %d %s provided"
             fn_name.v expected
             ( if expected > 1 then
                 "s"
               else
                 ""
             )
             found
             ( if expected > 1 then
                 "were"
               else
                 "was"
             )
          )
    | Found_no_Integer { fn_name; found } ->
        string_of_located_error found
          (sprintf
             "Builtin function \"%s\" expects an integer but an expression of \
              type \"%s\" was provided"
             fn_name (string_of_ktype found.v)
          )
    | Builin_type_tag { fn_name; position; ktype } ->
        string_of_positioned_error position
        @@ sprintf
             "Builtin function \"%s\", this expression has the type \"%s\" \
              which is a built-in type and not an enum"
             fn_name (string_of_ktype ktype)
    | Struct_type_tag { fn_name; position; ktype } ->
        string_of_positioned_error position
        @@ sprintf
             "Builtin function \"%s\", this expression has the type \"%s\" \
              which is a struct type and not an enum"
             fn_name (string_of_ktype ktype)

  let quoted = sprintf "\"%s\""

  let string_of_ast_error =
    let open Ast.Error in
    function
    | Wrong_Assoc_Length_for_Parametrics record ->
        string_of_located_error record.type_name
          (sprintf "type \"%s\" expects %d parametric%s but %d %s given"
             record.type_name.v record.expected
             ( if record.expected > 1 then
                 "s"
               else
                 ""
             )
             record.found
             ( if record.expected > 1 then
                 "was"
               else
                 "were"
             )
          )
    | No_struct_field_acc { variable; ktype } ->
        string_of_located_error variable
          (sprintf "variable %s has the type %s which is not a struct"
             (quoted variable.v)
             (quoted @@ string_of_ktype ktype)
          )
    | Undefined_Identifier s ->
        string_of_located_error s (sprintf "Undefined Identifier \"%s\"" s.v)
    | Undefine_function s ->
        string_of_located_error s (sprintf "Undefined Function \"%s\"" s.v)
    | Undefined_Const s ->
        string_of_located_error s (sprintf "Undefined Constant \"%s\"" s.v)
    | Undefined_Struct s ->
        string_of_located_error s (sprintf "Undefined Struct \"%s\"" s.v)
    | Unbound_Module s ->
        string_of_located_error s (sprintf "Unbound Module \"%s\"" s.v)
    | Undefine_Type s ->
        string_of_located_error s (sprintf "Undefined Type \"%s\"" s.v)
    | Struct_Error s ->
        string_of_struct_error s
    | Enum_Error e ->
        string_of_enum_error e
    | Statement_Error e ->
        string_of_statement_error e
    | Func_Error e ->
        string_of_function_error e
    | Operator_Error e ->
        string_of_operator_error e
    | Switch_error e ->
        string_of_switch_error e
    | Builtin_Func_Error e ->
        string_of_built_in_func_error e
    | Tuple_access_for_non_tuple_type { location; ktype } ->
        string_of_located_error
          { v = (); position = location }
          (sprintf
             "this expression has the type \"%s\" which is not a tuple. It \
              can't be accessed by index"
             (string_of_ktype ktype)
          )
    | Field_access_for_non_struct_type { location; ktype } ->
        string_of_located_error
          { v = (); position = location }
          (sprintf
             "this expression has the type \"%s\" which is not a struct. It \
              doesn't contain any field"
             (string_of_ktype ktype)
          )
    | Array_subscript_None_array { found } ->
        string_of_located_error found
        @@ sprintf
             "this expression has the type \"%s\" which is not an array. It \
              can't be accessed by subscripting"
             (string_of_ktype found.v)
    | Array_Non_Integer_Index { found } ->
        string_of_located_error found
        @@ sprintf
             "this expression has the type \"%s\" which is not an integer \
              type. It can't be used for array index"
             (string_of_ktype found.v)
    | Uncompatible_type { expected; found } ->
        string_of_located_error found
          (sprintf
             "this expression has the type \"%s\" but an expression of type \
              \"%s\" was expected, \"%s\" and \"%s\" aren't compatible"
             (string_of_ktype found.v) (string_of_ktype expected)
             (string_of_ktype expected) (string_of_ktype found.v)
          )
    | Uncompatible_type_If_Else e ->
        string_of_located_error e.position
          (sprintf
             "If block has the type \"%s\" but else block has type \"%s\", \
              \"%s\" and \"%s\" aren't compatible"
             (string_of_ktype e.if_type)
             (string_of_ktype e.else_type)
             (string_of_ktype e.if_type)
             (string_of_ktype e.else_type)
          )
    | Not_Boolean_Type_Condition e ->
        string_of_located_error e.found
          (sprintf
             "this expression has the type \"%s\" but an expression of type \
              \"%s\" was expected"
             (string_of_ktype e.found.v)
             (string_of_ktype TBool)
          )
    | Not_unit_type_while e ->
        string_of_located_error e.position
          (sprintf
             "While loop body must have the unit type, but has the \"%s\" type"
             (string_of_ktype e.wrong_type)
          )
    | Impossible_tuple_access { index; ktypes } ->
        string_of_located_error index
          (sprintf "Index \"%Lu\" is outside the arity of \"%s\" (%Lu >= %u)"
             index.v
             (string_of_ktype (TTuple ktypes))
             index.v (List.length ktypes)
          )
    | Impossible_field_Access { field; struct_decl } ->
        string_of_located_error field
          (sprintf "Struct \"%s\" doesn't contain a field named \"%s\""
             struct_decl.struct_name.v field.v
          )
    | Enum_Access_field record ->
        sprintf "%s Enum doesn't have field : \"%s\" for enum : \"%s\""
          (string_of_position_error record.field.position)
          record.field.v record.enum_decl.enum_name.v
    | Unvalid_Deference identifier ->
        string_of_located_error identifier
          (sprintf "Invalid Deference for \"%s\"" identifier.v)
    | Conflicting_function_declaration { fn_def_path; fn_name; fn_decls } ->
        string_of_located_error fn_name
          (sprintf
             "Function \"%s\" has multiple declarations. Declarations occures \
              here [%s]"
             fn_name.v
             (fn_decls
             |> List.map (fun fn_decl ->
                    sprintf "%s, %s::%s"
                      (fn_decl |> Ast.Function_Decl.calling_name
                     |> Position.position |> string_of_position_error
                      )
                      fn_def_path
                      (string_of_signature
                         (fn_decl |> Ast.Function_Decl.calling_name)
                         (fn_decl |> Ast.Function_Decl.parameters)
                         (fn_decl |> Ast.Function_Decl.return_type)
                      )
                )
             |> String.concat ", "
             )
          )
    | Conflicting_type_declaration { path; ktype_name; type_decls } ->
        string_of_located_error ktype_name
          (sprintf
             "Type \"%s\" has multiple declarations. Declarations occures here \
              [%s]"
             ktype_name.v
             (type_decls
             |> List.map (fun type_decl ->
                    sprintf "%s, %s::%s"
                      (type_decl |> Asthelper.Type_Decl.type_name
                     |> Position.position |> string_of_position_error
                      )
                      path
                      (type_decl |> Asthelper.Type_Decl.type_name
                     |> Position.value
                      )
                )
             |> String.concat ", "
             )
          )
    | Conflicting_module_path_declaration { module_path; _ } ->
        string_of_located_error module_path
          (sprintf "Multiple modules have the name \"%s\"" module_path.v)

  let string_of_external_func_error =
    let open Printf in
    let open VError in
    function
    | Unit_parameter external_func_decl ->
        string_of_located_error external_func_decl.sig_name
          (sprintf
             "External function \"%s\" has a unit/void parameter, which is not \
              valid"
             external_func_decl.sig_name.v
          )
    | Not_C_compatible_type (external_func_decl, ktype) ->
        string_of_located_error ktype
          (sprintf
             "External function \"%s\", Type \"%s\" is not a C compatible type"
             external_func_decl.sig_name.v (string_of_ktype ktype.v)
          )
    | Too_much_parameters { external_func_decl; limit; found } ->
        string_of_located_error external_func_decl.sig_name
          (sprintf
             "External function \"%s\" has %d parameters but the limit of \
              parameters is %d"
             external_func_decl.sig_name.v found limit
          )

  let string_of_sycall_error =
    let open Printf in
    let open VError in
    function
    | Syscall_Unit_parameter syscall_decl ->
        string_of_located_error syscall_decl.syscall_name
          (sprintf
             "System call \"%s\" has a unit/void parameter, which is not valid"
             syscall_decl.syscall_name.v
          )
    | Syscall_Not_C_compatible_type (syscall_decl, ktype) ->
        string_of_located_error ktype
          (sprintf "System call \"%s\", Type \"%s\" is not a C compatible type"
             syscall_decl.syscall_name.v (string_of_ktype ktype.v)
          )
    | Syscall_Too_much_parameters { syscall_decl; limit; found } ->
        string_of_located_error syscall_decl.syscall_name
          (sprintf
             "System call \"%s\" has %d parameters but the limit of parameters \
              is %d"
             syscall_decl.syscall_name.v found limit
          )

  let string_of_struct_error =
    let open Printf in
    let open VError in
    function
    | SCyclic_Declaration struct_decl ->
        string_of_located_error struct_decl.struct_name
          (sprintf "Struct \"%s\" has the cyclic declaration"
             struct_decl.struct_name.v
          )
    | SDuplicated_field { field; struct_decl } ->
        string_of_located_error field
          (sprintf "field \"%s\" appears multiple type in \"%s\" declaration"
             field.v struct_decl.struct_name.v
          )

  let string_of_enum_error =
    let open Printf in
    let open VError in
    function
    | ECyclic_Declaration enum_decl ->
        string_of_located_error enum_decl.enum_name
          (sprintf "Enum \"%s\" has the cyclic declaration"
             enum_decl.enum_name.v
          )
    | EDuplicated_variant_name { variant; enum_decl } ->
        string_of_located_error variant
          (sprintf "variant \"%s\" appears multiple type in \"%s\" declaration"
             variant.v enum_decl.enum_name.v
          )

  let string_of_operator_error =
    let open Printf in
    let open VError in
    function
    | Op_built_overload kt ->
        string_of_located_error kt
          (sprintf "Try to overload builtin type \"%s\"" (string_of_ktype kt.v))
    | Op_binary_op_diff_type { operator; lhs; rhs } ->
        string_of_located_error operator
          (sprintf
             "Operator \"%s\", left operande has the type \"%s\" but the right \
              one has the type \"%s\". In operator override, left operande and \
              right one must have the same type"
             operator.v (string_of_ktype lhs) (string_of_ktype rhs)
          )
    | Op_wrong_return_type_error { op; expected; found } ->
        string_of_located_error op
          (sprintf
             "Operator \"%s\" has the return type \"%s\" but \"%s\" type was \
              expected"
             op.v (found |> string_of_ktype)
             (expected |> string_of_ktype)
          )

  let string_of_function_error =
    let open Printf in
    let open VError in
    function
    | Wrong_signature_for_main function_decl ->
        string_of_located_error function_decl.fn_name
          (sprintf
             "Function \"%s\", this function doesn't have a valid signature \
              for a main function"
             function_decl.fn_name.v
          )
    | Duplicated_parameters { duplicatated_field; function_decl } ->
        string_of_located_error duplicatated_field
          (sprintf "Function \"%s\", parameter \"%s\" is duplicated"
             function_decl.fn_name.v duplicatated_field.v
          )
    | Function_Unit_parameter { field; function_decl } ->
        string_of_located_error field
          (sprintf
             "Function \"%s\",  parameter \"%s\" has the type \"%s\", which is \
              forbidden"
             function_decl.fn_name.v field.v (string_of_ktype TUnit)
          )

  let string_of_module_error =
    let open Printf in
    let open VError in
    function
    | Duplicate_function_declaration { path; functions } ->
        let calling_name = functions |> List.hd |> Function_Decl.calling_name in
        sprintf "Conflicting function declarations for \"%s\" between:\n\t-%s\n"
          calling_name.v
          (functions
          |> List.map (fun fn_decl ->
                 sprintf "%s, %s::%s"
                   (fn_decl |> Function_Decl.calling_name |> Position.position
                  |> string_of_position_error
                   )
                   path
                   (string_of_signature calling_name
                      (fn_decl |> Ast.Function_Decl.parameters)
                      (fn_decl |> Ast.Function_Decl.return_type)
                   )
             )
          |> String.concat "\n\t-"
          )
    | Duplicate_type_declaration { path; types } ->
        let type_name = types |> List.hd |> Asthelper.Type_Decl.type_name in
        sprintf "Conflicting type declarations for \"%s\" between:\n\t-%s\n"
          type_name.v
          (types
          |> List.map (fun type_decl ->
                 sprintf "%s, %s::%s"
                   (type_decl |> Asthelper.Type_Decl.type_name |> position
                  |> string_of_position_error
                   )
                   path
                   (string_of_type_decl type_decl)
             )
          |> String.concat "\n\t-"
          )
    | Duplicate_const_declaration { path; consts } ->
        let const_name = consts |> List.hd in
        sprintf "Conflicting constants declaration for \"%s\" between:\n\t-%s\n"
          const_name.const_name.v
          (consts
          |> List.map (fun const_decl ->
                 sprintf "%s, %s::%s"
                   (const_decl.const_name |> position
                  |> string_of_position_error
                   )
                   path const_decl.const_name.v
             )
          |> String.concat "\n\t-"
          )
    | Duplicate_operator_declaration { path; operators } ->
        let open Asthelper.ParserOperator in
        let operator =
          operators |> List.hd |> Asthelper.ParserOperator.operator
        in
        string_of_located_error operator
          (sprintf
             "Conflicting operator declaration for \"%s\" between:\n\t-%s\n"
             operator.v
             (operators
             |> List.map (fun operator_decl ->
                    let op =
                      operator_decl |> Asthelper.ParserOperator.operator
                    in
                    sprintf "%s, %s::%s"
                      (op |> Position.position |> string_of_position_error)
                      path
                      (string_of_signature op
                         (operator_decl |> parameters)
                         (operator_decl |> return_ktype)
                      )
                )
             |> String.concat "\n\t-"
             )
          )
    | Main_no_kosu_function decl ->
        let position, message =
          match decl with
          | `syscall_decl syscall_decl ->
              (syscall_decl.syscall_name, "A system call")
          | `external_decl external_func_decl ->
              (external_func_decl.sig_name, "An external function")
        in
        string_of_located_error position
          (sprintf "%s cannot be used as a main function" message)

  let string_of_validation_error =
    let open Printf in
    let open VError in
    function
    | External_Func_Error e ->
        string_of_external_func_error e
    | Syscall_Error e ->
        string_of_sycall_error e
    | Struct_Error e ->
        string_of_struct_error e
    | Enum_Error e ->
        string_of_enum_error e
    | Operator_Error e ->
        string_of_operator_error e
    | Function_Error e ->
        string_of_function_error e
    | Module_Error e ->
        string_of_module_error e
    | Too_many_Main list ->
        sprintf "Conflicting \"main\" function declaration between:\n\n%s\n"
          (list
          |> List.map (fun (path, fn_decls) ->
                 sprintf "\tModule \"%s\":\n\t%s" path
                   (fn_decls
                   |> List.map (fun fn_decl ->
                          sprintf "\t-%s, %s"
                            (fn_decl |> Ast.Function_Decl.calling_name
                           |> Position.position |> string_of_position_error
                            )
                            (string_of_signature
                               (fn_decl |> Ast.Function_Decl.calling_name)
                               (fn_decl |> Ast.Function_Decl.parameters)
                               (fn_decl |> Ast.Function_Decl.return_type)
                            )
                      )
                   |> String.concat "\n\t"
                   )
             )
          |> String.concat "\n\n"
          )
    | Ast_Error e ->
        string_of_ast_error e
end

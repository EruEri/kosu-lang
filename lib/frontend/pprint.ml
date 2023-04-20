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

open Ast
open Position
open Printf

let char_of_signedness = function Signed -> 's' | Unsigned -> 'u'

let string_of_isize = function
  | I8 -> "8"
  | I16 -> "16"
  | I32 -> "32"
  | I64 -> "64"

let string_of_fsize = function F32 -> "32" | F64 -> "64"

let located_symbole_of_operator = function
  | Unary { op; _ } ->
      op |> Position.map (function PNot -> "!" | PUMinus -> "(-.)")
  | Binary { op; _ } ->
      op
      |> Position.map (function
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
           | Equal -> "==")

let string_of_position_error { start_position; end_position } =
  let start_position_line, start_position_column =
    line_column_of_position start_position
  in
  let end_position_line, end_position_column =
    line_column_of_position end_position
  in
  let column_string =
    Printf.sprintf "%d%s" start_position_column
      (if start_position_column = end_position_column then ""
      else " - " ^ string_of_int end_position_column)
  in
  if start_position_line = end_position_line then
    Printf.sprintf "Line %d, Characters %s" start_position_line column_string
  else
    Printf.sprintf "Lines %d-%d, Characters %d-%d" start_position_line
      end_position_line start_position_column end_position_column

let string_of_located_error a b =
  Printf.sprintf "%s : %s" (string_of_position_error a.position) b

let rec string_of_ktype = function
  | TParametric_identifier { module_path; parametrics_type; name } ->
      sprintf "%s %s(%s)" module_path.v name.v
        (parametrics_type
        |> List.map (fun s -> string_of_ktype s.v)
        |> String.concat ", ")
  | TType_Identifier { module_path; name } ->
      sprintf "%s%s"
        (if module_path.v = "" then "" else sprintf "%s::" module_path.v)
        name.v
  | TInteger (sign, size) ->
      sprintf "%c%s" (char_of_signedness sign) (string_of_isize size)
  | TFloat fsize -> sprintf "f%s" (string_of_fsize fsize)
  | TPointer ktype -> sprintf "*%s" (string_of_ktype ktype.v)
  | TTuple ktypes ->
      sprintf "(%s)"
        (ktypes |> List.map (fun s -> string_of_ktype s.v) |> String.concat ", ")
  | TFunction (parameters, r_type) ->
      sprintf "(%s) -> %s"
        (parameters
        |> List.map (fun s -> string_of_ktype s.v)
        |> String.concat ", ")
        (string_of_ktype r_type.v)
  | TString_lit -> "stringl"
  | TChar -> "char"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TUnknow -> "unknow"

let rec string_of_kbody = function
  | (statements : kstatement location list), (expr : kexpression location) ->
      sprintf "{\n  %s  %s\n}"
        (statements
        |> List.map (fun s -> s |> value |> string_of_kstatement)
        |> String.concat "\n  ")
        (string_of_kexpression expr.v)

and string_of_affected_value = function
  | AFVariable s -> s.v
  | AFField { variable; fields } ->
      sprintf "%s.%s" variable.v
        (fields |> List.map Position.value |> String.concat ".")

and string_of_kstatement = function
  | SDeclaration { is_const; variable_name; explicit_type; expression } ->
      sprintf "%s %s : %s = %s;"
        (if is_const then "const" else "var")
        variable_name.v
        (explicit_type
        |> Option.map (fun kt -> string_of_ktype kt.v)
        |> Option.value ~default:"")
        (expression.v |> string_of_kexpression)
  | SAffection (id, expression) ->
      sprintf "%s = %s;"
        (string_of_affected_value id)
        (expression.v |> string_of_kexpression)
  | SDiscard expr -> sprintf "discard %s;" (string_of_kexpression expr.v)
  | SDerefAffectation (id, exprssion) ->
      sprintf "*%s = %s;"
        (string_of_affected_value id)
        (string_of_kexpression exprssion.v)

and string_of_kexpression = function
  | Empty -> "empty"
  | True -> "true"
  | False -> "false"
  | ENullptr -> "nullptr"
  | EChar c -> Printf.sprintf "\'%c\'" c
  | EInteger (sign, _, value) -> (
      match sign with
      | Signed -> sprintf "%Ld" value
      | Unsigned -> sprintf "%Lu" value)
  | EFloat (_, f) -> string_of_float f
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
  | EFieldAcces { first_expr; field } ->
      sprintf "(%s)->%s" (string_of_kexpression first_expr.v) field.v
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
        (enum_name |> Option.fold ~none:" " ~some:Position.value)
        variant.v
        (if assoc_exprs = [] then ""
        else
          sprintf "(%s)"
            (assoc_exprs |> List.map Position.value
            |> List.map string_of_kexpression
            |> String.concat ", "))
  | EWhile (condition, body) ->
      sprintf "while (%s) %s"
        (string_of_kexpression condition.v)
        (string_of_kbody body)
  | ETuple exprs ->
      sprintf "(%s)"
        (exprs |> List.map Position.value
        |> List.map string_of_kexpression
        |> String.concat ", ")
  | EFunction_call { modules_path; generics_resolver; fn_name; parameters } ->
      sprintf "%s%s%s(%s)"
        (Util.string_of_module_path modules_path.v)
        fn_name.v
        (generics_resolver
        |> Option.map (fun kts ->
               sprintf "::<%s>"
                 (kts
                 |> List.map (fun gkt -> gkt |> value |> string_of_ktype)
                 |> String.concat ", "))
        |> Option.value ~default:"")
        (parameters |> List.map Position.value
        |> List.map string_of_kexpression
        |> String.concat ", ")
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
        (parameters |> List.map Position.value
        |> List.map string_of_kexpression
        |> String.concat ", ")

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
        |> List.map (Option.fold ~none:"_" ~some:Position.value)
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
    |> List.map (fun (sl, kl) -> (sl.v, kl |> List.map Position.value))
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
  sprintf "%s(%s)%s" fn_name.v
    (parameters
    |> List.map (fun ktl -> ktl |> Position.value |> string_of_ktype)
    |> String.concat ", ")
    (return_type |> Position.value |> string_of_ktype)

let string_of_external_func_decl (efucn_decl : external_func_decl) =
  sprintf "external %s(%s%s) %s %s" efucn_decl.sig_name.v
    (efucn_decl.fn_parameters |> List.map Position.value
   |> List.map string_of_ktype |> String.concat ", ")
    (if efucn_decl.is_variadic then ";..." else "")
    (efucn_decl.r_type.v |> string_of_ktype)
    (efucn_decl.c_name
    |> Option.map (fun s -> sprintf " = %s" s)
    |> Option.value ~default:"")

let string_of_syscall (syscall_decl : syscall_decl) =
  sprintf "syscall %s(%s) %s" syscall_decl.syscall_name.v
    (syscall_decl.parameters |> List.map Position.value
   |> List.map string_of_ktype |> String.concat ", ")
    (syscall_decl.return_type.v |> string_of_ktype)

let string_of_func_decl (function_decl : function_decl) =
  sprintf "fn %s%s(%s)%s%s" function_decl.fn_name.v
    (if function_decl.generics = [] then ""
    else
      sprintf "<%s>"
        (function_decl.generics |> List.map Position.value |> String.concat ", "))
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

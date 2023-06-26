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

open Asttyped
open Printf
open KosuFrontend.Ast
open KosuFrontend.Pprint

let symbole_of_roperator = function
  | RUnary { op; _ } -> (
      op |> function PNot -> "!" | PUMinus -> "(-.)"
    )
  | RBinary { op; _ } -> (
      match op with
      | ParBinOp op -> (
          match op with
          | Add ->
              "+"
          | Minus ->
              "-"
          | Mult ->
              "*"
          | Div ->
              "/"
          | Modulo ->
              "%"
          | BitwiseAnd ->
              "&"
          | BitwiseOr ->
              "|"
          | BitwiseXor ->
              "^"
          | ShiftLeft ->
              "<<"
          | ShiftRight ->
              ">>"
          | Spaceship ->
              "<=>"
          | Equal ->
              "=="
        )
      | ParserDiff ->
          "!="
      | ParserInfEq ->
          "<="
      | ParserSupEq ->
          ">="
      | ParserInf ->
          "<"
      | ParserSup ->
          ">"
    )

let name_of_roperator = function
  | RUnary { op; _ } -> (
      op |> function PNot -> "not" | PUMinus -> "unminus"
    )
  | RBinary { op; _ } -> (
      match op with
      | ParBinOp op -> (
          match op with
          | Add ->
              "add"
          | Minus ->
              "minus"
          | Mult ->
              "mult"
          | Div ->
              "dic"
          | Modulo ->
              "module"
          | BitwiseAnd ->
              "bitwiseand"
          | BitwiseOr ->
              "bitwiseor"
          | BitwiseXor ->
              "botwisexor"
          | ShiftLeft ->
              "shiftleft"
          | ShiftRight ->
              "shiftright"
          | Spaceship ->
              "spaceshift"
          | Equal ->
              "equal"
        )
      | ParserDiff ->
          "diff"
      | ParserInfEq ->
          "infeq"
      | ParserSupEq ->
          "supeq"
      | ParserInf ->
          "inf"
      | ParserSup ->
          "sup"
    )

let string_name_of_extended_parser_binary = function
  | ParBinOp op ->
      KosuFrontend.Asthelper.ParserOperator.string_name_of_parser_binary op
  | ParserDiff ->
      "diff"
  | ParserInfEq ->
      "infeq"
  | ParserSupEq ->
      "supeq"
  | ParserInf ->
      "inf"
  | ParserSup ->
      "sup"

let rec string_of_rktype = function
  | RTParametric_identifier { module_path; parametrics_type; name } ->
      sprintf "(%s)%s %s"
        (parametrics_type |> List.map string_of_rktype |> String.concat ", ")
        module_path name
  | RTType_Identifier { module_path; name } ->
      sprintf "%s%s"
        ( if module_path = "" then
            ""
          else
            sprintf "%s::" module_path
        )
        name
  | RTInteger (sign, size) ->
      sprintf "%c%s" (char_of_signedness sign) (string_of_isize size)
  | RTFloat fsize ->
      sprintf "f%s" (string_of_fsize fsize)
  | RTPointer ktype ->
      sprintf "*%s" (string_of_rktype ktype)
  | RTTuple ktypes ->
      sprintf "(%s)" (ktypes |> List.map string_of_rktype |> String.concat ", ")
  | RTFunction (parameters, r_type) ->
      sprintf "(%s) -> %s"
        (parameters |> List.map string_of_rktype |> String.concat ", ")
        (string_of_rktype r_type)
  | RTArray { size; rktype } ->
      sprintf "array(%Lu, %s)" size @@ string_of_rktype rktype
  | RTString_lit ->
      "stringl"
  | RTOrdered ->
      "order"
  | RTBool ->
      "bool"
  | RTUnit ->
      "unit"
  | RTChar ->
      "char"
  | RTUnknow ->
      "unknwon"

let asm_module_path_name =
  String.map (fun c ->
      if c = ':' then
        '_'
      else
        c
  )

let rec string_of_label_rktype = function
  | RTParametric_identifier { module_path; parametrics_type; name } ->
      sprintf "%s_%s_%s"
        (parametrics_type
        |> List.map string_of_label_rktype
        |> String.concat "__"
        )
        (asm_module_path_name module_path)
        name
  | RTType_Identifier { module_path; name } ->
      sprintf "%s%s" (asm_module_path_name module_path) name
  | RTInteger (sign, size) ->
      sprintf "%c%s" (char_of_signedness sign) (string_of_isize size)
  | RTFloat fsize ->
      sprintf "f%s" (string_of_fsize fsize)
  | RTPointer ktype ->
      sprintf "ptr_%s" (string_of_rktype ktype)
  | RTTuple ktypes ->
      sprintf "(%s)"
        (ktypes |> List.map string_of_label_rktype |> String.concat "_")
  | RTArray { size; rktype } ->
      sprintf "array_%Lu_%s_" size @@ string_of_rktype rktype
  | RTFunction (parameters, r_type) ->
      sprintf "(%s) -> %s"
        (parameters |> List.map string_of_label_rktype |> String.concat "_")
        (string_of_label_rktype r_type)
  | RTString_lit ->
      "stringl"
  | RTOrdered ->
      "order"
  | RTBool ->
      "bool"
  | RTUnit ->
      "unit"
  | RTChar ->
      "char"
  | RTUnknow ->
      "unknwon"

let rec string_of_rkbody = function
  | (statements : rkstatement list), (expr : typed_expression) ->
      sprintf "{\n  \t%s  \n\t%s\n}"
        (statements |> List.map string_of_rkstatement |> String.concat "\n\t  ")
        (string_of_typed_expression expr)

and string_of_raffacted_value = function
  | RAFVariable (s, _) ->
      s
  | RAFField { variable = variable, _; fields } ->
      sprintf "%s.%s" variable (fields |> String.concat ".")

and string_of_rkstatement = function
  | RSDeclaration { is_const; variable_name; typed_expression } ->
      sprintf "%s %s : %s;"
        ( if is_const then
            "const"
          else
            "var"
        )
        variable_name
        (typed_expression |> string_of_typed_expression)
  | RSAffection (id, expression) ->
      sprintf "%s = %s;"
        (string_of_raffacted_value id)
        (expression |> string_of_typed_expression)
  | RSDiscard expr ->
      sprintf "discard %s;" (string_of_typed_expression expr)
  | RSDerefAffectation (id, exprssion) ->
      sprintf "*%s = %s;"
        (string_of_raffacted_value id)
        (string_of_typed_expression exprssion)

and string_of_typed_expression { rktype; rexpression } =
  sprintf "(%s : %s)"
    (string_of_rkexpression rexpression)
    (string_of_rktype rktype)

and string_of_rkexpression = function
  | REmpty ->
      "empty"
  | RTrue ->
      "true"
  | RFalse ->
      "false"
  | RENullptr ->
      "nullptr"
  | RECmpLess ->
      "lt"
  | RECmpEqual ->
      "eq"
  | RECmpGreater ->
      "gt"
  | REChar c ->
      Printf.sprintf "\'%c\'" c
  | REInteger (sign, _, value) -> (
      match sign with
      | Signed ->
          sprintf "%Ld" value
      | Unsigned ->
          sprintf "%Lu" value
    )
  | REFloat (_, f) ->
      string_of_float f
  | REBin_op bin | REBinOperator_Function_call bin ->
      string_of_rkbin_op bin
  | REUn_op un | REUnOperator_Function_call un ->
      string_of_rkunary_op un
  | RESizeof rktype ->
      sprintf "sizeof(%s)" (string_of_rktype rktype)
  | REstring s ->
      Printf.sprintf "\"%s\"" s
  | REAdress x ->
      sprintf "&%s" x
  | REDeference (indirection, id) ->
      sprintf "%s%s" (Util.string_of_chars indirection '*') id
  | REIdentifier { modules_path; identifier }
  | REConst_Identifier { modules_path; identifier } ->
      sprintf "%s%s" (Util.string_of_module_path modules_path) identifier
  | RETupleAccess { first_expr; index } ->
      sprintf "%s.%Lu" (string_of_typed_expression first_expr) index
  | REFieldAcces { first_expr; field } ->
      sprintf "%s.%s" (string_of_typed_expression first_expr) field
  | REStruct { modules_path; struct_name; fields } ->
      sprintf "%s%s { %s }"
        ( if modules_path = "" then
            ""
          else
            sprintf "%s::" modules_path
        )
        struct_name
        (fields
        |> List.map (fun (id, expr) ->
               sprintf "%s: %s" id (string_of_typed_expression expr)
           )
        |> String.concat ", "
        )
  | REEnum { modules_path; enum_name; variant; assoc_exprs } ->
      sprintf "%s%s.%s%s"
        (Util.string_of_module_path modules_path)
        (enum_name |> Option.fold ~none:" " ~some:Fun.id)
        variant
        ( if assoc_exprs = [] then
            ""
          else
            sprintf "(%s)"
              (assoc_exprs
              |> List.map string_of_typed_expression
              |> String.concat ", "
              )
        )
  | REArrayAccess {array_expr; index_expr} ->
    sprintf "%s[%s]" (string_of_typed_expression array_expr) 
    @@ string_of_typed_expression index_expr
  | REArray exprs ->
      sprintf "[%s]"
        (exprs |> List.map string_of_typed_expression |> String.concat ", ")
  | RETuple exprs ->
      sprintf "(%s)"
        (exprs |> List.map string_of_typed_expression |> String.concat ", ")
  | REFunction_call { modules_path; generics_resolver; fn_name; parameters } ->
      sprintf "%s%s%s(%s)"
        (Util.string_of_module_path modules_path)
        fn_name
        (generics_resolver
        |> Option.map (fun kts ->
               sprintf "::<%s>"
                 (kts |> List.map string_of_rktype |> String.concat ", ")
           )
        |> Option.value ~default:""
        )
        (parameters |> List.map string_of_typed_expression |> String.concat ", ")
  | REIf (expression, if_body, else_body) ->
      sprintf "if %s %s else %s"
        (string_of_typed_expression expression)
        (string_of_rkbody if_body)
        (string_of_rkbody else_body)
  | REWhile (expression, body) ->
      sprintf "while %s %s"
        (string_of_typed_expression expression)
        (string_of_rkbody body)
  | RECases { cases; else_case } ->
      sprintf "cases {\n %s else => %s}"
        (cases
        |> List.map (fun (expr, kbody) ->
               sprintf "%s => %s"
                 (expr |> string_of_typed_expression)
                 (string_of_rkbody kbody)
           )
        |> String.concat "\n"
        )
        (string_of_rkbody else_case)
  | RESwitch { rexpression; cases; wildcard_case } ->
      sprintf "switch %s {%s\n%s}"
        (string_of_typed_expression rexpression)
        (cases
        |> List.map (fun (sc, _, kbody) ->
               sprintf "%s => %s"
                 (sc |> List.map string_of_rswitch_case |> String.concat ", ")
                 (string_of_rkbody kbody)
           )
        |> String.concat "\n"
        )
        ( match wildcard_case with
        | None ->
            ""
        | Some kbody ->
            sprintf "_ => %s" (string_of_rkbody kbody)
        )
  | REBuiltin_Function_call { fn_name; parameters } ->
      sprintf "@%s(%s)"
        (KosuFrontend.Asthelper.Builtin_Function.fn_name_of_built_in_fn fn_name)
        (parameters |> List.map string_of_typed_expression |> String.concat ", ")

and string_of_rkbin_op = function
  | RBAdd (lhs, rhs) ->
      sprintf "(%s + %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBMinus (lhs, rhs) ->
      sprintf "(%s - %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBMult (lhs, rhs) ->
      sprintf "(%s * %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBDiv (lhs, rhs) ->
      sprintf "(%s / %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBMod (lhs, rhs) ->
      sprintf "(%s %% %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBBitwiseOr (lhs, rhs) ->
      sprintf "(%s | %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBBitwiseXor (lhs, rhs) ->
      sprintf "(%s ^ %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBBitwiseAnd (lhs, rhs) ->
      sprintf "(%s & %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBShiftLeft (lhs, rhs) ->
      sprintf "(%s << %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBShiftRight (lhs, rhs) ->
      sprintf "(%s >> %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBAnd (lhs, rhs) ->
      sprintf "(%s && %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBOr (lhs, rhs) ->
      sprintf "(%s || %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBInf (lhs, rhs) ->
      sprintf "(%s < %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBInfEq (lhs, rhs) ->
      sprintf "(%s <= %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBSup (lhs, rhs) ->
      sprintf "(%s > %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBSupEq (lhs, rhs) ->
      sprintf "(%s >= %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBEqual (lhs, rhs) ->
      sprintf "(%s == %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBDif (lhs, rhs) ->
      sprintf "(%s != %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)
  | RBCmp (lhs, rhs) ->
      sprintf "(%s <=> %s)"
        (string_of_typed_expression lhs)
        (string_of_typed_expression rhs)

and string_of_rkunary_op = function
  | RUMinus expr ->
      sprintf "-(%s)" (string_of_typed_expression expr)
  | RUNot expr ->
      sprintf "!(%s)" (string_of_typed_expression expr)

and string_of_rswitch_case = function
  | RSC_Enum_Identifier { variant } ->
      "." ^ variant
  | RSC_Enum_Identifier_Assoc { variant; assoc_ids } ->
      sprintf "%s(%s)" variant
        (assoc_ids
        |> List.map (Option.fold ~none:"_" ~some:Fun.id)
        |> String.concat ","
        )

let string_of_rfunc_decl (function_decl : rfunction_decl) =
  sprintf "fn %s%s(%s)%s%s" function_decl.rfn_name
    ( if function_decl.generics = [] then
        ""
      else
        sprintf "<%s>" (function_decl.generics |> String.concat ", ")
    )
    (function_decl.rparameters
    |> List.map (fun (id, ktype) -> sprintf "%s: %s" id (string_of_rktype ktype))
    |> String.concat ", "
    )
    (function_decl.return_type |> string_of_rktype)
    (function_decl.rbody |> string_of_rkbody)

let string_of_rtrue_func_decl (function_decl : rtrue_function_decl) =
  sprintf "fn %s(%s)%s%s" function_decl.rfn_name
    (function_decl.rparameters
    |> List.map (fun (id, ktype) -> sprintf "%s: %s" id (string_of_rktype ktype))
    |> String.concat ", "
    )
    (function_decl.return_type |> string_of_rktype)
    (function_decl.rbody |> string_of_rkbody)

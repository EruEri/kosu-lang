(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open SanPosition
open SanError
open SanPprint
open Printf

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

let string_of_lexer_error filename = function
  | Syntax_Error { position; current_lexeme; message; state } ->
      let s = string_of_position_error position in
      Printf.sprintf
        "\nFile \"%s\", %s : Unexpected \"%s\"\nSyntax Error : %s%s" filename s
        current_lexeme message
        (state
        |> Option.map (Printf.sprintf "Error in state \"%d\"")
        |> Option.value ~default:"")
  | Unexpected_escaped_char (position, lexeme) ->
      let s = position |> string_of_position_error in
      Printf.sprintf "%s: Unexpected Escaped character: %s" s lexeme
      |> Printf.sprintf "\nFile \"%s\", %s" filename
  | Unclosed_string position ->
      position |> string_of_position_error
      |> Printf.sprintf "\nFile \"%s\" %s: String litteral not terminated"
           filename

let string_of_san_error = function
  | Undefinded_Variable variable ->
      string_of_located_error variable
        (sprintf "undefined variable : \"%s\"" variable.value)
  | Undefined_Function fn_name ->
      string_of_located_error fn_name
        (sprintf "undefined function : \"%s\"" fn_name.value)
  | Already_define_variable redefined_variable ->
      string_of_located_error redefined_variable
        (sprintf "variable \"%s\" already defined" redefined_variable.value)
  | If_Not_boolean_type { error_location; expected = _; found } ->
      string_of_located_error error_location
        (sprintf
           "this expression has the type \"%s\" but the type \"%s\" is \
            requiered in condition"
           (string_of_san_type found)
           (string_of_san_type Boolean))
  | Wrong_return_type { error_location; expected; found } ->
      string_of_located_error error_location
        (sprintf
           "this expression return the type \"%s\" but the expected return \
            type is \"%s\""
           (string_of_san_type found)
           (string_of_san_type expected))
  | Incompatible_type { expected; found } ->
      string_of_located_error found
        (sprintf
           "this expression has the type \"%s\" but an expression of type \
            \"%s\" was expected"
           (string_of_san_type found.value)
           (string_of_san_type expected))
  | BinOp_diff_type { error_location; lhs; rhs } ->
      string_of_located_error error_location
        (sprintf
           "binary operation error: left expression has the type: \"%s\" but \
            the right one has the type: \"%s\""
           (string_of_san_type lhs) (string_of_san_type rhs))
  | Function_Wrong_args_number { error_location; fn_name; expected; found } ->
      string_of_located_error error_location
        (sprintf "the function \"%s\" expects %u arg%s but %u %s provided"
           fn_name expected
           (if expected > 1 then "s" else "")
           found
           (if found > 1 then "were" else "was"))
  | UnaryOperator_not_suppored { error_location; _ }
  | BinaryOperator_not_suppored { error_location; _ } ->
      string_of_located_error error_location
        (sprintf "bin / un unsuppoted op: todo improve later")

let string_of_san_validation_error = function
  | Duplicated_label (fn_name, label, duplicated_label) ->
      string_of_located_error fn_name
        (sprintf "Function \"%s\", label \"%s\" appears mutiples times: \n\t%s"
           fn_name.value label
           (duplicated_label
           |> List.map (fun dl -> string_of_position_error dl.position)
           |> String.concat "\n\t"))
  | Duplicated_paramters (fn_name, parameters, duplicated_parameters) ->
      string_of_located_error fn_name
        (sprintf
           "Function \"%s\", parameters \"%s\"  appears mutiples times: \n\t%s"
           fn_name.value parameters
           (duplicated_parameters
           |> List.map (fun dl -> string_of_position_error dl.position)
           |> String.concat "\n\t"))
  | Duplicated_function (fn_name, duplicated_function) ->
      string_of_located_error fn_name
        (sprintf "Function \"%s\": conflicting declaration: \n\t%s"
           fn_name.value
           (duplicated_function
           |> List.map (fun fd -> string_of_located_error fd fd.value)
           |> String.concat "\n\t"))
  | Sve_error san_err -> string_of_san_error san_err

let register_san_error () =
  Printexc.register_printer (fun exn ->
      match exn with
      | File_Lexer_Error (filename, e) ->
          Some (string_of_lexer_error filename e)
      | San_Validation_Error (filename, e) ->
          e |> string_of_san_validation_error |> Printf.sprintf "%s"
          |> Printf.sprintf "\nFile \"%s\", %s" filename
          |> Option.some
      | _ -> None)

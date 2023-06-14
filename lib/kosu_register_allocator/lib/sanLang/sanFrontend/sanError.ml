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

open SanAst
open SanPosition

type lexer_error =
  | Unexpected_escaped_char of san_position * string
  | Unclosed_string of san_position
  | Syntax_Error of {
      position : san_position;
      current_lexeme : string;
      message : string;
      state : int option;
    }

type san_error =
  | Undefinded_Variable of string loc
  | Undefined_Function of string loc
  | Already_define_variable of string loc
  | If_Not_boolean_type of {
      error_location : unit loc;
      expected : san_type;
      found : san_type;
    }
  | Wrong_return_type of {
      error_location : unit loc;
      expected : san_type;
      found : san_type;
    }
  | Incompatible_type of { expected : san_type; found : san_type loc }
  | BinOp_diff_type of {
      error_location : unit loc;
      lhs : san_type;
      rhs : san_type;
    }
  | UnaryOperator_not_suppored of {
      error_location : unit loc;
      unop : tac_unop;
      for_type : san_type;
    }
  | BinaryOperator_not_suppored of {
      error_location : unit loc;
      binop : tac_binop;
      for_type : san_type;
    }
  | Function_Wrong_args_number of {
      error_location : unit loc;
      fn_name : string;
      expected : int;
      found : int;
    }

type san_validation_error =
  | Duplicated_label of
      string loc * string * string loc list (* fnname, label , labelist*)
  | Duplicated_paramters of string loc * string * string loc list
  | Duplicated_function of string loc * string loc list
  | Sve_error of san_error

exception Raw_Lexer_Error of lexer_error
exception File_Lexer_Error of string * lexer_error
exception San_error of san_error
exception San_Validation_Error of string * san_validation_error

let raw_lexer_error e = Raw_Lexer_Error e
let san_error e = San_error e

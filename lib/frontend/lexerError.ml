(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
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

open Position

type lexer_error =
  | Forbidden_char of position * char
  | Unexpected_escaped_char of position * string
  | Invalid_keyword_for_build_in_function of position * string
  | Invalid_litteral_for_build_in_function of position * char
  | Not_finished_built_in_function of position
  | Unclosed_string of position
  | Unclosed_comment of position
  | Char_out_of_range of position * int
  | Char_Error of position
  | Syntax_Error of {
      position : Position.position;
      current_lexeme : string;
      message : string;
      state : int option;
    }

exception Raw_Lexer_Error of lexer_error
exception Lexer_Error of { filename : string; error : lexer_error }

let raw_lexer_error e = Raw_Lexer_Error e

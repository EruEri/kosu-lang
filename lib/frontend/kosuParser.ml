(*****************************************************************************************)
(*                                                                                       *)
(* This file is part of Kosu                                                             *)
(* Copyright (C) 2023 Yves Ndiaye                                                        *)
(*                                                                                       *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms       *)
(* of the GNU General Public License as published by the Free Software Foundation,       *)
(* either version 3 of the License, or (at your option) any later version.               *)
(*                                                                                       *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;     *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      *)
(* PURPOSE.  See the GNU General Public License for more details.                        *)
(* You should have received a copy of the GNU General Public License along with Kosu.    *)
(* If not, see <http://www.gnu.org/licenses/>.                                           *)
(*                                                                                       *)
(*****************************************************************************************)

open Lexing
module I = Parser.MenhirInterpreter

let get_parse_error env =
  match I.stack env with
  | (lazy Nil) ->
      ("Invalid syntax", None)
  | (lazy (Cons (I.Element (state, _element, _, _), _))) -> (
      let nb_state = I.number state in
      try (Kosu_parser_messages.message nb_state, Some nb_state)
      with Not_found ->
        ("invalid syntax (no specific message for this eror)", None)
    )

let rec parse lexbuf (checkpoint : Ast._module I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env -> (
      try
        let token = Lexer.token lexbuf in
        let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
        let checkpoint = I.offer checkpoint (token, startp, endp) in
        parse lexbuf checkpoint
      with
      | LexerError.Raw_Lexer_Error e ->
          Result.Error e
      | _ ->
          failwith "Uncatched Lexer Error"
    )
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError env ->
      let position = Position.current_position lexbuf in
      let current_lexeme = Lexing.lexeme lexbuf in
      let err, state = get_parse_error env in
      Result.error
      @@ LexerError.Syntax_Error
           { position; current_lexeme; message = err; state }
  | I.Accepted v ->
      Ok v
  | I.Rejected ->
      let position = Position.current_position lexbuf in
      let current_lexeme = Lexing.lexeme lexbuf in
      Result.error
      @@ LexerError.Syntax_Error
           {
             position;
             current_lexeme;
             message = "Parser reject the input";
             state = None;
           }

let rec kosu_repl_parse lexbuf
    (checkpoint : Ast.iexpression_node option I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env -> (
      try
        let token = Lexer.token lexbuf in
        let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
        let checkpoint = I.offer checkpoint (token, startp, endp) in
        kosu_repl_parse lexbuf checkpoint
      with
      | LexerError.Raw_Lexer_Error e ->
          Result.Error e
      | _ ->
          failwith "Uncatched Lexer Error"
    )
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      kosu_repl_parse lexbuf checkpoint
  | I.HandlingError env ->
      let position = Position.current_position lexbuf in
      let current_lexeme = Lexing.lexeme lexbuf in
      let err, state = get_parse_error env in
      Result.error
      @@ LexerError.Syntax_Error
           { position; current_lexeme; message = err; state }
  | I.Accepted v ->
      Ok v
  | I.Rejected ->
      let position = Position.current_position lexbuf in
      let current_lexeme = Lexing.lexeme lexbuf in
      Result.error
      @@ LexerError.Syntax_Error
           {
             position;
             current_lexeme;
             message = "Parser reject the input";
             state = None;
           }

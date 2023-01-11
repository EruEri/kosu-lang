open Lexing
open Lexer


module I = Parser.MenhirInterpreter


let get_parse_error env =
  match I.stack env with
  | lazy Nil -> "Invalid syntax", None
  | lazy (Cons (I.Element (state, _element, _, _), _)) ->
    let nb_state = (I.number state) in
      try (Kosu_parser_messages.message nb_state), Some nb_state with
      | Not_found -> "invalid syntax (no specific message for this eror)", None

let rec parse lexbuf ( checkpoint : Ast._module I.checkpoint) = 
  match checkpoint with
  | I.InputNeeded _env ->
    let token = Lexer.main lexbuf in
    let startp = lexbuf.lex_start_p
    and endp = lexbuf.lex_curr_p in 
    let checkpoint = I.offer checkpoint (token, startp, endp) in
    parse lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ -> 
    let checkpoint = I.resume checkpoint in
    parse lexbuf checkpoint
  | I.HandlingError env -> 
    let position = Position.current_position lexbuf in
    let current_lexeme = Lexing.lexeme lexbuf in 
    let err, state = get_parse_error env in
    raise (
      Syntax_Error {
        position;
        current_lexeme;
        message = err;
        state;
      }
    )

  | I.Accepted v -> v
  | I.Rejected -> 
    let position = Position.current_position lexbuf in
    let current_lexeme = Lexing.lexeme lexbuf in 
    raise (
      Syntax_Error {
        position = position;
        current_lexeme;
        message = "Parser reject the input";
        state = None
      }
    ) 

open Lexing

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

let line_column_of_position p = 
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

type position = {
  start_position: Lexing.position;
  end_position: Lexing.position
}


type 'a location = {
  v: 'a;
  position: position
}

let map f location = {
  location with v = f (location.v)  
}

let located_value start_position end_position v = {
  v;
  position = {
    start_position;
    end_position
  }
}

let current_position lexbuf = {
  start_position = lexbuf.lex_start_p;
  end_position = lexbuf.lex_curr_p
}
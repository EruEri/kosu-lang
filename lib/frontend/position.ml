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
  value: 'a;
  position: position
}

let located_value start_position end_position value = {
  value;
  position = {
    start_position;
    end_position
  }
}
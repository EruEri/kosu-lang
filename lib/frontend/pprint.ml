open Position

let string_of_position_error {start_position; end_position} = 
  let start_position_line, start_position_column = line_column_of_position start_position in
  let end_position_line, end_position_column = line_column_of_position end_position in
  let column_string = Printf.sprintf "%d%s" (start_position_column) (if start_position_column = end_position_column then "" else " - "^(string_of_int end_position_column)) in 
  if start_position = end_position then Printf.sprintf "Line %d, Characters %s" start_position_line column_string
  else
    Printf.sprintf "Lines %d-%d, Characters %d-%d" start_position_line end_position_line start_position_column end_position_column
  

let register_kosu_error filename () =  
  Printexc.register_printer 
  (fun exn -> (match exn with
  | Astvalidation.Error.Validation_error e -> e |> Astvalidation.Error.string_of_validation_error |> Printf.sprintf "%s" |> Option.some
  | Lexer.Forbidden_char (position, char) -> begin
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Forbidden character : %c" s char |> Option.some 
  end
  | Lexer.Unexpected_escaped_char (position, lexeme) -> begin 
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Unexpected Escaped character: %s" s lexeme |> Option.some
  end
  | Lexer.Invalid_keyword_for_build_in_function (position, id) -> begin 
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Invalid Keyword For Builtin Function: %s" s id |> Option.some
  end
  | Lexer.Invalid_litteral_for_build_in_function (position, litteral) -> begin 
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Invalid Litteral For Builtin Function: %c" s litteral |> Option.some
  end
  | Lexer.Not_finished_built_in_function position -> position |> string_of_position_error |> Printf.sprintf "%s: Builtin function not finished" |> Option.some
  | Lexer.Unclosed_comment position -> position |> string_of_position_error |> Printf.sprintf "%s: Comments not terminated" |> Option.some
  | Lexer.Unclosed_string position -> position |> string_of_position_error |> Printf.sprintf "%s: String litteral not terminated" |> Option.some 
  | _ -> None) |> Option.map (Printf.sprintf "\nFile \"%s\", %s" filename) ) 
open Position

let string_of_position_error {start_position; end_position} = 
  let start_position_line, start_position_column = line_column_of_position start_position in
  let end_position_line, end_position_column = line_column_of_position end_position in
  let column_string = Printf.sprintf "%d%s" (start_position_column) (if start_position_column = end_position_column then "" else " - "^(string_of_int end_position_column)) in 
  if start_position = end_position then Printf.sprintf "Line %d, Column %s" start_position_line column_string
  else
    Printf.sprintf "Line %d, Column %d -- Line %d, Column %d" start_position_line start_position_column end_position_line end_position_column
  

let register_kosu_error () =  
  Printexc.register_printer (function
  | Astvalidation.Error.Validation_error e -> e |> Astvalidation.Error.string_of_validation_error |> Printf.sprintf "\n%s" |> Option.some
  | Lexer.Unclosed_comment position -> position |> string_of_position_error |> Printf.sprintf "\n%s: Comments not terminated" |> Option.some
  | Lexer.Unclosed_string (line, column) -> Printf.sprintf "\nLine : %d, Column : %d : String litteral not terminated" line column |> Option.some
  | _ -> None ) 
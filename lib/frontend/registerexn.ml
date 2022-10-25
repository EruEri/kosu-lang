open Pprint
open Pprinterr

let register_kosu_error filename () =  
  Printexc.register_printer 
  (fun exn -> (match exn with
  | Astvalidation.Error.Validation_error (filename, e) -> e |> string_of_validation_error |> Printf.sprintf "%s" |> (Printf.sprintf "\nFile \"%s\", %s" filename) |> Option.some
  | Lexer.Forbidden_char (position, char) -> begin
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Forbidden character : %c" s char |> (Printf.sprintf "\nFile \"%s\", %s" filename) |> Option.some 
  end
  | Lexer.Unexpected_escaped_char (position, lexeme) -> begin 
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Unexpected Escaped character: %s" s lexeme |> (Printf.sprintf "\nFile \"%s\", %s" filename) |> Option.some
  end
  | Lexer.Invalid_keyword_for_build_in_function (position, id) -> begin 
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Invalid Keyword For Builtin Function: %s" s id |> (Printf.sprintf "\nFile \"%s\", %s" filename) |> Option.some
  end
  | Lexer.Invalid_litteral_for_build_in_function (position, litteral) -> begin 
    let s = position |> string_of_position_error in
    Printf.sprintf "%s: Invalid Litteral For Builtin Function: %c" s litteral |> (Printf.sprintf "\nFile \"%s\", %s" filename) |> Option.some
  end
  | Lexer.Not_finished_built_in_function position -> position |> string_of_position_error |> Printf.sprintf "%s: Builtin function not finished" |> Option.some
  | Lexer.Unclosed_comment position -> position |> string_of_position_error |> Printf.sprintf "%s: Comments not terminated" |> Option.some
  | Lexer.Unclosed_string position -> position |> string_of_position_error |> Printf.sprintf "%s: String litteral not terminated" |> Option.some 
  | _ -> None) ) 
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

{
    open Parser
    open Lexing
    open SanError
    open SanPosition

    let current_position lexbuf = 
        { start_position = lexbuf.lex_start_p; end_position = lexbuf.lex_curr_p }

    let keywords = Hashtbl.create 6
    let () = 
    [("if", IF); ("goto", GOTO); ("def", DEF); ("external", EXTERNAL);  ("end", END);
        ("true", TRUE); ("false", FALSE); ("discard", DISCARD); ("lateinit", LATEINIT); ("return", RETURN)
    ] |> List.iter (fun (s, k) -> Hashtbl.replace keywords s k)


    let next_line_and f lexbuf =
        Lexing.new_line lexbuf;
        f lexbuf
}

let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']

let label = (loLetter | upLetter) (loLetter | upLetter | '.' | digit | '_')*

let identifiant = '$' (loLetter | upLetter | digit | "_")*

let decimal_integer = digit (digit | '_')*
let hex_integer = '0' ('x' | 'X') (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'] | '_')*
let octal_intger = '0' ('o' | 'O') (['0'-'7']) (['0'-'7'] | '_')*
let binary_integer = '0' ('b' | 'B') ('0' | '1') ('0' | '1' | '_')*
let number = ('-')? decimal_integer | hex_integer | octal_intger | binary_integer

let escaped_char =  ['n' 'r' 't' '\\' '\'' '\"']
let hexa_char = '\\' 'x' (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'])


let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']


rule token = parse
| newline {  
    (* let _ = if ( String.contains s '\n') then (line := !line + 1) else () in  *)
    next_line_and token lexbuf 
}
| blank+ { token lexbuf }
| ";" { SEMICOLON }
| ":" { COLON }
| "(" { LPARENT }
| ")" { RPARENT }
| "," { COMMA }
| "&"  { AMPERSAND }
| "^" { XOR }
| "&&" { AND }
| "|" { PIPE }
| "||" { OR }
| "=" { EQUAL }
| "==" { DOUBLEQUAL }
| "!=" { DIF }
| "!" { NOT }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT } 
| "/" { DIV }
(* | "%" { MOD } *)
| "<" { INF }
| "<=" { INFEQ }
| ">=" { SUPEQ }
| ">" { SUP }
| "<<" { SHIFTLEFT }
| ">>" { SHIFTRIGHT }
| "\"" { lexbuf |> read_string (Buffer.create 16) }
| (number as n) {
    Integer_lit(Nativeint.of_string n)
}
| "ssize" { SSIZE }
| "bool"  { BOOL }
| "stringl" { STRINGL } 
| "unit" { UNIT }
| label as s {
    match Hashtbl.find_opt keywords s with
    | Some keyword -> keyword
    | None -> Label s
}
| identifiant as s {
    match Hashtbl.find_opt keywords s with
    | Some keyword -> keyword
    | None -> Identifier s
}
| eof { EOF }
and read_string buffer = parse
| '"' { String_lit (Buffer.contents buffer) }
(* | '\\' 'n' { 
    let () = Buffer.add_char buffer '\\' in
    Buffer.add_char buffer 'n'; 
    read_string buffer lexbuf 
} *)
| (hexa_char as s) {
    let s_len = String.length s in
    let s_number = String.sub s 1 (s_len - 1) in
    let code =  int_of_string ("0" ^  s_number) in
    let char = Char.chr code in
    let escaped = char |> Printf.sprintf "%c" |> String.escaped in
    let () = Buffer.add_string buffer escaped in 
    read_string buffer lexbuf 
}
| '\\' ( escaped_char as c ){ 
    let () = if c = '\\' then () else Buffer.add_char buffer '\\' in
    let () = Buffer.add_char buffer c in
    read_string buffer lexbuf 
}
| '\\' { ( Unexpected_escaped_char ((current_position lexbuf) , (lexbuf |> lexeme) ))  |> raw_lexer_error |> raise }

| _ as c { 
    if c = '\n' then 
        next_line_and (read_string buffer) lexbuf
    else
        let () = Buffer.add_char buffer c in
        read_string buffer lexbuf 
}
| eof {
    (Unclosed_string (current_position lexbuf)  |> raw_lexer_error |> raise )  
}
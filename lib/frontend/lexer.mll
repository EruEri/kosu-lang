(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
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


{
    open Parser
    open Lexing
    open Position
    open LexerError

  let next_line_and f lexbuf =
    Lexing.new_line lexbuf;
    f lexbuf

    let keywords = Hashtbl.create 23
    let _ = [("addressof", ADDRESSOF); ("and", FULLAND); ("array", ARRAY); ("cases", CASES); ("const", CONST); ("discard", DISCARD); 
    ("enum", ENUM); ("external", EXTERNAL); ("empty", EMPTY); ("else", ELSE); ("eq", CMP_EQUAL);
    ("fn", FUNCTION); ("false", FALSE); ("gt", CMP_GREATER); ("lt", CMP_LESS); ("match", MATCH); ("nullptr", NULLPTR); 
    ("struct", STRUCT); ("syscall", SYSCALL); ("of", OF); ("or", FULLOR); ("opaque", OPAQUE); ("operator", OPERATOR); ("true", TRUE); 
    ("type", TYPE); ("switch", SWITCH); ("sizeof", SIZEOF); ("if", IF); ("var", VAR); ("while", WHILE)
    ] |> List.iter (fun (s,t) -> Hashtbl.add keywords s t)
}

let integer_sigdness = ('s' | 'u')
let integer_size = ('8' | "16" | "32" | "64")
let fsize = ("f32" | "f64")
let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']
let identifiant = (loLetter | '_') (loLetter | upLetter | digit | "_")*
let module_identifier = (upLetter) (loLetter | digit | "_" | upLetter )*
let constante = upLetter (upLetter | "_" | digit)*
let escaped_char =  ['n' 'r' 't' '\\' '\'' '\"']
let float_literal = (digit) (digit | "_" )* (('.') (digit | "_")*  | ('e' | 'E')  ('+' | '-') digit (digit | '_')*)

let decimal_integer = digit (digit | '_')*
let hex_integer = '0' ('x' | 'X') (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'] | '_')*
let octal_intger = '0' ('o' | 'O') (['0'-'7']) (['0'-'7'] | '_')*
let binary_integer = '0' ('b' | 'B') ('0' | '1') ('0' | '1' | '_')*
let number = decimal_integer | hex_integer | octal_intger | binary_integer

let number_escaped = ( digit+ )

let char_ascii_code = ('\\') number_escaped
let hexa_char = '\\' 'x' (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'])

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let whitespace = [' ' '\t' '\r' '\n']+

rule token = parse
| newline {  
    (* let _ = if ( String.contains s '\n') then (line := !line + 1) else () in  *)
    next_line_and token lexbuf 
}
| blank+ { token lexbuf }
| "(" { LPARENT }
| ")" { RPARENT }
| "{" { LBRACE }
| "}" { RBRACE }
| ";" { SEMICOLON }
| ":" { COLON }
| "::" { DOUBLECOLON }
| "," { COMMA }
| "." { DOT }
| "..." { TRIPLEDOT }
| "_" { WILDCARD }
| "@" { built_in_function lexbuf }
| "=" { EQUAL }
| "#" { CROISILLION }
| "&"  { AMPERSAND }
| "^" { XOR }
| "&&" { AND }
| "|" { PIPE }
| "||" { OR }
| "|>" { PIPESUP }
| "=>" { ARROWFUNC }
| "$" { DOLLAR }
| "==" { DOUBLEQUAL }
| "<=>" { INF_EQ_SUP }
| "!=" { DIF }
| "!" { NOT }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT } 
| "/" { DIV }
| "%" { MOD }
| "<" { INF }
| "[" { LSQBRACE }
| "]" { RSQBRACE }
| "\"" { lexbuf |> read_string (Buffer.create 16) }
| "//" { lexbuf |> single_line_comment }
| "/*" { lexbuf |> multiple_line_comment }
| "<=" { INFEQ }
| ">=" { SUPEQ }
| ">" { SUP }
| "<<" { SHIFTLEFT }
| ">>" { SHIFTRIGHT }
| '\'' (hexa_char as s) '\'' {
    let s_len = String.length s in
    let s_number = String.sub s 1 (s_len - 1) in
    let code =  int_of_string ("0" ^  s_number) in
    let char = Char.chr code in
    Char_lit char
}
| '\'' (char_ascii_code as s) '\'' {
    let s_len = String.length s in
    let s_number = String.sub s 1 (s_len - 1) in
    let code = int_of_string s_number in
    let char = match Char.chr code with
    | code -> code
    | exception Invalid_argument _ -> raise @@ raw_lexer_error ( Char_out_of_range ((current_position lexbuf), code) )
    in
    Char_lit char
}

| '\'' '\\' (escaped_char as c) '\'' {
    let c = match c with
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | '\\' -> '\\'
    | '\'' -> '\''
    | '\"' -> '\"'
    | _ -> failwith "Unreachable code"
    in
    Char_lit c
}
| '\'' (_ as c) '\'' {
    Char_lit c
}
| (float_literal as f) (fsize as fsize) {
    let size = match fsize with
    | "f32" -> Ast.F32
    | "f64" -> Ast.F64 
    | _ -> failwith "Unreachable code"
    in 
    Float_lit (Some size, float_of_string f)
}
| (float_literal as f) {
    Float_lit (None, float_of_string f)
}
| (number as n) (integer_sigdness as sign) (integer_size as size) {
    let signdess = if sign = 'u' then Ast.Unsigned else Ast.Signed in
    let isize = match size with
    | "8" -> Ast.I8
    | "16" -> Ast.I16
    | "32" -> Ast.I32
    | "64" -> Ast.I64
    | _ -> failwith "Unreachable code" in
    let ss = Some (signdess, isize) in
    Integer_lit(ss, Int64.of_string n)
}
| (number as n) {
    Integer_lit(None, Int64.of_string n)
}
| constante as s {
    Constant s
}
| module_identifier as s {
    Module_IDENT s
}
| identifiant as s {
    try 
        Hashtbl.find keywords s
    with Not_found -> IDENT s
}
| _ as c { (Forbidden_char( (current_position lexbuf), c) )  |> raw_lexer_error |> raise  }
| eof { EOF }
and built_in_function = parse
| identifiant as s {
    match Hashtbl.find_opt keywords s with
    | None -> BUILTIN s
    | Some _ -> (Invalid_keyword_for_build_in_function ((current_position lexbuf) , s) |> raw_lexer_error |> raise )
}
| _ as lit {
     (Invalid_litteral_for_build_in_function ( current_position lexbuf ,lit)  |> raw_lexer_error |> raise )
}
| eof {  Not_finished_built_in_function (current_position lexbuf)  |> raw_lexer_error |> raise }


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
and single_line_comment = parse
| newline { next_line_and token lexbuf }
| _ { single_line_comment lexbuf}
| eof { EOF }
and multiple_line_comment = parse 
| "*/" { token lexbuf }
| newline { next_line_and multiple_line_comment lexbuf}
| _ { multiple_line_comment lexbuf }
| eof {
     (Unclosed_comment (current_position lexbuf) |> raw_lexer_error |> raise )
}
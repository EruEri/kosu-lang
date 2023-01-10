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
    open Util

    exception Lexical_error of position*string
    exception Forbidden_char of position*char
    exception Unexpected_escaped_char of position*string
    exception Invalid_keyword_for_build_in_function of position*string
    exception Invalid_litteral_for_build_in_function of position*char
    exception Not_finished_built_in_function of position
    exception Unclosed_string of position
    exception Unclosed_comment of position

    exception Syntax_Error of {
        coordinate: coordinate option;
        message: string;
    }

  let next_line_and f lexbuf =
    Lexing.new_line lexbuf;
    f lexbuf

    let keywords = Hashtbl.create 19
    let _ = ["cases", CASES; "const", CONST; "enum", ENUM; "external", EXTERNAL; "empty", EMPTY; "sig", SIG; "discard", DISCARD ; "else", ELSE; "fn", FUNCTION; 
    "for", FOR; "false", FALSE; "nullptr", NULLPTR ;"struct", STRUCT; "syscall", SYSCALL ;"of", OF; "operator", OPERATOR; "true", TRUE; "switch", SWITCH; "sizeof", SIZEOF; "if", IF; 
     "var", VAR; 
    ] |> List.iter (fun (s,t) -> Hashtbl.add keywords s t)
}

let integer_sigdness = ('s' | 'u')
let integer_size = ('8' | "16" | "32" | "64")
let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']
let identifiant = (loLetter | '_') (loLetter | upLetter | digit | "_")*
let module_identifier = (upLetter) (loLetter | digit | "_")*
let constante = upLetter (upLetter | "_")*
let escaped_char =  ['n' 'r' 't' '\\' '0' '\'' '\"']
let float_literal = (digit) (digit | "_" )* (('.') (digit | "_")*  | ('e' | 'E')  ('+' | '-') digit (digit | '_')*)

let decimal_integer = digit (digit | '_')*
let hex_integer = '0' ('x' | 'X') (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'] | '_')*
let octal_intger = '0' ('o' | 'O') (['0'-'7']) (['0'-'7'] | '_')*
let binary_integer = '0' ('b' | 'B') ('0' | '1') ('0' | '1' | '_')*
let number = decimal_integer | hex_integer | octal_intger | binary_integer


let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let whitespace = [' ' '\t' '\r' '\n']+

rule main = parse
| newline {  
    (* let _ = if ( String.contains s '\n') then (line := !line + 1) else () in  *)
    next_line_and main lexbuf 
}
| blank+ { main lexbuf }
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
| "&"  { AMPERSAND }
| "^" { XOR }
| "&&" { AND }
| "|" { PIPE }
| "||" { OR }
| "|>" { PIPESUP }
| "=>" { ARROWFUNC }
| "->" { MINUSUP }
| "$" { DOLLAR }
| "==" { DOUBLEQUAL }
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
| float_literal as f {
    Float_lit ( float_of_string f)
}
| (number as n) (integer_sigdness as sign) (integer_size as size) {
    let signdess = if sign = 'u' then Ast.Unsigned else Ast.Signed in
    let isize = match size with
    | "8" -> Ast.I8
    | "16" -> Ast.I16
    | "32" -> Ast.I32
    | "64" -> Ast.I64
    | _ -> failwith "Unreachable code" in
    Integer_lit(signdess, isize, Int64.of_string n)
}
| (number as n) {
    Integer_lit(Ast.Signed, Ast.I32, Int64.of_string n)
}
| module_identifier as s {
    Module_IDENT s
}
| constante as s {
    Constant s
}
| identifiant as s {
    try 
        Hashtbl.find keywords s
    with Not_found -> IDENT s
}
| _ as c { raise (Forbidden_char( (current_position lexbuf), c) ) }
| eof { EOF }
and built_in_function = parse
| identifiant as s {
    match Hashtbl.find_opt keywords s with
    | None -> BUILTIN s
    | Some _ -> raise (Invalid_keyword_for_build_in_function ((current_position lexbuf) , s) )
}
| _ as lit {
    raise (Invalid_litteral_for_build_in_function ( current_position lexbuf ,lit))
}
| eof {  Not_finished_built_in_function (current_position lexbuf) |> raise }
and read_string buffer = parse
| '"' { String_lit (Buffer.contents buffer) }
(* | '\\' 'n' { 
    let () = Buffer.add_char buffer '\\' in
    Buffer.add_char buffer 'n'; 
    read_string buffer lexbuf 
} *)
| '\\' ( escaped_char as c ){ 
    let () = if c = '\\' then () else Buffer.add_char buffer '\\' in
    let () = Buffer.add_char buffer c in
    read_string buffer lexbuf 
}
| '\\' { raise ( Unexpected_escaped_char ((current_position lexbuf) , (lexbuf |> lexeme) )) }
| _ as s { Buffer.add_char buffer s; read_string buffer lexbuf }
| eof {
    raise (Unclosed_string (current_position lexbuf))  
}
and single_line_comment = parse
| newline { next_line_and main lexbuf }
| _ { single_line_comment lexbuf}
| eof { EOF }
and multiple_line_comment = parse 
| "*/" { main lexbuf }
| newline { next_line_and multiple_line_comment lexbuf}
| _ { multiple_line_comment lexbuf }
| eof {
    raise  (Unclosed_comment (current_position lexbuf) )
}
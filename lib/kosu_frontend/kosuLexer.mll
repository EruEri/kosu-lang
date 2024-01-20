(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
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
    open KosuParser
    open Lexing
    open Position
    open KosuError

    let lkeywords = 
    [
        ("as", AS); ("array", ARRAY); ("const", CONST); ("cases", CASES);
        ("closure", CLOSURE); ("discard", DISCARD); ("enum", ENUM); 
        ("external", EXTERNAL); ("eq", CMP_EQUAL); ("else", ELSE); 
        ("fn", FUNCTION); ("false", FALSE); ("gt", CMP_GREATER); 
        ("lt", CMP_GREATER); ("match", MATCH); ("mut", MUT); 
        ("nullmutptr", NULLMUTPTR); ("nullptr", NULLPTR); ("struct", STRUCT); 
        ("of", OF); ("open", OPEN); ("opaque", OPAQUE); ("true", TRUE);  
        ("type", TYPE); ("sizeof", SIZEOF); ("if", IF); ("var", VAR);
        ("while", WHILE)

    ]

    let keywords = Hashtbl.of_seq @@ List.to_seq lkeywords

    let current_position = Position.current_position
}

let integer_sigdness = ('s' | 'u')
let integer_size = ('8' | "16" | "32" | "64" | "size")
let fsize = ("f32" | "f64")
let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']
let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']
let identifiant = (loLetter | '_') (loLetter | upLetter | digit | "_")*
let lower_identifier = loLetter+
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

let infix_symbol = ['|' '=' '<' '>' '^' '&' '+' '-' '*' '/' '$' '%']
let prefix_symbol = [ '!' '?' '~']
let operator_symbol = ['|' '=' '<' '>' '^' '&' '+' '-' '*' '/' '$' '%' '~' '!' '?']
let not_identifier = [^ 'a'-'z']

rule token = parse
| newline {
    let () = Lexing.new_line lexbuf in
    token lexbuf
}

| blank+ { token lexbuf }
| "(" { LPARENT }
| ")" { RPARENT }
| "{" { LBRACE }
| "}" { RBRACE }
| "[" { LSQBRACE }
| "]" { RSQBRACE }
| ";" { SEMICOLON }
| ":" { COLON }
| "::" { DOUBLECOLON }
| "," { COMMA }
| "." { DOT }
| "_" { WILDCARD }
| "#" { CROISILLION }
| "`" { BACKTICK }
| '\'' (lower_identifier as s) { 
    PolymorphicVar s
}
| '\'' (hexa_char as s) '\'' {
    let s_len = String.length s in
    let s_number = String.sub s 1 (s_len - 1) in
    let code =  int_of_string ("0" ^  s_number) in
    let char = Char.chr code in
    CharLitteral char
}
| '\'' (char_ascii_code as s) '\'' {
    let s_len = String.length s in
    let s_number = String.sub s 1 (s_len - 1) in
    let code = int_of_string s_number in
    let char = match Char.chr code with
    | code -> code
    | exception Invalid_argument _ -> raise @@ KosuError.Exn.kosu_lexer_error @@ CharOutOfRange { value = code; position = current_position lexbuf} 
    in
    CharLitteral char
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
    CharLitteral c
}
| '\'' (_ as c) '\'' {
    CharLitteral c
}
| (float_literal as f) (fsize as fsize) {
    let size = match fsize with
    | "f32" -> KosuUtil.TyLoc.fsize_32
    | "f64" ->  KosuUtil.TyLoc.fsize_64 
    | _ -> failwith "Unreachable code"
    in 
    FloatLitteral (size, float_of_string f)
}
| (float_literal as f) {
    FloatLitteral (None, float_of_string f)
}
| (number as n) (integer_sigdness as sign) (integer_size as size) {
    let open KosuUtil.IntegerInfo in
    let open KosuUtil.TyLoc in
    let signdess = if sign = 'u' then unsigned else signed in
    let info = match size with
    | "8" ->  sized (signdess, isize_8 )
    | "16" -> sized (signdess, isize_16 )
    | "32" -> sized (signdess, isize_32 )
    | "64" -> sized (signdess, isize_64 )
    | "size" -> worded signdess
    | _ -> failwith "Unreachable code" in

    IntegerLitteral(Some info, Int64.of_string n)
}
| (number as n) {
    IntegerLitteral(None, Int64.of_string n)
}
| '\"' {
    let buffer = Buffer.create 17 in
    let current = Lexing.lexeme_start_p lexbuf in
    let s = read_string buffer lexbuf in
    let () = lexbuf.lex_start_p <- current in
    StringLitteral s
}
| (infix_symbol as i) (operator_symbol* as os) as all {
    match i with
    | '|' when os = String.empty -> PIPE
    | '*' when os = String.empty -> STAR
    | '-' when os = String.empty -> MINUS
    (* | '&' when os = String.empty -> AMPERSAND *)
    | '=' when os = String.empty -> EQUAL
    (* | '$' when os = String.empty -> DOLLAR *)
    (* | '=' when all = "=>" -> EQUAL_SUP *)
    | '-' when all = "->" -> MINUS_SUP
    | '/' when all = "//" -> single_line_comment lexbuf
    | '/' when all = "/*" ->
        multiple_line_comment lexbuf
    | '|' -> INFIX_PIPE all
    | '&' -> INFIX_AMPERSAND all
    | '=' -> INFIX_EQUAL all
    | '<' -> INFIX_INF all 
    | '>' -> INFIX_SUP all
    | '^' -> INFIX_CARET all
    | '+' -> INFIX_PLUS all
    | '-' -> INFIX_MINUS all
    | '*' -> INFIX_MULT all
    | '/' -> INFIX_DIV all
    | '$' -> INFIX_DOLLAR all
    | '%' -> INFIX_PERCENT all
  
    | _ -> failwith "Unreachable: no other infix characters"
}
| (prefix_symbol as i) (operator_symbol* as os) as all {
    let () = ignore os in
    match i with
    | '!' -> PREFIX_EXCLA all
    | '?' -> PREFIX_QUESTIONMARK all
    | '~' -> PREFIX_TILDE all
    | _ -> failwith "Unreachable: no other prefix characters"
}
| constante as s {
    Constant s
}
| module_identifier as s {
    ModuleIdentifier s
}
| identifiant as s {
    try 
        Hashtbl.find keywords s
    with Not_found -> Identifier s
}
| _ as c { 
    raise @@ KosuError.Exn.kosu_lexer_error
    @@ ForbiddenChar { value = c; position = current_position lexbuf}
}
| eof { EOF }

and built_in_function = parse
| identifiant as s {
    Builtin s
}
| _ as lit {
    raise @@ KosuError.Exn.kosu_lexer_error
    @@ InvalidLitteralBuiltinFunction {
        value = lit;
        position = current_position lexbuf
    }
}
| eof {  
    raise @@ KosuError.Exn.kosu_lexer_error
    @@ NotFinishedBuiltinFunction (current_position lexbuf)
}

and read_string buffer = parse
| '"' { (Buffer.contents buffer) }
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
| '\\' { 
    let le = UnexpectedEscapedChar {
            position = current_position lexbuf;
            value = lexeme lexbuf
        }
    in
    raise @@ KosuError.Exn.kosu_lexer_error @@ le  
}

| _ as c { 
    if c = '\n' then 
        let () = Lexing.new_line lexbuf in
        read_string buffer lexbuf
    else
        let () = Buffer.add_char buffer c in
        read_string buffer lexbuf 
}
| eof { 
    let e = UnclosedString (current_position lexbuf) in
    raise @@ KosuError.Exn.kosu_lexer_error e
}

and single_line_comment = parse
| newline {  
    let () = Lexing.new_line lexbuf in
    token lexbuf 
}
| _ { single_line_comment lexbuf}
| eof { EOF }

and multiple_line_comment = parse 
| "*/" { token lexbuf }
| newline { 
    let () = Lexing.new_line lexbuf in
    multiple_line_comment lexbuf
}
| _ { multiple_line_comment lexbuf }
| eof {
    raise @@ KosuError.Exn.kosu_lexer_error @@ UnclosedComment (current_position lexbuf)
}
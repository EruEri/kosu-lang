{
    open Parser
    open Lexing

    exception Lexical_error of string
    exception Forbidden_char of char
    exception Unexpected_escaped_char of string
    exception Unclosed_string

    let keywords = Hashtbl.create 14
    let _ = ["enum", ENUM; "external", EXTERNAL; "sig", SIG; "fn", FUNCTION; "struct", STRUCT;
    "true", TRUE; "false", FALSE; "empty", EMPTY; "switch", SWITCH; "if", IF; "else", ELSE;
    "for", FOR; "const", CONST; "var", VAR; 
    ] |> List.iter (fun (s,t) -> Hashtbl.add keywords s t)
}

let integer_sigdness = ('s' | 'u')
let integer_size = ('8' | "16" | "32" | "64")
let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']
let identifiant = (loLetter | '_') (loLetter | upLetter | digit | "_")*
let module_identifier = (upLetter) (loLetter | loLetter | digit | "_")*
let escaped_char =  ['n' 'r' 't' '\\' '0' '\'' '\"']

let decimal_integer = digit (digit | '_')*
let hex_integer = '0' ('x' | 'X') (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'] | '_')*
let octal_intger = '0' ('o' | 'O') (['0'-'7']) (['0'-'7'] | '_')*
let binary_integer = '0' ('b' | 'B') ('0' | '1') ('0' | '1' | '_')*
let number = decimal_integer | hex_integer | octal_intger | binary_integer

let whitespace = [' ' '\t' '\r' '\n']+

rule main = parse
| whitespace { main lexbuf }
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
| "=" { EQUAL }
| "&"  { AMPERSAND }
| "^" { XOR }
| "&&" { AND }
| "|" { PIPE }
| "||" { OR }
| "|>" { PIPESUP }
| "=>" { ARROWFUNC }
| "==" { DOUBLEQUAL }
| "!=" { DIF }
| "!" { NOT }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT } 
| "/" { DIV }
| "%" { MOD }
| "<" { SUP }
| "\"" { lexbuf |> read_string (Buffer.create 16) }
| "<=" { SUPEQ }
| ">=" { INFEQ }
| ">" { INF }
| "<<" { SHIFTLEFT }
| ">>" { SHIFTRIGHT }
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
| identifiant as s {
    try 
        Hashtbl.find keywords s
    with Not_found -> IDENT s
}
| _ as c { raise (Forbidden_char c) }
| eof { EOF }
and read_string buffer = parse
| '"' { String_lit (Buffer.contents buffer) }
| '\\' ( escaped_char as c ){ Buffer.add_string buffer ( (if c = '\\' then "" else "\\")^( lexbuf |> lexeme)); read_string buffer lexbuf }
| '\\' { raise ( Unexpected_escaped_char (lexbuf |> lexeme) ) }
| _ as s { Buffer.add_char buffer s; read_string buffer lexbuf }
| eof { raise Unclosed_string }

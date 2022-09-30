{
    open Parser
    open Lexing
    open Position

    exception Lexical_error of string
    exception Forbidden_char of char
    exception Unexpected_escaped_char of string
    exception Invalid_keyword_for_build_in_function of string
    exception Invalid_litteral_for_build_in_function of char
    exception Not_finished_built_in_function
    exception Unclosed_string of position
    exception Unclosed_comment of position


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
| _ as c { raise (Forbidden_char c) }
| eof { EOF }
and built_in_function = parse
| identifiant as s {
    match Hashtbl.find_opt keywords s with
    | None -> BUILTIN s
    | Some _ -> raise (Invalid_keyword_for_build_in_function s)
}
| _ as lit {
    raise (Invalid_litteral_for_build_in_function lit)
}
| eof { raise Not_finished_built_in_function }
and read_string buffer = parse
| '"' { String_lit (Buffer.contents buffer) }
| '\\' ( escaped_char as c ){ Buffer.add_string buffer ( (if c = '\\' then "" else "\\")^( lexbuf |> lexeme)); read_string buffer lexbuf }
| '\\' { raise ( Unexpected_escaped_char (lexbuf |> lexeme) ) }
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
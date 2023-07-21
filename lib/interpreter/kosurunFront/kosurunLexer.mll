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
    open KosurunParser
    open Lexing
}

let newline = '\010'
let blank   = [' ' '\009' '\012']
let code_splitter = ("={79}") (newline)
let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']
let letter = loLetter | upLetter
let number = ('-'?) digit (digit | '_')*
let cfn_name = ('_' | letter) ('_' | letter | digit)*
let key_id = loLetter (loLetter  | '_')?
let forbidden_char = [ '\000' ]

rule token = parse
| newline { 
    let () = new_line lexbuf in
    NEWLINE 
}
| blank { token lexbuf }
| "(" { LPARENT }
| ")" { RPARENT }
| "{" { LBRACE }
| "}" { RBRACE }
| "#!" {
    let buffer = Buffer.create 25 in
    let content = read_line buffer lexbuf in
    Shebang content
}
| "#" { Croisillon }
| "=" { EQUAL }
| "$" { DOLLAR }
| number as n {
    let n = int_of_string n in
    Integer n
}
| "'" (cfn_name as name) "'" {
    Cfn_name name
}
| "s8"  { TY_S8 }
| "s16" { TY_S16 }
| "s32" { TY_S32 }
| "s64" { TY_S64 }
| "u8"  { TY_U8 }
| "u16" { TY_U16 }
| "u32" { TY_U32 }
| "u64" { TY_U64 }
| "ptr" { TY_PTR }
| '"' {
    let buffer = Buffer.create 32 in
    read_string buffer lexbuf
}
| code_splitter {
    let buffer = Buffer.create 256 in
    read_bytecode buffer lexbuf
}
| key_id as key {
    Identifier key
}
| _ as c {
    failwith @@ Printf.sprintf "Unknwon char : %c" c 
}
| eof {
    failwith "EOF: NO Bytecode"
}
and read_bytecode buffer = parse
| eof { 
    let content = Buffer.contents buffer in
    Bytecode content
}
| _ as c {
    let () = Buffer.add_char buffer c in
    read_bytecode buffer lexbuf
}
and read_line buffer = parse
| eof { failwith "EOF: READLINE" }
| newline {
    let () = new_line lexbuf in
    let content = Buffer.contents buffer in
    content
}
| _ as c {
    let () = Buffer.add_char buffer c in
    read_line buffer lexbuf
}
and read_string buffer = parse
| eof { failwith "EOF: STRING"}
| '"' {
    let content = Buffer.contents buffer in
    StringLit content
}
| newline {
    failwith "NEWLINE: New line in string"
}
| forbidden_char {
    failwith "Forbidden char: in string"
}
| _ as c {
    let () = Buffer.add_char buffer c in
    read_string buffer lexbuf
}
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
}

let newline = '\010'
let blank   = [' ' '\009' '\012']
let code_splitter = ("={79}") (newline)

rule token = parse
| newline { NEWLINE }
| blank { token lexbuf }
| "(" { LPARENT }
| ")" { RPARENT }
| "{" { LBRACE }
| "}" { RBRACE }
| "#" { Croisillon }
| "$" { DOLLAR }
| code_splitter {
    let buffer = Buffer.create 256 in
    read_bytecode buffer lexbuf
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
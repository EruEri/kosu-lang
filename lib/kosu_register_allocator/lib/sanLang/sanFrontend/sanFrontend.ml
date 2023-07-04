(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
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

module SanEnv = SanEnv
module SanTypechecker = SanTypechecker
module SanPosition = SanPosition
module SanAst = SanAst
module SanPprint = SanPprint

let register_san_error = SanPprintErr.register_san_error

let san_module_parse file =
  let san_module_res =
    In_channel.with_open_bin file (fun ic ->
        let lexbuf = Lexing.from_channel ic in
        SanParser.parse lexbuf (Parser.Incremental.san_module lexbuf.lex_curr_p)
    )
  in
  let san_modules =
    match san_module_res with
    | Ok san_module ->
        SanValidation.validate file san_module
    | Error error ->
        raise @@ SanError.File_Lexer_Error (file, error)
  in
  san_modules

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

open Pprint
open Pprinterr

let register_kosu_error filename () =
    Printexc.register_printer (fun exn ->
        match exn with
        | Astvalidation.Error.Validation_error (filename, e) ->
            e |> string_of_validation_error |> Printf.sprintf "%s"
            |> Printf.sprintf "\nFile \"%s\", %s" filename
            |> Option.some
        | Lexer.Syntax_Error {position; message} -> 
            let s = string_of_position_error position in
            Printf.sprintf "\nFile \"%s\", %s\nSyntax Error : %s" filename s message
            |> Option.some
        | Lexer.Forbidden_char (position, char) ->
            let s = position |> string_of_position_error in
            Printf.sprintf "%s: Forbidden character : %c" s char
            |> Printf.sprintf "\nFile \"%s\", %s" filename
            |> Option.some
        | Lexer.Unexpected_escaped_char (position, lexeme) ->
            let s = position |> string_of_position_error in
            Printf.sprintf "%s: Unexpected Escaped character: %s" s lexeme
            |> Printf.sprintf "\nFile \"%s\", %s" filename
            |> Option.some
        | Lexer.Invalid_keyword_for_build_in_function (position, id) ->
            let s = position |> string_of_position_error in
            Printf.sprintf "%s: Invalid Keyword For Builtin Function: %s" s id
            |> Printf.sprintf "\nFile \"%s\", %s" filename
            |> Option.some
        | Lexer.Invalid_litteral_for_build_in_function (position, litteral) ->
            let s = position |> string_of_position_error in
            Printf.sprintf "%s: Invalid Litteral For Builtin Function: %c" s
                litteral
            |> Printf.sprintf "\nFile \"%s\", %s" filename
            |> Option.some
        | Lexer.Not_finished_built_in_function position ->
            position |> string_of_position_error
            |> Printf.sprintf "\nFile \"%s\" %s: Builtin function not finished" filename
            |> Option.some
        | Lexer.Unclosed_comment position ->
            position |> string_of_position_error
            |> Printf.sprintf "\nFile \"%s\" %s: Comments not terminated" filename
            |> Option.some
        | Lexer.Unclosed_string position ->
            position |> string_of_position_error
            |> Printf.sprintf "\nFile \"%s\" %s: String litteral not terminated" filename
            |> Option.some
        | _ -> None)

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

type position = {
  start_position : Lexing.position;
  end_position : Lexing.position;
}

type 'a location = { value : 'a; position : position }

let create position value = { position; value }
let map f location = { location with value = f location.value }
let map_use f location = { value = f location; position = location.position }
let value { value; _ } = value
let values list = List.map value list
let position { position; _ } = position

let located_value start_position end_position value =
  { value; position = { start_position; end_position } }

let current_position lexbuf =
  let open Lexing in
  { start_position = lexbuf.lex_start_p; end_position = lexbuf.lex_curr_p }

let flatten ~default = function
  | [] ->
      default
  | t :: [] ->
      t.position
  | t :: tl ->
      let start_position = t.position.start_position in
      let end_position = position @@ List.hd @@ List.rev tl in
      let end_position = end_position.end_position in
      { start_position; end_position }

let dummy = Lexing.{ start_position = dummy_pos; end_position = dummy_pos }
let dummy_located value = { value; position = dummy }

let line_column_of_position p =
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol in
  (line_number, column)

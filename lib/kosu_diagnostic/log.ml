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

let ascii_reset = "\u{001B}[0m"
let ( $ ) str fs s = str @@ fs s

let sprintf ?color ?(bold = false) ?(underline = false) input =
  let m = Option.is_some color || bold || underline in
  let s =
    match m with
    | true ->
        Printf.sprintf "\u{001B}%s"
    | false ->
        Printf.sprintf "%s"
  in
  let s =
    match color with
    | None ->
        s
    | Some color ->
        s $ Printf.sprintf ";%u%s" (Severity.fg color)
  in
  let s = match bold with true -> s $ Printf.sprintf ";%u%s" 1 | false -> s in
  let s =
    match underline with true -> s $ Printf.sprintf ";%u%s" 4 | false -> s
  in

  let s = match m with true -> s $ Printf.sprintf "m%s" | false -> s in
  s @@ Printf.sprintf "%s%s" input ascii_reset

let emit ?(_message = fun _ -> String.empty) file (loc : Util.Position.position)
    _default _kind _fwhat _what =
  let lines =
    String.split_on_char '\n'
    @@ In_channel.with_open_bin file (fun ic -> Util.Io.read_file ic ())
  in
  let _lines =
    List.filteri
      (fun i _ ->
        let i = i + 1 in
        loc.start_position.pos_lnum = i || loc.end_position.pos_lnum = i
      )
      lines
  in
  failwith ""

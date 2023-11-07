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

let same_line (loc : Util.Position.position) =
  loc.start_position.pos_lnum = loc.end_position.pos_lnum

let one_line ?(to_end = `range) ?(prefix = String.empty) ?color ?(bold = false)
    ?(underline = false) loc line =
  let s =
    match to_end with
    | `range ->
        String.sub line loc.Util.Position.start_position.pos_cnum
          (loc.end_position.pos_cnum - loc.start_position.pos_cnum)
    | `start_to_end ->
        String.sub line 0 loc.end_position.pos_cnum
    | `start_to_full ->
        String.sub line loc.start_position.pos_cnum
          (String.length line - loc.start_position.pos_cnum)
    | `full ->
        line
  in
  Printf.sprintf "%s%s" prefix @@ sprintf ?color ~bold ~underline s

let multiple_line ?(prefix = fun _ -> String.empty) ?color ?(bold = false)
    ?(underline = false) loc lines =
  let open Util.Position in
  let len = List.length lines in
  List.mapi
    (fun i line ->
      let prefix = prefix (loc.start_position.pos_lnum + i) in
      let f =
        match i with
        | 0 ->
            one_line ~to_end:`start_to_full
        | n when n = len - 1 ->
            one_line ~to_end:`start_to_end
        | _ ->
            one_line ~to_end:`full
      in
      f ~prefix ?color ~bold ~underline loc line
    )
    lines

type 'b t =
  | LogFile :
      string
      * 'b
      * ('b ->
        (Util.Position.position, Util.Position.position list) Either.t option
        )
      -> (string * 'b) t
  | LogString : string -> string t

let log_file file elt fn = LogFile (file, elt, fn)
let log_string s = LogString s

let pos_format ?(std = stdout) ?prefix severity loc file =
  let open Util.Position in
  let lines =
    String.split_on_char '\n'
    @@ In_channel.with_open_bin file (fun ic -> Util.Io.read_file ic ())
  in
  let lines =
    List.filteri
      (fun i _ ->
        let i = i + 1 in
        loc.start_position.pos_lnum = i || loc.end_position.pos_lnum = i
      )
      lines
  in
  (* let () = Printf.fprintf std "%s" (message (file, loc)) in *)
  let () =
    match same_line loc with
    | true ->
        let prefix =
          Option.map (fun f -> f loc.start_position.pos_lnum) prefix
        in
        let s =
          one_line ?prefix ~to_end:`range ~color:(Severity.color severity)
            ~underline:true loc (List.hd lines)
        in
        Printf.fprintf std "%s" s
    | false ->
        let lines =
          multiple_line ?prefix ~color:(Severity.color severity) ~underline:true
            loc lines
        in
        List.iter (fun line -> Printf.fprintf std "%s\n" line) lines
  in
  ()

let emit :
    type a.
    ?std:out_channel ->
    ?prefix:(int -> string) ->
    ?message:(a -> string) ->
    Severity.t ->
    a t ->
    unit =
 fun ?(std = stdout) ?prefix ?message severity string_or_file ->
  match string_or_file with
  | LogString string ->
      let message =
        match message with None -> fun _ -> String.empty | Some f -> f
      in
      let () = Printf.fprintf std "%s" @@ message string in
      let s = sprintf ~color:(Severity.color severity) ~underline:true string in
      let () = Printf.fprintf std "%s" s in
      ()
  | LogFile (file, elt, fn) ->
      let arg = (file, elt) in
      let pos = fn elt in
      let message =
        match message with None -> fun _ -> String.empty | Some f -> f
      in
      let () = Printf.fprintf std "%s" (message arg) in
      let () =
        match pos with
        | None ->
            ()
        | Some (Either.Left pos) ->
            pos_format ~std ?prefix severity pos file
        | Some (Either.Right posses) ->
            List.iter
              (fun pos -> pos_format ~std ?prefix severity pos file)
              posses
      in

      ()

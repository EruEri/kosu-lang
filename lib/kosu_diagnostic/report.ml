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

module type S = sig
  type t

  val loc : t -> Util.Position.position list
  val prefix : string -> t -> string

  val line : int -> string
  (**
      [line n] is a function to be called before the display of the error text.
      It's usually the current line
      [n] is the current line
  *)

  val error : t -> string option
  val hint : t -> string option
  val warning : t -> string option
  val severity : t -> Severity.t option
end

module Make (TS : S) = struct
  type _ kind =
    | LogFile : string * TS.t -> (string * TS.t) kind
    | LogString : string * Severity.t option -> string kind

  let file f e = LogFile (f, e)
  let string ?severity s = LogString (s, severity)

  let emit :
      type a.
      ?std:out_channel -> ?bold:bool -> ?underline:bool -> a kind -> unit =
   fun ?(std = stdout) ?bold ?underline -> function
    | LogString (s, severity) ->
        let () = Printf.fprintf std "%s" @@ s in
        let color = Option.map Severity.color severity in
        let s = Log.sprintf ?color ?bold ?underline s in
        let () = Printf.fprintf std "%s" s in
        ()
    | LogFile (file, e) ->
        let pos = TS.loc e in
        let message = TS.prefix file e in
        let () = Printf.fprintf std "%s\n" message in

        let () =
          match pos with
          | [] ->
              ()
          | _ :: _ as posses ->
              let severity = TS.severity e in
              List.iter
                (fun pos ->
                  Log.report_file ?underline ?severity ~std
                    (Option.some TS.line) pos file
                )
                posses
        in
        let ee = TS.error e in
        let ew = TS.warning e in
        let eh = TS.hint e in
        let () =
          Option.iter (fun s ->
              let e = Log.sprintf ~color:Severity.Red ?bold "Error" in
              Printf.fprintf std "\n%s: %s" e s
          )
          @@ ee
        in

        let () =
          Option.iter (fun s ->
              let e = Log.sprintf ~color:Severity.Yellow ?bold "Warning " in
              Printf.fprintf std "\n%s: %s" e s
          )
          @@ ew
        in

        let () =
          Option.iter (fun s ->
              let e = Log.sprintf ~color:Severity.Blue ?bold "Hint" in
              Printf.fprintf std "\n%s: %s" e s
          )
          @@ eh
        in

        let () =
          match Option.is_none ee && Option.is_none eh && Option.is_none ew with
          | true ->
              ()
          | false ->
              Printf.fprintf std "\n%!"
        in
        ()
end

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

type line =
  | CCentry of
      KosuVirtualMachine.FFIType.args KosuVirtualMachine.FFIType.ccall_entry
  | Parameter of string * string

type _ line_kind =
  | LK_Ccentry
      : KosuVirtualMachine.FFIType.args KosuVirtualMachine.FFIType.ccall_entry
        line_kind
  | LK_Parameter : (string * string) line_kind

type kosurun_ast = {
  shebang : string option;
  lines : line list;
  bytecode : string;
}

let create ?shebang ?(entries = []) ~pc bytecode =
  let lines = entries |> List.map @@ fun e -> CCentry e in
  let line = Parameter ("pc", Printf.sprintf "%u" pc) in
  let lines = line :: lines in
  { shebang; lines; bytecode }

let add_key_value ~key ~value kosurun_ast =
  { kosurun_ast with lines = kosurun_ast.lines @ [ Parameter (key, value) ] }

let add_ccall_entry entry kosurun_ast =
  { kosurun_ast with lines = kosurun_ast.lines @ [ CCentry entry ] }

let lines : type a. a line_kind -> kosurun_ast -> a list =
 fun kind kosurun_ast ->
  match kind with
  | LK_Ccentry ->
      kosurun_ast.lines
      |> List.filter_map (function CCentry e -> Some e | Parameter _ -> None)
  | LK_Parameter ->
      kosurun_ast.lines
      |> List.filter_map (function
           | CCentry _ ->
               None
           | Parameter (key, value) ->
               Some (key, value)
           )

let keyvals = lines LK_Parameter
let centries = lines LK_Ccentry
let delimited char s = Printf.sprintf "%c%s%c" char s char
let single_quoted = delimited '\''
let quoted = delimited '\"'

let string_of_line = function
  | CCentry entry ->
      KosuVirtualMachine.FFIType.string_of_ccall_entry entry
  | Parameter (key, value) ->
      Printf.sprintf "%s=%s" key @@ quoted value

let exec_splitter =
  String.init 80 (fun n -> match n with 79 -> '\n' | _ -> '=')

let to_string { shebang; lines; bytecode } =
  Printf.sprintf "%s%s\n%s%s"
    (shebang |> Option.map (Printf.sprintf "%s\n") |> Option.value ~default:"")
    (lines |> List.map string_of_line |> String.concat "\n")
    exec_splitter bytecode

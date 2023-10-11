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

module StringSet = Set.Make (String)

type pkg_config = {
  include_path : StringSet.t;
  libs_path : StringSet.t;
  linked_libs : StringSet.t;
  others : StringSet.t;
}

let add_include_path path pkg_config =
  { pkg_config with include_path = StringSet.add path pkg_config.include_path }

let add_libs_path path pkg_config =
  { pkg_config with libs_path = StringSet.add path pkg_config.libs_path }

let add_linked_libs libs pkg_config =
  { pkg_config with linked_libs = StringSet.add libs pkg_config.linked_libs }

let add_others option pkg_config =
  { pkg_config with others = StringSet.add option pkg_config.others }

let read_file ch () = really_input_string ch (in_channel_length ch)
let cmd = "pkg-config"

let create ?(include_path = StringSet.empty) ?(libs_path = StringSet.empty)
    ?(linked_libs = StringSet.empty) others =
  { include_path; libs_path; linked_libs; others }

let empty = create StringSet.empty

let parse_single pkg_config option =
  if String.length option < 2 then
    add_others option pkg_config
  else
    match String.sub option 0 2 with
    | "-L" ->
        add_libs_path option pkg_config
    | "-I" ->
        add_include_path option pkg_config
    | "-l" ->
        add_libs_path option pkg_config
    | _ ->
        add_others option pkg_config

let parse pkg_output =
  pkg_output |> String.split_on_char ' ' |> List.map String.trim
  |> List.fold_left parse_single empty

let of_command_opt ~verbose ?(cflags = false) ?(clibs = true) ~libname () =
  let command =
    Printf.sprintf "%s %s %s %s" cmd
      ( if cflags then
          "--cflags"
        else
          ""
      )
      ( if clibs then
          "--libs"
        else
          ""
      )
      libname
  in
  let () =
    if verbose then
      print_endline command
  in

  let in_channel = Unix.open_process_in command in
  let content = In_channel.input_all in_channel in
  let process_status = Unix.close_process_in in_channel in
  match process_status with
  | WEXITED code when code = 0 ->
      content |> parse |> Option.some
  | _ ->
      None

let of_command ~verbose ?(cflags = false) ?(clibs = true) ~libname () =
  let command =
    Printf.sprintf "%s %s %s %s" cmd
      ( if cflags then
          "--cflags"
        else
          ""
      )
      ( if clibs then
          "--libs"
        else
          ""
      )
      libname
  in
  let () =
    if verbose then
      print_endline command
  in

  let in_channel = Unix.open_process_in command in
  let content = In_channel.input_all in_channel in
  let process_status = Unix.close_process_in in_channel in
  match process_status with
  | WEXITED code when code = 0 ->
      parse content
  | WEXITED code | WSIGNALED code | WSTOPPED code ->
      let () = prerr_endline content in
      exit code

let union pkg1 pkg2 =
  let union = StringSet.union in
  {
    include_path = union pkg1.include_path pkg2.include_path;
    libs_path = union pkg1.libs_path pkg2.libs_path;
    linked_libs = union pkg1.linked_libs pkg2.linked_libs;
    others = union pkg1.others pkg2.others;
  }

let mix pkg_config =
  pkg_config.others
  |> StringSet.union pkg_config.include_path
  |> StringSet.union pkg_config.libs_path
  |> StringSet.union pkg_config.linked_libs

let cc_flags_format pkg_config =
  let union = StringSet.union in
  pkg_config.others
  |> union pkg_config.include_path
  |> StringSet.elements |> String.concat " " |> Printf.sprintf "%s"

let linker_flags_format pkg_config =
  let union = StringSet.union in
  pkg_config.linked_libs |> union pkg_config.libs_path |> StringSet.elements
  |> String.concat " " |> Printf.sprintf "%s"

let to_string pkg_config =
  pkg_config |> mix |> StringSet.elements |> String.concat " "
  |> Printf.sprintf "%s"

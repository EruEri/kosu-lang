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

module Args = Args

type stringlit_label = SLit of string
type floatlit_label = FLit of string
type coordinate = { line : int; column : int }

let () = Callback.register "c_caml_list_length" List.length
let couple a b = (a, b)

let is_what_file ~extension filename =
  filename |> Filename.extension |> ( = ) extension

let is_object_file = is_what_file ~extension:".o"

let is_asm_file filename =
  filename |> Filename.extension |> String.lowercase_ascii |> ( = ) ".s"

let rec string_of_chars_aux count result char =
  if count <= 0 then
    result
  else
    string_of_chars_aux (count - 1) (Printf.sprintf "%c%s" char result) char

let string_of_chars count = string_of_chars_aux count ""

let string_of_module_path path =
  if path = "" then
    ""
  else
    Printf.sprintf "%s::" path

let are_same_lenght l1 l2 = 0 = List.compare_lengths l1 l2
let are_diff_lenght l1 l2 = not @@ are_same_lenght l1 l2

let dummy_generic_map generic_names parametrics_types =
  let list_len = parametrics_types |> List.length in
  let dummy_list = List.init list_len (fun _ -> ()) in
  let dummy_parametrics = List.combine dummy_list parametrics_types in
  let generics_mapped = List.combine generic_names dummy_parametrics in
  Hashtbl.of_seq (generics_mapped |> List.to_seq)

module Occurence = struct
  type 'a occurence = Empty | One of 'a | Multiple of 'a list

  exception Too_Many_Occurence
  exception No_Occurence

  let is_one = function One _ -> true | _ -> false

  let one = function
    | Empty ->
        raise No_Occurence
    | Multiple _ ->
        raise Too_Many_Occurence
    | One f ->
        f

  let find_map_occurence predicate list =
    match list |> List.filter_map predicate with
    | [] ->
        Empty
    | [ t ] ->
        One t
    | t :: q ->
        Multiple (t :: q)

  let find_occurence predicate list =
    match list |> List.find_all predicate with
    | [] ->
        Empty
    | [ t ] ->
        One t
    | t :: q ->
        Multiple (t :: q)
end

module Operator = struct
  let ( >? ) o v = Option.value ~default:v o
end

module Io = struct
  let read_file ch () = really_input_string ch (in_channel_length ch)
end

module Checksum = struct
  let checksum bytes = Digest.bytes bytes
end

module ListHelper = struct
  let rec index_of_aux f index = function
    | [] ->
        raise Not_found
    | t :: q ->
        if f t then
          index
        else
          index_of_aux f (index + 1) q

  let index_of f = index_of_aux f 0
  let head_opt = function [] -> None | t :: _ -> Some t

  let rec duplicate_aux hashmap list =
    match list with
    | [] ->
        hashmap |> Hashtbl.to_seq |> List.of_seq
        |> List.filter_map (fun (key, value) ->
               if value > 1 then
                 Some key
               else
                 None
           )
    | t :: q ->
        let () =
          match Hashtbl.find_opt hashmap t with
          | Some value ->
              Hashtbl.replace hashmap t (value + 1)
          | None ->
              Hashtbl.add hashmap t 1
        in
        duplicate_aux hashmap q

  let duplicate l = duplicate_aux (Hashtbl.create (l |> List.length)) l

  let rec duplic_aux cmp ~acc ~list =
    match list with
    | [] ->
        acc
    | t :: q ->
        let duplicate, no_duplicated = q |> List.partition (cmp t) in
        let duplicate =
          if duplicate = [] then
            acc
          else
            (t :: duplicate) :: acc
        in
        duplic_aux cmp ~acc:duplicate ~list:no_duplicated

  let duplicated cmp list = duplic_aux cmp ~acc:[] ~list

  let inner_count list =
    List.fold_left (fun acc (_, value) -> acc + (value |> List.length)) 0 list

  let rec combine_safe lhs rhs =
    match (lhs, rhs) with
    | [], _ | _, [] ->
        []
    | t1 :: q1, t2 :: q2 ->
        (t1, t2) :: combine_safe q1 q2

  let rec diff base ~remains =
    match (base, remains) with
    | _, [] ->
        []
    | [], l ->
        l
    | _ :: q1, _ :: q2 ->
        diff q1 ~remains:q2

  let rec ldiff fcompare lhs rhs =
    match (lhs, rhs) with
    | [], e | e, [] ->
        e
    | x1 :: xs1, x2 :: xs2 -> (
        match fcompare x1 x2 with
        | 0 ->
            ldiff fcompare xs1 xs2
        | _ ->
            x2 :: ldiff fcompare xs1 xs2
      )

  let rec shrink ~atlength list =
    match (atlength, list) with
    | n, _ when n < 0 ->
        invalid_arg "Negative number"
    | 0, _ ->
        []
    | _, [] ->
        []
    | n, t :: q ->
        t :: shrink ~atlength:(n - 1) q
end

module StringSet = Set.Make (String)

module PkgConfig = struct
  type pkg_config = {
    include_path : StringSet.t;
    libs_path : StringSet.t;
    linked_libs : StringSet.t;
    others : StringSet.t;
  }

  let add_include_path path pkg_config =
    {
      pkg_config with
      include_path = StringSet.add path pkg_config.include_path;
    }

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
        content |> parse
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
end

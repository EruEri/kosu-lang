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

let rec string_of_chars_aux count result char =
  if count <= 0 then result
  else string_of_chars_aux (count - 1) (Printf.sprintf "%c%s" char result) char

let string_of_chars count = string_of_chars_aux count ""

let string_of_module_path path =
  if path = "" then "" else Printf.sprintf "%s::" path

let are_same_lenght l1 l2 = 0 = List.compare_lengths l1 l2
let are_diff_lenght l1 l2 = not (are_same_lenght l1 l2)

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
    | Empty -> raise No_Occurence
    | Multiple _ -> raise Too_Many_Occurence
    | One f -> f

  let find_map_occurence predicate list =
    match list |> List.filter_map predicate with
    | [] -> Empty
    | [ t ] -> One t
    | t :: q -> Multiple (t :: q)

  let find_occurence predicate list =
    match list |> List.find_all predicate with
    | [] -> Empty
    | [ t ] -> One t
    | t :: q -> Multiple (t :: q)
end

module ListHelper = struct
  let rec index_of_aux f index = function
    | [] -> raise Not_found
    | t :: q -> if f t then index else index_of_aux f (index + 1) q

  let index_of f = index_of_aux f 0
  let head_opt = function [] -> None | t :: _ -> Some t

  let rec duplicate_aux hashmap list =
    match list with
    | [] ->
        hashmap |> Hashtbl.to_seq |> List.of_seq
        |> List.filter_map (fun (key, value) ->
               if value > 1 then Some key else None)
    | t :: q ->
        let () =
          match Hashtbl.find_opt hashmap t with
          | Some value -> Hashtbl.replace hashmap t (value + 1)
          | None -> Hashtbl.add hashmap t 1
        in
        duplicate_aux hashmap q

  
  let duplicate l = duplicate_aux (Hashtbl.create (l |> List.length)) l

  let rec duplic_aux cmp ~acc ~list =
    match list with
    | [] -> acc
    | t :: q ->
        let duplicate, no_duplicated = q |> List.partition (cmp t) in
        let duplicate =
          if duplicate = [] then acc else (t :: duplicate) :: acc
        in
        duplic_aux cmp ~acc:duplicate ~list:no_duplicated

  let duplicated cmp list = duplic_aux cmp ~acc:[] ~list

  let inner_count list =
    List.fold_left (fun acc (_, value) -> acc + (value |> List.length)) 0 list
end

module Either3 = struct
  type ('a, 'b, 'c) t = ELeft of 'a | EMiddle of 'b | ERight of 'c

  let is_left = function ELeft _ -> true | _ -> false
  let is_middle = function EMiddle _ -> true | _ -> false
  let is_right = function ERight _ -> true | _ -> false

  let fold ~left ~middle ~right = function
    | ELeft l -> left l
    | EMiddle m -> middle m
    | ERight r -> right r
end

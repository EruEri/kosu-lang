(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kyoumi                                                                *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Kyoumi is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kyoumi is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kyoumi.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

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

let rec popn n = function
  | [] ->
      []
  | _ :: q as list ->
      if n = 0 then
        list
      else
        popn (n - 1) q

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

let are_same_length l1 l2 = 0 = List.compare_lengths l1 l2
let are_diff_length l1 l2 = not @@ are_same_length l1 l2

let rec map_ok f = function
  | [] ->
      Result.ok []
  | t :: q ->
      let ( let* ) = Result.bind in
      let* res = f t in
      let* list = map_ok f q in
      Result.ok @@ (res :: list)

let rec map_some f = function
  | [] ->
      Option.some []
  | t :: q ->
      let ( let* ) = Option.bind in
      let* res = f t in
      let* list = map_some f q in
      Option.some @@ (res :: list)

let rec fold_some f acc = function
  | [] ->
      Option.some acc
  | t :: q ->
      let ( let* ) = Option.bind in
      let* acc = f acc t in
      fold_some f acc q

let rec fold_ok f acc = function
  | [] ->
      Result.ok acc
  | t :: q ->
      let ( let* ) = Result.bind in
      let* acc = f acc t in
      fold_ok f acc q

let is_empty = function [] -> true | _ :: _ -> false

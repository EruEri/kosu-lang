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

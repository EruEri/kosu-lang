(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module AsmAst = AsmAst
module NamingConvention = NamingConvention

module Sizeof = struct
  let align n b =
    let m = n mod b in
    if m = 0 then n else n + b - m

  let align_16 b = align b 16

  let sizeof = function
    | (Ssize : SanTyped.SanTyAst.san_type) | Stringl -> Nativeint.size / 8
    | Unit | Boolean -> 1

  let alignmentof = sizeof

  let sizeof_tuple san_types =
    let size, alignment =
      san_types
      |> List.fold_left
           (fun (acc_size, acc_align) st ->
             let comming_size = sizeof st in
             let comming_align = alignmentof st in

             let aligned = align acc_size comming_align in
             let new_align = max acc_align comming_align in
             (aligned + comming_size, new_align))
           (0, 1)
    in
    align size alignment

  let offset_of_tuple_index index san_types =
    if index = 0 then 0
    else
      san_types
      |> List.mapi (fun i v -> (i - 1, v))
      |> List.fold_left
           (fun ((acc_size, acc_align, found) as acc) (tindex, st) ->
             let comming_size = sizeof st in
             let comming_align = alignmentof st in

             let aligned = align acc_size comming_align in
             let new_align = max acc_align comming_align in

             if found then acc
             else if index = tindex then (aligned, new_align, true)
             else (aligned + comming_size, new_align, found))
           (0, 1, false)
      |> function
      | a, _, _ -> a
end

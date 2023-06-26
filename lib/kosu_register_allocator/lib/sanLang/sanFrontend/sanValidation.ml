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

open SanAst
open SanPosition
open SanError
open SanTypechecker

type 'a error = (unit, 'a) result

module Error = Result

let ok : 'a error = Ok ()
let err e : 'a error = Error e

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

module ValidateFunction = struct
  let label_of_function san_function =
    san_function.san_basic_blocks |> List.map (fun blocks -> blocks.label)

  let check_duplicated_label san_function =
    let labels = label_of_function san_function in
    let duplicated =
      labels |> duplicated (fun lhs rhs -> lhs.value = rhs.value)
    in
    match duplicated with
    | [] ->
        ok
    | t :: _ ->
        err @@ Duplicated_label (san_function.fn_name, (t |> List.hd).value, t)

  let check_duplicated_parameters san_function =
    let duplicated =
      san_function.parameters
      |> duplicated (fun (l, _) (r, _) -> l.value = r.value)
    in
    match duplicated with
    | [] ->
        ok
    | t :: _ ->
        err
        @@ Duplicated_paramters
             (san_function.fn_name, (t |> List.hd |> fst).value, List.map fst t)

  let check_body san_module san_function =
    match SanTypechecker.type_check_function san_module san_function with
    | _ ->
        ok
    | exception San_error e ->
        err @@ Sve_error e

  let check_function san_module san_function : 'a error =
    let ( >== ) = Error.bind in
    check_duplicated_label san_function
    >== fun () ->
    check_duplicated_parameters san_function
    >== fun () -> check_body san_module san_function
end

module ValidateModule = struct
  let check_duplicate_function (san_module : san_module) =
    let duplicated =
      san_module
      |> duplicated (fun lhs rhs ->
             let lcalling_name = SanHelper.calling_name lhs in
             let rcalling_name = SanHelper.calling_name rhs in
             lcalling_name.value = rcalling_name.value
         )
    in
    match duplicated with
    | [] ->
        ok
    | t :: _ ->
        let calling_name = SanHelper.calling_name (List.hd t) in
        err
        @@ Duplicated_function
             (calling_name, t |> List.map SanHelper.calling_name)

  let valide_node san_module = function
    | External _ ->
        ok
    | Declaration san_function ->
        ValidateFunction.check_function san_module san_function
end

let validate (san_module : san_module) =
  match ValidateModule.check_duplicate_function san_module with
  | Error e ->
      Error e
  | Ok () ->
      let validation =
        san_module
        |> List.fold_left
             (fun acc node ->
               if Error.is_error acc then
                 acc
               else
                 ValidateModule.valide_node san_module node
             )
             ok
      in
      validation |> Result.map (fun _ -> san_module)

(**
  @raise San_Validation_Error
*)
let validate filename (san_module : san_module) =
  match validate san_module with
  | Error e ->
      raise @@ San_Validation_Error (filename, e)
  | Ok san_mod ->
      san_mod

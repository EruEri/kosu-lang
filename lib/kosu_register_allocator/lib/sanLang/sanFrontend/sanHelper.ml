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

let does_support_unop unop san_type =
  match (unop, san_type) with
  | TacNot, (Ssize | Boolean) -> true
  | TacNot, (Stringl | Unit) -> false
  | TacUminus, Ssize -> true
  | TacUminus, (Stringl | Unit | Boolean) -> false

let does_support_arithmetic_operator tacself san_type =
  match ((tacself : tac_binop_self), san_type) with
  | _, Ssize -> true
  | _, _ -> false

let does_support_logicial_operator tac_bool san_type =
  match (tac_bool, (san_type : san_type)) with
  | (TacOr | TacAnd), Boolean -> true
  | (TacOr | TacAnd), (Ssize | Unit | Stringl) -> false
  | (TacSup | TacSupEq | TacInf | TacInfEq), Ssize -> true
  | (TacSup | TacSupEq | TacInf | TacInfEq), (Boolean | Stringl | Unit) -> false
  | (TacDiff | TacEqual), (Boolean | Ssize) -> true
  | (TacDiff | TacEqual), (Unit | Stringl) -> false

let find_sig_opt (san_module : SanAst.san_module) name =
  san_module
  |> List.find_map (fun node ->
         match node with
         | Declaration { fn_name; parameters; return_type; _ }
           when fn_name.value = name ->
             Option.some @@ (parameters |> List.map snd, return_type)
         | External { fn_name; signature; _ } when fn_name.value = name ->
             Option.some @@ signature
         | External _ -> None
         | Declaration _ -> None)

let calling_name = function
  | Declaration { fn_name } | External { fn_name; _ } -> fn_name

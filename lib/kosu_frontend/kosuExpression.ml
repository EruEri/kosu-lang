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

(*let rec is_const_expression : KosuAst.kosu_expression -> bool = function
    | EEmpty
    | ETrue
    | EFalse
    | Enullprt
    | ECmpLess
    | ECmpGreater
    | EStringl _
    | EChar _
    | EInteger _
    | EFloat _
    | ESizeof _
    |
    -> true
    | EFieldAccess { first_expr; field = _} ->
        is_const_expression first_expr
    | EArrayAccess {array_expr; index_expr } ->
        is_const_expression array_expr && is_const_expression index_expr
    |
    | _ -> false*)

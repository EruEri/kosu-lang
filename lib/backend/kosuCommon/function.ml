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

open Util.Args

let kosu_passing_style_kt kt =
  if KosuIrTyped.Asttyhelper.RType.is_float kt then Simple_Reg Float
  else Simple_Reg Other

let kosu_passing_style (_, kt) = kosu_passing_style_kt kt

let kosu_passing_style_tte :
    KosuIrTAC.Asttac.tac_typed_expression -> passing_style =
 fun { expr_rktype; _ } -> kosu_passing_style_kt expr_rktype

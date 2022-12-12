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

open KosuIrTyped.Asttyped
open Asttac

module Operator = struct
  let bin_operantor = function
    | RBAdd _ -> TacSelf TacAdd
    | RBMinus _ -> TacSelf TacMinus
    | RBMult _ -> TacSelf TacMult
    | RBDiv _ -> TacSelf TacDiv
    | RBMod _ -> TacSelf TacModulo
    | RBBitwiseOr _ -> TacSelf TacBitwiseOr
    | RBBitwiseAnd _ -> TacSelf TacBitwiseAnd
    | RBBitwiseXor _ -> TacSelf TacBitwiseXor
    | RBShiftLeft _ -> TacSelf TacShiftLeft
    | RBShiftRight _ -> TacSelf TacShiftRight
    | RBAnd _ -> TacBool TacAnd
    | RBOr _ -> TacBool TacOr
    | RBSup _ -> TacBool TacSup
    | RBSupEq _ -> TacBool TacSupEq
    | RBInf _ -> TacBool TacInf
    | RBInfEq _ -> TacBool TacInfEq
    | RBEqual _ -> TacBool TacEqual
    | RBDif _ -> TacBool TacDiff

  let unary_operator = function RUMinus _ -> TacUminus | RUNot _ -> TacNot
  let typed_operand = function RUMinus e | RUNot e -> e
  let typed_operandes = KosuIrTyped.Asttyhelper.Binop.operands
end

module OperatorDeclaration = struct
  type t = Asttac.tac_operator_decl

  let tac_body = function
    | TacUnary { tac_body; _ } | TacBinary { tac_body; _ } -> tac_body
end

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
  let typed_operandes = KosuIrTyped.Asttyped.Binop.operands
end

module OperatorDeclaration = struct
  type t = Asttac.tac_operator_decl

  let tac_body = function
  | TacUnary {tac_body; _ } | TacBinary {tac_body; _} -> tac_body
  
end
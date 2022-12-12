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

open KosuFrontend.Ast

type rswitch_case =
  | RSC_Enum_Identifier of { variant : string }
  | RSC_Enum_Identifier_Assoc of {
      variant : string;
      assoc_ids : string option list;
    }

type rktype =
  | RTParametric_identifier of {
      module_path : string;
      parametrics_type : rktype list;
      name : string;
    }
  | RTType_Identifier of { module_path : string; name : string }
  | RTInteger of (signedness * isize)
  | RTPointer of rktype
  | RTTuple of rktype list
  | RTFunction of rktype list * rktype
  | RTString_lit
  | RTUnknow
  | RTFloat
  | RTBool
  | RTUnit

type rkbody = rkstatement list * typed_expression
and typed_expression = { rktype : rktype; rexpression : rkexpression }

and rkstatement =
  | RSDeclaration of {
      is_const : bool;
      variable_name : string;
      typed_expression : typed_expression;
    }
  | RSAffection of string * typed_expression
  | RSDiscard of typed_expression
  | RSDerefAffectation of string * typed_expression

and rkexpression =
  | REmpty
  | RTrue
  | RFalse
  | RENullptr
  | REInteger of (signedness * isize * int64)
  | REFloat of float
  | RESizeof of rktype
  | REstring of string
  | REAdress of string
  | REDeference of int * string
  | REIdentifier of { modules_path : string; identifier : string }
  | REFieldAcces of { first_expr : typed_expression; field : string }
  | REConst_Identifier of { modules_path : string; identifier : string }
  | REStruct of {
      modules_path : string;
      struct_name : string;
      fields : (string * typed_expression) list;
    }
  | REEnum of {
      modules_path : string;
      enum_name : string option;
      variant : string;
      assoc_exprs : typed_expression list;
    }
  | RETuple of typed_expression list
  | REBuiltin_Function_call of {
      fn_name : string;
      parameters : typed_expression list;
    }
  | REFunction_call of {
      modules_path : string;
      generics_resolver : rktype list option;
      fn_name : string;
      parameters : typed_expression list;
    }
  | REBinOperator_Function_call of rkbin_op
  | REUnOperator_Function_call of rkunary_op
  | REIf of typed_expression * rkbody * rkbody
  | RECases of { cases : (typed_expression * rkbody) list; else_case : rkbody }
  | RESwitch of {
      rexpression : typed_expression;
      cases : (rswitch_case list * (int * string * rktype) list * rkbody) list;
      wildcard_case : rkbody option;
    }
  | REBin_op of rkbin_op
  | REUn_op of rkunary_op

and rkbin_op =
  | RBAdd of typed_expression * typed_expression
  | RBMinus of typed_expression * typed_expression
  | RBMult of typed_expression * typed_expression
  | RBDiv of typed_expression * typed_expression
  | RBMod of typed_expression * typed_expression
  | RBBitwiseOr of typed_expression * typed_expression
  | RBBitwiseAnd of typed_expression * typed_expression
  | RBBitwiseXor of typed_expression * typed_expression
  | RBShiftLeft of typed_expression * typed_expression
  | RBShiftRight of typed_expression * typed_expression
  | RBAnd of typed_expression * typed_expression
  | RBOr of typed_expression * typed_expression
  | RBSup of typed_expression * typed_expression
  | RBSupEq of typed_expression * typed_expression
  | RBInf of typed_expression * typed_expression
  | RBInfEq of typed_expression * typed_expression
  | RBEqual of typed_expression * typed_expression
  | RBDif of typed_expression * typed_expression

and rkunary_op = RUMinus of typed_expression | RUNot of typed_expression

type rstruct_decl = {
  rstruct_name : string;
  generics : string list;
  rfields : (string * rktype) list;
}

type renum_decl = {
  renum_name : string;
  generics : string list;
  rvariants : (string * rktype list) list;
}

type rfunction_decl = {
  rfn_name : string;
  generics : string list;
  rparameters : (string * rktype) list;
  return_type : rktype;
  rbody : rkbody;
}

type roperator_decl =
  | RUnary of {
      op : parser_unary_op;
      rfield : string * rktype;
      return_type : rktype;
      kbody : rkbody;
    }
  | RBinary of {
      op : parser_binary_op;
      rfields : (string * rktype) * (string * rktype);
      return_type : rktype;
      kbody : rkbody;
    }

type rsyscall_decl = {
  rsyscall_name : string;
  parameters : rktype list;
  return_type : rktype;
  opcode : int64;
}

type rexternal_func_decl = {
  rsig_name : string;
  fn_parameters : rktype list;
  return_type : rktype;
  is_variadic : bool;
  c_name : string option;
}

type rtrue_function_decl = {
  rfn_name : string;
  rparameters : (string * rktype) list;
  return_type : rktype;
  rbody : rkbody;
}

type rconst_decl = { rconst_name : string; value : typed_expression }

type rmodule_node =
  | RNExternFunc of rexternal_func_decl
  | RNFunction of rfunction_decl
  | RNOperator of roperator_decl
  | RNSyscall of rsyscall_decl
  | RNStruct of rstruct_decl
  | RNEnum of renum_decl
  | RNConst of rconst_decl

type rmodule = RModule of rmodule_node list
type rmodule_path = { path : string; rmodule : rmodule }
type named_rmodule_path = { filename : string; rmodule_path : rmodule_path }
type rprogram = named_rmodule_path list

type raw_function =
  | RFFunction of rtrue_function_decl
  | RFOperator of roperator_decl

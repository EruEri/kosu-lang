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
  | RTFloat of fsize
  | RTPointer of rktype
  | RTTuple of rktype list
  | RTFunction of rktype list * rktype
  | RTArray of { size : int64; rktype : rktype }
  | RTOpaque of { module_path : string; name : string }
  | RTOrdered
  | RTString_lit
  | RTUnknow
  | RTChar
  | RTBool
  | RTUnit

type extended_parser_operator =
  | ParBinOp of parser_binary_op
  | ParserSup
  | ParserSupEq
  | ParserInf
  | ParserInfEq
  | ParserDiff

type rkbody = rkstatement list * typed_expression
and typed_expression = { rktype : rktype; rexpression : rkexpression }

and rkstatement =
  | RSDeclaration of {
      is_const : bool;
      variable_name : string;
      typed_expression : typed_expression;
    }
  | RSAffection of raffacted_value * typed_expression
  | RSDiscard of typed_expression
  | RSDerefAffectation of raffacted_value * typed_expression

and raffacted_value =
  | RAFVariable of (string * rktype)
  | RAFField of { variable : string * rktype; fields : string list }

and rkexpression =
  | REmpty
  | RTrue
  | RFalse
  | RENullptr
  | RECmpLess
  | RECmpEqual
  | RECmpGreater
  | REInteger of (signedness * isize * int64)
  | REFloat of (fsize * float)
  | REChar of char
  | RESizeof of rktype
  | REstring of string
  | REAdress of string
  | REAdressof of raffacted_value
  | REDeference of int * string
  | REIdentifier of { modules_path : string; identifier : string }
  | RETupleAccess of { first_expr : typed_expression; index : int64 }
  | REFieldAcces of { first_expr : typed_expression; field : string }
  | REArrayAccess of {
      array_expr : typed_expression;
      index_expr : typed_expression;
    }
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
  | REArray of typed_expression list
  | RETuple of typed_expression list
  | REBuiltin_Function_call of {
      fn_name : Builtin_Function.functions;
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
  | REWhile of typed_expression * rkbody
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
  | RBCmp of typed_expression * typed_expression

and rkunary_op = RUMinus of typed_expression | RUNot of typed_expression

type function_call_info = {
  varia_index : int option;
  parameters : rktype list;
  return_type : rktype;
}

type function_call_infos = function_call_info list

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
  true_generics : bool;
  rparameters : (string * rktype) list;
  return_type : rktype;
  rbody : rkbody;
}

type unary_operator_decl = {
  op : parser_unary_op;
  rfield : string * rktype;
  return_type : rktype;
  kbody : rkbody;
}

type binary_operator_decl = {
  op : extended_parser_operator;
  rbfields : (string * rktype) * (string * rktype);
  return_type : rktype;
  kbody : rkbody;
}

type roperator_decl =
  | RUnary of unary_operator_decl
  | RBinary of binary_operator_decl

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
  rmaped_generics : rktype list;
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
  | RNOpaque of string

type rmodule = RModule of rmodule_node list
type rmodule_path = { path : string; rmodule : rmodule }
type named_rmodule_path = { filename : string; rmodule_path : rmodule_path }
type rprogram = named_rmodule_path list

type raw_function =
  | RFFunction of rtrue_function_decl
  | RFOperator of roperator_decl

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
open KosuFrontend.Ast

type tac_binop_bool =
  | TacOr
  | TacSup
  | TacSupEq
  | TacInf
  | TacInfEq
  | TacEqual
  | TacDiff
  | TacAnd

type tac_binop_self =
  | TacAdd
  | TacMinus
  | TacMult
  | TacDiv
  | TacModulo
  | TacBitwiseOr
  | TacBitwiseAnd
  | TacBitwiseXor
  | TacShiftLeft
  | TacShiftRight

type tac_binop_cmp = TacOrdered

type tac_binop =
  | TacSelf of tac_binop_self
  | TacBool of tac_binop_bool
  | TacCmp of tac_binop_cmp

type tac_unop = TacNot | TacUminus

type tac_local_variable =
  | Locale of string
  | Enum_Assoc_id of {
      name : string;
      from : tac_typed_expression;
      assoc_index_bound : int;
    }

and tac_typed_locale_variable = {
  locale_ty : rktype;
  locale : tac_local_variable;
}

and tac_expression =
  | TEFalse
  | TETrue
  | TEmpty
  | TENullptr
  | TECmpLesser
  | TECmpGreater
  | TECmpEqual
  | TEInt of (signedness * isize * int64)
  | TEChar of char
  | TEFloat of (fsize * float)
  | TEIdentifier of string
  | TEString of string
  | TEConst of { module_path : string; name : string }
  | TESizeof of rktype

and tac_typed_expression = {
  expr_rktype : rktype;
  tac_expression : tac_expression;
}

and tac_typed_rvalue = { rval_rktype : rktype; rvalue : tac_rvalue }

and tac_fncall = {
  module_path : string;
  fn_name : string;
  generics_resolver : rktype list option;
  tac_parameters : tac_typed_expression list;
}

and binary = {
  binop : tac_binop;
  blhs : tac_typed_expression;
  brhs : tac_typed_expression;
}

and unary = { unop : tac_unop; expr : tac_typed_expression }

and tac_rvalue =
  | RVExpression of tac_typed_expression
  | RVFunction of tac_fncall
  | RVStruct of {
      module_path : string;
      struct_name : string;
      fields : (string * tac_typed_expression) list;
    }
  | RVEnum of {
      module_path : string;
      enum_name : string option;
      variant : string;
      assoc_tac_exprs : tac_typed_expression list;
    }
  | RVBuiltinCall of {
      fn_name : KosuFrontend.Ast.Builtin_Function.functions;
      parameters : tac_typed_expression list;
    }
  | RVTuple of tac_typed_expression list
  | RVArray of tac_typed_expression list
  | RVFieldAcess of { first_expr : tac_typed_expression; field : string }
  | RVTupleAccess of { first_expr : tac_typed_expression; index : int64 }
  | RVArrayAccess of {
      array_expr : tac_typed_expression;
      index_expr : tac_typed_expression;
    }
  | RVAdressof of raffacted_value
  | RVAdress of string
  | RVDefer of string
  | RVCustomBinop of binary
  | RVCustomUnop of unary
  | RVBuiltinBinop of binary
  | RVBuiltinUnop of unary
  | RVDiscard
  | RVLater

and tac_case = {
  condition_label : string option;
  statement_for_condition : tac_statement list;
  condition : tac_typed_expression;
  goto : string;
  jmp_false : string;
  end_label : string;
  tac_body : tac_body;
}

and tac_switch_variant = {
  variant_label : string;
  variant_next_label : string option;
  variant_index : int;
  cmp_statement : tac_statement;
  cmp_atom : tac_typed_expression;
}

and tac_switch_tmp = {
  variants : tac_switch_variant list;
  tmp_assoc_bound : (int * string * rktype) list;
  tmp_sw_goto : string;
  tmp_sw_false : string option;
  tmp_sw_exit_label : string;
  tmp_switch_tac_body : tac_body;
}

and tac_switch = {
  variants_to_match : string list;
  assoc_bound : (int * string * rktype) list;
  sw_goto : string;
  sw_exit_label : string;
  switch_tac_body : tac_body;
}

and tac_statement =
  | STacDeclaration of { identifier : string; trvalue : tac_typed_rvalue }
  | STacModification of { identifier : string; trvalue : tac_typed_rvalue }
  | STacModificationField of {
      identifier_root : string * rktype;
      fields : string list;
      trvalue : tac_typed_rvalue;
    }
  | STDerefAffectation of { identifier : string; trvalue : tac_typed_rvalue }
  | STDerefAffectationField of {
      identifier_root : string * rktype;
      fields : string list;
      trvalue : tac_typed_rvalue;
    }
  | STWhile of {
      statements_condition : tac_statement list;
      condition : tac_typed_expression;
      loop_body : tac_body;
      self_label : string;
      inner_body_label : string;
      exit_label : string;
    }
  | STIf of {
      statement_for_bool : tac_statement list;
      condition_rvalue : tac_typed_expression;
      goto1 : string;
      goto2 : string;
      exit_label : string;
      if_tac_body : tac_body;
      else_tac_body : tac_body;
    }
  | SCases of {
      cases : tac_case list;
      else_tac_body : tac_body;
      exit_label : string;
    }
  | STSwitch of {
      statemenets_for_case : tac_statement list;
      condition_switch : tac_typed_expression;
      sw_cases : tac_switch list;
      wildcard_label : string option;
      wildcard_body : tac_body option;
      sw_exit_label : string;
    }
  | STSwitchTmp of {
      tmp_statemenets_for_case : tac_statement list;
      enum_tte : tac_typed_expression;
      tag_atom : tac_typed_expression;
      tmp_switch_list : tac_switch_tmp list;
      tmp_wildcard_label : string option;
      tmp_wildcard_body : tac_body option;
      tmp_sw_exit_label : string;
    }

and tac_body = {
  label : string;
  body : tac_statement list * tac_typed_expression option;
}

type tac_function_decl = {
  rfn_name : string;
  generics : string list;
  rparameters : (string * rktype) list;
  return_type : rktype;
  tac_body : tac_body;
  fn_call_infos : function_call_info list;
  locale_var : tac_typed_locale_variable list;
  discarded_values : (string * rktype) list;
}

type tac_unary_operator_decl = {
  op : parser_unary_op;
  asm_name : string;
  rfield : string * rktype;
  return_type : rktype;
  tac_body : tac_body;
  fn_call_infos : function_call_info list;
  locale_var : tac_typed_locale_variable list;
  discarded_values : (string * rktype) list;
}

type tac_binary_operator_decl = {
  op : extended_parser_operator;
  asm_name : string;
  rfields : (string * rktype) * (string * rktype);
  return_type : rktype;
  tac_body : tac_body;
  fn_call_infos : function_call_info list;
  locale_var : tac_typed_locale_variable list;
  discarded_values : (string * rktype) list;
}

type tac_operator_decl =
  | TacUnary of tac_unary_operator_decl
  | TacBinary of tac_binary_operator_decl

type tac_module_node =
  | TNExternFunc of rexternal_func_decl
  | TNSyscall of rsyscall_decl
  | TNFunction of tac_function_decl
  | TNOperator of tac_operator_decl
  | TNStruct of rstruct_decl
  | TNEnum of renum_decl
  | TNConst of rconst_decl
  | TNOpaque of string

type tac_module = TacModule of tac_module_node list
type tac_module_path = { path : string; tac_module : tac_module }

type named_tacmodule_path = {
  filename : string;
  tac_module_path : tac_module_path;
  rprogram : KosuIrTyped.Asttyped.rprogram;
}

type tac_program = named_tacmodule_path list

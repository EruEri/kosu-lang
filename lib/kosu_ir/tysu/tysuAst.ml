(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
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

open TysuBase
open TysuType

type tysu_lvalue =
  | TysuLvalue of { variable : string typed; fields : string list }

(*
  Wrap kosu_function_parameters and kosu_anon_parameters
  since in tysu everything is typed
*)
type tysu_function_parameter = {
  is_var : bool;
  name : string;
  tysu_type : tysu_type;
}

(*
  Analoge to kosu_anon_function_kind   
*)

type tysu_statement =
  | SDeclaration of {
      is_const : bool;
      pattern : tysu_pattern typed;
      expression : tysu_expression typed;
    }
  | SAffection of {
      is_deref : bool;
      lvalue : tysu_lvalue;
      expression : tysu_expression typed;
    }
  | SDiscard of tysu_expression typed
  | SOpen of { module_resolver : module_resolver }

and tysu_pattern =
  | PTrue
  | PFalse
  | PEmpty
  | PCmpLess
  | PCmpEqual
  | PCmpGreater
  | PNullptr
  | PWildcard
  | PFloat of float
  | PChar of char
  | PIdentifier of string
  | PTuple of tysu_pattern typed list
  | PInteger of int64
  | PCase of {
      module_resolver : module_resolver;
      enum_name : string;
      variant : string;
      assoc_patterns : tysu_pattern typed list;
    }
  | PRecord of {
      module_resolver : module_resolver;
      struct_name : string;
      pfields : (string * tysu_pattern typed) list;
    }
  | POr of tysu_pattern typed list
  | PAs of { pas_pattern : tysu_pattern typed; pas_bound : string }

and tysu_expression =
  | EEmpty
  | ETrue
  | EFalse
  | ECmpLess
  | ECmpEqual
  | ECmpGreater
  | ENullptr of { is_const : bool }
  | EStringl of string
  | EChar of char
  | EInteger of int64
  | EFloat of float
  | ESizeof of tysu_type
  | EFieldAccess of { first_expr : tysu_expression typed; field : string }
  | EArrayAccess of {
      array_expr : tysu_expression typed;
      index_expr : tysu_expression typed;
    }
  | ETupleAccess of { first_expr : tysu_expression typed; index : int64 }
  | EConstIdentifier of {
      module_resolver : module_resolver;
      identifier : string;
    }
  | EIdentifier of { module_resolver : module_resolver; id : string }
  | EStruct of {
      module_resolver : module_resolver;
      struct_name : string;
      fields : (string * tysu_expression typed) list;
    }
  | EEnum of {
      module_resolver : module_resolver;
      enum_name : string;
      variant : string;
      assoc_exprs : tysu_expression typed list;
    }
  | EBlock of tysu_block
  (* * expr *)
  | EDeref of tysu_expression typed
  | ETuple of tysu_expression typed list
  | EArray of tysu_expression typed list
  | EBuiltinFunctionCall of {
      builtin : Kosu.Ast.kosu_builtin_function;
      parameters : tysu_expression typed list;
    }
  | EFunctionCall of {
      module_resolver : module_resolver;
      generics_resolver : tysu_type list option;
      fn_name : string;
      parameters : tysu_expression typed list;
    }
  | EWhile of { condition_expr : tysu_expression typed; body : tysu_block }
  (* If expression will be a syntaxic sugar of ecases *)
  | ECases of {
      cases : (tysu_expression typed * tysu_block) list;
      else_body : tysu_block;
    }
  | EMatch of {
      expression : tysu_expression typed;
      patterns : (tysu_pattern typed * tysu_block) list;
    }
  | EAnonFunction of {
      kind : tysu_anon_function_kind;
      parameters : tysu_function_parameter list;
      body : tysu_expression typed;
      captured : (string * tysu_type) list;
    }

and tysu_block = {
  tysu_stmts : tysu_statement list;
  tysu_expr : tysu_expression typed;
}

type tysu_struct_decl = {
  struct_name : string;
  poly_vars : tysu_variable_polymorphic list;
  fields : (string * tysu_type) list;
}

type tysu_enum_decl = {
  enum_name : string;
  poly_vars : tysu_variable_polymorphic list;
  tag_type : integer_info;
  variants : (string * tysu_type list) list;
}

type tysu_function_decl = {
  fn_name : string;
  poly_vars : tysu_variable_polymorphic list;
  parameters : tysu_function_parameter list;
  return_type : tysu_type;
  body : tysu_expression typed;
}

type tysu_const_decl = {
  const_name : string;
  explicit_type : tysu_type;
  c_expression : tysu_expression typed;
}

type tysu_opaque_decl = { name : string }

(* forbid variadic function *)
type tysu_external_func_decl = {
  sig_name : string;
  parameters : tysu_type list;
  return_type : tysu_type;
  c_name : string option;
}

type tysu_module_node =
  | NExternFunc of tysu_external_func_decl
  | NFunction of tysu_function_decl
  | NStruct of tysu_struct_decl
  | NEnum of tysu_enum_decl
  | NConst of tysu_const_decl
  | NOpaque of tysu_opaque_decl

type tysu_module = tysu_module_node list
type tysu_named_module = { filename : string; tysu_module : tysu_module }
type tysu_program = tysu_named_module list

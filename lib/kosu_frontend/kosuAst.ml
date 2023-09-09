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

include KosuBaseAst
open Position
open KosuType

type kosu_lvalue =
  | KosuLvalue of { variable : string location; fields : string location list }

type kosu_function_parameters = {
  is_var : bool;
  name : string location;
  kosu_type : TyLoc.kosu_loctype location;
}

type kosu_anon_function_kind = KAClosure | KAFunctionPointer

type kosu_statement =
  | SDeclaration of {
      is_const : bool;
      pattern : kosu_pattern location;
      explicit_type : TyLoc.kosu_loctype location option;
      expression : kosu_expression location;
    }
  | SAffection of {
      is_deref : bool;
      lvalue : kosu_lvalue;
      expression : kosu_expression location;
    }
  | SDiscard of kosu_expression location
  | SOpen of { module_resolver : module_resolver_loc }

and kosu_pattern =
  | PTrue
  | PFalse
  | PEmpty
  | PCmpLess
  | PCmpEqual
  | PCmpGreater
  | PNullptr
  | PWildcard
  | PFloat of float location
  | PChar of char location
  | PIdentifier of string location
  | PTuple of kosu_pattern location list
  | PInteger of { value : int64 location }
  | PCase of {
      module_resolver : module_resolver_loc;
      enum_name : string location option;
      variant : string location;
      assoc_patterns : kosu_pattern location list;
    }
  | PRecord of {
      module_resolver : module_resolver_loc;
      struct_name : string location;
      pfields : (string location * kosu_pattern location) list;
    }
  | POr of kosu_pattern location list
  | PAs of { pas_pattern : kosu_pattern location; pas_bound : string location }

and kosu_expression =
  | EEmpty
  | ETrue
  | EFalse
  | ENullptr
  | ECmpLess
  | ECmpEqual
  | ECmpGreater
  | EStringl of string
  | EChar of char
  | EInteger of { integer_info : integer_info option; ivalue : int64 }
  | EFloat of { fsize : fsize option; fvalue : float }
  | ESizeof of (TyLoc.kosu_loctype location, kosu_expression location) Either.t
  | EFieldAccess of {
      first_expr : kosu_expression location;
      field : string location;
    }
  | EArrayAccess of {
      array_expr : kosu_expression location;
      index_expr : kosu_expression location;
    }
  | ETupleAccess of {
      first_expr : kosu_expression location;
      index : int64 location;
    }
  | EConstIdentifier of {
      module_resolver : module_resolver_loc;
      identifier : string location;
    }
  | EIdentifier of {
      module_resolver : module_resolver_loc;
      id : string location;
    }
  | EStruct of {
      module_resolver : module_resolver_loc;
      struct_name : string location;
      fields : (string location * kosu_expression location) list;
    }
  | EEnum of {
      module_resolver : module_resolver_loc;
      enum_name : string location option;
      variant : string location;
      assoc_exprs : kosu_expression location list;
    }
  | EBlock of kosu_block
  (* * expr *)
  | EDeref of kosu_expression location
  | ETuple of kosu_expression location list
  | EArray of kosu_expression location list
  | EBuiltinFunctionCall of {
      fn_name : string location;
      parameters : kosu_expression location list;
    }
  | EFunctionCall of {
      module_resolver : module_resolver_loc;
      generics_resolver : TyLoc.kosu_loctype location list option;
      fn_name : string location;
      parameters : kosu_expression location list;
    }
  | EWhile of { condition_expr : kosu_expression location; body : kosu_block }
  (* If expression will be a syntaxique sugar of ecases *)
  | ECases of {
      cases : (kosu_expression location * kosu_block) list;
      else_body : kosu_block;
    }
  | EMatch of {
      expression : kosu_expression location;
      patterns : (kosu_pattern location * kosu_block) list;
    }
  | EAnonFunction of {
      kind : kosu_anon_function_kind;
      parameters : kosu_function_parameters list;
      body : kosu_block;
    }

and kosu_block = {
  kosu_stmts : kosu_statement location list;
  kosu_expr : kosu_expression location;
}

type kosu_struct_decl = {
  struct_name : string location;
  poly_vars : TyLoc.kosu_loctype_polymorphic list;
  fields : (string location * TyLoc.kosu_loctype location) list;
}

type kosu_enum_decl = {
  enum_name : string location;
  poly_vars : TyLoc.kosu_loctype_polymorphic list;
  (*
     Should be an integer type
     Default to s32
  *)
  tag_type : TyLoc.kosu_loctype location;
  variants : (string location * TyLoc.kosu_loctype location list) list;
}

type kosu_function_decl = {
  fn_name : string location;
  poly_vars : TyLoc.kosu_loctype_polymorphic list;
  parameters : kosu_function_parameters list;
  return_type : TyLoc.kosu_loctype location;
  (* if block will be transform into EBlock expression in parser *)
  body : kosu_expression location;
}

type kosu_syscall_decl = {
  syscall_name : string location;
  parameters : TyLoc.kosu_loctype location list;
  return_type : TyLoc.kosu_loctype location;
  opcode : int64 location;
}

(* forbid variadic function *)
type kosu_external_func_decl = {
  sig_name : string location;
  parameters : TyLoc.kosu_loctype location list;
  return_type : TyLoc.kosu_loctype location;
  c_name : string option;
}

type kosu_const_decl = {
  const_name : string location;
  explicit_type : TyLoc.kosu_loctype location;
  value : kosu_expression location;
}

type kosu_opaque_decl = { name : string location }
type kosu_type_decl = DStruct of kosu_struct_decl | DEnum of kosu_enum_decl

type kosu_module_node =
  | NExternFunc of kosu_external_func_decl
  | NFunction of kosu_function_decl
  | NSyscall of kosu_syscall_decl
  | NStruct of kosu_struct_decl
  | NEnum of kosu_enum_decl
  | NConst of kosu_const_decl
  | NOpaque of kosu_opaque_decl

type kosu_module = kosu_module_node list
type kosu_named_module = { filename : string; kosu_module : kosu_module }
type kosu_program = kosu_named_module list

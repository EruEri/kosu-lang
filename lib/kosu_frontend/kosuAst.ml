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

open Position

type signedness = Signed | Unsigned
type isize = I8 | I16 | I32 | I64
type fsize = F32 | F64
type pointer_state = 
  | Const
  | Mutable

type module_resolver = ModuleResoler of string location list

module TyLoc = struct
  type kosu_loctype_polymorphic = PolymorphicVar of string location
  type kosu_inner_closure_type = ClosureType of {
    id: string;
    parameters: kosu_loctype;
    return_type: kosu_loctype
  }

  and kosu_loctype =
    | TyLocParametricIdentifier of {
        module_resolver : module_resolver;
        parametrics_type : kosu_loctype location list;
        name : string location;
      }
    | TyLocIdentifier of {
        module_resolver : module_resolver;
        name : string location;
      }
    | TyLocPolymorphic of kosu_loctype_polymorphic
    | TyLocPointer of { pointer_state : pointer_state; pointee_type : kosu_loctype location }
    | TyLocInteger of signedness option * isize option
    | TyLocPointerSize of signedness 
    | TyLocFloat of fsize option
    | TyLocFunctionPtr of kosu_loctype location list * kosu_loctype location
    (* This closure type is used by the user in function signature*)
    | TyLocClosure of kosu_loctype location list * kosu_loctype location
    (* Used by the typecker to give an unique id to the closure *)
    | TyLocInnerClosureId of kosu_inner_closure_type
    | TyLocArray of { ktype : kosu_loctype location; size : int64 location }
    | TyLocTuple of kosu_loctype location list
    | TyLocOpaque of {
        module_resolver : module_resolver;
        name : string location;
      }
    | TyLocOrdered
    | TyLocStringLit
    | TyLocChar
    | TyLocBool
    | TyLocUnit
end

type kosu_lvalue =
  | KosuLvalue of { variable : string location; fields : string location list }

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
  | SOpen of { module_resolver : module_resolver }

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
  | PInteger of { neg_sig : bool; value : int64 location }
  | PCase of {
      variant : string location;
      assoc_patterns : kosu_pattern location list;
    }
  | POr of kosu_pattern location list
  | PRecord of {
      module_resolver : module_resolver;
      pfields : (string location * kosu_pattern location) list;
    }
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
  | EInteger of {
      signedness : signedness option;
      isize : isize option;
      ivalue : int64;
    }
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
      module_resolver : module_resolver;
      identifier : string location;
    }
  | EIdenfifier of { id : string location }
  | EStruct of {
      module_resolver : module_resolver;
      struct_name : string location;
      fields : (string location * kosu_expression location) list;
    }
  | EBlock of kosu_block
  | EEnum of {
      module_resolver : module_resolver;
      enum_name : string location option;
      variant : string location;
      assoc_exprs : kosu_expression location list;
    }
  | ETuple of kosu_expression location list
  | EArray of kosu_expression location list
  | EBuiltinFunctionCall of {
      fn_name : string location;
      parameters : kosu_expression location list;
    }
  | EFunctionCall of {
      modules_path : module_resolver;
      generics_resolver : TyLoc.kosu_loctype location list option;
      fn_name : string location;
      parameters : kosu_expression location list;
    }
  (* If expression will be a syntaxique sugar of ecases *)
  | ECases of {
      condition_expr : kosu_expression location;
      if_body : kosu_block;
      else_body : kosu_block;
    }
  | EMatch of {
      expression : kosu_expression location;
      patterns : (kosu_pattern location * kosu_block) list;
    }
  | EBinOp of kosu_bin_op
  | EUnOp of kosu_unary_op

and kosu_bin_op =
  | BAdd of kosu_expression location * kosu_expression location
  | BMinus of kosu_expression location * kosu_expression location
  | BMult of kosu_expression location * kosu_expression location
  | BDiv of kosu_expression location * kosu_expression location
  | BMod of kosu_expression location * kosu_expression location
  | BBitwiseOr of kosu_expression location * kosu_expression location
  | BBitwiseAnd of kosu_expression location * kosu_expression location
  | BBitwiseXor of kosu_expression location * kosu_expression location
  | BShiftLeft of kosu_expression location * kosu_expression location
  | BShiftRight of kosu_expression location * kosu_expression location
  | BAnd of kosu_expression location * kosu_expression location
  | BOr of kosu_expression location * kosu_expression location
  | BSup of kosu_expression location * kosu_expression location
  | BSupEq of kosu_expression location * kosu_expression location
  | BInf of kosu_expression location * kosu_expression location
  | BInfEq of kosu_expression location * kosu_expression location
  | BEqual of kosu_expression location * kosu_expression location
  | BDif of kosu_expression location * kosu_expression location
  | BCmp of kosu_expression location * kosu_expression location

and kosu_unary_op =
  | UMinus of kosu_expression location
  | UNot of kosu_expression location

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

type kosu_function_parameters = {
  is_var : bool;
  name : string location;
  kosu_type : TyLoc.kosu_loctype location;
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

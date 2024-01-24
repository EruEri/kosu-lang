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

open TyzuBase
open TyzuType

type tyzu_lvalue =
  | TyzuLvalue of { variable : string typed; fields : string list }

(*
  Wrap kosu_function_parameters and kosu_anon_parameters
  since in tyzu everything is typed
*)
type tyzu_function_parameters = {
  is_var : bool;
  name : string;
  tyzu_type : tyzu_type;
}

(*
  Analoge to kosu_anon_function_kind   
*)
type tyzu_anon_function_kind = TAClosure | TAFunctionPointer

type tyzu_statement =
  | TsDecleration of {
      is_const : bool;
      pattern : tyzu_pattern typed;
      expression : tyzu_expression typed;
    }
  | SAffection of {
      is_deref : bool;
      lvalue : tyzu_lvalue;
      expression : tyzu_expression typed;
    }
  | SDiscard of tyzu_expression typed
  | SOpen of { module_resolver : module_resolver }

and tyzu_pattern =
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
  | PTuple of tyzu_pattern typed list
  | PInteger of int64
  | PCase of {
      module_resolver : module_resolver;
      enum_name : string option;
      variant : string;
      assoc_patterns : tyzu_pattern typed list;
    }
  | PRecord of {
      module_resolver : module_resolver;
      struct_name : string;
      pfields : (string * tyzu_pattern typed) list;
    }
  | POr of tyzu_pattern typed list
  | PAs of { pas_pattern : tyzu_pattern typed; pas_bound : string }

and tyzu_expression =
  | EEmpty
  | ETrue
  | EFalse
  | ECmpLess
  | ECmpEqual
  | ECmpGreater
  | ENullptr of { is_const : bool }
  | EStringl of string
  | EChar of char
  | EInteger of { integer_info : integer_info; ivalue : int64 }
  | EFloat of { fsize : fsize; fvalue : float }
  | ESizeof of tyzu_type
  | EFieldAccess of { first_expr : tyzu_expression typed; field : string }
  | EArrayAccess of {
      array_expr : tyzu_expression typed;
      index_expr : tyzu_expression typed;
    }
  | ETupleAccess of { first_expr : tyzu_expression typed; index : int64 }
  | EConstIdentifier of {
      module_resolver : module_resolver;
      identifier : string;
    }
  | EIdentifier of { module_resolver : module_resolver; id : string }
  | EStruct of {
      module_resolver : module_resolver;
      struct_name : string;
      fields : (string * tyzu_expression typed) list;
    }
  | EEnum of {
      module_resolver : module_resolver;
      enum_name : string option;
      variant : string;
      assoc_exprs : tyzu_expression typed list;
    }
  | EBlock of tyzu_block
  (* * expr *)
  | EDeref of tyzu_expression typed
  | ETuple of tyzu_expression typed list
  | EArray of tyzu_expression typed list
  | EBuiltinFunctionCall of {
      fn_name : string;
      parameters : tyzu_expression typed list;
    }
  | EFunctionCall of {
      module_resolver : module_resolver;
      generics_resolver : tyzu_type list option;
      fn_name : string;
      parameters : tyzu_expression typed list;
    }
  | EWhile of { condition_expr : tyzu_expression typed; body : tyzu_block }
  (* If expression will be a syntaxique sugar of ecases *)
  | ECases of {
      cases : (tyzu_expression typed * tyzu_block) list;
      else_body : tyzu_block;
    }
  | EMatch of {
      expression : tyzu_expression typed;
      patterns : (tyzu_pattern typed * tyzu_block) list;
    }
  | EAnonFunction of {
      kind : tyzu_anon_function_kind;
      parameters : tyzu_function_parameters list;
      body : tyzu_expression typed;
    }

and tyzu_block = {
  tyzu_stmts : tyzu_statement list;
  tyzu_expr : tyzu_expression typed;
}

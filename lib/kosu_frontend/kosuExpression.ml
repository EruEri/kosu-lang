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

type kosu_anon_parameters = {
  ais_var : bool;
  aname : string location;
  akosu_type : TyLoc.kosu_loctype location option;
}

type kosu_anon_function_kind = KAClosure | KAFunctionPointer

type 'a kosu_statement =
  | SDeclaration of {
      is_const : bool;
      pattern : 'a kosu_pattern location;
      explicit_type : TyLoc.kosu_loctype location option;
      expression : 'a kosu_expression location;
    }
  | SAffection of {
      is_deref : bool;
      lvalue : kosu_lvalue;
      expression : 'a kosu_expression location;
    }
  | SDiscard of 'a kosu_expression location
  | SOpen of { module_resolver : module_resolver_loc }

and 'a pattern =
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
  | PTuple of 'a kosu_pattern location list
  | PInteger of { value : int64 location }
  | PCase of {
      module_resolver : module_resolver_loc;
      enum_name : string location option;
      variant : string location;
      assoc_patterns : 'a kosu_pattern location list;
    }
  | PRecord of {
      module_resolver : module_resolver_loc;
      struct_name : string location;
      pfields : (string location * 'a kosu_pattern location) list;
    }
  | POr of 'a kosu_pattern location list
  | PAs of {
      pas_pattern : 'a kosu_pattern location;
      pas_bound : string location;
    }

and 'a kosu_pattern = { kosu_pattern : 'a pattern; pattern_associated : 'a }

and 'a expression =
  | EEmpty
  | ETrue
  | EFalse
  | ECmpLess
  | ECmpEqual
  | ECmpGreater
  | ENullptr of { is_const : bool }
  | EStringl of string
  | EChar of char
  | EInteger of { integer_info : integer_info option; ivalue : int64 }
  | EFloat of { fsize : fsize option; fvalue : float }
  | ESizeof of
      (TyLoc.kosu_loctype location, 'a kosu_expression location) Either.t
  | EFieldAccess of {
      first_expr : 'a kosu_expression location;
      field : string location;
    }
  | EArrayAccess of {
      array_expr : 'a kosu_expression location;
      index_expr : 'a kosu_expression location;
    }
  | ETupleAccess of {
      first_expr : 'a kosu_expression location;
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
      fields : (string location * 'a kosu_expression location) list;
    }
  | EEnum of {
      module_resolver : module_resolver_loc;
      enum_name : string location option;
      variant : string location;
      assoc_exprs : 'a kosu_expression location list;
    }
  | EBlock of 'a kosu_block
  | EDeref of 'a kosu_expression location
  | ETuple of 'a kosu_expression location list
  | EArray of 'a kosu_expression location list
  | EBuiltinFunctionCall of {
      fn_name : string location;
      parameters : 'a kosu_expression location list;
    }
  | EFunctionCall of {
      module_resolver : module_resolver_loc;
      generics_resolver : TyLoc.kosu_loctype location list option;
      fn_name : string location;
      parameters : 'a kosu_expression location list;
    }
  | EWhile of {
      condition_expr : 'a kosu_expression location;
      body : 'a kosu_block;
    }
  (* If expression will be a syntaxique sugar of ecases *)
  | ECases of {
      cases : ('a kosu_expression location * 'a kosu_block) list;
      else_body : 'a kosu_block;
    }
  | EMatch of {
      expression : 'a kosu_expression location;
      patterns : ('a kosu_pattern location * 'a kosu_block) list;
    }
  | EAnonFunction of {
      kind : kosu_anon_function_kind;
      parameters : kosu_anon_parameters list;
      body : 'a kosu_expression location;
    }

and 'a kosu_block = {
  kosu_stmts : 'a kosu_statement location list;
  kosu_expr : 'a kosu_expression location;
}

and 'a kosu_expression = {
  kosu_expression : 'a expression;
  expression_associated : 'a;
}

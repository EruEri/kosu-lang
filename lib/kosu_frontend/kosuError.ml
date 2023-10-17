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

type kosu_lexer_error =
  | UnexpectedEscapedChar of string Position.location
  | UnclosedComment of Position.position
  | CharOutOfRange of int Position.location
  | ForbiddenChar of char Position.location
  | InvalidLitteralBuiltinFunction of char Position.location
  | NotFinishedBuiltinFunction of Position.position

type kosu_syntax_error = {
  position : Position.position;
  current_lexeme : string;
  message : string;
  state : int option;
}

type kosu_analytics_error =
  | KosuAnalysLexerError of kosu_lexer_error
  | KosuAnalysSyntaxError of kosu_syntax_error

type kosu_error =
  | LexerError of kosu_lexer_error
  | AnalyticsError of kosu_analytics_error
  | SizeofPolymorphicType of Position.position
  | DerefNonPointerType of KosuType.Ty.kosu_type Position.location
  | PatternAlreadyBoundIdentifier of string Position.location list
  | PatternIdentifierNotBoundEveryTime of string Position.location list
  | UnboundModule of KosuBaseAst.module_resolver_loc
  | UnboundIdentifier of string Position.location
  | UnboundConstante of {
      module_resolver : KosuAst.module_resolver_loc;
      identifier : string Position.location;
    }
  | IdentifierAlreadyBound of string Position.location
  | NoFieldInStruct of {
      struct_decl : KosuAst.kosu_raw_struct_decl;
      field : string Position.location;
    }
  | NoStructDeclFoundForType of KosuType.Ty.kosu_type Position.location
  | TypingError of KosuType.Ty.kosu_type_constraint
  | NonStructTypeExpression of Position.position
  | NonTupleAccess of Position.position
  | NonArrayAccess of Position.position
  | CannotInferType of Position.position
  | CannotFindStructDecl of KosuType.Ty.kosu_type Position.location
  | ArraySubscribeNotInteger of Position.position
  | TupleIndexOutBound of { expect : int; found : int64 Position.location }
  | UnsupportedFile of string

exception KosuRawErr of kosu_error
exception KosuErr of string * kosu_error
exception KosuLexerError of kosu_lexer_error

let kosu_error (f, e) = KosuErr (f, e)
let kosu_raw_error e = KosuRawErr e
let kosu_lexer_error e = KosuLexerError e
let analytics_error e = kosu_raw_error @@ AnalyticsError e
let sizeof_polytype p = kosu_raw_error @@ SizeofPolymorphicType p
let deref_non_pointer e = kosu_raw_error @@ DerefNonPointerType e

let pattern_already_bound_identifier e =
  kosu_raw_error @@ PatternAlreadyBoundIdentifier e

let pattern_identifier_not_bound e =
  kosu_raw_error @@ PatternIdentifierNotBoundEveryTime e

let unbound_module e = kosu_raw_error @@ UnboundModule e
let unbound_identifier e = kosu_raw_error @@ UnboundIdentifier e

let unbound_constante module_resolver identifier =
  kosu_raw_error @@ UnboundConstante { module_resolver; identifier }

let identifier_already_bound e = kosu_raw_error @@ IdentifierAlreadyBound e

let field_not_in_struct struct_decl field =
  kosu_raw_error @@ NoFieldInStruct { struct_decl; field }

let no_struct_decl_for_type t = kosu_raw_error @@ NoStructDeclFoundForType t
let typing_error consts = kosu_raw_error @@ TypingError consts
let non_struct_type p = kosu_raw_error @@ NonStructTypeExpression p
let non_tuple_access p = kosu_raw_error @@ NonTupleAccess p
let non_array_access p = kosu_raw_error @@ NonArrayAccess p
let cannot_infer_type p = kosu_raw_error @@ CannotInferType p
let cannot_find_struct_decl p = kosu_raw_error @@ CannotFindStructDecl p
let array_subscribe_not_integer p = kosu_raw_error @@ ArraySubscribeNotInteger p

let index_out_of_bounds expect found =
  kosu_raw_error @@ TupleIndexOutBound { expect; found }

let unsupported_file f = kosu_raw_error @@ UnsupportedFile f

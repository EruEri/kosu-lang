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
  | LexerErrof of kosu_lexer_error
  | SizeofPolymorphicType of Position.position
  | DerefNonPointerType of KosuType.Ty.kosu_type Position.location
  | PatternAlreadyBoundIdentifier of string Position.location list
  | PatternIdentifierNotBoundEveryTime of string Position.location list

exception KosuErr of kosu_error
exception KosuLexerError of kosu_lexer_error

let kosu_error e = KosuErr e
let kosu_lexer_error e = KosuLexerError e
let sizeof_polytype p = kosu_error @@ SizeofPolymorphicType p
let deref_non_pointer e = kosu_error @@ DerefNonPointerType e

let pattern_already_bound_identifier e =
  kosu_error @@ PatternAlreadyBoundIdentifier e

let pattern_identifier_not_bound e =
  kosu_error @@ PatternIdentifierNotBoundEveryTime e

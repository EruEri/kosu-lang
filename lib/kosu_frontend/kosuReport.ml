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

let some = Option.some
let none = Option.none
let some_left e = some @@ Either.left e
let some_right e = some @@ Either.right e

let kosu_lexer_error_range ?file = function
  | KosuError.UnexpectedEscapedChar { value = _; position }
  | UnclosedComment position
  | CharOutOfRange { value = _; position }
  | ForbiddenChar { value = _; position }
  | InvalidLitteralBuiltinFunction { value = _; position }
  | NotFinishedBuiltinFunction position ->
      to_asai_range ?file position

let kosu_syntax_error_range ?file = function
  | KosuError.{ position; _ } ->
      to_asai_range ?file position

let kosu_error_range ?file = function
  | KosuError.AnalyticsError (file, KosuAnalysLexerError e) ->
      some_left @@ kosu_lexer_error_range ~file e
  | AnalyticsError (file, KosuAnalysSyntaxError e) ->
      some_left @@ kosu_syntax_error_range ~file e
  | DerefNonPointerType { value = _; position }
  | UnboundIdentifier { value = _; position }
  | IdentifierAlreadyBound { value = _; position }
  | NoStructDeclFoundForType { value = _; position }
  | CannotFindStructDecl { value = _; position }
  | ConstNonStaticExpression { position; value = _ }
  | NonStructTypeExpression position
  | NonTupleAccess position
  | NonArrayAccess position
  | CannotInferType position
  | ArraySubscribeNotInteger position
  | TupleIndexOutBound { expect = _; found = { value = _; position } }
  | UnboundConstante
      { identifier = { value = _; position }; module_resolver = _ }
  | NoFieldInStruct { field = { value = _; position }; struct_decl = _ }
  | TypingError { position; _ } ->
      some_left @@ to_asai_range ?file position
  | PatternAlreadyBoundIdentifier patterns
  | PatternIdentifierNotBoundEveryTime patterns ->
      some_right @@ Bwd.Bwd.of_list
      @@ List.map (Position.to_loctext ?file) patterns
  | ConfictingCallableDeclaration list ->
      some_right @@ Bwd.Bwd.of_list
      @@ List.map
           (let open KosuAst in
            function
            | `External decl ->
                Position.to_loctext ?file decl.sig_name
            | `KosuFunction decl ->
                Position.to_loctext ?file decl.fn_name
            | `Syscall decl ->
                Position.to_loctext ?file decl.syscall_name
           )
           list
  | VariableTypeNotBound vars ->
      some_right @@ Bwd.Bwd.of_list
      @@ List.map
           (fun (KosuType.TyLoc.PolymorphicVarLoc s) ->
             Position.to_loctext ?file s
           )
           vars
  | UnboundModule _
  | ConfictingTypeDeclaration _
  | UnsupportedFile _
  | SizeofPolymorphicType _
  | DuplicatedParametersName _ ->
      None

let kosu_error_formatted = function
  | s ->
      String.escaped @@ KosuPrint.Formatted.string_of_kosu_error s

let emitf ?file kosu_error =
  let f =
    match kosu_error_range ?file kosu_error with
    | None ->
        KosuError.Reporter.emitf ?loc:None ?backtrace:None
    | Some (Either.Left loc) ->
        KosuError.Reporter.emitf ~loc ?backtrace:None
    | Some (Either.Right backtrace) ->
        (*
          Currently nullify the the backtrace   
        *)
        let backtrace =
          Bwd.Bwd.map
            (fun e ->
              Asai.Range.{ e with value = Asai.Diagnostic.text String.empty }
            )
            backtrace
        in
        KosuError.Reporter.emitf ?loc:None ~backtrace
  in

  f kosu_error "%s" (kosu_error_formatted kosu_error)

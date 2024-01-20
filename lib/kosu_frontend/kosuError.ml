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
  | UnclosedString of Position.position
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
  | AnalyticsError of (string * kosu_analytics_error)
  | SizeofPolymorphicType of Position.position
  | DerefNonPointerType of KosuType.Ty.kosu_type Position.location
  | PatternAlreadyBoundIdentifier of string Position.location list
  | PatternIdentifierNotBoundEveryTime of string Position.location list
  | UnboundModule of KosuBaseAst.module_resolver_loc
  | UnboundIdentifier of string Position.location
  | UnboundBuiltinFunction of string Position.location
  | UnboundConstante of {
      module_resolver : KosuAst.module_resolver_loc;
      identifier : string Position.location;
    }
  | UnboundStruct of {
      module_resolver : KosuAst.module_resolver_loc;
      struct_name : string Position.location;
    }
  | UnboundEnum of {
      module_resolver : KosuAst.module_resolver_loc;
      enum_name : string Position.location option;
      variant : string Position.location;
    }
  | IdentifierAlreadyBound of string Position.location
  | NoFieldInStruct of {
      struct_decl : KosuAst.kosu_raw_struct_decl;
      field : string Position.location;
    }
  | StructInitWrongField of {
      expected : string;
      found : string Position.location;
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
  | ConstNonStaticExpression of KosuAst.kosu_expression Position.location
  | ConfictingTypeDeclaration of
      [ `NEnum of KosuAst.kosu_enum_decl
      | `NOpaque of KosuAst.kosu_opaque_decl
      | `NStruct of KosuAst.kosu_struct_decl ]
      list
  | ConfictingCallableDeclaration of KosuAst.kosu_callable_decl list
  | UnsupportedFile of string
  | VariableTypeNotBound of KosuType.TyLoc.kosu_loctype_polymorphic list
  | CapturedVariableForFunctionPointer of string Position.location list
  | DuplicatedParametersName of {
      function_location : string Position.location;
      lhs : string Position.location;
      rhs : string Position.location;
    }
  | DuplicatedFieldName of {
      type_name : string Position.location;
      lhs : string Position.location;
      rhs : string Position.location;
    }
  | DuplicatedEnumVariant of {
      enum_name : string Position.location;
      lhs : string Position.location;
      rhs : string Position.location;
    }
  | WrongArityCallable of {
      callable : string Position.location;
      expected : int;
      found : int;
    }
  | EnumVariantWrongArity of {
      variant : string Position.location;
      expected : int;
      found : int;
    }
  | StructFieldWrongArity of {
      struct_name : string Position.location;
      expected : int;
      found : int;
    }
  | CyclicTypeDeclaration of KosuAst.kosu_type_decl
  | TypeDeclarationNotFound of KosuType.TyLoc.kosu_loctype Position.location

exception KosuRawErr of kosu_error
exception KosuErr of string * kosu_error
exception KosuLexerError of kosu_lexer_error

module Raw = struct
  let analytics_error e = AnalyticsError e
  let sizeof_polytype p = SizeofPolymorphicType p
  let deref_non_pointer e = DerefNonPointerType e
  let pattern_already_bound_identifier e = PatternAlreadyBoundIdentifier e
  let pattern_identifier_not_bound e = PatternIdentifierNotBoundEveryTime e
  let unbound_module e = UnboundModule e
  let unbound_identifier e = UnboundIdentifier e

  let unbound_constante module_resolver identifier =
    UnboundConstante { module_resolver; identifier }

  let unbound_struct module_resolver struct_name =
    UnboundStruct { module_resolver; struct_name }

  let unbound_enum module_resolver enum_name variant =
    UnboundEnum { module_resolver; enum_name; variant }

  let identifier_already_bound e = IdentifierAlreadyBound e

  let field_not_in_struct struct_decl field =
    NoFieldInStruct { struct_decl; field }

  let struct_init_wrong_field expected found =
    StructInitWrongField { expected; found }

  let unbound_builtin_function name = UnboundBuiltinFunction name
  let no_struct_decl_for_type t = NoStructDeclFoundForType t
  let typing_error consts = TypingError consts
  let non_struct_type p = NonStructTypeExpression p
  let non_tuple_access p = NonTupleAccess p
  let non_array_access p = NonArrayAccess p
  let cannot_infer_type p = CannotInferType p
  let cannot_find_struct_decl p = CannotFindStructDecl p
  let array_subscribe_not_integer p = ArraySubscribeNotInteger p
  let const_non_static_expression n = ConstNonStaticExpression n
  let conflicting_type_declaration s = ConfictingTypeDeclaration s
  let conflicting_callable_declaration s = ConfictingCallableDeclaration s
  let index_out_of_bounds expect found = TupleIndexOutBound { expect; found }
  let unsupported_file f = UnsupportedFile f
  let variable_type_not_bound l = VariableTypeNotBound l
  let type_declaration_not_found e = TypeDeclarationNotFound e

  let duplicated_param_name function_location lhs rhs =
    DuplicatedParametersName { function_location; lhs; rhs }

  let duplicated_fiels type_name lhs rhs =
    DuplicatedFieldName { type_name; rhs; lhs }

  let duplicated_variants enum_name lhs rhs =
    DuplicatedEnumVariant { enum_name; rhs; lhs }

  let callable_wrong_arity callable expected found =
    WrongArityCallable { callable; expected; found }

  let enum_variant_wrong_arity variant expected found =
    EnumVariantWrongArity { variant; expected; found }

  let struct_wrong_arity struct_name expected found =
    StructFieldWrongArity { struct_name; expected; found }

  let cyclic_type_declaration e = CyclicTypeDeclaration e
  let captured_variables_for_fnptr s = CapturedVariableForFunctionPointer s
end

module Exn = struct
  open Util.Operator

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

  let unbound_builtin_function = kosu_raw_error $ Raw.unbound_builtin_function
  let unbound_module e = kosu_raw_error @@ UnboundModule e
  let unbound_identifier e = kosu_raw_error @@ UnboundIdentifier e

  let unbound_struct module_resolver struct_name =
    kosu_raw_error @@ Raw.unbound_struct module_resolver struct_name

  let unbound_enum module_resolver enum_name variant =
    kosu_raw_error @@ Raw.unbound_enum module_resolver enum_name variant

  let struct_init_wrong_field expected found =
    kosu_raw_error @@ Raw.struct_init_wrong_field expected found

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

  let array_subscribe_not_integer p =
    kosu_raw_error @@ ArraySubscribeNotInteger p

  let const_non_static_expression n =
    kosu_raw_error @@ ConstNonStaticExpression n

  let conflicting_type_declaration s =
    kosu_raw_error @@ ConfictingTypeDeclaration s

  let conflicting_callable_declaration s =
    kosu_raw_error @@ Raw.conflicting_callable_declaration s

  let index_out_of_bounds expect found =
    kosu_raw_error @@ TupleIndexOutBound { expect; found }

  let unsupported_file f = kosu_raw_error @@ UnsupportedFile f

  let variable_type_not_bound l =
    kosu_raw_error @@ Raw.variable_type_not_bound l

  let type_declaration_not_found =
    kosu_raw_error $ Raw.type_declaration_not_found

  let enum_variant_wrong_arity variant expected found =
    kosu_raw_error @@ Raw.enum_variant_wrong_arity variant expected found

  let callable_wrong_arity callable expected found =
    kosu_raw_error @@ Raw.callable_wrong_arity callable expected found

  let struct_wrong_arity struct_name expected found =
    kosu_raw_error @@ Raw.struct_wrong_arity struct_name expected found

  let duplicated_param_name function_location lhs rhs =
    kosu_raw_error @@ Raw.duplicated_param_name function_location lhs rhs

  let duplicated_enum_name enum_name lhs rhs =
    kosu_raw_error @@ Raw.duplicated_variants enum_name lhs rhs

  let cyclic_type_declaration e =
    kosu_raw_error @@ Raw.cyclic_type_declaration e

  let captured_variables_for_fnptr e =
    kosu_raw_error @@ Raw.captured_variables_for_fnptr e
end

module Function = struct
  let kosu_lexer_error_range = function
    | UnexpectedEscapedChar { value = _; position }
    | UnclosedComment position
    | UnclosedString position
    | CharOutOfRange { value = _; position }
    | ForbiddenChar { value = _; position }
    | InvalidLitteralBuiltinFunction { value = _; position }
    | NotFinishedBuiltinFunction position ->
        position

  let kosu_syntax_error_range = function
    | { position; state = _; _ } ->
        position

  let to_position = function
    | AnalyticsError (_, KosuAnalysLexerError e) ->
        kosu_lexer_error_range e :: []
    | AnalyticsError (_, KosuAnalysSyntaxError e) ->
        kosu_syntax_error_range e :: []
    | StructFieldWrongArity { struct_name = { position; value = _ }; _ }
    | EnumVariantWrongArity { variant = { position; value = _ }; _ }
    | WrongArityCallable { callable = { position; value = _ }; _ }
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
    | SizeofPolymorphicType position
    | UnboundBuiltinFunction { position; value = _ }
    | UnboundConstante
        { identifier = { value = _; position }; module_resolver = _ }
    | UnboundStruct { struct_name = { position; value = _ }; _ }
    | NoFieldInStruct { field = { value = _; position }; struct_decl = _ }
    | StructInitWrongField { found = { position; value = _ }; expected = _ }
    | TypeDeclarationNotFound { value = _; position }
    | TypingError { position; _ } ->
        position :: []
    | CyclicTypeDeclaration s ->
        let p =
          match s with
          | DEnum { enum_name; _ } ->
              enum_name.position
          | DStruct { struct_name; _ } ->
              struct_name.position
        in
        [ p ]
    | UnboundEnum { enum_name; variant; _ } ->
        let p = Position.position @@ Option.value ~default:variant enum_name in
        [ p ]
    | PatternAlreadyBoundIdentifier patterns
    | PatternIdentifierNotBoundEveryTime patterns ->
        List.map Position.position patterns
    | ConfictingCallableDeclaration list ->
        List.map
          (let open KosuAst in
           function
           | CdExternalFunction decl ->
               decl.sig_name.position
           | CdKosuFuntion decl ->
               decl.fn_name.position
          )
          list
    | VariableTypeNotBound vars ->
        List.map
          (fun (KosuType.TyLoc.PolymorphicVarLoc s) -> Position.position s)
          vars
    | DuplicatedEnumVariant { rhs; lhs; _ }
    | DuplicatedParametersName { rhs; lhs; _ }
    | DuplicatedFieldName { lhs; rhs; _ } ->
        [ rhs.position; lhs.position ]
    | CapturedVariableForFunctionPointer variables ->
        List.map Position.position variables
    | UnboundModule _ | ConfictingTypeDeclaration _ | UnsupportedFile _ ->
        []
end

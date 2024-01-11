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

open Printf
open Position

let string_of_position_error { start_position; end_position } =
  let start_position_line, start_position_column =
    line_column_of_position start_position
  in
  let end_position_line, end_position_column =
    line_column_of_position end_position
  in
  let column_string =
    Printf.sprintf "%d%s" start_position_column
      ( if start_position_column = end_position_column then
          ""
        else
          " - " ^ string_of_int end_position_column
      )
  in
  if start_position_line = end_position_line then
    Printf.sprintf "Line %d, Characters %s" start_position_line column_string
  else
    Printf.sprintf "Lines %d-%d, Characters %d-%d" start_position_line
      end_position_line start_position_column end_position_column

let string_of_located_error ?(new_line = false) a b =
  Printf.sprintf "%s :%s %s"
    (string_of_position_error a.position)
    ( if new_line then
        "\n"
      else
        String.empty
    )
    b

let string_of_module_resolver : KosuBaseAst.module_resolver -> string = function
  | ModuleResolver_ [] ->
      String.empty
  | ModuleResolver_ (_ :: _ as l) ->
      l |> String.concat "::" |> Printf.sprintf "%s."

let string_of_module_resolver_loc : KosuBaseAst.module_resolver_loc -> string =
  function
  | ModuleResolverLoc [] ->
      String.empty
  | ModuleResolverLoc (_ :: _ as l) ->
      l |> List.map Position.value |> String.concat "::" |> Printf.sprintf "%s."

let string_of_pointee_state : KosuAst.pointer_state -> string = function
  | Const ->
      "const"
  | Mutable ->
      "mut"

let char_of_signdess : KosuAst.signedness -> char = function
  | Signed ->
      's'
  | Unsigned ->
      'u'

let string_of_isize : KosuAst.isize -> string = function
  | I8 ->
      "8"
  | I16 ->
      "16"
  | I32 ->
      "32"
  | I64 ->
      "64"

let string_of_fsize : KosuAst.fsize -> string = function
  | F32 ->
      "32"
  | F64 ->
      "64"

let string_of_integer_info : KosuAst.integer_info -> string = function
  | Worded sign ->
      sprintf "%csize" (char_of_signdess sign)
  | Sized (sign, size) ->
      sprintf "%c%s" (char_of_signdess sign) (string_of_isize size)

let rec string_of_kosu_type : KosuType.Ty.kosu_type -> string = function
  | TyIdentifier { module_resolver; parametrics_type; name } ->
      Printf.sprintf "%s%s%s"
        (string_of_module_resolver module_resolver)
        name
        (string_of_parenthisis string_of_kosu_type parametrics_type)
  | TyPolymorphic s ->
      string_of_polymorphic_var s
  | TyPointer { pointer_state; pointee_type } ->
      sprintf "*%s %s"
        (string_of_pointee_state pointer_state)
        (string_of_kosu_type pointee_type)
  | TyInteger integer ->
      integer
      |> Option.map string_of_integer_info
      |> Option.value ~default:"i(unknow)"
  | TyFloat float ->
      float |> Option.map string_of_fsize |> Option.value ~default:"f(unknown)"
  | TyFunctionPtr schema ->
      sprintf "fn %s" @@ string_of_schema schema
  | TyClosure schema ->
      sprintf "closure %s" @@ string_of_schema schema
  | TyArray { ktype; size } ->
      sprintf "array(%s : %Lu)" (string_of_kosu_type ktype) size
  | TyTuple ttes ->
      (* TyTuples is never empty*)
      string_of_parenthisis string_of_kosu_type ttes
  | TyOpaque { module_resolver; name } ->
      sprintf "%s#%s" (string_of_module_resolver module_resolver) name
  | TyOrdered ->
      "order"
  | TyStringLit ->
      "stringl"
  | TyChar ->
      "char"
  | TyBool ->
      "bool"
  | TyUnit ->
      "unit"

and string_of_polymorphic_var = function
  | PolymorphicVar s ->
      Printf.sprintf "'%s" s
  | CompilerPolymorphicVar s ->
      Printf.sprintf "''%s" s

(* and string_of_closure_type = function
   | ClosureType { id; schema; env } ->
       let schema = string_of_schema schema in
       let senv =
         env
         |> List.map (fun (s, ty) ->
                sprintf "%s : %s" s @@ string_of_kosu_type ty
            )
         |> String.concat ", "
       in
       sprintf "%s : %s with { %s }" id schema senv *)

and string_of_schema = function
  | { poly_vars; parameters_type; return_type } ->
      let spolyvars =
        match poly_vars with
        | [] ->
            String.empty
        | _ :: _ as poly_vars ->
            poly_vars
            |> List.map string_of_polymorphic_var
            |> String.concat ", " |> sprintf "%s ."
      in
      sprintf "%s(%s) %s" spolyvars
        (parameters_type |> List.map string_of_kosu_type |> String.concat ", ")
        (string_of_kosu_type return_type)

and string_of_parenthisis f = function
  | [] ->
      ""
  | _ :: _ as l ->
      Printf.sprintf "(%s)" (l |> List.map f |> String.concat ", ")

let string_of_file_error filename = sprintf "\nFile \"%s\", %s" filename

let string_of_kosu_lexer_error : string -> KosuError.kosu_lexer_error -> string
    =
 fun filename ->
  let sfile = string_of_file_error filename in
  function
  | UnexpectedEscapedChar s ->
      let sloc = string_of_located_error s in
      sfile @@ sloc @@ sprintf "Unexpected Escaped character: %s" s.value
  | UnclosedComment position ->
      let sloc = string_of_position_error position in
      sfile @@ sprintf "%s : Comments not terminated" sloc
  | UnclosedString position ->
      let sloc = string_of_position_error position in
      sfile @@ sprintf "%s : String litteral is not closed" sloc
  | CharOutOfRange int ->
      let sloc = string_of_located_error int in
      sfile @@ sloc @@ sprintf "Ascii value '%u' is not in [0-255]" int.value
  | ForbiddenChar char ->
      let sloc = string_of_located_error char in
      sfile @@ sloc @@ sprintf "Forbidden character : %c" char.value
  | InvalidLitteralBuiltinFunction char ->
      let sloc = string_of_located_error char in
      sfile @@ sloc
      @@ sprintf ": Invalid Litteral For Builtin Function: %c" char.value
  | NotFinishedBuiltinFunction position ->
      let sloc = string_of_position_error position in
      sfile @@ sprintf "%s : Builtin function not finished" sloc

let string_of_kosu_syntax_error :
    string -> KosuError.kosu_syntax_error -> string =
 fun filename { position; current_lexeme; message; state } ->
  let s = string_of_position_error position in
  let state =
    state
    |> Option.map (sprintf "Error in state \"%d\"")
    |> Option.value ~default:""
  in
  sprintf "\nFile \"%s\", %s : Unexpected \"%s\"\nSyntax Error : %s%s" filename
    s current_lexeme message state

let string_of_analytics_error :
    string -> KosuError.kosu_analytics_error -> string =
 fun filename -> function
  | KosuAnalysLexerError e ->
      string_of_kosu_lexer_error filename e
  | KosuAnalysSyntaxError e ->
      string_of_kosu_syntax_error filename e

let string_of_kosu_error : string -> KosuError.kosu_error -> string =
 fun filename ->
  let sfile = string_of_file_error filename in
  function
  (* | LexerError kosu_lexer_error ->
      string_of_kosu_lexer_error filename kosu_lexer_error *)
  | AnalyticsError (_, kae) ->
      string_of_analytics_error filename kae
  | SizeofPolymorphicType _ ->
      "Dummy eerror"
  | DerefNonPointerType kosu_type ->
      let sloc = string_of_located_error kosu_type in
      sfile @@ sloc
      @@ sprintf
           ": This expression has the type \"%s\" which is not a pointer. \
            Therefore, it can't be deferenced"
           (string_of_kosu_type kosu_type.value)
  | PatternAlreadyBoundIdentifier already_bound ->
      let s =
        already_bound
        |> List.map (fun sid -> string_of_located_error sid sid.value)
        |> String.concat "\n"
      in
      sfile @@ sprintf "The following identifiers are already bound : \n%s" s
  | PatternIdentifierNotBoundEveryTime missing_binding ->
      let s =
        missing_binding
        |> List.map (fun sid -> string_of_located_error sid sid.value)
        |> String.concat "\n"
      in
      sfile
      @@ sprintf "The following identifiers aren't bound in each pattern : \n%s"
           s
  | UnboundModule module_resolver ->
      let s : string location =
        match module_resolver with
        | ModuleResolverLoc [] ->
            Position.dummy_located "Empty module doesnt exist"
        | ModuleResolverLoc (_ :: _) as _modules ->
            failwith ""
      in
      let sloc = string_of_located_error s in
      sfile @@ sloc @@ sprintf "Unbound Module : %s" s.value
  | UnboundIdentifier i ->
      let sloc = string_of_located_error i in
      sfile @@ sloc @@ sprintf "Unbound idenfier : %s" i.value
  | IdentifierAlreadyBound identifier ->
      let sloc = string_of_located_error identifier in
      sfile @@ sloc
      @@ sprintf "Identifier \"%s\" is already defined" identifier.value
  | NoFieldInStruct { struct_decl; field } ->
      let sloc = string_of_located_error field in
      sfile @@ sloc
      @@ sprintf "Struct \"%s\" doesnt' have a field \"%s\""
           struct_decl.struct_name field.value
  | NoStructDeclFoundForType ty ->
      let sloc = string_of_located_error ty in
      sfile @@ sloc
      @@ sprintf "Type %s isn't the type of a struct"
      @@ string_of_kosu_type @@ value ty
  | TypingError { cexpected; cfound; position = p } ->
      let sloc =
        string_of_located_error Position.{ value = (); position = p }
      in
      sfile @@ sloc
      @@ sprintf "Incompatible type : Expected \"%s\", Found \"%s\""
           (string_of_kosu_type cexpected)
           (string_of_kosu_type cfound)
  | NonStructTypeExpression p ->
      let sloc =
        string_of_located_error Position.{ value = (); position = p }
      in
      sfile @@ sloc @@ sprintf "This expressions is not an struct type"
  | NonTupleAccess p ->
      let sloc =
        string_of_located_error Position.{ value = (); position = p }
      in
      sfile @@ sloc @@ sprintf "This expressions is not a tuple"
  | NonArrayAccess p ->
      let sloc =
        string_of_located_error Position.{ value = (); position = p }
      in
      sfile @@ sloc @@ sprintf "This expressions is not a array"
  | CannotInferType p ->
      let sloc =
        string_of_located_error Position.{ value = (); position = p }
      in
      sfile @@ sloc @@ sprintf "Cannot infer the type of this expression"
  | CannotFindStructDecl kosu_type ->
      let sloc = string_of_located_error kosu_type in
      sfile @@ sloc
      @@ sprintf "No struct declaration associated this the type : %s"
      @@ string_of_kosu_type @@ Position.value kosu_type
  | ArraySubscribeNotInteger p ->
      let sloc =
        string_of_located_error Position.{ value = (); position = p }
      in
      sfile @@ sloc
      @@ sprintf
           "This expression is not an integer; It's can not be used as array \
            subscribe"
  | TupleIndexOutBound { expect; found } ->
      let sloc = string_of_located_error found in
      sfile @@ sloc
      @@ sprintf
           "This expression is a tuple (arity %u) but of try to access %Lu"
           expect found.value
  | ConstNonStaticExpression expr ->
      let sloc = string_of_located_error expr in
      sfile @@ sloc @@ sprintf "This expression cannot be none at compile time"
  | UnboundConstante { module_resolver; identifier } ->
      let sloc = string_of_located_error identifier in
      sfile @@ sloc
      @@ sprintf "Unbound Constante : %s%s"
           (string_of_module_resolver_loc module_resolver)
           identifier.value
  | ConfictingTypeDeclaration list ->
      let open KosuAst in
      let name =
        list
        |> List.map (function
             | `NEnum decl ->
                 string_of_located_error decl.enum_name
                 @@ Printf.sprintf "enum %s" decl.enum_name.value
             | `NOpaque opaque ->
                 string_of_located_error opaque.name
                 @@ Printf.sprintf "opaque type %s" opaque.name.value
             | `NStruct str ->
                 string_of_located_error str.struct_name
                 @@ Printf.sprintf "struct %s" str.struct_name.value
             )
        |> String.concat "\n\t"
      in
      sfile @@ Printf.sprintf "Conflicting type declaration:\n\t%s" name
  | ConfictingCallableDeclaration list ->
      let open KosuAst in
      let name =
        list
        |> List.map (function
             | CdKosuFuntion decl ->
                 string_of_located_error decl.fn_name
                 @@ Printf.sprintf "function \"%s\"" decl.fn_name.value
             | CdExternalFunction decl ->
                 string_of_located_error decl.sig_name
                 @@ Printf.sprintf "external function \"%s\""
                      decl.sig_name.value
             )
        |> String.concat "\n\t"
      in
      sfile @@ Printf.sprintf "Conflicting callable declaration:\n\t%s" name
  | UnsupportedFile f ->
      sprintf "Unknown file kind : %s" f
  | VariableTypeNotBound l ->
      sfile
      @@ sprintf "The following type variable aren't bound:\n\t%s"
      @@ String.concat "\n\t"
      @@ List.map
           (fun (KosuType.TyLoc.PolymorphicVarLoc s) ->
             string_of_located_error s @@ sprintf "'%s" s.value
           )
           l
  | DuplicatedParametersName { function_location; lhs; rhs } ->
      let lhs = string_of_located_error lhs @@ lhs.value in
      let rhs = string_of_located_error rhs @@ rhs.value in
      let sloc = string_of_located_error function_location in
      sfile @@ sloc
      @@ sprintf "function \"%s\" duplicated paramater identifier\n\t%s\n\t%s"
           function_location.value lhs rhs
  | DuplicatedEnumVariant { enum_name; lhs; rhs } ->
      let lhs = string_of_located_error lhs @@ lhs.value in
      let rhs = string_of_located_error rhs @@ rhs.value in
      let sloc = string_of_located_error enum_name in
      sfile @@ sloc
      @@ sprintf "enum \"%s\" duplicated variant identifier\n\t%s\n\t%s"
           enum_name.value lhs rhs
  | DuplicatedFieldName { type_name; lhs; rhs } ->
      let lhs = string_of_located_error lhs @@ lhs.value in
      let rhs = string_of_located_error rhs @@ rhs.value in
      let sloc = string_of_located_error type_name in
      sfile @@ sloc
      @@ sprintf "type \"%s\" duplicated field identifier\n\t%s\n\t%s"
           type_name.value lhs rhs
  | TypeDeclarationNotFound kosu_type ->
      let sloc = string_of_located_error kosu_type in
      sfile @@ sloc
      @@ sprintf "Cannot find type \"%s\""
      @@ string_of_kosu_type
      @@ KosuUtil.Ty.of_tyloc' kosu_type
  | CyclicTypeDeclaration type_decl ->
      let string_kind, name =
        match type_decl with
        | DEnum { enum_name; _ } ->
            ("enum", enum_name)
        | DStruct { struct_name; _ } ->
            ("struct", struct_name)
      in
      let sloc = string_of_located_error name in
      sfile @@ sloc
      @@ sprintf "%s \"%s\" has a cyclic declaration" string_kind name.value

module Formatted = struct
  let string_of_kosu_lexer_error : KosuError.kosu_lexer_error -> string =
    function
    | UnexpectedEscapedChar s ->
        sprintf "Unexpected Escaped character: %s" s.value
    | UnclosedComment _ ->
        "Comments not terminated"
    | UnclosedString _ ->
        "string litteral is not closed"
    | CharOutOfRange int ->
        sprintf "Ascii value '%u' is not in [0-255]" int.value
    | ForbiddenChar char ->
        sprintf "Forbidden character : %c" char.value
    | InvalidLitteralBuiltinFunction char ->
        sprintf "Invalid Litteral For Builtin Function: %c" char.value
    | NotFinishedBuiltinFunction _ ->
        "Builtin function not finished"

  let string_of_kosu_syntax_error : KosuError.kosu_syntax_error -> string =
   fun { position = _; current_lexeme; message; state } ->
    let state =
      state
      |> Option.map (sprintf "Error in state \"%d\"")
      |> Option.value ~default:String.empty
    in
    Printf.sprintf "Unexpected \"%s\"\nSyntax Error : %s%s" current_lexeme
      message state

  let string_of_analytics_error : KosuError.kosu_analytics_error -> string =
    function
    | KosuAnalysLexerError e ->
        string_of_kosu_lexer_error e
    | KosuAnalysSyntaxError e ->
        string_of_kosu_syntax_error e

  let string_of_kosu_error : KosuError.kosu_error -> string = function
    | AnalyticsError (_, kae) ->
        string_of_analytics_error kae
    | SizeofPolymorphicType _ ->
        "Dummy eerror"
    | DerefNonPointerType kosu_type ->
        sprintf
          "This expression has the type \"%s\" which is not a pointer. \
           Therefore, it can't be deferenced"
          (string_of_kosu_type kosu_type.value)
    | PatternAlreadyBoundIdentifier _ ->
        String.escaped @@ sprintf "The following identifiers are already bound"
    | PatternIdentifierNotBoundEveryTime _ ->
        sprintf "The following identifiers aren't bound in each patterns"
    | UnboundModule module_resolver ->
        let s : string location =
          match module_resolver with
          | ModuleResolverLoc [] ->
              Position.dummy_located "Empty module doesnt exist"
          | ModuleResolverLoc (_ :: _) as _modules ->
              failwith ""
        in
        sprintf "Unbound Module : %s" s.value
    | UnboundIdentifier i ->
        sprintf "Unbound identifier : %s" i.value
    | IdentifierAlreadyBound identifier ->
        sprintf {| Identifier %s is already defined |} identifier.value
    | NoFieldInStruct { struct_decl; field } ->
        sprintf "Struct \"%s\" doesnt' have a field \"%s\""
          struct_decl.struct_name field.value
    | NoStructDeclFoundForType ty ->
        sprintf "Type %s isn't the type of a struct"
        @@ string_of_kosu_type @@ value ty
    | TypingError { cexpected; cfound; position = _ } ->
        sprintf "Incompatible type : Expected \"%s\", Found \"%s\""
          (string_of_kosu_type cexpected)
          (string_of_kosu_type cfound)
    | NonStructTypeExpression _ ->
        sprintf "This expressions is not an struct type"
    | NonTupleAccess _ ->
        sprintf "This expressions is not a tuple"
    | NonArrayAccess _ ->
        sprintf "This expressions is not a array"
    | CannotInferType _ ->
        sprintf "Cannot infer the type of this expression"
    | CannotFindStructDecl kosu_type ->
        sprintf "No struct declaration associated this the type : %s"
        @@ string_of_kosu_type @@ Position.value kosu_type
    | ArraySubscribeNotInteger _ ->
        sprintf
          "This expression is not an integer; It's can not be used as array \
           subscribe"
    | TupleIndexOutBound { expect; found } ->
        sprintf "This expression is a tuple (arity %u) but of try to access %Lu"
          expect found.value
    | ConstNonStaticExpression _ ->
        sprintf "This expression cannot be none at compile time"
    | UnboundConstante { module_resolver; identifier } ->
        sprintf "Unbound Constante : %s%s"
          (string_of_module_resolver_loc module_resolver)
          identifier.value
    | ConfictingTypeDeclaration _ ->
        Printf.sprintf "Conflicting type declarations"
    | ConfictingCallableDeclaration _ ->
        Printf.sprintf "Conflicting callable declarations"
    | UnsupportedFile f ->
        sprintf "Unknown file kind : %s" f
    | VariableTypeNotBound _ ->
        sprintf "The following type variable aren't bound"
    | DuplicatedParametersName { function_location = _; lhs = _; rhs = _ } ->
        sprintf "Duplicated paramater identifiers"
    | DuplicatedFieldName { type_name = _; lhs = _; rhs = _ } ->
        sprintf "Duplicated field identifiers"
    | DuplicatedEnumVariant { enum_name = _; lhs = _; rhs = _ } ->
        sprintf "Duplicated variant identifiers"
    | TypeDeclarationNotFound kosu_type ->
        sprintf "Cannot find type \"%s\""
        @@ string_of_kosu_type (KosuUtil.Ty.of_tyloc' kosu_type)
    | CyclicTypeDeclaration s -> (
        match s with
        | DEnum { enum_name; _ } ->
            sprintf "enum \"%s\" has a cyclic declaration" @@ enum_name.value
        | DStruct { struct_name; _ } ->
            sprintf "struct \"%s\" has a cyclic declaration"
            @@ struct_name.value
      )
end

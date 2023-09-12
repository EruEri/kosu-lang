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

let string_of_module_resolver : KosuBaseAst.module_resolver -> string = function
  | ModuleResolver_ [] ->
      String.empty
  | ModuleResolver_ (_ :: _ as l) ->
      l |> String.concat "::" |> Printf.sprintf "%s."

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
  | TyInnerClosureId closure_type ->
      string_of_closure_type closure_type
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

and string_of_closure_type = function
  | ClosureType { id; schema; env } ->
      let schema = string_of_schema schema in
      let senv =
        env
        |> List.map (fun (s, ty) ->
               sprintf "%s : %s" s @@ string_of_kosu_type ty
           )
        |> String.concat ", "
      in
      sprintf "%s : %s with { %s }" id schema senv

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

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

open KosuAst

let ok = Ok ()
let ( let* ) = Result.bind

module StringLoc = Set.Make (struct
  type t = string Position.location

  let compare lhs rhs =
    let open Position in
    String.compare lhs.value rhs.value
end)

module Duplicated = struct
  let types_name =
    List.filter_map
    @@ function
    | NOpaque { name }
    | NStruct { struct_name = name; _ }
    | NEnum { enum_name = name; _ } ->
        Some name
    | NExternFunc _ | NFunction _ | NSyscall _ | NConst _ ->
        None

  (**
    [kosu_type name kosu_module] lists all of the kosu type declarations (enum, struct, opaque) in [kosu_module] with the name [name]
  *)
  let kosu_type name =
    List.filter_map
    @@ fun kosu_node ->
    match kosu_node with
    | NOpaque opaque ->
        let r =
          match opaque.name.value = name with
          | true ->
              Option.some @@ `NOpaque opaque
          | false ->
              None
        in
        r
    | NStruct kosu_struct ->
        let r =
          match kosu_struct.struct_name.value = name with
          | true ->
              Option.some @@ `NStruct kosu_struct
          | false ->
              None
        in
        r
    | NEnum kosu_enum ->
        let r =
          match kosu_enum.enum_name.value = name with
          | true ->
              Option.some @@ `NEnum kosu_enum
          | false ->
              None
        in
        r
    | NExternFunc _ | NFunction _ | NSyscall _ | NConst _ ->
        None

  let kosu_type kosu_module =
    let names = types_name kosu_module in
    Util.Ulist.fold_ok
      (fun () name ->
        match kosu_type name kosu_module with
        | [] | _ :: [] ->
            Ok ()
        | list ->
            Result.error @@ KosuError.ConfictingTypeDeclaration list
      )
      ()
      (List.map Position.value names)
end

let validate_kosu_node kosu_program current_module =
  let module KTVLS = KosuUtil.KosuTypeVariableLocSet in
  function
  | NOpaque _ ->
      ok
  | NExternFunc _kosu_external_decl ->
      ok
  | NFunction kosu_function_decl ->
      let () =
        Printf.printf "Fuction %s\n%!" kosu_function_decl.fn_name.value
      in

      (* chech that argumenst name is unique *)
      let* _ =
        Util.Ulist.fold_ok
          (fun set (elt : KosuAst.kosu_function_parameters) ->
            match StringLoc.find_opt elt.name set with
            | None ->
                Result.ok @@ StringLoc.add elt.name set
            | Some _ ->
                failwith "Parameters already exist"
          )
          StringLoc.empty kosu_function_decl.parameters
      in

      (* Check that poly vars in type are bound in the fields *)
      let for_all_vars = KTVLS.of_list kosu_function_decl.poly_vars in
      let for_all_vars_in_types =
        List.fold_left KTVLS.union KTVLS.empty
        @@ List.map
             (fun elt ->
               KosuUtil.TyLoc.polymorphic_vars' KTVLS.empty KTVLS.empty
                 elt.kosu_type
             )
             kosu_function_decl.parameters
      in
      let diff = KTVLS.diff for_all_vars_in_types for_all_vars in

      let* () =
        match KTVLS.is_empty diff with
        | true ->
            Ok ()
        | false ->
            failwith "some variable arent bound"
      in

      (* Check function unitity in the module *)
      (* chach that each type exit *)

      (* Check typeching *)
      let kosu_env = KosuEnv.create current_module kosu_program in
      let kosu_env =
        List.fold_left
          (fun kosu_env variable_info ->
            KosuEnv.add_variable (not variable_info.is_var) variable_info.name
              (KosuUtil.Ty.of_tyloc' variable_info.kosu_type)
              kosu_env
          )
          kosu_env kosu_function_decl.parameters
      in
      let kosu_env =
        KosuEnv.add_bound_poly_vars
          (List.map KosuUtil.Ty.of_tyloc_polymorphic
             kosu_function_decl.poly_vars
          )
          kosu_env
      in
      let* env, ty =
        match KosuTypechecking.typeof kosu_env kosu_function_decl.body with
        | res ->
            Ok res
        | exception KosuError.KosuRawErr e ->
            Error e
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint
          ~lhs:(KosuUtil.Ty.of_tyloc' kosu_function_decl.return_type)
          ~rhs:ty kosu_function_decl.body kosu_env
      in
      let solutions = KosuEnv.solve kosu_env in
      let () =
        KosuEnv.KosuTypingSolution.iter
          (fun key value ->
            Printf.printf "-> %s = %s\n"
              (KosuPrint.string_of_polymorphic_var key)
              (KosuPrint.string_of_kosu_type value)
          )
          solutions
      in
      ok
  | NSyscall _ ->
      ok
  | NStruct _ ->
      ok
  | NEnum _ ->
      ok
  | NConst const_decl ->
      let kosu_env = KosuEnv.create current_module kosu_program in

      let* env, ty =
        match KosuTypechecking.typeof kosu_env const_decl.c_value with
        | res ->
            Ok res
        | exception KosuError.KosuRawErr e ->
            Error e
      in
      let kosu_env = KosuEnv.merge_constraint env kosu_env in
      let kosu_env =
        KosuEnv.add_typing_constraint
          ~lhs:(KosuUtil.Ty.of_tyloc' const_decl.explicit_type)
          ~rhs:ty const_decl.explicit_type kosu_env
      in
      let solutions = KosuEnv.solve kosu_env in
      let () =
        KosuEnv.KosuTypingSolution.iter
          (fun key value ->
            Printf.printf "-> %s = %s\n"
              (KosuPrint.string_of_polymorphic_var key)
              (KosuPrint.string_of_kosu_type value)
          )
          solutions
      in
      let* () =
        Result.map_error (fun n -> KosuError.ConstNonStaticExpression n)
        @@ KosuStaticExpression.is_static_expression const_decl.c_value
      in
      ok

let validate_kosu_module kosu_program kosu_modules =
  Util.Ulist.fold_ok
    (fun () kosu_module ->
      validate_kosu_node kosu_program kosu_modules kosu_module
    )
    () kosu_modules

let validate kosu_program =
  Util.Ulist.fold_ok
    (fun () { filename; kosu_module } ->
      match validate_kosu_module kosu_program kosu_module with
      | Ok _ ->
          ok
      | Error e ->
          Error (filename, e)
      | exception KosuError.KosuRawErr e ->
          Error (filename, e)
    )
    () kosu_program

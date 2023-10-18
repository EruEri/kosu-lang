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

module Duplicated = struct
  let types name = 
    List.filter_map @@ function
      | NOpaque {name = oname} as opaque when oname.Position.value = name -> Option.some @@ `opaque opaque
      | NOpaque _ -> None
      | NStruct {struct_name; _} as ostruct when struct_name.value = name -> Option.some @@ `strust ostruct
      | NStruct _ -> None
      | NEnum {enum_name; _} as oenum when enum_name.value = name -> Option.some @@ `enum oenum
      | NEnum _ -> None
      | NConst _
      | NExternFunc _ 
      | NFunction _
      | NSyscall _ -> None


end

let validate_kosu_node kosu_program current_module = function
  | NOpaque _ ->
      ok
  | NExternFunc _ ->
      ok
  | NFunction kosu_function_decl ->
      let () =
        Printf.printf "Fuction %s\n%!" kosu_function_decl.fn_name.value
      in
      (* Check that poly vars in type are bound in the fields *)
      (* Check function unitity in the module *)
      (* chach that each type exit *)
      (* chech that argumenst name is unique *)
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
  | NConst _ ->
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
    )
    () kosu_program

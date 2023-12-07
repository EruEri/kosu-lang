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
let err = Result.error
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
    | NExternFunc _ | NFunction _ | NConst _ ->
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
    | NExternFunc _ | NFunction _ | NConst _ ->
        None

  let kosu_callable name =
    List.filter_map
    @@ function
    | NExternFunc external_decl ->
        if external_decl.sig_name.value = name then
          Option.some @@ CdExternalFunction external_decl
        else
          None
    | NFunction kosu_function_decl ->
        if kosu_function_decl.fn_name.value = name then
          Option.some @@ CdKosuFuntion kosu_function_decl
        else
          None
    | NConst _ | NEnum _ | NOpaque _ | NStruct _ ->
        None

  (* let kosu_type kosu_module =
     let names = types_name kosu_module in
     Util.Ulist.fold_ok
       (fun () name ->
         match kosu_type name kosu_module with
         | [] | _ :: [] ->
             Ok ()
         | list ->
             Result.error @@ KosuError.Raw.conflicting_type_declaration list
       )
       ()
       (List.map Position.value names) *)
end

module Type = struct

  let rec is_cyclic_struct current_module kosu_program kosu_struct = 
    let module_res = KosuUtil.Program.module_resolver_of_module current_module kosu_program in
    let (raw_struct, ty) = KosuUtil.Struct.substitution_fresh ~fresh:KosuType.Ty.fresh_variable_type module_res kosu_struct in
    failwith ""
  and is_cyclic_enum current_module kosu_program kosu_enum = 
    failwith ""
  and is_cyclic current_module kosu_program type_decl =  
    match type_decl with
    | DStruct kosu_struct -> is_cyclic_struct current_module kosu_program kosu_struct
    | DEnum kosu_enum -> is_cyclic_enum current_module kosu_program kosu_enum
  and does_type_appears current_module kosu_program base target = 
    match target with
    | _ -> failwith ""

end

module Common = struct
  let check_type_existence current_module kosu_program kosu_type =
    match
      KosuUtil.Program.type_decl
        (KosuUtil.Ty.of_tyloc' kosu_type)
        ~current_module kosu_program
    with
    | None | Some (_ :: []) ->
        ok
    | Some [] ->
        err @@ KosuError.Raw.type_declaration_not_found kosu_type
    | Some list ->
        let list =
          List.map
            (function DEnum e -> `NEnum e | DStruct s -> `NStruct s)
            list
        in
        err @@ KosuError.Raw.conflicting_type_declaration list

  let check_boundness_variable_type
      (bound : KosuType.TyLoc.kosu_loctype_polymorphic list) kosu_type =
    let module KTVLS = KosuUtil.KosuTypeVariableLocSet in
    let for_all_vars = KTVLS.of_list bound in
    let for_all_vars_in_types =
      KosuUtil.TyLoc.polymorphic_vars' KTVLS.empty KTVLS.empty kosu_type
    in
    let diff = KTVLS.diff for_all_vars_in_types for_all_vars in
    match KTVLS.is_empty diff with
    | true ->
        Ok ()
    | false ->
        Result.error
        @@ KosuError.Raw.variable_type_not_bound (KTVLS.elements diff)

  let check_boundness_variable_types bound types =
    Util.Ulist.fold_ok (fun () -> check_boundness_variable_type bound) () types
end

module KosuFunction = struct
  let check_duplicated_parameters kosu_function_decl =
    Result.map (fun _ -> ())
    @@ Util.Ulist.fold_ok
         (fun set (elt : KosuAst.kosu_function_parameters) ->
           match StringLoc.find_opt elt.name set with
           | None ->
               Result.ok @@ StringLoc.add elt.name set
           | Some exist ->
               Result.error
               @@ KosuError.Raw.duplicated_param_name kosu_function_decl.fn_name
                    exist elt.name
         )
         StringLoc.empty kosu_function_decl.parameters

  let check_boundness_variable_type
      (kosu_function_decl : KosuAst.kosu_function_decl) =
    let types =
      List.map (fun { kosu_type; _ } -> kosu_type) kosu_function_decl.parameters
    in
    let types = kosu_function_decl.return_type :: types in
    Common.check_boundness_variable_types kosu_function_decl.poly_vars types

  let check_duplicated_callable_identifier current_module kosu_function_decl =
    match
      Duplicated.kosu_callable kosu_function_decl.fn_name.value current_module
    with
    | [] | _ :: [] ->
        ok
    | list ->
        err @@ KosuError.Raw.conflicting_callable_declaration list

  let check_type_existence current_module kosu_program
      (kosu_function_decl : KosuAst.kosu_function_decl) =
    let* () =
      Util.Ulist.fold_ok
        (fun () { kosu_type; is_var = _; name = _ } ->
          Common.check_type_existence current_module kosu_program kosu_type
        )
        () kosu_function_decl.parameters
    in
    Common.check_type_existence current_module kosu_program
      kosu_function_decl.return_type

  let typecheck current_module kosu_program
      (kosu_function_decl : KosuAst.kosu_function_decl) =
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
        (List.map KosuUtil.Ty.of_tyloc_polymorphic kosu_function_decl.poly_vars)
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
        ~cexpected:(KosuUtil.Ty.of_tyloc' kosu_function_decl.return_type)
        ~cfound:ty kosu_function_decl.body kosu_env
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
    (* TODO: Check that a for all variable is not equal to an concrete type*)
    (* ie: 'a = s32 shound be an error *)
    ok
end

module ExternFunction = struct
  let check_boundness_variable_type external_function =
    let types = external_function.return_type :: external_function.parameters in
    Common.check_boundness_variable_types [] types

  let check_type_existence current_module kosu_program
      (kosu_external_decl : KosuAst.kosu_external_func_decl) =
    let* () =
      Util.Ulist.fold_ok
        (fun () -> Common.check_type_existence current_module kosu_program)
        () kosu_external_decl.parameters
    in
    Common.check_type_existence current_module kosu_program
      kosu_external_decl.return_type
end

module Struct = struct
  let check_type_existence current_module kosu_program
      (kosu_struct_decl : KosuAst.kosu_struct_decl) =
    Util.Ulist.fold_ok
      (fun () (_, kosu_type) ->
        Common.check_type_existence current_module kosu_program kosu_type
      )
      () kosu_struct_decl.fields

  let check_type_duplicated current_module
      (kosu_struct_decl : KosuAst.kosu_struct_decl) =
    match
      Duplicated.kosu_type kosu_struct_decl.struct_name.value current_module
    with
    | [] | _ :: [] ->
        ok
    | _ :: _ :: _ as list ->
        err @@ KosuError.Raw.conflicting_type_declaration list

  let check_duplicated_fields (kosu_struct_decl : KosuAst.kosu_struct_decl) =
    Result.map (fun _ -> ())
    @@ Util.Ulist.fold_ok
         (fun set (name, _) ->
           match StringLoc.find_opt name set with
           | None ->
               Result.ok @@ StringLoc.add name set
           | Some exist ->
               err
               @@ KosuError.Raw.duplicated_fiels kosu_struct_decl.struct_name
                    exist name
         )
         StringLoc.empty kosu_struct_decl.fields
end

let validate_kosu_node kosu_program current_module = function
  | NOpaque _ ->
      ok
  | NExternFunc kosu_external_decl ->
      let* () =
        ExternFunction.check_type_existence current_module kosu_program
          kosu_external_decl
      in
      let* () =
        ExternFunction.check_boundness_variable_type kosu_external_decl
      in
      ok
  | NFunction kosu_function_decl ->
      let () =
        Printf.printf "Function %s\n%!" kosu_function_decl.fn_name.value
      in
      (* chech that arguments name is unique *)
      let* () = KosuFunction.check_duplicated_parameters kosu_function_decl in

      (* Check that poly vars in type are bound in the fields *)
      let* () = KosuFunction.check_boundness_variable_type kosu_function_decl in

      (* Check function unitity in the module *)
      let* () =
        KosuFunction.check_duplicated_callable_identifier current_module
          kosu_function_decl
      in
      (* chach that each type exit *)
      let* () =
        KosuFunction.check_type_existence current_module kosu_program
          kosu_function_decl
      in
      (* Check typeching *)
      let* () =
        KosuFunction.typecheck current_module kosu_program kosu_function_decl
      in
      ok
  | NStruct kosu_struct ->
      let* () = Struct.check_type_duplicated current_module kosu_struct in

      let* () =
        Struct.check_type_existence current_module kosu_program kosu_struct
      in

      let* () = Struct.check_duplicated_fields kosu_struct in
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
          ~cexpected:(KosuUtil.Ty.of_tyloc' const_decl.explicit_type)
          ~cfound:ty const_decl.explicit_type kosu_env
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
        Result.map_error KosuError.Raw.const_non_static_expression
        @@ KosuExpression.is_static_expression const_decl.c_value
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

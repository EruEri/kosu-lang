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
end

module Type = struct
  let make_fresh () ?hint () =
    let counter = ref 0 in
    let n = !counter in
    let () = incr counter in
    let name = Printf.sprintf "'t%u" n in
    KosuType.Ty.TyPolymorphic (CompilerPolymorphicVar { name; hint })

  let rec is_cyclic_struct current_module kosu_program kosu_struct =
    let fresh = make_fresh () in
    let module_res =
      KosuUtil.Program.module_resolver_of_module current_module kosu_program
    in
    is_cyclic_raw_struct ~visited:[] current_module kosu_program
    @@ KosuUtil.Struct.substitution_fresh ~fresh module_res kosu_struct

  and is_cyclic_raw_struct ~visited current_module kosu_program
      (raw_struct, struct_type) =
    List.exists
      (fun (_field, kosu_type) ->
        let visited = struct_type :: visited in
        does_type_appears ~visited current_module kosu_program struct_type
          kosu_type
      )
      raw_struct.fields

  and is_cyclic_enum current_module kosu_program kosu_enum =
    let fresh = make_fresh () in
    let module_res =
      KosuUtil.Program.module_resolver_of_module current_module kosu_program
    in
    is_cyclic_raw_enum ~visited:[] current_module kosu_program
    @@ KosuUtil.Enum.substitution_fresh ~fresh module_res kosu_enum

  and is_cyclic_raw_enum ~visited current_module kosu_program
      (raw_enum, struct_type) =
    List.exists
      (fun (_, assoc_type) ->
        List.exists
          (fun kt ->
            let visited = struct_type :: visited in
            does_type_appears ~visited current_module kosu_program struct_type
              kt
          )
          assoc_type
      )
      raw_enum.variants

  and does_type_appears ~visited current_module kosu_program base target =
    List.mem target visited
    ||
    match target with
    | TyIdentifier { module_resolver; parametrics_type; name } ->
        let eq_type =
          match base with
          | TyIdentifier
              { module_resolver = bmr; parametrics_type = _bpt; name = bname }
            ->
              module_resolver = bmr && bname = name
          | _ ->
              false
        in
        let type_decl =
          KosuUtil.Program.type_decl ~current_module target kosu_program
        in
        let type_decl =
          match type_decl with
          | None ->
              failwith "To type decl"
          | Some [] ->
              failwith "No type decl"
          | Some (t :: []) ->
              t
          | Some (_ :: _) ->
              failwith "Multiple type declaration"
        in
        let ic () =
          match type_decl with
          | DStruct struct_decl ->
              let raw_decl_x_type =
                Option.get
                @@ KosuUtil.Struct.substitution module_resolver parametrics_type
                     struct_decl
              in
              is_cyclic_raw_struct ~visited current_module kosu_program
                raw_decl_x_type
          | DEnum enum_decl ->
              let raw_decl_x_type =
                Option.get
                @@ KosuUtil.Enum.substitution module_resolver parametrics_type
                     enum_decl
              in
              is_cyclic_raw_enum ~visited current_module kosu_program
                raw_decl_x_type
        in
        eq_type || ic ()
    | TyTuple types ->
        List.exists
          (does_type_appears ~visited current_module kosu_program base)
          types
    | TyArray { ktype; size = _ } ->
        does_type_appears ~visited current_module kosu_program base ktype
    | TyPolymorphic _
    | TyPointer _
    | TyInteger _
    | TyFloat _
    | TyFunctionPtr _
    | TyClosure _
    | TyOpaque _
    | TyOrdered
    | TyStringLit
    | TyChar
    | TyBool
    | TyUnit ->
        false
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
    let* (env, ty), _ =
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

  let check_boundness_variable_type (struct_decl : KosuAst.kosu_struct_decl) =
    let types = List.map snd struct_decl.fields in
    Common.check_boundness_variable_types struct_decl.poly_vars types
end

module Enum = struct
  let check_type_existence current_module kosu_program
      (kosu_enum_decl : KosuAst.kosu_enum_decl) =
    Util.Ulist.fold_ok
      (fun () (_, assoc_types) ->
        Util.Ulist.fold_ok
          (fun () -> Common.check_type_existence current_module kosu_program)
          () assoc_types
      )
      () kosu_enum_decl.variants

  let check_type_duplicated current_module
      (kosu_enum_decl : KosuAst.kosu_enum_decl) =
    match
      Duplicated.kosu_type kosu_enum_decl.enum_name.value current_module
    with
    | [] | _ :: [] ->
        ok
    | _ :: _ :: _ as list ->
        err @@ KosuError.Raw.conflicting_type_declaration list

  let check_duplicated_variant (kosu_enum_decl : KosuAst.kosu_enum_decl) =
    Result.map (fun _ -> ())
    @@ Util.Ulist.fold_ok
         (fun set (name, _) ->
           match StringLoc.find_opt name set with
           | None ->
               Result.ok @@ StringLoc.add name set
           | Some exist ->
               err
               @@ KosuError.Raw.duplicated_fiels kosu_enum_decl.enum_name exist
                    name
         )
         StringLoc.empty kosu_enum_decl.variants

  let check_boundness_variable_type (enum_decl : KosuAst.kosu_enum_decl) =
    let types = List.concat_map snd enum_decl.variants in
    Common.check_boundness_variable_types enum_decl.poly_vars types
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

      let* () = Struct.check_boundness_variable_type kosu_struct in

      let* () =
        Struct.check_type_existence current_module kosu_program kosu_struct
      in

      let* () = Struct.check_duplicated_fields kosu_struct in

      let* () =
        match Type.is_cyclic_struct current_module kosu_program kosu_struct with
        | false ->
            Ok ()
        | true ->
            Result.error @@ KosuError.Raw.cyclic_type_declaration
            @@ DStruct kosu_struct
      in
      ok
  | NEnum enum ->
      let* () = Enum.check_type_existence current_module kosu_program enum in

      let* () = Enum.check_boundness_variable_type enum in

      let* () = Enum.check_type_duplicated current_module enum in

      let* () = Enum.check_duplicated_variant enum in

      let* () =
        match Type.is_cyclic_enum current_module kosu_program enum with
        | false ->
            Ok ()
        | true ->
            Result.error @@ KosuError.Raw.cyclic_type_declaration @@ DEnum enum
      in
      ok
  | NConst const_decl ->
      let kosu_env = KosuEnv.create current_module kosu_program in

      let* (env, ty), _ =
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
      let _solutions = KosuEnv.solve kosu_env in
      let* () =
        Result.map_error KosuError.Raw.const_non_static_expression
        @@ KosuUtil.Expression.is_static_expression const_decl.c_value
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

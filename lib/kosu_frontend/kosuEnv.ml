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

module KosuTypeConstraint = KosuTypeConstraint
module KosuTypeConstraintSet = Set.Make (KosuTypeConstraint)

module KosuTypeVariableSet = Set.Make (struct
  type t = KosuType.Ty.kosu_type_polymorphic

  let compare = Stdlib.compare
end)

module KosuVariableInfo = struct
  type kosu_variable_info = {
    is_const : bool;
    identifier : string Position.location;
    kosu_type : KosuType.Ty.kosu_type;
  }

  type t = kosu_variable_info

  (*
     This compare is only use in [merge_variable_disjoint]
     which difference variable only by there identifier
  *)
  let compare (lhs : t) (rhs : t) =
    String.compare lhs.identifier.value rhs.identifier.value
end

module KosuVariableInfoSet = Set.Make (KosuVariableInfo)

type kyo_tying_env = KosuVariableInfo.t list

type kosu_env = {
  program : KosuAst.kosu_program;
  opened_modules : KosuAst.kosu_module list;
  env_variable : kyo_tying_env;
  env_tying_constraint : KosuTypeConstraintSet.t;
  env_bound_ty_vars : KosuTypeVariableSet.t;
}

let create current_module program =
  {
    program;
    opened_modules = current_module :: [];
    env_variable = [];
    env_tying_constraint = KosuTypeConstraintSet.empty;
    env_bound_ty_vars = KosuTypeVariableSet.empty;
  }

let add_bound_poly_vars poly_vars kosu_env =
  {
    kosu_env with
    env_bound_ty_vars =
      KosuTypeVariableSet.add_seq (List.to_seq poly_vars)
        kosu_env.env_bound_ty_vars;
  }

let current_module kosu_env =
  let rec last = function [] -> None | t :: [] -> Some t | _ :: q -> last q in
  match last kosu_env.opened_modules with
  | Some kosu_module ->
      kosu_module
  | None ->
      failwith
        "Should not append since: the first env of kosu module is the current \
         module"

let merge_constraint rhs lhs =
  {
    lhs with
    env_tying_constraint =
      KosuTypeConstraintSet.union lhs.env_tying_constraint
        rhs.env_tying_constraint;
  }

(**
  [merge_variable_disjoint env1 env2] adds the variable present in 
  [env1] which aren't in [env2]. 

  In case of variable having the same name in [env1] and [env2], the identifier kept
  is within [env2]
*)
let merge_variable_disjoint env1 env2 =
  let env1set = KosuVariableInfoSet.of_list env1.env_variable in
  let env2set = KosuVariableInfoSet.of_list env2.env_variable in
  let diff = KosuVariableInfoSet.diff env1set env2set in
  let set = KosuVariableInfoSet.union env2set diff in
  { env2 with env_variable = KosuVariableInfoSet.elements set }

let add_typing_constraint ~lhs ~rhs (location : 'a Position.location) env =
  let constr =
    KosuType.Ty.{ clhs = lhs; crhs = rhs; position = location.position }
  in
  let skt = KosuPrint.string_of_kosu_type in
  let () = Printf.fprintf stdout "eq : %s == %s \n%!" (skt lhs) (skt rhs) in
  {
    env with
    env_tying_constraint =
      KosuTypeConstraintSet.add constr env.env_tying_constraint;
  }

(** 
  [assoc_type_opt name kosu_env] returns the type associated with the identifier [name] in the variable environment [kosu_env].
  Returns [None] if [name] doesn't exist in [kosu_env]
*)
let assoc_type_opt name kosu_env =
  let open KosuVariableInfo in
  List.find_opt
    (fun { identifier; _ } -> identifier.value = name)
    kosu_env.env_variable

(** [mem name kosu_env] checks if the identifier [name] exists in the variable environment [kosu_env]*)
let mem name kosu_env = Option.is_some @@ assoc_type_opt name kosu_env

(**
  [add_module kosu_module kosu_env] adds the module [kosu_module] to the list of opened modules in [kosu_env]
*)
let add_module kosu_module kosu_env =
  { kosu_env with opened_modules = kosu_module :: kosu_env.opened_modules }

(**
  [add_variable ?check const identifier kosu_type kosu_env] extends the variable environment [kosu_env] by the binding of [identifier] with the type [kosu_type]

  if [check], the function will raise [KosuError.IdentifierAlreadyBound] if [identifier] already exist in [kosu_env]

  @raise KosuRawErr(IdentifierAlreadyBound)
*)
let add_variable ?(check = true) is_const identifier kosu_type kosu_env =
  let exist = mem identifier.Position.value kosu_env in
  let () =
    match check with
    | true when exist ->
        raise @@ KosuError.identifier_already_bound identifier
    | true | false ->
        ()
  in
  {
    kosu_env with
    env_variable = { is_const; identifier; kosu_type } :: kosu_env.env_variable;
  }

let modules kosu_env = kosu_env.opened_modules

let resolve_module kosu_module kosu_env =
  (* Shouldn't be [None] since the module mostly come from [kosu_env.opened_modules or kosu_env.program] *)
  Option.get
  @@ List.find_map
       (fun named_module ->
         let open KosuAst in
         (* Can use physic equal since module aren't recreated*)
         match named_module.kosu_module == kosu_module with
         | true ->
             Option.some
             @@ KosuUtil.ModuleResolver.of_filename named_module.filename
         | false ->
             None
       )
       kosu_env.program

let find_module_opt module_resolver kosu_env =
  KosuUtil.Program.find_module_opt module_resolver kosu_env.program

(** 
  [free_ty_variable bound acc ty kosu_env] returns all the type variable within [ty] as [acc] which 
  aren't bound to an existing type variable in [kosu_env] and in [bound]
*)
let rec free_ty_variable bound acc ty kosu_env =
  let open KosuType.Ty in
  match ty with
  | TyPolymorphic variable ->
      let condition =
        KosuTypeVariableSet.mem variable kosu_env.env_bound_ty_vars
        && (not @@ KosuTypeVariableSet.mem variable bound)
      in
      let acc =
        match condition with
        | true ->
            acc
        | false ->
            KosuTypeVariableSet.add variable acc
      in
      acc
  | TyFunctionPtr schema
  | TyClosure schema
  (* Should captured variabe be in the compuation ? *)
  | TyInnerClosureId (ClosureType { id = _; env = _; schema }) ->
      let bound =
        KosuTypeVariableSet.add_seq (List.to_seq schema.poly_vars) bound
      in
      let acc =
        List.fold_left
          (fun acc ty -> free_ty_variable bound acc ty kosu_env)
          acc schema.parameters_type
      in
      let acc = free_ty_variable bound acc schema.return_type kosu_env in
      acc
  | TyIdentifier { parametrics_type = tys; module_resolver = _; name = _ }
  | TyTuple tys ->
      let acc =
        List.fold_left
          (fun acc ty -> free_ty_variable bound acc ty kosu_env)
          acc tys
      in
      acc
  | TyPointer { pointee_type = ktype; pointer_state = _ }
  | TyArray { ktype; size = _ } ->
      free_ty_variable bound acc ktype kosu_env
  | TyInteger _
  | TyOpaque _
  | TyFloat _
  | TyOrdered
  | TyChar
  | TyStringLit
  | TyUnit
  | TyBool ->
      acc

(** 
  [free_ty_variable ty kosu_env] returns all the type variable within [ty] which 
  aren't bound to an existing type variable in [kosu_env]
*)
let free_ty_variable =
  free_ty_variable KosuTypeVariableSet.empty KosuTypeVariableSet.empty

(**
    [find_declaration ~fmodule ~ffilter module_resolver kosu_env] try to find a
    declaration kind specified by [fmodule] and filter the elements by applying [ffilter] 
    to the return of [fmodule]
    If [module_resolver] is empty, the function will try to find the declaration in the opened modules of [kosu_env]
    If the module doesn't exist or no identifier matching is found, return [None].
*)
let find_declaration ~fmodule ~ffilter module_resolver kosu_env =
  let open KosuAst in
  let decl_opt kmodule =
    let const_decls = fmodule kmodule in
    const_decls |> List.find_opt ffilter
    |> Option.map (fun c ->
           let resolve_module = resolve_module kmodule kosu_env in
           (resolve_module, c)
       )
  in

  let ( let* ) = Option.bind in
  match module_resolver with
  | ModuleResolverLoc [] ->
      List.find_map decl_opt kosu_env.opened_modules
  | ModuleResolverLoc (_ :: _) ->
      let* kmodule = find_module_opt module_resolver kosu_env in
      decl_opt kmodule

(** 
  [find_const_declaration (module_resolver, identifier) kosu_env] try to find the constant with
  the name [identifier] in the module provided by [module_resolver].
  If [module_resolver] is empty, the function will try to find the declaration in the opened modules of [kosu_env]
  If the module doesn't exist or no identifier matching is found, return [None].
*)
let find_const_declaration (module_resolver, identifier) kosu_env =
  let open Position in
  find_declaration ~fmodule:KosuUtil.Module.constant_decls
    ~ffilter:(fun const_decl -> const_decl.const_name.value = identifier.value)
    module_resolver kosu_env

let find_struct_declaration (module_resolver, identifier) kosu_env =
  let open Position in
  find_declaration ~fmodule:KosuUtil.Module.struct_decls
    ~ffilter:(fun struct_decl ->
      struct_decl.struct_name.value = identifier.value
    )
    module_resolver kosu_env

(**
    [find_enum_declaration module_resolver enum_name variant kosu_env] find the enum declaration with
    a variant named [named] with the optionnal enum precision [enum_name] in the module provided by [module_resolver].

    If [module_resolver] is empty, the function will try to find the declaration in the opened modules of [kosu_env]
    If the module doesn't exist or no identifier matching is found, return [None].

*)
let find_enum_declaration module_resolver enum_name variant kosu_env =
  let open Position in
  let open KosuAst in
  (* If there is an explicit enum name, we add a filter layer *)
  let ffilter =
    enum_name
    |> Option.map (fun name enum_decl -> enum_decl.enum_name.value = name)
    |> Option.value ~default:(fun _ -> true)
  in
  find_declaration
    ~fmodule:(KosuUtil.Module.enum_decls_from_variant_name variant.value)
    ~ffilter module_resolver kosu_env

(**
    [find_struct_declaration_type ty kosu_env] try to find a struct declaration for type [ty] in [kosu_env]
*)
let find_struct_declaration_type (ty : KosuType.Ty.kosu_type) kosu_env =
  match ty with
  | TyIdentifier { module_resolver; name; parametrics_type = _ } ->
      find_struct_declaration
        ( KosuUtil.ModuleResolver.dummy_located module_resolver,
          Position.dummy_located name
        )
        kosu_env
  | TyPolymorphic _
  | TyPointer _
  | TyInteger _
  | TyFloat _
  | TyFunctionPtr _
  | TyClosure _
  | TyInnerClosureId _
  | TyArray _
  | TyTuple _
  | TyOpaque _
  | TyOrdered
  | TyStringLit
  | TyChar
  | TyBool
  | TyUnit ->
      None

let find_callable_declaration module_resolver identifier kosu_env =
  find_declaration
    ~fmodule:(KosuUtil.Module.callable_decls_name identifier)
    ~ffilter:(fun _ -> true)
    module_resolver kosu_env

(**
  [equations ty_var kosu_env] filters the equations collected in [kosu_env] by
  equation where [ty_vars] appears
*)
let equations ty_var kosu_env =
  KosuTypeConstraintSet.filter
    (fun { clhs; crhs; position = _ } ->
      let ty_var = KosuType.Ty.TyPolymorphic ty_var in
      clhs = ty_var || crhs = ty_var
    )
    kosu_env.env_tying_constraint

(**
  [try_solve ty_var kosu_env] try to solve the equations collected in [kosu_env] where
  [ty_var] appears.

  Returns [None] with [ty_var] doesn't appears
  @raise something (tbd) where the equations don't have a solution
*)
let try_solve ty_var kosu_env =
  let eqs = KosuTypeConstraintSet.elements @@ equations ty_var kosu_env in
  match eqs with
  | [] ->
      None
  | constra :: constraints ->
      let ty_ty_vars = KosuType.Ty.TyPolymorphic ty_var in
      let first = Option.get @@ KosuTypeConstraint.other ty_ty_vars constra in
      let elts_types =
        List.map
          (fun t -> Option.get @@ KosuTypeConstraint.other ty_ty_vars t)
          constraints
      in
      let ty =
        Util.Ulist.fold_some
          (fun ty_acc elt_ty ->
            KosuTypeConstraint.restrict ~with_ty:elt_ty ty_acc
          )
          first elts_types
      in
      ty

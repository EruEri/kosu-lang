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

module KosuTypeConstraint = struct
  type t = KosuType.Ty.kosu_type_constraint

  let compare (lhs : t) (rhs : t) =
    match compare lhs.clhs rhs.clhs with
    | 0 ->
        compare lhs.clhs rhs.crhs
    | n ->
        n
end

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
  {
    env with
    env_tying_constraint =
      KosuTypeConstraintSet.add constr env.env_tying_constraint;
  }

(**
  [add_module kosu_module kosu_env] adds the module [kosu_module] to the list of opened modules in [kosu_env]
*)
let add_module kosu_module kosu_env =
  { kosu_env with opened_modules = kosu_module :: kosu_env.opened_modules }

(**
  [add_variable const identifier kosu_type kosu_env] extends the variable environment [kosu_env] by the binding of [identifier] with the type [kosu_type]
*)
let add_variable is_const identifier kosu_type kosu_env =
  {
    kosu_env with
    env_variable = { is_const; identifier; kosu_type } :: kosu_env.env_variable;
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

let modules kosu_env = kosu_env.opened_modules

let find_module_opt (KosuBaseAst.ModuleResolverLoc modules) kosu_env =
  let open KosuAst in
  let module_name = Position.filename_of_module modules in
  kosu_env.program
  |> List.find_map (fun { filename; kosu_module } ->
         match filename = module_name with
         | true ->
             Some kosu_module
         | false ->
             None
     )

let rec free_ty_variable acc ty kosu_env =
  let open KosuType.Ty in
  match ty with
  | TyPolymorphic variable ->
      let acc =
        match KosuTypeVariableSet.mem variable kosu_env.env_bound_ty_vars with
        | true ->
            acc
        | false ->
            KosuTypeVariableSet.add variable acc
      in
      acc
  | TyFunctionPtr (parameters, return_type)
  | TyClosure (parameters, return_type)
  (* Should captured variabe be in the compuation ? *)
  | TyInnerClosureId (ClosureType { id = _; env = _; parameters; return_type })
    ->
      let acc =
        List.fold_left
          (fun acc ty -> free_ty_variable acc ty kosu_env)
          acc parameters
      in
      let acc = free_ty_variable acc return_type kosu_env in
      acc
  | TyIdentifier { parametrics_type = tys; module_resolver = _; name = _ }
  | TyTuple tys ->
      let acc =
        List.fold_left (fun acc ty -> free_ty_variable acc ty kosu_env) acc tys
      in
      acc
  | TyPointer { pointee_type = ktype; pointer_state = _ }
  | TyArray { ktype; size = _ } ->
      free_ty_variable acc ktype kosu_env
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
  aren't bound an existing type variable in [kosu_env]
*)
let free_ty_variable = free_ty_variable KosuTypeVariableSet.empty

(** 
  [find_const_declaration (module_resolver, identifier) kosu_env] try to find the constant with
  the name [identifier] in the module provided by [module_resolver].
  If [module_resolver] is empty, the function will try to find the declaration in the opened modules of [kosu_env]
  If the module doesn't exist or no identifier matching is found, return [None].
*)
let find_const_declaration (module_resolver, identifier) kosu_env =
  let open KosuAst in
  let open Position in
  let enum_decl_opt kmodule =
    let const_decls = KosuUtil.Module.constant_decls kmodule in
    const_decls
    |> List.find_opt (fun const_decl ->
           const_decl.const_name.value = identifier.value
       )
  in

  let ( let* ) = Option.bind in
  match module_resolver with
  | ModuleResolverLoc [] ->
      List.find_map enum_decl_opt kosu_env.opened_modules
  | ModuleResolverLoc (_ :: _) ->
      let* kmodule = find_module_opt module_resolver kosu_env in
      enum_decl_opt kmodule

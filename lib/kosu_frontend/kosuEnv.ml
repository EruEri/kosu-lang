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

module KosuTypeConstraintSet = Set.Make (struct
  type t = KosuType.Ty.kosu_type_constraint

  let compare = Stdlib.compare
end)

type kyo_tying_env = (string Position.location * KosuType.Ty.kosu_type) list

type kosu_env = {
  program : KosuAst.kosu_program;
  opened_modules : KosuAst.kosu_module;
  env_variable : kyo_tying_env;
  env_tying_constraint : KosuTypeConstraintSet.t;
}

let merge_constraint lhs rhs =
  {
    lhs with
    env_tying_constraint =
      KosuTypeConstraintSet.union lhs.env_tying_constraint
        rhs.env_tying_constraint;
  }

let add_typing_constraint ~lhs ~rhs env =
  let constr = KosuType.Ty.{ clhs = lhs; crhs = rhs } in
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
  [add_variable variable kyo_type kosu_env] extends the variable environment [kosu_env] by the binding of [variable] with the type [kyo_type]
*)
let add_variable variable kyo_type kosu_env =
  { kosu_env with env_variable = (variable, kyo_type) :: kosu_env.env_variable }

(** 
  [assoc_type_opt name kosu_env] returns the type associated with the identifier [name] in the variable environment [kosu_env].
  Returns [None] if [name] doesn't exist in [kosu_env]
*)
let assoc_type_opt name kosu_env = List.assoc_opt name kosu_env.env_variable

(** [mem name kosu_env] checks if the identifier [name] exists in the variable environment [kosu_env]*)
let mem name kosu_env = Option.is_some @@ assoc_type_opt name kosu_env

let modules kosu_env = kosu_env.opened_modules

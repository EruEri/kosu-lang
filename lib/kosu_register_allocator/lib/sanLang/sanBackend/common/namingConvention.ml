(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
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



let asm_module_path = String.map (fun c -> if c = ':' then '_' else c)

module type NamingSig = sig
  val label_prefix : string
  val main : string
end

module Make (N : NamingSig) = struct
  include N

  let label_of_function ~fn_name =
    if fn_name = "main" then main
    else
      Printf.sprintf "%s%s" 
        label_prefix
        fn_name
end
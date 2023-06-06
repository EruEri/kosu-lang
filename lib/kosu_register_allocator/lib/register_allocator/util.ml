(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of kosu-register-allocator                                               *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
(*                                                                                            *)
(* kosu-register-allocator is free software: you can redistribute it and/or modify            *)
(*  it under the terms either:                                                                *)
(*                                                                                            *)
(*    - the GNU General Public License as published by the Free Software Foundation,          *)
(*        either version 3 of the License, or (at your option) any later version              *)
(*   or                                                                                       *)
(*                                                                                            *)
(*     - the GNU Lesser General Public License as published by the Free Software Foundation,  *)
(*        either version 3 of the License, or (at your option) any later version              *)
(*                                                                                            *)
(* kosu-register-allocator is distributed in the hope that it will be useful,                 *)
(*   but WITHOUT ANY WARRANTY;                                                                *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along                    *)
(* with kosu-register-allocator. If not, see <http://www.gnu.org/licenses/>.                  *)
(*                                                                                            *)
(**********************************************************************************************)

module Date = struct
  type 'a dated = { value : 'a; start : int option; ending : int option }

  let has_started { start; _ } = Option.is_some start
  let has_finished { ending; _ } = Option.is_some ending
  let value { value; _ } = value
  let set_start debut dated = { dated with start = Some debut }
  let set_end ending dated = { dated with ending = Some ending }

  let add_date date dated =
    match dated.start with
    | None -> { dated with start = Some date }
    | Some _ -> set_end date dated
end

let couple i v = i, v 

let combinaison f l1 l2 =
  l1 |> List.map (fun elt -> l2 |> List.filter_map (fun elt2 -> f elt elt2))

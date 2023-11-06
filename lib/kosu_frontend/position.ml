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

include Util.Position

let to_asai_range ?file position =
  (* let () = Printf.eprintf "%s\n%!" position.start_position.pos_fname in *)
  let source = Option.map (fun s -> `File s) file in
  Asai.Range.of_lex_range ?source
    (position.start_position, position.end_position)

let to_asai_range_loc ?file e = to_asai_range ?file e.position

let to_loctext ?file value =
  let loc = to_asai_range_loc ?file value in
  let text = Asai.Diagnostic.loctext ~loc value.value in
  text

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

module S : KosuDiagnostic.S with type t = KosuError.kosu_error = struct
  type t = KosuError.kosu_error

  let loc = KosuError.Function.to_position

  let prefix filename kosu_error =
    let location = loc kosu_error in
    let sfile = KosuPrint.string_of_file_error filename in

    match location with
    | position :: [] ->
        let sloc = KosuPrint.string_of_located_error { value = (); position } in
        sfile @@ sloc ":"
    | [] | _ :: _ :: _ ->
        sfile ":"

  let line = Printf.sprintf "%u|  "
  let error e = Option.some @@ KosuPrint.Formatted.string_of_kosu_error e
  let hint _kosu_error = None
  let warning _kosu_error = None
  let severity _ = Some KosuDiagnostic.Severity.Error
end

module Reporter = KosuDiagnostic.Make (S)

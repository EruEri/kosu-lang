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

  let loc _kosu_error = failwith ""
  let prefix _filename _kosu_error = failwith ""
  let line = Printf.sprintf "%u|  "
  let error _kosu_error = failwith ""
  let hint _kosu_error = failwith ""
  let warning _kosu_error = failwith ""
  let severity _ = Some KosuDiagnostic.Severity.Error
end

module Reporter = KosuDiagnostic.Make (S)

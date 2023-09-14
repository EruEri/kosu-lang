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

module Ast = KosuAst
module Env = KosuEnv
module TypeChecking = KosuTypechecking
module Type = KosuType
module Parsing = KosuParsing
module Error = KosuError
module Print = KosuPrint
module Validation = KosuValidation

let register_exn () =
  Printexc.register_printer (function
    | Error.KosuErr kosu ->
        Option.some @@ Print.string_of_kosu_error "" kosu
    | _ ->
        None
    )

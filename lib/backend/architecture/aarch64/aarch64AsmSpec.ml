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

type variadic_args_stye = AbiDarwin | AbiSysV
type address_load_style = MacOS | Other

module type Aarch64AsmSpecification = sig
  
  val main : string
  val function_directives : string -> string list
  val adrp_style : address_load_style

  val variadic_style : variadic_args_stye

  val constant_directives :
    string ->
    [ `IntVal of KosuFrontend.Ast.isize * int64 | `StrVal of string ] ->
    string list

  val comment_prefix : string
  val string_litteral_section_start : string
  val string_litteral_section_end : string
  val string_litteral_directive : string
  val size_directive_of_size : KosuFrontend.Ast.isize -> string
  val directive_of_fsize : KosuFrontend.Ast.fsize -> string
  val label_prefix : string
  val label_of_constant : ?module_path:string -> string -> string

  val label_of_external_function :
    KosuIrTyped.Asttyped.rexternal_func_decl -> string

  val label_of_kosu_function :
    module_path:string -> KosuIrTyped.Asttyped.rfunction_decl -> string
  (** This function should produce the same label name taht [label_of_tac_function] if the kosu function is the conterpart as the tac one  *)

  val label_of_tac_function :
    module_path:string -> KosuIrTAC.Asttac.tac_function_decl -> string
  (** This function should produce the same label name taht [label_of_kosu_function] if the tac function is the conterpart as the kosu one  *)

  val label_of_kosu_operator :
    module_path:string -> KosuIrTyped.Asttyped.roperator_decl -> string

  val label_of_tac_operator :
    module_path:string -> KosuIrTAC.Asttac.tac_operator_decl -> string
end

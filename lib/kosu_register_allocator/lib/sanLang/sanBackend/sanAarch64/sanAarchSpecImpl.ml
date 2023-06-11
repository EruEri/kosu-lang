(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

module MacOSAarch64AsmSpec : SanAarchSpecification.Aarch64AsmSpecification =
struct
  open Printf

  module MacosNamingSig = struct
    let main = "_main"
    let label_prefix = "_"
  end

  module MacosNamingConvention = SanCommon.NamingConvention.Make (MacosNamingSig)
  include MacosNamingConvention

  type address_load_style = MacOS | Other

  let adrp_style : address_load_style = MacOS

  let function_directives fn_name =
    [ sprintf ".globl %s" fn_name; ".p2align 4" ]

  let constant_directives const_name = function
    | `IntVal _ ->
        let _align_size = (Nativeint.size / 8) - 1 in
        [ Printf.sprintf ".globl %s" const_name; sprintf ".p2align %u" 2 ]
    | `StrVal _ ->
        [
          Printf.sprintf ".globl %s" const_name; Printf.sprintf ".p2align %u" 3;
        ]

  let comment_prefix = ";"

  let string_litteral_section_start =
    ".section\t__TEXT,__cstring,cstring_literals,"

  let string_litteral_section_end = ".subsections_via_symbols"
  let string_litteral_directive = ".asciz"

  let label_of_function fn_name =
    MacosNamingConvention.label_of_function ~fn_name
end

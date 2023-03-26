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


module MacOSAarch64AsmSpec : Aarch64AsmSpec.Aarch64AsmSpecification = struct
  open Printf

  module MacosNamingSig = struct
    let main = "_main"
    let label_prefix = "_"
  end

  module MacosNamingConvention = Common.NamingConvention.Make(MacosNamingSig)

  include MacosNamingConvention

  type address_load_style = 
  | MacOS
  | Other

  let adrp_style : address_load_style = MacOS

  let function_directives is_global fn_name =
    if is_global then
    [ sprintf ".globl %s" fn_name; ".p2align 4" ]
    else
      [ ".p2align 4"]

  let constant_directives const_name = function
    | `IntVal (size, _) ->
        let _align_size = (KosuFrontend.Ast.Isize.size_of_isize size / 8) - 1 in
        [ Printf.sprintf ".globl %s" const_name; sprintf ".p2align %u" 2 ]
    | `StrVal _ ->
        [
          Printf.sprintf ".globl %s" const_name; Printf.sprintf ".p2align %u" 3;
        ]

  let comment_prefix = "#"
  

  let string_litteral_section_start =
    ".section\t__TEXT,__cstring,cstring_literals,"

  let string_litteral_section_end = ".subsections_via_symbols"
  let string_litteral_directive = ".asciz"

  let size_directive_of_size =
    let open KosuFrontend.Ast in
    function I8 -> "byte" | I16 -> "value" | I32 -> "long" | I64 -> "quad"

end


module FreeBSDAarch64AsmSpec : Aarch64AsmSpec.Aarch64AsmSpecification = struct
  open Printf

  module FreeBSDNamingSig = struct
    let main = "main"
    let label_prefix = ""
  end

  module FreeBSDNamingConvention = Common.NamingConvention.Make(FreeBSDNamingSig)

  include FreeBSDNamingConvention

  type address_load_style = 
  | MacOS
  | Other

  let adrp_style : address_load_style = Other

  let function_directives is_global fn_name = 
    if is_global then
      [ sprintf ".globl %s" fn_name; ".p2align 4" ]
    else
      [".p2align 4"]

  let constant_directives const_name = function
    | `IntVal (size, _) ->
        let align_size = (KosuFrontend.Ast.Isize.size_of_isize size / 8) - 1 in
        [ Printf.sprintf ".globl %s" const_name; sprintf ".p2align %u" align_size ]
    | `StrVal _ ->
        [
          Printf.sprintf ".globl %s" const_name; Printf.sprintf ".p2align %u" 3;
        ]

  let comment_prefix = "//"
  

  let string_litteral_section_start = ""

  let string_litteral_section_end = ""
  let string_litteral_directive = ".asciz"

  let size_directive_of_size =
    let open KosuFrontend.Ast in
    function I8 -> "byte" | I16 -> "hword" | I32 -> "word" | I64 -> "xword"
end
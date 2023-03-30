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

module type X86_64_Spec = sig
  val function_directives : string -> string list

  val constant_directives :
    string ->
    [ `IntVal of KosuFrontend.Ast.isize * int64 | `StrVal of string ] ->
    string list

  val comment_prefix : string
  val label_prefix : string
  val main : string
  val string_litteral_section_end : string
  val string_litteral_section_start : string
  val string_litteral_directive : string
  val size_directive_of_size : KosuFrontend.Ast.isize -> string
  val directive_of_fsize: KosuFrontend.Ast.fsize -> string
  val should_create_entry_point : string option
end

module Make (X86_Spec : X86_64_Spec) : X86_64AsmSpec.X86_64AsmSpecification =
struct
  include X86_Spec
  module NamingConvention = Common.NamingConvention.Make (X86_Spec)
  include NamingConvention
end

module X86_64LinuxAsmSpec = Make (struct
  let function_directives fn_name = [ Printf.sprintf ".globl %s" fn_name ]

  let constant_directives const_name = function
    | `IntVal (size, _) ->
        [
          Printf.sprintf ".globl %s" const_name;
          Printf.sprintf ".align %u"
            (KosuFrontend.Ast.Isize.size_of_isize size / 8);
        ]
    | `StrVal _ ->
        [ Printf.sprintf ".globl %s" const_name; Printf.sprintf ".align %u" 8 ]

  let comment_prefix = "#"
  let label_prefix = ""
  let main = "main"
  let string_litteral_section_start = ""
  let string_litteral_section_end = ""
  let string_litteral_directive = ".string"

  let size_directive_of_size =
    let open KosuFrontend.Ast in
    function I8 -> "byte" | I16 -> "value" | I32 -> "long" | I64 -> "quad"

    let directive_of_fsize = function
    | KosuFrontend.Ast.F32 -> "long" 
    | F64 -> "quad"

  let should_create_entry_point = Some "_start"
end)

module X86MacOsAsmSpec = Make (struct
  let function_directives fn_name =
    [ Printf.sprintf ".globl %s" fn_name; ".p2align 4, 0x90" ]

  let constant_directives const_name = function
    | `IntVal (size, _) ->
        [
          Printf.sprintf ".globl %s" const_name;
          Printf.sprintf ".align %u"
            (KosuFrontend.Ast.Isize.size_of_isize size / 8);
        ]
    | `StrVal _ ->
        [ Printf.sprintf ".globl %s" const_name; Printf.sprintf ".align %u" 8 ]

  let comment_prefix = "#"
  let label_prefix = "_"
  let main = "_main"

  let string_litteral_section_start =
    ".section\t__TEXT,__cstring,cstring_literals,"

  let string_litteral_section_end = ".subsections_via_symbols"
  let string_litteral_directive = ".asciz"

  let size_directive_of_size =
    let open KosuFrontend.Ast in
    function I8 -> "byte" | I16 -> "value" | I32 -> "long" | I64 -> "quad"

    let directive_of_fsize = function
    | KosuFrontend.Ast.F32 -> "long" 
    | F64 -> "quad"
  let should_create_entry_point = None
end)

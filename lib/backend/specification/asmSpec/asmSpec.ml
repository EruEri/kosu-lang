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

open KosuIrTyped.Asttyped

module type X86_64_Spec = sig
  val comment_prefix: string

  val label_prefix: string

  val main: string

  val p2align: string

  val p2align_function: string

  val string_litteral_section_end: string

  val string_litteral_section_start: string

  val string_litteral_directive: string

  val size_directive_of_size: KosuFrontend.Ast.isize -> string
end

module X86_64Spec_Make(X86_Spec: X86_64_Spec): Common.AsmSpecification = struct

  include X86_Spec

  let p2align = ".p2align 4, 0x90"



  let label_of_constant ?module_path const_name = 
    Printf.sprintf "%s%s" 
    (module_path |> Option.map (Printf.sprintf "%s._") |> Option.value ~default:"")
    const_name
  let label_of_function ~label_prefix ~main ~module_path ~fn_name ~generics = 
    if fn_name = "main" then main else
      Printf.sprintf "%s%s.%s%s" 
        label_prefix
        module_path
        ( if generics = [] then "" else generics |> String.concat "." |> Printf.sprintf "_%s_")
        fn_name

      let label_of_external_function rextern_func_decl = 
        rextern_func_decl.c_name |> Option.value ~default:rextern_func_decl.rsig_name |> Printf.sprintf "%s%s" label_prefix
    
      let label_of_kosu_function ~module_path (rfunction_decl: KosuIrTyped.Asttyped.rfunction_decl) =
        label_of_function ~module_path 
          ~main
          ~label_prefix
          ~fn_name:rfunction_decl.rfn_name 
          ~generics: rfunction_decl.generics
    
      let label_of_tac_function ~module_path (tac_function_decl: KosuIrTAC.Asttac.tac_function_decl) =
        label_of_function ~module_path 
        ~main
        ~label_prefix
        ~fn_name:tac_function_decl.rfn_name 
        ~generics:tac_function_decl.generics
end



module X86_64LinuxAsmSpec = X86_64Spec_Make(struct
  let comment_prefix = "#"
  let label_prefix = ""
  let main = "main"

  let p2align = ""

  let p2align_function = ""

  let string_litteral_section_start = ""

  let string_litteral_section_end = ""
  let string_litteral_directive = ".string"


  let size_directive_of_size =
  let open KosuFrontend.Ast in
  function I8 -> "byte" | I16 -> "value" | I32 -> "long" | I64 -> "quad"
end) 

module X86MacOsAsmSpec = X86_64Spec_Make(struct

  let comment_prefix = "#"
  let label_prefix = "_"
  let main = "_main"

  let p2align = "?"

  let p2align_function = ".p2align 4, 0x90"

  let string_litteral_section_start = ".section\t__TEXT,__cstring,cstring_literals,"

  let string_litteral_section_end = ".subsections_via_symbols"

  let string_litteral_directive = ".asciz"

  let size_directive_of_size =
    let open KosuFrontend.Ast in
    function I8 -> "byte" | I16 -> "value" | I32 -> "long" | I64 -> "quad"
end)
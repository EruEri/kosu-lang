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

open Util

module Codegen(AsmSpec: Common.AsmSpecification) = struct

  module X86_64 = X86_64Conv.Make(AsmSpec)

  module X86_64Pprint = X86_64Pprint.Make(AsmSpec)
  module AsmProgram = Common.AsmProgram(X86_64Core.Instruction)
  include AsmProgram
  open AsmProgram

  let asm_program_of_tac_program = X86_64.asm_program_of_tac_program
  let asm_module_of_tac_module = X86_64.asm_module_of_tac_module
  let sort_asm_module asm_module = 
    let AsmModule x =  X86_64.sort_asm_module (AsmModule asm_module) in
    x

  let string_litteral_section_start = ".section\t__TEXT,__cstring,cstring_literals,"

  let string_litteral_section_end = ".subsections_via_symbols"

  let string_of_asm_node = X86_64Pprint.string_of_asm_node

  let string_litteral_directive (_: string) (_: stringlit_label) = "asciz" 

  let filename_of_named_asm_module_path namp = namp.filename
  let asm_module_path_of_named_asm_module_path namp = namp.asm_module_path.asm_module
  let str_lit_map_of_name_asm_module namp = namp.str_lit_map
  let asm_module_node_list_of_asm_module = function
  | AsmModule rnodes -> rnodes
end
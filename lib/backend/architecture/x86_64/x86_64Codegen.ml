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

module Codegen (AsmSpec : X86_64AsmSpec.X86_64AsmSpecification) = struct
  module X86_64 = X86_64Conv.Make (AsmSpec)
  module X86_64Pprint = X86_64Pprint.Make (AsmSpec)
  module AsmProgram = Common.AsmProgram (X86_64Core.Instruction)
  include AsmProgram

  let asm_program_of_tac_program = X86_64.asm_program_of_tac_program
  let asm_module_of_tac_module = X86_64.asm_module_of_tac_module

  let sort_asm_module asm_module =
    let (AsmModule x) = X86_64.sort_asm_module (AsmModule asm_module) in
    x

  let string_of_asm_node = X86_64Pprint.string_of_asm_node
  let string_litteral_section_start = AsmSpec.string_litteral_section_start
  let string_litteral_section_end = AsmSpec.string_litteral_section_end
  let string_litteral_directive = AsmSpec.string_litteral_directive

  let directive_of_fsize = AsmSpec.directive_of_fsize
  
  let filename_of_named_asm_module_path namp = namp.filename

  let asm_module_path_of_named_asm_module_path namp =
    namp.asm_module_path.asm_module

  let str_lit_map_of_name_asm_module namp = namp.litterals.str_lit_map

  let float_lit_map_of_name_asm_module namp = namp.litterals.float_lit_map
  let asm_module_node_list_of_asm_module = function AsmModule rnodes -> rnodes
end

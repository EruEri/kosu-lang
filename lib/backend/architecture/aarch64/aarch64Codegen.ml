(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
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

module Codegen (AsmSpec : Aarch64AsmSpec.Aarch64AsmSpecification) = struct
  module Pprint = Aarch64Pprint.Make (AsmSpec)
  module AsmProgram = Common.AsmProgram (Aarch64Core.Instruction)
  module Aarch64Conv = Aarch64Conv.Make (AsmSpec)
  include AsmProgram
  open AsmProgram

  let asm_program_of_tac_program = Aarch64Conv.asm_program_of_tac_program
  let asm_module_of_tac_module = Aarch64Conv.asm_module_of_tac_module

  let sort_asm_module asm_module =
    let (AsmModule x) = Aarch64Conv.sort_asm_module (AsmModule asm_module) in
    x

  let string_litteral_section_start = AsmSpec.string_litteral_section_start
  let string_litteral_section_end = AsmSpec.string_litteral_section_end
  let string_of_asm_node = Pprint.string_of_asm_node
  let string_litteral_directive = AsmSpec.string_litteral_directive
  let filename_of_named_asm_module_path namp = namp.filename

  let asm_module_path_of_named_asm_module_path namp =
    namp.asm_module_path.asm_module

  let str_lit_map_of_name_asm_module namp = namp.str_lit_map
  let asm_module_node_list_of_asm_module = function AsmModule rnodes -> rnodes
end

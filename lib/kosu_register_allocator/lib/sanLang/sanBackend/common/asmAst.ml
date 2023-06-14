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

module type InstructionLine = sig
  type asmline
end

module Make (InstructionLine : InstructionLine) = struct
  type litterals = { str_lit_map : (string, Util.stringlit_label) Hashtbl.t }
  type asmline = InstructionLine.asmline
  type asm_function_decl = { asm_name : string; asm_body : asmline list }

  type asm_const_decl = {
    asm_const_name : string;
    value : [ `IntVal of int64 | `StrVal of string ];
  }

  type asm_module_node = Afunction of asm_function_decl
  type asm_module = AsmModule of asm_module_node list
end

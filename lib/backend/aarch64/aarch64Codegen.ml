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

open Aarch64Core
open Aarch64Conv
open Aarch64Pprint
open Util


let export_asm_module {filename; asm_module_path; rprogram = _; str_lit_map} = 
  let file = open_out filename in
  let AsmModule rnodes = sort_asm_module asm_module_path.asm_module in
  let () = rnodes |> List.iter (fun node -> Printf.fprintf file "%s\n\n" (string_of_asm_node node)) in
  let () = Printf.fprintf file "\n\t.section	__TEXT,__cstring,cstring_literals\n" in 
  let () = str_lit_map |> Hashtbl.to_seq |> Seq.iter (fun (str, SLit label) -> Printf.fprintf file "%s:\n\t .asciz \"%s\"\n\n" label str) in 
  let () = Printf.fprintf file "\n.subsections_via_symbols\n" in
  let () = close_out file in
  filename

let export_asm_module_tmp {filename; asm_module_path; rprogram = _; str_lit_map} = 
  let filename = filename |> String.map (fun c -> if (Char.escaped c)  = Filename.dir_sep then '_' else c) in
  let filename, file = Filename.open_temp_file filename ".S" in
  let AsmModule rnodes = sort_asm_module asm_module_path.asm_module in
  let () = rnodes |> List.iter (fun node -> Printf.fprintf file "%s\n\n" (string_of_asm_node node)) in
  let () = Printf.fprintf file "\n\t.section	__TEXT,__cstring,cstring_literals\n" in 
  let () = str_lit_map |> Hashtbl.to_seq |> Seq.iter (fun (str, SLit label) -> Printf.fprintf file "%s:\n\t .asciz \"%s\"\n\n" label str) in 
  let () = Printf.fprintf file "\n.subsections_via_symbols\n" in
  let () = close_out file in
  filename

let compile_asm_tmp asm_program = 
  asm_program |> List.map (fun asm_module -> export_asm_module_tmp asm_module)

let compile_asm asm_program = 
  asm_program |> List.map (fun asm_module -> 
    export_asm_module asm_module
  )

let compile_asm_from_tac_tmp tac_program = 
  tac_program
  |> Aarch64Conv.asm_program_of_tac_program
  |> compile_asm_tmp

let compile_asm_from_tac tac_program = 
  tac_program 
    |> Aarch64Conv.asm_program_of_tac_program 
    |> compile_asm
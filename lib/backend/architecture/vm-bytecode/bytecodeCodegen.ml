(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
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

open BytecodeConv
open BytecodeCore.BytecodeProgram
open BytecodePprint


let compile_asm_readable ?outfile tac_rpogram = 
  let asm_program = asm_program_of_tac_program ~start:None tac_rpogram in
  let on_file_function file = 
    asm_program
    |> List.iter (fun {filename; asm_module_path = {apath = _; asm_module = AsmModule asm_nodes}; rprogram = _; litterals = _} -> 
      let () = Printf.fprintf file "\n// %s\n\n" filename in
      let () = asm_nodes |> List.iter (fun node -> 
        Printf.fprintf file "%s\n" (string_of_asm_node node)
      ) in
      ()
    )
  in 
  match outfile with
  | Some file -> Out_channel.with_open_bin file on_file_function
  | None -> 
    let _, outchan = Filename.open_temp_file "a.kosu.bc" ".s" in
    let () = on_file_function outchan in
    close_out outchan  



let compile_bytecode () = failwith "TODO"
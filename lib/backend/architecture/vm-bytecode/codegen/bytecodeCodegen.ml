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

open BytecodeCompiler.Convertion
open BytecodeCompiler.BytecodeProgram
open BytecodeCompiler.Pprint

let compile_asm_readable ?outfile tac_rpogram =
  let asm_program = asm_program_of_tac_program ~start:None tac_rpogram in
  let on_file_function file =
    asm_program
    |> List.iter
         (fun
           {
             filename;
             asm_module_path = { apath = _; asm_module = AsmModule asm_nodes };
             rprogram = _;
             litterals = _;
           }
         ->
           let () = Printf.fprintf file "\n// %s\n\n" filename in
           let () =
             asm_nodes
             |> List.iter (fun node ->
                    Printf.fprintf file "%s\n" (string_of_asm_node node)
                )
           in
           ()
       )
  in
  let file = Option.value ~default:"a.out.bc.s" outfile in
  Out_channel.with_open_bin file on_file_function

let compile_as_readable ?outfile tac_rpogram =
  let asm_program = asm_program_of_tac_program ~start:None tac_rpogram in
  let _, as_prgram =
    BytecodeAssembler.Convertion.nodes_of_asm_program asm_program
  in
  let on_file_function file =
    as_prgram
    |> List.iter (fun node ->
           Printf.fprintf file "%s\n\n"
           @@ BytecodeAssembler.Pprint.string_of_asm_node node
       )
  in
  let file = Option.value ~default:"a.out.bc.as" outfile in
  Out_channel.with_open_bin file on_file_function

let compile_bytecode ?outfile tac_rpogram =
  let on_file checksum bytes oc =
    (* let fd = Unix.descr_of_out_channel oc in *)
    let () = Printf.fprintf oc "checksum=%s\n%!" checksum in
    let () = Printf.fprintf oc "%s" (Bytes.unsafe_to_string bytes) in
    ()
  in
  let asm_program = asm_program_of_tac_program ~start:None tac_rpogram in
  let bytes =
    asm_program |> BytecodeAssembler.Convertion.nodes_of_asm_program |> snd
    |> BytecodeAssembler.VmValue.bytes_of_nodes
  in
  let checksum = Util.Checksum.checksum bytes in
  let file = Option.value ~default:"a.out.bc" outfile in
  Out_channel.with_open_bin file (on_file checksum bytes)

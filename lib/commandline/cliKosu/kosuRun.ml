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

open Cmdliner

let name = "run"

type cmd = { bytecode_file : string; argv : string list }

let bytecode_file_term =
  Arg.(
    required
    & pos 0 (some file) None
    & info [] ~doc:"The bytecode file produces by $(b,kosuc.bc)"
        ~docv:"<BYTECODE_FILE>"
  )

let argv_term =
  Arg.(
    value & pos_right 0 string [] & info [] ~doc:"prgram arguments" ~docv:"ARGV"
  )

let cmd_term run =
  let combine bytecode_file argv = run @@ { bytecode_file; argv } in
  Term.(const combine $ bytecode_file_term $ argv_term)

let run_doc = "The Kosu Bytecode Interpreter"

let run_man =
  [
    `S Manpage.s_description;
    `P "$(iname) executes the bytecode produced by the $(b,kosuc.bc)";
  ]

let prebytecode_keys_val content =
  let first_line_index =
    match String.index_opt content '\n' with
    | Some n ->
        n
    | None ->
        failwith "Wierd"
  in
  let strlen = String.length content in
  let next_start_index = first_line_index + 1 in
  let first = String.sub content 0 first_line_index in
  let remains =
    String.sub content next_start_index (strlen - next_start_index)
  in
  (first, remains)

let splitter = '='

let check_key_checksum = function
  | "checksum" ->
      ()
  | _ ->
      failwith "Expecting checksum key"

let check_pc_key = function "pc" -> () | _ -> failwith "Expecting pc key"

let keyvals line =
  match String.split_on_char splitter line with
  | [ key; value ] ->
      (key, value)
  | _ ->
      failwith "Key value: ill formated"

let pc_value = int_of_string

let check_checksum check string =
  let ch = Digest.string string in
  let () =
    match ch = check with
    | true ->
        ()
    | false ->
        failwith @@ Printf.sprintf "Checksum doesnt match: %s <> %s" ch check
  in
  ()

let run_vm pc code =
  let vm = KosuVirtualMachine.CamlVm.vm_init code 4096 pc () in
  let status = KosuVirtualMachine.CamlVm.vm_run vm () in
  let () = KosuVirtualMachine.CamlVm.vm_free vm () in
  status

let run_main cmd =
  let { bytecode_file; argv } = cmd in
  let () = ignore (bytecode_file, argv) in
  let content =
    In_channel.with_open_bin bytecode_file (fun ic -> Util.Io.read_file ic ())
  in
  let checksum_line, remains = prebytecode_keys_val content in
  let keys, checksum = keyvals checksum_line in
  (* let () = String.iter (fun c -> Printf.printf "%d" @@ Char.code c) checksum in *)
  let () = check_key_checksum keys in
  let () = check_checksum checksum remains in

  let pc_line, bytecode = prebytecode_keys_val remains in
  let pc_key, pc_value = keyvals pc_line in
  let () = check_pc_key pc_key in
  let value = int_of_string pc_value in
  let _ = run_vm value bytecode in
  ()

let run : unit Cmd.t =
  let info = Cmd.info ~doc:run_doc ~man:run_man name in
  Cmd.v info (cmd_term run_main)

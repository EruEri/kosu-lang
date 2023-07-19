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

let name = "kosurun"

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

(*
   let check_checksum check string =
     let ch = Digest.string string in
     let () =
       match ch = check with
       | true ->
           ()
       | false ->
           failwith @@ Printf.sprintf "Checksum doesnt match: %s <> %s" ch check
     in
     () *)

let run_vm pc code =
  let vm = KosuVirtualMachine.kosuvm_init code 4096 pc () in
  let status = KosuVirtualMachine.kosuvm_run vm () in
  let () = KosuVirtualMachine.kosuvm_free vm () in
  status

let is_shebang string = String.starts_with ~prefix:"#!" string
let is_kosurun_shebang = String.ends_with ~suffix:name

let handle_first_line content =
  let first_line, remains = prebytecode_keys_val content in
  let remains_content =
    match is_shebang first_line with
    | true ->
        let () =
          match is_kosurun_shebang first_line with
          | true ->
              ()
          | false ->
              failwith "Shebang doesnt point toward kosurun"
        in
        remains
    | false ->
        content
  in
  remains_content

let str_splitter =
  Str.regexp @@ Str.quote KosuBackend.Bytecode.Codegen.exec_splitter

(** [spliting_file content] split the file by the [KosuBackend.Bytecode.Codegen.exec_splitter] string*)
let spliting_file content =
  match Str.split str_splitter content with
  | [ keys; content ] ->
      (keys, content)
  | _ ->
      failwith "Ill formed file, cannot find splitter"

let values params =
  params |> String.trim |> String.split_on_char '\n'
  |> List.map (fun kv ->
         match String.split_on_char '=' kv with
         | [ k; v ] ->
             (k, v)
         | _ ->
             failwith "params ill formed"
     )

let get_value key map =
  let ( let* ) = Option.bind in
  let* str_value = List.assoc_opt key map in
  Some str_value

let pc_value keyvals =
  let ( let* ) = Option.bind in
  let* str_value = get_value "pc" keyvals in
  let* pc = int_of_string_opt str_value in
  Some pc

let run_main cmd =
  let size = Array.length Sys.argv - 1 in
  let _argv = Array.make size String.empty in
  let () = Array.blit Sys.argv 1 _argv 0 size in
  (* let () = Array.iter print_endline Sys.argv in *)
  let { bytecode_file; argv } = cmd in
  let () = ignore argv in
  let content =
    In_channel.with_open_bin bytecode_file (fun ic -> Util.Io.read_file ic ())
  in
  let shebang_params, bytecode = spliting_file content in
  let params = handle_first_line shebang_params in
  let keys_values_paramas = values params in
  (* let () = String.iter (fun c -> Printf.printf "%d" @@ Char.code c) checksum in *)
  let pc_value = Option.get @@ pc_value keys_values_paramas in
  let status = run_vm pc_value bytecode in
  let () = Printf.eprintf "status = %d\n" status in
  ()

let kosurun () =
  let info =
    Cmd.info ~version:CliCommon.version ~doc:run_doc ~man:run_man name
  in
  Cmd.v info (cmd_term run_main)

let eval () = Cmd.eval ~catch:true @@ kosurun ()

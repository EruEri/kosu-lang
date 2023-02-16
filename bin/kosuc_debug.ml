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

open KosuFrontend.Astvalidation
open KosuIrTyped
open KosuIrTAC
open KosuCli

let () = 
  let files = Clap.list_string ~description:"files" ~placeholder:"FILES" () in

  let kosu_files, _ =
    files |> List.partition (fun s -> s |> Filename.extension |> ( = ) ".kosu")
  in

  let () = Clap.close () in

  let modules_opt = Cli.files_to_ast_program kosu_files in

  let tac_program =
    match modules_opt with
    | Error e -> (
        match e with
        | No_input_file -> raise (Invalid_argument "no Input file")
        | File_error (s, exn) ->
            Printf.eprintf "%s\n" s;
            raise exn
        | Filename_error _ -> raise (Invalid_argument "Filename Error")
        | Lexer_Error e -> raise e)
    | Ok modules -> (
        match valide_program modules with
        | filename, Error e ->
            (* Printf.eprintf "\nFile \"%s\", %s\n" filename (Kosu_frontend.Pprint.string_of_validation_error e); *)
            raise (Error.Validation_error (filename, e))
        | _, Ok () ->
            let typed_program =
              try Asttyconvert.from_program modules
              with KosuFrontend.Ast.Error.Ast_error e ->
                let () =
                  Printf.printf "%s\n"
                    (KosuFrontend.Pprinterr.string_of_ast_error e)
                in
                failwith "failwith"
            in
            let tac_program =
              Asttacconv.tac_program_of_rprogram ~dump_ast:false typed_program
            in
            tac_program)
  in
  let named_cfgs = KosuIrCfg.Astcfgconv.cfgs_of_tac_program tac_program in
  (* let s_cfgs = KosuIrCfg.Asttaccfg.CfgPprint.string_of_named_cfg named_cfgs in
  let () = Printf.printf "%s\n\n" s_cfgs in *)

  let named_cfgs_details = named_cfgs |> List.map (fun (name, cfgs) -> 
    (name, cfgs |> List.map KosuIrCfg.Asttaccfg.Cfg.Detail.of_cfg)
  ) in
  let s = KosuIrCfg.Astcfgpprint.string_of_named_cfg_details named_cfgs_details in
  let () = Printf.printf "%s\n" s in
  ()
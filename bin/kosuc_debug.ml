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

type cfg_type = 
| Basic
| Detail
| Liveness

let cfg_type_of_string_opt = function
| "basic" -> Some Basic
| "detail" -> Some Detail
| "live" -> Some Liveness
| _ -> None

let string_of_cfg_type = function
| Basic -> "basic"
| Detail -> "detail"
| Liveness -> "liveness"


let cfg_type = Clap.typ ~name:"cfg_type" ~dummy:Basic ~parse:cfg_type_of_string_opt ~show:string_of_cfg_type

let () = 
  let () = KosuFrontend.Registerexn.register_kosu_error () in

  let out = Clap.optional_string ~long:"output" ~short:'o' () in

  let cfg_type_opt = Clap.optional cfg_type ~description:"Which control flow graph iteration should be displayed [basic, detail, live]\n default : detail" ~long:"cfg-type" () in

  let dot_function = Clap.optional_string ~long:"dot" ~short:'d' ~description:"Generate the Control flow graph for a specifique funtion" () in

  let infered_grah = Clap.optional_string ~long:"infer" ~short:'i' ~description:"Gererate the Inferenced graph for the selected function" () in

  let colored_graph = Clap.flag ~set_long:"colored" ~set_short:'c' ~description:"Colored graph" false in

  let delete_useless_stmt = Clap.flag ~set_long:"del-stmt" ~set_short:'D' ~description:"Delete used statements in the graph" false in

  let files = Clap.list_string ~description:"files" ~placeholder:"FILES" () in

  let kosu_files, _ =
    files |> List.partition (fun s -> s |> Filename.extension |> ( = ) ".kosu")
  in

  let () = Clap.close () in

  let modules_opt = KosuCli.files_to_ast_program kosu_files in

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

  let named_cfgs_details = named_cfgs |> List.map (fun (name, cfgs) -> 
    (name, cfgs |> List.map KosuIrCfg.Asttaccfg.Cfg.Detail.of_cfg)
  ) in

  let named_cfgs_liveness_details = named_cfgs_details |> List.map (fun (name, cfgs) -> 
    (name, cfgs |> List.map (KosuIrCfg.Asttaccfg.Cfg.Liveness.of_cfg_details ~delete_useless_stmt) )
  ) in

  match dot_function with
  | Some name -> 
    let block = KosuIrCfg.Astcfgpprint.fetch_function_liveness name named_cfgs_liveness_details in
    let _ = block |> Option.iter (fun block -> 
      let outchan = match out with
        | None -> stdout
        | Some o -> open_out o 
      in
      let () = block |> KosuIrCfg.Astcfgpprint.dot_diagrah_of_cfg_liveness |> KosuIrCfg.Astcfgpprint.string_of_dot_graph ~out:outchan in
      let infered_grah_fn = if colored_graph then KosuIrCfg.Astcfgpprint.export_colored_graph else KosuIrCfg.Astcfgpprint.export_infer_graph_of_cfg in
      let () = Option.iter (fun oc_name -> 
        Out_channel.with_open_bin oc_name (fun oc -> 
            infered_grah_fn ~outchan:oc block ()
          ) 
      ) infered_grah in
      let () = Option.iter (fun _ -> close_out outchan) out in
      ()
      ) 
    in
    ()
  | None ->
    let cfg_type_opt = Option.value ~default:Detail cfg_type_opt in

    let string_cfg = match cfg_type_opt with
      | Basic -> KosuIrCfg.Astcfgpprint.string_of_named_cfg named_cfgs 
      | Detail -> KosuIrCfg.Astcfgpprint.string_of_named_cfg_details named_cfgs_details
      | Liveness -> KosuIrCfg.Astcfgpprint.string_of_named_cfg_liveness_details named_cfgs_liveness_details
    in

    let () = Printf.printf "%s\n" string_cfg in
  ()
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
module KosuFront = CliCore.DefaultFront.KosuFront
module Asttyconvert = CliCore.DefaultFront.Asttyconvert

type cfg_type = 
| Basic
| Detail
| Liveness

type cmd = {
  dot: string option;
  colored: bool;
  cfg_type: cfg_type;
  variable_infer: bool;
  fn_name: string option;
  module_name: string;
  files: string list
}

let name = "cfg"

let string_of_enum ?(splitter = "|") ?(quoted = false) enum =
  let f = if quoted then Cmdliner.Arg.doc_quote else Fun.id in
  enum |> List.map (fun (elt, _) -> f elt) |> String.concat splitter


let cfg_type_enum = [
  ("basic", Basic); ("detail", Detail); ("liveness", Liveness )
]

  let swap (a,b) = b, a

  let lswap list = list |> List.map swap

  let dot_term = 
    Arg.(
      value & opt (some string) None & info ~docv:"format" ~doc:"invoke the $(b,dot) command and generate graph with $(docv) image format" ["d"; "dot"] 
    )

  let colored_term = 
    Arg.(
      value & flag & info ~doc:"Color the variable inference graph" ["c"; "colored"]
    )

  let module_term = 
    Arg.(
      required & opt (some string) None & info ~docv:"module" ~doc:"look for the $(b,function) into $(docv). If $(docv) start with a uppercase letter \
      the module name is matched, otherwise it's the filename with or without the \".kosu\" extension
      " ["m"; "module"]
    )

  let infered_term = 
    Arg.(
      value & flag & info ~doc:"Generate the variable inference graph" ["i"; "infered"] 
    )
    
  let function_name_term = 
    Arg.(
      value & opt (some string) None & info ~doc:"Generate graph for a the $(opt) function" ~docv:"function_name" ["f"; "function"]
    )

  let cfg_type_term =
    Arg.(
      value & opt (enum cfg_type_enum) Detail & info ~docv:(string_of_enum cfg_type_enum) ~doc:"Precise which iteration of the cfg should be printed" ["t"; "type"]
    )

    let file_term = 
      Arg.(non_empty
      & pos_all Arg.non_dir_file []
      & info [] ~docv:"FILES" 
      )

  let cmd_term run = 
    let combine dot colored cfg_type variable_infer fn_name module_name files = 
      run @@ { dot; colored; cfg_type; variable_infer; fn_name; module_name; files }
    in
    Term.(const combine
      $ dot_term
      $ colored_term
      $ cfg_type_term
      $ infered_term
      $ function_name_term
      $ module_term
      $ file_term
    )

  let cfg_doc = "Visualise control flow graphs and inference graphs"

  let cfg_man = [
    `S Manpage.s_description;
    `P 
      "kosuc-cfg allows you to visual control flow graphs (cfg) and variable inference graph";
    `P "kosuc-cfg relies heavily on the $(b,dot) language and the $(b,graphviz) library to display graphs";  
    `S Manpage.s_see_also;
    `Noblank;
    `P "$(b,dot)(1)";
    `Noblank;
    `P "Graphviz:  https://graphviz.org";
    `Noblank;
  ]

let infered_name ~extension ~colored fn_name =
  Printf.sprintf "%s.infered%s.%s" fn_name (if colored then ".colored" else "") extension

let cfg_name ~extension ~ctype fn_name = 
  Printf.sprintf "%s.%s.%s" fn_name ( cfg_type_enum |> lswap |> List.assoc ctype ) extension

let dot_infered_name = infered_name ~extension:"dot"
let dot_cfg_name = cfg_name ~extension:"dot"

let target_file file_type format fn_name = match format with
  | None -> begin match file_type with
    | `Infered colored -> dot_infered_name ~colored fn_name
    | `Cfg (cfg_type) -> dot_cfg_name ~ctype:cfg_type fn_name
  end 
  | Some extension -> begin match file_type with
    | `Infered colored -> infered_name ~colored fn_name ~extension
    | `Cfg (cfg_type) -> cfg_name ~ctype:cfg_type fn_name ~extension
  end

let write_cfg cfg_type ~oc tac_function = 
  match cfg_type with
    | Basic -> 
      let converted = KosuIrCfg.Astcfgconv.cfg_of_tac_function tac_function in
      converted |> KosuIrCfg.Astcfgpprint.dot_diagrah_of_cfg_basic |> KosuIrCfg.Astcfgpprint.string_of_dot_graph ~out:oc 
    | Detail ->
      let converted = KosuIrCfg.Astcfgconv.cfg_detail_of_tac_function tac_function in
      converted |>  KosuIrCfg.Astcfgpprint.dot_diagrah_of_cfg_detail |> KosuIrCfg.Astcfgpprint.string_of_dot_graph ~out:oc 
    | Liveness -> 
      let converted = KosuIrCfg.Astcfgconv.cfg_liveness_of_tac_function tac_function in
      converted |>  KosuIrCfg.Astcfgpprint.dot_diagrah_of_cfg_liveness |> KosuIrCfg.Astcfgpprint.string_of_dot_graph ~out:oc 


let write_infered ~infered ~colored ~oc (tac_function: KosuIrTAC.Asttac.tac_function_decl) = 
  match infered with
  | false -> ()
  | true -> 
    let livecfg = KosuIrCfg.Astcfgconv.cfg_liveness_of_tac_function tac_function in
    let transform = if colored then failwith "Colored todo" else KosuIrCfg.Astcfgpprint.export_infer_graph_of_cfg in
    transform ~outchan:oc livecfg ()

  let export_from_san_function cmd (tac_function: KosuIrTAC.Asttac.tac_function_decl) = 
    match cmd.dot with
    | None -> (
      let outchan_name = target_file (`Cfg cmd.cfg_type) cmd.dot tac_function.rfn_name in
      let () = Out_channel.with_open_bin outchan_name (fun oc ->
        write_cfg cmd.cfg_type ~oc tac_function
      ) in

      match cmd.variable_infer with
      | false -> ()
      | true -> 
        let infered_ouchan = target_file (`Infered cmd.colored) cmd.dot tac_function.rfn_name in
        Out_channel.with_open_bin infered_ouchan (fun oc ->
          write_infered ~infered:cmd.variable_infer ~colored:cmd.colored ~oc tac_function
        )
    )
    | Some export_format -> begin
      let cfg_outname = target_file (`Cfg cmd.cfg_type) (cmd.dot) tac_function.rfn_name in
      let filename, tmp_cfg_out = Filename.open_temp_file "cfg" ".dot" in
      let () = write_cfg cmd.cfg_type ~oc:tmp_cfg_out tac_function in
      let () = close_out tmp_cfg_out in
      let _ = Sys.command (Printf.sprintf "dot -T%s -o %s %s" export_format cfg_outname filename) in
      match cmd.variable_infer with
      | false -> ()
      | true ->
        let infered_ouchan = target_file (`Infered cmd.colored) cmd.dot tac_function.rfn_name in 
        let tmp_infered_filename, tmp_infered_out = Filename.open_temp_file "infered" ".dot" in
        let () = write_infered ~infered:cmd.variable_infer ~colored:cmd.colored ~oc:tmp_infered_out tac_function in
        let () = close_out tmp_infered_out in
        let _ = Sys.command (Printf.sprintf "dot -T%s -o %s %s" export_format infered_ouchan tmp_infered_filename ) in
        ()
    end

let cfg_main cmd = 
  let open KosuIrTAC.Asttac in
  let { dot = _; colored = _; cfg_type = _; variable_infer = _; fn_name; module_name; files } = cmd in
  let () = KosuFront.Registerexn.register_kosu_error () in
  let program  = KosuFront.ast_modules files in
  let typed_program = Asttyconvert.from_program program in
  let tac_program = KosuIrTAC.Asttacconv.tac_program_of_rprogram typed_program in
  let tac_nodes = tac_program |> List.find_map (fun {filename; tac_module_path; rprogram = _} -> 
    let capitalized = String.capitalize_ascii module_name in
    if module_name = capitalized then
      if module_name <> tac_module_path.path  then None else 
        let TacModule nodes = tac_module_path.tac_module in
        Some nodes
    else 
      let query_module = if String.ends_with ~suffix:".kosu" module_name then module_name else (module_name ^ ".kosu") in 
      if query_module <> filename then None else 
        let TacModule nodes = tac_module_path.tac_module in
        Some nodes  
  ) |> Option.value ~default:[] in
  let filtered_function_in_module = fn_name |> Option.map (fun s -> 
    tac_nodes |> List.filter (fun node -> 
      match node with
      | TNFunction {rfn_name; _}  -> rfn_name = s
      | _ -> false 
    )
  ) |> Option.value ~default:tac_nodes in
  let function_decl_list = filtered_function_in_module |> List.filter_map (
    function 
    | TNFunction fn -> Some fn
    | _ -> None
  ) in

  let () = function_decl_list |> List.iter (fun (tac_function: tac_function_decl) ->
    export_from_san_function cmd tac_function
  ) in
  ()

let cfg = 
  let info = Cmd.info ~doc:cfg_doc ~man:cfg_man name in
  Cmd.v info (cmd_term cfg_main)
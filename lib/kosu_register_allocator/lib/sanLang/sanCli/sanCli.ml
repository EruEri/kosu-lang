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

open Cmdliner

module CmdError = struct
  type t = 
  | No_function of string

  exception CdmError of t

  let string_of_cmd_erorr = function
  | No_function s -> Printf.sprintf "No function \"%s\" was defined" s
  
end

module Cfg_Command = struct

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

    file: string
  }

  let name = "cfg"

  let string_of_enum ?(splitter = "|") ?(quoted = false) enum =
    let f = if quoted then Arg.doc_quote else Fun.id in
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
      let info = 
        Arg.info []
        ~docv:"FILE"
      in
      Arg.required (Arg.pos  0 (Arg.some Arg.non_dir_file) None info) 

  let cmd_term run = 
    let combine dot colored cfg_type variable_infer fn_name file = 
      run @@ { dot; colored; cfg_type; variable_infer; fn_name; file }
    in
    Term.(const combine
      $ dot_term
      $ colored_term
      $ cfg_type_term
      $ infered_term
      $ function_name_term
      $ file_term
    )

  let cfg_doc = "Visualise control flow graphs and inference graphs"

  let cfg_man = [
    `S Manpage.s_description;
    `P 
      "san-cfg allows you to visual control flow graphs (cfg) and variable inference graph";
    `P "san-cfg relies heavily on the $(b,dot) language and the $(b,graphviz) library";  
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

  let write_cfg cfg_type ~oc san_function = 
    match cfg_type with
      | Basic -> 
        let converted = SanCfg.SanCfgConv.basic_of_san_tyfunction san_function in
        converted |> SanCfg.SanCfgPprint.dot_diagrah_of_cfg_basic |> SanCfg.SanCfgPprint.string_of_dot_graph ~out:oc 
      | Detail ->
        let converted = SanCfg.SanCfgConv.detail_of_san_tyfunction san_function in
        converted |> SanCfg.SanCfgPprint.dot_diagrah_of_cfg_detail |> SanCfg.SanCfgPprint.string_of_dot_graph ~out:oc 
      | Liveness -> 
        let converted = SanCfg.SanCfgConv.liveness_of_san_tyfunction san_function in
        converted |> SanCfg.SanCfgPprint.dot_diagrah_of_cfg_liveness |> SanCfg.SanCfgPprint.string_of_dot_graph ~out:oc 

  let write_infered ~infered ~colored ~oc (san_function: SanTyped.SanTyAst.ty_san_function) = 
    match infered with
    | false -> ()
    | true -> 
      let livecfg = SanCfg.SanCfgConv.liveness_of_san_tyfunction san_function in
      let transform = if colored then SanCfg.SanCfgPprint.export_colored_graph else SanCfg.SanCfgPprint.export_infer_graph_of_cfg in
      transform ~outchan:oc livecfg ()

  let export_from_san_function cmd (san_function: SanTyped.SanTyAst.ty_san_function) = 
    match cmd.dot with
    | None -> (
      let outchan_name = target_file (`Cfg cmd.cfg_type) cmd.dot san_function.fn_name in
      let () = Out_channel.with_open_bin outchan_name (fun oc ->
        write_cfg cmd.cfg_type ~oc san_function
      ) in

      match cmd.variable_infer with
      | false -> ()
      | true -> 
        let infered_ouchan = target_file (`Infered cmd.colored) cmd.dot san_function.fn_name in
        Out_channel.with_open_bin infered_ouchan (fun oc ->
          write_infered ~infered:cmd.variable_infer ~colored:cmd.colored ~oc san_function
        )
    )
    | Some export_format -> begin
      let cfg_outname = target_file (`Cfg cmd.cfg_type) (cmd.dot) san_function.fn_name in
      let filename, tmp_cfg_out = Filename.open_temp_file "cfg" ".dot" in
      let () = write_cfg cmd.cfg_type ~oc:tmp_cfg_out san_function in
      let () = close_out tmp_cfg_out in
      let _ = Sys.command (Printf.sprintf "dot -T%s -o %s %s" export_format cfg_outname filename) in
      match cmd.variable_infer with
      | false -> ()
      | true ->
        let infered_ouchan = target_file (`Infered cmd.colored) cmd.dot san_function.fn_name in 
        let tmp_infered_filename, tmp_infered_out = Filename.open_temp_file "infered" ".dot" in
        let () = write_infered ~infered:cmd.variable_infer ~colored:cmd.colored ~oc:tmp_infered_out san_function in
        let () = close_out tmp_infered_out in
        let _ = Sys.command (Printf.sprintf "dot -T%s -o %s %s" export_format infered_ouchan tmp_infered_filename ) in
        ()
    end

  let cfg_main cmd = 
    let { dot; colored; cfg_type; variable_infer; fn_name; file } = cmd in
    let typed_san_module = SanTyped.of_file file in
    let typed_san_module = fn_name |> Option.map (fun s -> 
      typed_san_module |> List.filter (fun node -> 
        let open SanTyped.SanTyAst in
        match node with
        | TyDeclaration {fn_name; _} | TyExternal {fn_name; } -> fn_name = s
      )
    ) |> Option.value ~default:typed_san_module in
    let filtered_san_module = typed_san_module |> List.filter_map (
      let open SanTyped.SanTyAst in
      function 
      | TyDeclaration fn -> Some fn
      | TyExternal _ -> None
    ) in

    let () = filtered_san_module |> List.iter (fun (san_function: SanTyped.SanTyAst.ty_san_function) ->
      export_from_san_function cmd san_function
    ) in
    ()

  let cfg = 
    let info = Cmd.info ~doc:cfg_doc ~man:cfg_man name in
    Cmd.v info (cmd_term cfg_main)
end

module Compile_Cmd = struct
  let default_outfile = "a.out"

  let name = "compile"

  type cmd = {
    asm: bool;
    outfile: string;
    file: string;
    other_files: string list
  }


  let target_asm_term =
    Arg.(value & flag & info [ "S" ] ~doc:"Produce assembly files")

  let output_term =
    Arg.(
      value & opt string default_outfile
      & info [ "o" ] ~docv:"FILENAME"
          ~doc:
            (Printf.sprintf "write output to <%s>"
                (String.lowercase_ascii "$(docv)")))


  let file_term = 
    let info = 
      Arg.info []
      ~docv:"FILE"
    in
    Arg.required  ( Arg.pos 0 (Arg.some Arg.non_dir_file) None info) 

  let other_file_term = 
    Arg.(
      value & pos_right 0 file [] & info ~doc:"Pass files to $(b,cc)(1)" ~docv:"FILES" []
    )


  let cmd_term run = 
    let combine asm outfile file other_files = 
      run @@ { asm; outfile; file; other_files }
    in
    Term.(const combine
      $ target_asm_term
      $ output_term
      $ file_term
      $ other_file_term
    )

  let compile = "Compile san file"

  let compile_doc = "Compile san programs"

  let compile_man = [
    `S Manpage.s_description;
    `P 
      "san-compile allows you to compile san program into executable or assembly files";
    `P
      "san-compile use the C compilateur to generate the executable";
    `S Manpage.s_see_also;
    `Noblank;
    `P "$(b,cc)(1)";
    `Noblank;
  ]

  let compile_main cmd =
  let { asm; outfile; file; other_files } = cmd in
  let san_module = SanFrontend.san_module_parse file in
  let ty_san_module = SanTyped.of_san_module san_module in
  let module Codegen = SanBackend.Aarch64.Codegen.Make(SanBackend.Aarch64.Implementation.MacOSAarch64AsmSpec) in
  match asm with
  | true -> 
    let outfile = if outfile = default_outfile then
      Printf.sprintf "%s.s" default_outfile 
    else
      outfile
    in
    let () = Out_channel.with_open_bin outfile (fun oc -> 
      Codegen.compile_s ~outfile:oc ty_san_module
    ) in
    ()
  | false ->
    let () = Codegen.compile ~outname:outfile other_files ty_san_module () in
    ()

  let compile =    
     let info = Cmd.info ~doc:compile_doc ~man:compile_man name in
  Cmd.v info (cmd_term compile_main)



end


let name = "san"
let version = "0.0.1"

let san_doc = "The minimalist 3 address code compiler"

let san_man = [
  `S Manpage.s_description;
  `P 
    "San is the compiler of a minimalist 3 address code language";
  `P
    "San exists to more easily test the register allocator of Kosu";
  `P
    "San produce the executable by invoking the C compiler";
  `S Manpage.s_see_also;
  `P "$(b,cc)(1)";
  `P "Repository: https://github.com/EruEri/kosu-register-allocator";
  `P "Repository: https://github.com/EruEri/kosu-lang";
  `S Manpage.s_authors;
  `P "Yves Ndiaye";
  `S "COPYRIGHT";
  `P "Yves Ndiaye";
  `S "LICENSE";
  `P "San is distributed under the GNU GPL-3.0"; 
]


  

let san =
  let info = Cmd.info ~doc:san_doc ~man:san_man ~version name in
  Cmd.group info [Cfg_Command.cfg; Compile_Cmd.compile]



let eval () = 
  (* let () = Printexc.print_backtrace stderr in *)
  san |> Cmd.eval ~catch:true
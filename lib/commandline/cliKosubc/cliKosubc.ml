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

open CliCommon
open Cmdliner

module Cli = struct
  let name = "kosuc.bc"
  let default_outfile = "a.out.bc"

  type cmd = {
    f_allow_generic_in_variadic : bool;
    no_std : bool;
    is_target_asm : bool;
    output : string;
    files : string list;
  }

  let f_allow_generic_in_variadic_term =
    Arg.(
      value & flag
      & info
          [ "fallow-generic-variadic" ]
          ~docs:"COMPILATION OPTION"
          ~doc:
            "Allow the use of variables with generic type in variadic function \
             parameters such as $(b,printf(3))")

  let no_std_term =
    Arg.(
      value & flag & info [ "no-std" ] ~doc:"Don't include the standard library")

  let target_asm_term =
    Arg.(value & flag & info [ "S" ] ~doc:"Produce bytecode assembly files")

  let output_term =
    Arg.(
      value & opt string default_outfile
      & info [ "o" ] ~docv:"FILENAME"
          ~doc:
            (Printf.sprintf "write output to <%s>"
               (String.lowercase_ascii "$(docv)")))

  let files_term =
    Arg.(
      non_empty
      & pos_all Arg.non_dir_file []
      & info [] ~docv:"FILES"
          ~doc:
            "Input files of the compiler. Kosu files must have the extension \
             .kosu")

  let cmd_term run =
    let combine f_allow_generic_in_variadic no_std is_target_asm output files =
      run
      @@ { f_allow_generic_in_variadic; no_std; is_target_asm; output; files }
    in
    Term.(
      const combine $ f_allow_generic_in_variadic_term $ no_std_term
      $ target_asm_term $ output_term $ files_term)

  let kosubc_doc = "The Kosu bytecode compiler"

  let kosubc_man =
    [
      `S Manpage.s_description;
      `P
        "The Kosu bytecode compiler $(b,kosu.bc) compiles kosu files into \
         bytecodes which  \n\
        \      is interpreted by the kosu virtual machine";
      `S Manpage.s_see_also;
      `P "$(b,kosuc)(1), $(b,kosu)(1)";
      `Noblank;
      `P "Repository:  https://github.com/EruEri/kosu-lang";
      `S Manpage.s_authors;
      `P "Yves Ndiaye";
      `S "COPYRIGHT";
      `P "Yves Ndiaye";
      `S "LICENSE";
      `P "Kosuc is distributed under the GNU GPL-3.0";
    ]

  let compile cmd =
    let {
      f_allow_generic_in_variadic : bool;
      no_std : bool;
      is_target_asm : bool;
      output : string;
      files : string list;
    } =
      cmd
    in

    let kosu_files, other_files = files |> List.partition is_kosu_file in

    let std_file = fetch_std_file ~no_std () in

    let kosu_files = kosu_files @ std_file in
    let module ValidationRule : KosuFrontend.KosuValidationRule = struct end in
    let module TypeCheckerRule : KosuFrontend.TypeCheckerRule = struct
      let allow_generics_in_variadic = f_allow_generic_in_variadic
    end in
    let module Compilation_Files : KosuFrontend.Compilation_Files = struct
      let std_global_variable = std_global_variable
    end in
    let module KosuFront =
      KosuFrontend.Make (Compilation_Files) (ValidationRule) (TypeCheckerRule)
    in
    let module Asttyconvert = KosuIrTyped.Asttyconvert.Make (TypeCheckerRule) in
    let () = KosuFront.Registerexn.register_kosu_error () in
    let ast_module = KosuFront.ast_modules kosu_files in
    let typed_program =
      match Asttyconvert.from_program ast_module with
      | typed_program -> typed_program
      | exception KosuFrontend.Ast.Error.Ast_error e ->
          let () =
            e |> KosuFront.Pprinterr.string_of_ast_error |> print_endline
          in
          failwith "Error while typing ast: Shouldn't append"
    in
    let tac_program =
      KosuIrTAC.Asttacconv.tac_program_of_rprogram typed_program
    in
    let () =
      match is_target_asm with
      | true ->
          KosuBackend.Bytecode.Codegen.compile_asm_readable ~outfile:output
            tac_program
      | false -> failwith ""
    in
    ()

  let kosuc_bc =
    let info =
      Cmd.info ~doc:kosubc_doc ~man:kosubc_man ~version:CliCommon.version name
    in
    Cmd.v info (cmd_term compile)

  let eval () = Cmd.eval @@ kosuc_bc
end

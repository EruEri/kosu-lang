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

open CliCommon

module Cli = struct
  open Cmdliner

  module Mac0SX86 =
    KosuBackend.Codegen.Make
      (KosuBackend.X86_64.X86_64Codegen.Codegen
         (KosuBackend.X86_64.X86_64AsmSpecImpl.X86MacOsAsmSpec))

  module LinuxX86 =
    KosuBackend.Codegen.Make
      (KosuBackend.X86_64.X86_64Codegen.Codegen
         (KosuBackend.X86_64.X86_64AsmSpecImpl.X86_64LinuxAsmSpec))

  module MacOSAarch64 =
    KosuBackend.Codegen.Make
      (KosuBackend.Aarch64.Aarch64Codegen.Codegen
         (KosuBackend.Aarch64.Aarch64AsmSpecImpl.MacOSAarch64AsmSpec))

  module FreeBSDAarch64 =
    KosuBackend.Codegen.Make
      (KosuBackend.Aarch64.Aarch64Codegen.Codegen
         (KosuBackend.Aarch64.Aarch64AsmSpecImpl.FreeBSDAarch64AsmSpec))

  let name = "kosuc"
  let c_compiler = "cc"

  type cmd = {
    architecture : architecture option;
    os : os option;
    check_only : bool;
    config : bool;
    f_allow_generic_in_variadic : bool;
    no_std : bool;
    is_target_asm : bool;
    cc : bool;
    verbose : bool;
    output : string;
    pkg_configs : string list;
    cclib : string list;
    frameworks : string list;
    files : string list;
  }

  let default_outfile = "a.out"

  (* let target_archi_term =
     Arg.(
       required
       & opt (some & enum target_enum) None
       & info ~docv:"Assembly Target"
           ~doc:(doc_alts_enum ~quoted:true target_enum)
           [ "t"; "target" ]) *)

  let target_archi_term =
    Arg.(
      value
      & opt (some & enum architecture_enum) None
      & info
          ~docv:(string_of_enum architecture_enum)
          ~env:
            (Cmd.Env.info
               ~doc:
                 "If this environment variable is present, the architecture \
                  compilation target doesn't need to be explicitly set. See \
                  option $(b, --arch)"
               Variable.architecture_global_variable
            )
          ~doc:"architecture compilation target" [ "arch" ]
    )

  let check_only_term =
    Arg.(
      value & flag
      & info [ "check-only" ]
          ~doc:"Run only the parsing and type checking stages"
    )

  let os_target_term =
    Arg.(
      value
      & opt (some & enum os_enum) None
      & info ~docv:(string_of_enum os_enum)
          ~env:
            (Cmd.Env.info
               ~doc:
                 "If this environment variable is present, the os compilation \
                  target doesn't need to be explicitly set. See option $(b, \
                  --os)"
               Variable.os_global_variable
            )
          ~doc:"Os compilation target" [ "os" ]
    )

  let f_allow_generic_in_variadic_term =
    Arg.(
      value & flag
      & info
          [ "fallow-generic-variadic" ]
          ~docs:"COMPILATION OPTION"
          ~doc:
            "Allow the use of variables with a generic type in variadic \
             function parameters such as $(b,printf(3))"
    )

  let no_std_term =
    Arg.(
      value & flag & info [ "no-std" ] ~doc:"Don't include the standard library"
    )

  let verbose_term =
    Arg.(
      value & flag
      & info [ "v"; "verbose" ] ~doc:"Print each command before it execution"
    )

  let cc_term =
    Arg.(
      value & flag
      & info [ "cc" ] ~doc:"Generate executable by using a C compiler"
    )

  let target_asm_term =
    Arg.(value & flag & info [ "S" ] ~doc:"Produce assembly files")

  let pkg_config_term =
    Arg.(
      value
      & opt (list string) []
      & info [ "pkg-config"; "pc" ] ~docv:"LIBRARIES"
          ~doc:
            "A comma separated list of libraries. Invoke $(b,pkg-config)(1) to \
             retreive compilation flags and libs"
    )

  let cclib_term =
    Arg.(
      value & opt_all string []
      & info [ "l" ] ~docv:"libname" ~doc:"Pass $(i,libname) to the linker"
    )

  let framework_term =
    Arg.(
      value & opt_all string []
      & info [ "framework" ] ~docv:"framework"
          ~doc:"Pass $(i,framework) to the linker"
    )

  let output_term =
    Arg.(
      value & opt string default_outfile
      & info [ "o" ] ~docv:"FILENAME"
          ~doc:
            (Printf.sprintf "write output to <%s>"
               (String.lowercase_ascii "$(docv)")
            )
    )

  let ccol_term =
    Arg.(
      value
      & opt (list string) []
      & info [ "ccol" ] ~docv:"<C_FILES>"
          ~doc:
            "A comma separated list of c_files. Invoke the default C compiler \
             to generate object file and link those files"
    )

  (* let x =   "Input files of the compiler. Kosu files must have the extension .kosu.
     Files ending with .c are treated as C files, are compiled to object files with $(b, cc)
     and linked to the program
     Files ending with .s are treated as Assembly file, are assembled with $(b, as)
     and linked to the program
     Files ending with .o are treated as object files to be passed to the linker. \
     If --cc flag is set, files ending with .c, .s or .o are passed as it to $(b, cc)
     \ " *)

  let files_term =
    Arg.(
      value
      & pos_all Arg.non_dir_file []
      & info [] ~docv:"FILES" ~doc:"Compiler Input files"
    )

  let cmd_term run =
    let combine architecture os check_only config f_allow_generic_in_variadic
        no_std verbose cc is_target_asm output pkg_configs cclib frameworks
        files =
      run
      @@ {
           architecture;
           f_allow_generic_in_variadic;
           config;
           check_only;
           os;
           no_std;
           verbose;
           is_target_asm;
           cc;
           output;
           pkg_configs;
           cclib;
           frameworks;
           files;
         }
    in
    Term.(
      const combine $ target_archi_term $ os_target_term $ check_only_term
      $ CliCommon.config_term $ f_allow_generic_in_variadic_term $ no_std_term
      $ verbose_term $ cc_term $ target_asm_term $ output_term $ pkg_config_term
      $ cclib_term $ framework_term $ files_term
    )

  let kosuc_doc = "The Kosu compiler"

  let kosuc_man =
    [
      `S Manpage.s_description;
      `P
        "Kosu is (or will be at least I hope) a statically-typed, \
         expression-oriented language.";
      `P
        "The philosophy of Kosu is to have as control over memory as C (manual \
         memory management, pointers) while having some higher features like \
         generics or sum type.";
      `P "Kosu files must have the extension .kosu.";
      `P
        "Files ending with .c are treated as C files, are compiled to object \
         files with $(b,cc)(1)\n\
        \      and linked to the program";
      `P
        "Files ending with .s are treated as Assembly file, are assembled with \
         $(b,as)(1)\n\
        \      and linked to the program";
      `P
        "Files ending with .o are treated as object files to be passed to the \
         linker.";
      `P
        "If --cc flag is set, files ending with .c, .s or .o are passed as it \
         is to $(b,cc)(1)";
      `S Manpage.s_environment;
      `I
        ( Printf.sprintf "$(b,%s)" Variable.std_global_variable,
          "If this environment variable is present, kosu files inside the \
           folder are recessively included in the compilation except if \
           --no-std is present"
        );
      `S Manpage.s_see_also;
      `P "$(b,cc)(1), $(b,as)(1), $(b,ld)(1)";
      `Noblank;
      `P "Repository:  https://github.com/EruEri/kosu-lang";
      `S Manpage.s_authors;
      `P "Yves Ndiaye";
      `S "COPYRIGHT";
      `P "Yves Ndiaye";
      `S "LICENSE";
      `P "Kosuc is distributed under the GNU GPL-3.0";
    ]

  let kosuc run =
    let info =
      Cmd.info ~doc:kosuc_doc ~man:kosuc_man ~version:CliCommon.fversion name
    in
    Cmd.v info (cmd_term run)

  let run cmd =
    let {
      architecture;
      os;
      config;
      check_only;
      f_allow_generic_in_variadic;
      no_std;
      verbose;
      is_target_asm;
      cc;
      pkg_configs;
      output;
      files;
      frameworks;
      cclib;
    } =
      cmd
    in
    let () =
      match () with
      | _ when config ->
          let () = CliCommon.kosu_config_print () in
          ()
      | _ when files = [] ->
          ()
      | () ->
          let ( `KosuFile kosu_files,
                `CFile c_files,
                `ObjFile object_files,
                `AssemblyFile assembly_files ) =
            match CliCommon.input_file files with
            | Ok e ->
                e
            | Error e ->
                failwith @@ Printf.sprintf "unsported file %s" e
          in

          let std_file = fetch_std_file ~no_std () in

          let kosu_files = kosu_files @ std_file in

          let architecture =
            match architecture with
            | None ->
                Option.value CliCommon.default_architecture
                  ~default:CliCommon.X86_64
            | Some a ->
                a
          in

          let os =
            match os with
            | None ->
                Option.value CliCommon.default_os ~default:CliCommon.Linux
            | Some a ->
                a
          in

          let module ValidationRule : KosuFrontend.KosuValidationRule = struct end in
          let module TypeCheckerRule : KosuFrontend.TypeCheckerRule = struct
            let allow_generics_in_variadic = f_allow_generic_in_variadic
          end in
          let module Compilation_Files : KosuFrontend.Compilation_Files = struct
            let std_global_variable = Variable.std_global_variable
          end in
          let module KosuFront =
            KosuFrontend.Make (Compilation_Files) (ValidationRule)
              (TypeCheckerRule)
          in
          let module Asttyconvert =
            KosuIrTyped.Asttyconvert.Make (TypeCheckerRule) in
          let module Codegen =
            ( val match (architecture, os) with
                  | X86_64, (FreeBSD | Linux) ->
                      (module LinuxX86)
                  | X86_64, Darwin ->
                      (module Mac0SX86)
                  | Arm64, Darwin ->
                      (module MacOSAarch64)
                  | Arm64, (FreeBSD | Linux) ->
                      (module FreeBSDAarch64)
                : KosuBackend.Codegen.S
              )
          in
          let module LinkerOption =
            ( val match os with
                  | FreeBSD ->
                      (module LdSpec.FreeBSDLdSpec)
                  | Linux ->
                      (module LdSpec.LinuxLdSpec)
                  | Darwin ->
                      (module LdSpec.MacOSLdSpec)
                : KosuBackend.Compil.LinkerOption
              )
          in
          let module Compiler = KosuBackend.Compil.Make (Codegen) (LinkerOption)
          in
          let () = KosuFront.Registerexn.register_kosu_error () in

          let ast_module = KosuFront.ast_modules kosu_files in
          let typed_program =
            match Asttyconvert.from_program ast_module with
            | typed_program ->
                typed_program
            | exception KosuFrontend.Ast.Error.Ast_error e ->
                let () =
                  e |> KosuFront.Pprinterr.string_of_ast_error |> print_endline
                in
                failwith "Error while typing ast: Shouldn't append"
          in

          let () = match check_only with true -> exit 0 | false -> () in

          let tac_program =
            KosuIrTAC.Asttacconv.tac_program_of_rprogram typed_program
          in
          let _code =
            match is_target_asm with
            | true ->
                Compiler.generate_asm_only tac_program ()
            | false ->
                let args =
                  KosuBackend.Compil.{ c_files; assembly_files; object_files }
                in
                let compilation = Compiler.compilation ~cc in
                compilation ~outfile:output ~frameworks ~debug:true ~args ~cclib
                  ~verbose ~pkg_config_names:pkg_configs tac_program
          in
          ()
    in
    ()

  let eval () = run |> kosuc |> Cmd.eval ~catch:true
  let eval' = Cmd.eval_result
end

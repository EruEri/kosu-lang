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

let name = "configure"
let kosu_config = "kosu_config.mk"
let ( / ) = Filename.concat
let kosu_config_ml = "lib" / "commandline" / "cliCommon" / "kosuConfig.ml"
let default_install = "/usr" / "local"
let install_bin install = install / "bin"
let install_lib install = install / "lib"
let install_headers install = install / "include"
let install_man install = install / "share" / "man"
let install_std install = install / "share" / "kosu" / "std"

let shell command =
  let in_channel = Unix.open_process_in command in
  let content = In_channel.input_all in_channel in
  let process_status = Unix.close_process_in in_channel in
  match process_status with
  | WEXITED code when code = 0 ->
      String.trim content
  | WEXITED code | WSIGNALED code | WSTOPPED code ->
      let () = prerr_endline content in
      exit code

let shell_branch () = shell "git rev-parse --abbrev-ref HEAD"
let shell_commit_hash () = shell "git describe --always --dirty --abbrev=7"
let shell_uname () = shell "uname -s | tr A-Z a-z"
let shell_arch () = shell "uname -m"

let shell_dynlib os () =
  match String.lowercase_ascii os with "darwin" -> ".dylib" | _ -> ".so"

let kosu_version () =
  shell "cat kosu-lang.opam | grep ^version | awk '{print $2}' | sed 's/\"//g'"

let make_variable ~prefix ~bindir ~includedir ~stddir ~mandir ~libdir () =
  Out_channel.with_open_bin kosu_config
  @@ fun oc ->
  let () = Printf.fprintf oc "INSTALL_DIR=%s\n" prefix in
  let () = Printf.fprintf oc "INSTALL_BIN_DIR=%s\n" bindir in
  let () = Printf.fprintf oc "INSTALL_LIB_DIR=%s\n" libdir in
  let () = Printf.fprintf oc "INSTALL_HEADER_DIR=%s\n" includedir in
  let () = Printf.fprintf oc "INSTALL_MAN_DIR=%s\n" mandir in
  let () = Printf.fprintf oc "INSTALL_STD_DIR=%s\n" stddir in
  let () = Printf.fprintf oc "KOSU_VERSION=%s\n" @@ kosu_version () in
  ()

let linker_options_default uname =
  match String.lowercase_ascii uname with
  | "darwin" ->
      [ "syslibroot `xcrun --sdk macosx --show-sdk-path`"; "lSystem" ]
  | "freebsd" ->
      [ "lc"; "dynamic-linker /libexec/ld-elf.so.1"; "L/usr/lib" ]
  | "linux" ->
      [
        "lc"; "dynamic-linker /lib64/ld-linux-x86-64.so.2"; "L/usr/lib L/lib64";
      ]
  | _ ->
      []

let linker_args_default uname =
  match String.lowercase_ascii uname with
  | "darwin" ->
      []
  | "freebsd" ->
      [
        "/usr/lib/crt1.o";
        "/usr/lib/crti.o";
        "/usr/lib/crtbegin.o";
        "/usr/lib/crtend.o";
        "/usr/lib/crtn.o";
      ]
  | "linux" ->
      [ "/lib64/crt1.o"; "/lib64/crti.o"; "/lib64/crtn.o" ]
  | _ ->
      []

type t = {
  arch : string option;
  os : string option;
  cc : string option;
  linker_options : string list;
  linker_raw_args : string list;
  prefix : string;
  bindir : string;
  libdir : string;
  includedir : string;
  mandir : string;
  stddir : string;
}

let arch_term =
  Arg.(
    value
    & opt (some string) None
    & info [ "a"; "arch" ] ~absent:"uname -m" ~docv:"architecture Target"
        ~doc:"Indicate which architecture kosuc should target"
  )

let os_term =
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "os" ] ~absent:"uname -s (lowercased)" ~docv:"OS_Target"
        ~doc:"Indicate which os kosuc should target"
  )

let cc_term =
  Arg.(
    value
    & opt (some string) None
    & info [ "cc" ] ~absent:"cc" ~docv:"C compiler"
        ~doc:"Indicate which C compiler kosuc should invoke when needed"
  )

let linker_options_term =
  Arg.(
    value
    & opt (list string) []
    & info [ "lo" ] ~docv:"Linker options"
        ~doc:"Indicate which linker option kosuc should invoke when compiling"
  )

let linker_raw_args_term =
  Arg.(
    value
    & opt (list string) []
    & info [ "lar" ] ~docv:"Linker raw args"
        ~doc:
          "Indicate which arguments kosuc should give to the linker when \
           compiling"
  )

let term_prefix =
  let default = Some default_install in
  Arg.(
    required
    & opt ~vopt:default (some string) default
    & info [ "prefix" ] ~doc:"Indicate the root directory of the kosu install"
  )

let term_bindir =
  let default = install_bin "--prefix" in
  Arg.(
    value
    & opt (some string) None
    & info [ "bindir" ] ~absent:default
        ~doc:"Indicate the root directory of binary"
  )

let term_libdir =
  let default = install_lib "--prefix" in
  Arg.(
    value
    & opt (some string) None
    & info [ "libdir" ] ~absent:default
        ~doc:"Indicate the root directory of library"
  )

let term_includedir =
  let absent = install_headers "--prefix" in
  Arg.(
    value
    & opt (some string) None
    & info [ "includedir" ] ~absent
        ~doc:"Indicate the root directory of headers"
  )

let term_mandir =
  let absent = install_man "--prefix" in
  Arg.(
    value
    & opt (some string) None
    & info [ "mandir" ] ~absent ~doc:"Indicate the root directory of man page"
  )

let term_stddir =
  let absent = install_std "--prefix" in
  Arg.(
    value
    & opt (some string) None
    & info [ "stddir" ] ~absent
        ~doc:"Indicate the root directory of the standard library"
  )

let cmd_term run =
  let combine arch os cc linker_options linker_raw_args prefix bindir libdir
      includedir mandir stddir =
    let bindir = Option.value ~default:(install_bin prefix) bindir in
    let libdir = Option.value ~default:(install_lib prefix) libdir in
    let includedir =
      Option.value ~default:(install_headers prefix) includedir
    in
    let mandir = Option.value ~default:(install_man prefix) mandir in
    let stddir = Option.value ~default:(install_std prefix) stddir in
    run
    @@ {
         arch;
         os;
         cc;
         linker_options;
         linker_raw_args;
         prefix;
         bindir;
         libdir;
         includedir;
         mandir;
         stddir;
       }
  in
  Term.(
    const combine $ arch_term $ os_term $ cc_term $ linker_options_term
    $ linker_raw_args_term $ term_prefix $ term_bindir $ term_libdir
    $ term_includedir $ term_mandir $ term_stddir
  )

let doc = "Configure kosu compilation option"

let configure run =
  let info = Cmd.info ~doc name in
  Cmd.v info (cmd_term run)

let run cmd =
  let {
    arch;
    os;
    cc;
    linker_options;
    linker_raw_args;
    bindir;
    libdir;
    mandir;
    stddir;
    includedir;
    prefix;
  } =
    cmd
  in
  let arch = Option.value ~default:(shell_arch ()) arch in
  let os = Option.value ~default:(shell_uname ()) os in
  let os_extension = shell_dynlib os () in
  let cc = Option.value ~default:"cc" cc in
  let commit_hash = shell_commit_hash () in
  let branch = shell_branch () in

  let linker_options =
    match linker_options with
    | [] ->
        linker_options_default os
    | _ :: _ as l ->
        l
  in

  let linker_raw_args =
    match linker_raw_args with [] -> linker_args_default os | _ :: _ as l -> l
  in

  let s_linker_option =
    linker_options
    |> List.map @@ Printf.sprintf "\"%s\""
    |> String.concat "; " |> Printf.sprintf "[%s]"
  in
  let s_linker_raw_args =
    linker_raw_args
    |> List.map @@ Printf.sprintf "\"%s\""
    |> String.concat "; " |> Printf.sprintf "[%s]"
  in
  let () =
    Out_channel.with_open_bin kosu_config_ml
    @@ fun oc ->
    Printf.fprintf oc
      {| 
let kosu_target_arch = "%s"
let kosu_target_os = "%s"
let kosu_target_os_extentsion = "%s"
let kosu_target_cc = "%s" 
let kosu_target_hash = "%s"
let kosu_target_branch = "%s"
let kosu_target_headers = "%s"
let kosu_target_std_path = "%s"
let kosu_target_runtime_path = "%s"
let kosu_target_linker_option = %s
let kosu_target_linker_args = %s
let kosu_version = "%s"
|}
      arch os os_extension cc commit_hash branch includedir stddir libdir
      s_linker_option s_linker_raw_args (kosu_version ())
  in

  let () =
    make_variable ~prefix ~libdir ~includedir ~mandir ~stddir ~bindir ()
  in
  ()

let eval = Cmd.eval @@ configure @@ run
let () = exit eval

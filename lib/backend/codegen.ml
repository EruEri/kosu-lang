(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
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

open Util
open KosuIrTAC.Asttac

module type AsmProgram = sig
  type asm_function_decl
  type asm_module
  type asm_module_node
  type asm_module_path
  type named_asm_module_path
  type asm_program = named_asm_module_path list

  val asm_program_of_tac_program :
    start:string option -> tac_program -> asm_program

  val sort_asm_module : asm_module_node list -> asm_module_node list
  val string_litteral_section_start : string
  val string_litteral_section_end : string
  val string_of_asm_node : asm_module_node -> string
  val string_litteral_directive : string
  val directive_of_fsize : KosuFrontend.Ast.fsize -> string
  val filename_of_named_asm_module_path : named_asm_module_path -> string

  val asm_module_path_of_named_asm_module_path :
    named_asm_module_path -> asm_module

  val str_lit_map_of_name_asm_module :
    named_asm_module_path -> (string, stringlit_label) Hashtbl.t

  val float_lit_map_of_name_asm_module :
    named_asm_module_path ->
    (KosuFrontend.Ast.fsize * float, floatlit_label) Hashtbl.t

  val asm_module_node_list_of_asm_module : asm_module -> asm_module_node list
end

module type S = sig
  val compile_asm_from_tac_tmp :
    start:string option -> tac_program -> string list

  val compile_asm_from_tac : tac_program -> string list
end

module Make (AsmProgram : AsmProgram) : S = struct
  open AsmProgram

  let is_asm_module_empty asm_module =
    let str_lit_map = str_lit_map_of_name_asm_module asm_module in
    let float_lit_map = float_lit_map_of_name_asm_module asm_module in
    let asm_module_nodes =
      asm_module |> AsmProgram.asm_module_path_of_named_asm_module_path
      |> AsmProgram.asm_module_node_list_of_asm_module
    in
    List.length asm_module_nodes = 0
    && Hashtbl.length str_lit_map = 0
    && Hashtbl.length float_lit_map = 0

  let export_asm_module_opened_file file named_asm_module_path =
    let str_lit_map = str_lit_map_of_name_asm_module named_asm_module_path in
    let float_lit_map =
      float_lit_map_of_name_asm_module named_asm_module_path
    in
    let asm_module_nodes =
      named_asm_module_path
      |> AsmProgram.asm_module_path_of_named_asm_module_path
      |> AsmProgram.asm_module_node_list_of_asm_module
    in
    let rnodes = AsmProgram.sort_asm_module asm_module_nodes in
    let () =
      rnodes
      |> List.iter (fun node ->
             Printf.fprintf file "%s\n\n" (string_of_asm_node node))
    in

    let () =
      float_lit_map |> Hashtbl.to_seq
      |> Seq.iter (fun ((fsize, float), FLit label) ->
             let s =
               match fsize with
               | KosuFrontend.Ast.F32 ->
                   float |> Int32.bits_of_float |> Printf.sprintf "0x%lX"
               | F64 -> float |> Int64.bits_of_float |> Printf.sprintf "0x%LX"
             in
             Printf.fprintf file "%s:\n\t.%s %s\n\n" label
               (directive_of_fsize fsize) s)
    in

    let () = Printf.fprintf file "\n\t%s\n" string_litteral_section_start in
    let () =
      str_lit_map |> Hashtbl.to_seq
      |> Seq.iter (fun (str, SLit label) ->
             let string_directive = string_litteral_directive in
             Printf.fprintf file "%s:\n\t%s \"%s\"\n\n" label string_directive
               str)
    in
    let () = Printf.fprintf file "\n%s\n" string_litteral_section_end in
    ()

  let export_asm_module_tmp named_asm_module_path =
    let filename =
      named_asm_module_path |> filename_of_named_asm_module_path
      |> String.map (fun c ->
             if Char.escaped c = Filename.dir_sep then '_' else c)
    in
    match is_asm_module_empty named_asm_module_path with
    | true -> None
    | false ->
        let filename, file = Filename.open_temp_file filename ".S" in
        let () = export_asm_module_opened_file file named_asm_module_path in
        let () = close_out file in
        Some filename

  let export_asm_module named_asm_module_path =
    let filename = named_asm_module_path |> filename_of_named_asm_module_path in
    match is_asm_module_empty named_asm_module_path with
    | true -> None
    | false ->
        let file = open_out filename in
        let () = export_asm_module_opened_file file named_asm_module_path in
        let () = close_out file in
        Some filename

  let compile_asm_tmp asm_program =
    asm_program
    |> List.filter_map (fun asm_module -> export_asm_module_tmp asm_module)

  let compile_asm asm_program =
    asm_program
    |> List.filter_map (fun asm_module -> export_asm_module asm_module)

  let compile_asm_from_tac_tmp ~start tac_program =
    tac_program |> asm_program_of_tac_program ~start |> compile_asm_tmp

  let compile_asm_from_tac tac_program =
    tac_program |> asm_program_of_tac_program ~start:None |> compile_asm
end

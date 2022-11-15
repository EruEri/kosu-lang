(**********************************************************************************************)
(*                                                                                            *)
(* This file is a part of Kosu                                                                *)
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
  Clap.description "kosuc - The Kosu compiler";

  let _output = Clap.optional_string ~long:"output" ~short:'o' () in

  let _is_target_asm =
    Clap.flag ~set_short:'S' ~description:"Produce an assembly file" false
  in

  let _is_without_link =
    Clap.flag ~set_short:'c' ~description:"Produce an object file" false
  in

  let _files = Clap.list_string ~description:"files" ~placeholder:"FILES" () in

  Clap.close ();

  let modules_opt = Cli.files_to_ast_program _files in

  match modules_opt with
  | Error e -> (
      match e with
      | No_input_file -> raise (Invalid_argument "no Input file")
      | File_error (s, exn) ->
          Printf.eprintf "%s\n" s;
          raise exn
      | Filename_error _ -> raise (Invalid_argument "Filename Error")
      | Parser_Error (filename, position) ->
          position |> KosuFrontend.Pprint.string_of_position_error
          |> Printf.eprintf "File \"%s\", %s: Parser Error\n" filename;
          raise (Invalid_argument "Parser Error")
      | Lexer_Error e -> raise e)
  | Ok modules -> (
      match valide_program modules with
      | filename, Error e ->
          (* Printf.eprintf "\nFile \"%s\", %s\n" filename (Kosu_frontend.Pprint.string_of_validation_error e); *)
          raise (Error.Validation_error (filename, e))
      | _, Ok () ->
          let typed_program =
            try Astconvert.from_program modules
            with KosuFrontend.Ast.Error.Ast_error e ->
              let () =
                Printf.printf "%s\n"
                  (KosuFrontend.Pprinterr.string_of_ast_error e)
              in
              failwith ""
          in
          let () = Printf.printf "Successfult converted\n\n" in
          let _tac_program = Asttacconv.tac_program_of_rprogram typed_program in
          ())
(* let () = modules |> List.iter (fun record -> Printf.printf "module name : %s\n" record.path) in
     let { path; _module } = modules |> List.hd in
     let main =  _module
     |> Kosu_frontend.Asthelper.Module.retrieve_func_decl
     |> List.find ( fun fn -> fn.fn_name = "main")
   in
   (try
     typeof_kbody Env.create_empty_env path modules main.body
   with Ast.Error.Ast_error e -> Printf.printf "%s\n" (Asthelper.string_of_ast_error e);  failwith "" ) |> ignore *)

(* let _ =
   let file = open_in "test.kosu" in
   let source = file |> Lexing.from_channel in

   let program = Parser.modul  Lexer.main source in
   let enum_dels = Asthelper.Module.retrieve_enum_decl program in
   let struct_decls = Asthelper.Module.retrieve_struct_decl program in
   let external_fn_decls = Asthelper.Module.retrieve_external_func_decl program in
   let func_decls = Asthelper.Module.retrieve_func_decl program in

   enum_dels |> List.iter (fun s -> Printf.printf "%s\n" (Asthelper.Enum.string_of_enum_decl s));
   struct_decls |> List.iter (fun s -> Printf.printf "%s\n" (Asthelper.Struct.string_of_struct_decl s));
   external_fn_decls |> List.iter (fun s -> Printf.printf "%s\n" (Asthelper.ExternalFunc.string_of_external_func_decl s));
   func_decls |> List.iter (fun s -> Printf.printf "%s\n" (Asthelper.Function.string_of_func_decl s));
   close_in file
*)

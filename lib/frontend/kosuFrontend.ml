(*****************************************************************************************)
(*                                                                                       *)
(* This file is part of Kosu                                                             *)
(* Copyright (C) 2023 Yves Ndiaye                                                        *)
(*                                                                                       *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms       *)
(* of the GNU General Public License as published by the Free Software Foundation,       *)
(* either version 3 of the License, or (at your option) any later version.               *)
(*                                                                                       *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;     *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR      *)
(* PURPOSE.  See the GNU General Public License for more details.                        *)
(* You should have received a copy of the GNU General Public License along with Kosu.    *)
(* If not, see <http://www.gnu.org/licenses/>.                                           *)
(*                                                                                       *)
(*****************************************************************************************)


module Ast = Ast
module Pprint = Pprint
module Asthelper = Asthelper
module Position = Position
module Lexer = Lexer
module KosuParser = KosuParser
module Parser = Parser
module Typecheck = Typecheck

module type KosuValidationRule = Astvalidation.KosuValidationRule
module type TypeCheckerRule = Typecheck.TypeCheckerRuleS
module type Compilation_Files = sig
  val kosu_files: string list

  val std_global_variable: string
  val architecture_global_variable: string
  val os_global_variable: string
end

module Make (Compilation_Files: Compilation_Files) (VRule: Astvalidation.KosuValidationRule)(TyRule: Typecheck.TypeCheckerRuleS) = struct
  module Typecheck = Typecheck.Make(TyRule)
  module Astvalidation = Astvalidation.Make(VRule)(TyRule)
  module Pprinterr = Pprinterr.Make(VRule)(TyRule)
  module Registerexn = Registerexn.Make(VRule)(TyRule)

  module Ast = Ast
  module Pprint = Pprint
  module Asthelper = Asthelper
  module Position = Position
  module Lexer = Lexer
  module KosuParser = KosuParser
  module Parser = Parser

  open Astvalidation

  type filename_error = Mutiple_dot_in_filename | No_extension | Unknow_error

  type cli_error =
  | No_input_file
  | Lexer_Error of exn
  | File_error of string * exn
  | Filename_error of filename_error

  let ( >>= ) = Result.bind

  let f s =
  String.concat "::"
  @@ List.map String.capitalize_ascii
  @@ String.split_on_char '/' s

  let convert_filename_to_path filename =
  filename |> String.split_on_char '.'
  |> (function
      | [ t; _ ] -> Ok t
      | [ _ ] -> Error No_extension
      | _ :: _ :: _ -> Error Mutiple_dot_in_filename
      | _ -> Error Unknow_error)
  |> Result.map f

  let std_path = Sys.getenv_opt Compilation_Files.std_global_variable
  let module_path_of_file filename =
  let chomped_filename =
    match  std_path with
    | None -> filename
    | Some path ->
        if String.starts_with ~prefix:path filename then
          let filename_len = String.length filename in
          let dir_sep_len = String.length @@ Filename.dir_sep in
          let path_len = String.length path in
          let prefix_len = path_len + dir_sep_len in
          String.sub filename prefix_len (filename_len - prefix_len)
        else filename
  in
  (* let () = Printf.printf "filename = %s\nchomped = %s\n" filename  chomped_filename in *)
  let open Ast in
  let ( >>= ) = Result.bind in
  let lexbuf_res =
    try
      let file = open_in filename in
      let source = Lexing.from_channel file in
      at_exit (fun () -> close_in file);
      source |> Result.ok
    with e -> Error (File_error (filename, e))
  in
  lexbuf_res >>= fun lexbuf ->
  KosuParser.parse lexbuf (Parser.Incremental.modul lexbuf.lex_curr_p)
  |> Result.map_error (fun lexer_error ->
        Lexer_Error (Lexer.Lexer_Error { filename; error = lexer_error }))
  >>= fun _module ->
  chomped_filename |> convert_filename_to_path
  |> Result.map (fun path -> { filename; module_path = { path; _module } })
  |> Result.map_error (fun e -> Filename_error e)

  (**
    Takes all the kosuc files and transform into the ast.
    It also revomes all the implicit Module type function with the function
    [Kosu_frontend.Astvalidation.Help.program_remove_implicit_type_path]
  *)
  let files_to_ast_program (files : string list) =
  files |> List.map module_path_of_file |> function
  | [] -> Error No_input_file
  | l -> (
      match
        l
        |> List.find_map (fun s -> match s with Error e -> Some e | _ -> None)
      with
      | None ->
          Ok
            (l |> List.map Result.get_ok
          |> Astvalidation.Help.program_remove_implicit_type_path
            )
      | Some error -> Error error)

  
  let ast_modules =
    let modules_opt = files_to_ast_program Compilation_Files.kosu_files in
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
            raise (VError.Validation_error (filename, e))
        | _, Ok () ->
            modules
      )
  let register_kosu_error = Registerexn.register_kosu_error
end
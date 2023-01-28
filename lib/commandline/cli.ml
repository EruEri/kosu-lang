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


type filename_error = Mutiple_dot_in_filename | No_extension | Unknow_error
type archi_target = Arm64e | X86_64m | X86_64

let std_global_variable = "KOSU_STD_PATH"

let std_path = Sys.getenv_opt std_global_variable

let is_kosu_file file = file |> Filename.extension |> ( = ) ".kosu"

let archi_parse = function
  | "x86_64m" -> Some X86_64m
  | "x86_64" -> Some X86_64
  | "arm64e" -> Some Arm64e
  | _ -> None

let string_of_archi = function X86_64m | X86_64 -> "x86_64" | Arm64e -> "arm64e"

let archi_clap_type =
  Clap.typ ~name:"target" ~dummy:X86_64 ~parse:archi_parse ~show:string_of_archi

let cc_compilation outputfile ~asm ~other =
  Sys.command
    (Printf.sprintf "cc -g -o %s %s %s" outputfile
       (asm |> String.concat " ")
       (other |> String.concat " "))

let ccol_compilation cfiles =
  cfiles
  |> List.map (fun s ->
         let tmp_name = Filename.temp_file s ".o" in
         let code = Sys.command (Printf.sprintf "cc -c -o %s %s" tmp_name s) in
         if code == 0 then Ok tmp_name else Error code)

let find_error_code_opt l =
  l
  |> List.find_map (function
       | Error code when code <> 0 -> Some code
       | _ -> None)

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

let module_path_of_file filename =
  let chomped_filename = match std_path with
  | None -> filename
  | Some path ->
    if String.starts_with ~prefix:path filename 
      then 
        let filename_len = String.length filename in
        let dir_sep_len = String.length @@ Filename.dir_sep in
        let path_len = String.length path in
        let prefix_len = path_len + dir_sep_len in
        String.sub filename prefix_len (filename_len - prefix_len)
   else filename
in
  (* let () = Printf.printf "filename = %s\nchomped = %s\n" filename  chomped_filename in *)
  let open KosuFrontend in
  let open KosuFrontend.Ast in
  let ( >>= ) = Result.bind in
  ( (try
       let file = open_in filename in
       let source = Lexing.from_channel file in
       at_exit (fun () ->
           close_in file);
       source |> Result.ok
     with e -> Error (File_error (filename, e)))
  >>= fun lexbuf ->
    KosuParser.parse lexbuf (Parser.Incremental.modul lexbuf.lex_curr_p)
    |> Result.map_error (fun lexer_error -> 
      Lexer_Error ( Lexer.Lexer_Error {filename; error = lexer_error} )
    )
  )
  >>= fun _module ->
  chomped_filename 
  |> convert_filename_to_path
  |> Result.map (fun path -> 
    { filename; module_path = { path; _module } }
  )
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
           |> KosuFrontend.Astvalidation.Help.program_remove_implicit_type_path
            )
      | Some error -> Error error)



let rec fetch_kosu_file direname () = 
  let file_in_dir = Sys.readdir direname in
  let kosu_files = file_in_dir |> Array.fold_left (fun acc_kosu_files file ->
    let file = Printf.sprintf "%s%s%s" direname (Filename.dir_sep) (file) in
    if Sys.is_directory file then
      acc_kosu_files @ (fetch_kosu_file file () )
    else 
      if is_kosu_file file 
        then
          file::acc_kosu_files
    else
      acc_kosu_files
  ) [] in
  kosu_files


let fetch_std_file ~no_std () = 
  if no_std || (Option.is_none std_path ) 
    then []
  else
    let std_path = Option.get std_path in
    fetch_kosu_file std_path ()
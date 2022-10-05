open Kosu_frontend.Position

type filename_error = Mutiple_dot_in_filename | No_extension | Unknow_error

type cli_error =
  | No_input_file
  | Parser_Error of string*position
  | Lexer_Error of exn
  | File_error of string * exn
  | Filename_error of filename_error

let ( >>= ) o f = Result.bind o f

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
  let () = Kosu_frontend.Registerexn.register_kosu_error filename () in
  let open Kosu_frontend in
  let open Kosu_frontend.Ast in
  let ( >>= ) = Result.bind in
  ( (try
       let file = open_in filename in
       let source = Lexing.from_channel file in
       at_exit (fun () ->
           print_endline "At end closed";
           close_in file);
       source |> Result.ok
     with e -> Error (File_error (filename, e)))
  >>= fun lexbuf ->
    try Parser.modul Lexer.main lexbuf |> Result.ok
    with 
    | Parser.Error -> Parser_Error (filename, (Position.current_position lexbuf)) |> Result.error
    | e -> Lexer_Error e |> Result.error )
  >>= fun _module ->
  filename |> convert_filename_to_path
  |> Result.map (fun path -> { path; _module })
  |> Result.map_error (fun e -> Filename_error e)

let files_to_ast_program (files : string list) =
  files |> List.map module_path_of_file |> function
  | [] -> Error No_input_file
  | l -> (
      match
        l
        |> List.find_map (fun s -> match s with Error e -> Some e | _ -> None)
      with
      | None -> Ok (l |> List.map Result.get_ok)
      | Some error -> Error error)
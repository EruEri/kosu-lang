type filename_error =
| Mutiple_dot_in_filename
| No_extension
| Unknow_error

type cli_error = 
| No_input_file
| File_error of string*exn
| Filename_error of filename_error



let (>>=) o f = Result.bind o f

let f s = String.concat "::" @@ (List.map (String.capitalize_ascii)) @@ (String.split_on_char '/' s)
let convert_filename_to_path filename = 
  filename 
  |> String.split_on_char '.'
  |> (function | t::_::[] -> Ok t | _::[] -> Error No_extension | _::_::_ -> Error Mutiple_dot_in_filename | _ -> Error Unknow_error)
  |> Result.map ( f )

let module_path_of_file filename = 
  let open Kosu_frontend in let open Kosu_frontend.Ast in
  try
    let file = open_in filename in
    let source = Lexing.from_channel file in
    let Mod (module_node) = Parser.modul  Lexer.main source in
    let () = close_in file in
    filename 
    |> convert_filename_to_path
    |> Result.map (fun path -> {
      path = path;
      _module = Mod (module_node)
    })
    |> Result.map_error (fun e -> Filename_error e)
  with e -> Error (File_error (filename, e))
let files_to_ast_program (files: string list) = 
  files 
  |> List.map module_path_of_file 
  |> function [] -> Error No_input_file 
    | l -> 
      match l |> List.find_map (fun s -> match s with Error e -> Some e | _ -> None) with
      | None -> Ok (l |> List.map Result.get_ok)
      | Some error -> Error error
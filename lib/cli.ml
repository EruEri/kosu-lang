type cli_error = 
| No_input_file
| File_error of string*exn

let module_path_of_file filename = 
  let open Ast in
  try
    let file = open_in filename in
    let source = Lexing.from_channel file in
    let Prog (module_node) = Parser.prog  Lexer.main source in
    Ok ({
      path = filename;
      _module = Mod (module_node)
    })
  with e -> Error (File_error (filename, e))
let files_to_ast_program (files: string list) = 
  files 
  |> List.map module_path_of_file 
  |> function [] -> Error No_input_file 
    | l -> 
      match l |> List.find_map (fun s -> match s with Error e -> Some e | _ -> None) with
      | None -> Ok (l |> List.map Result.get_ok)
      | Some error -> Error error
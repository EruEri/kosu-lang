open Kosu_lang
open Ast
open Typecheck

let () = 
  Clap.description "kosuc - The Kosu compiler";

  let _output = Clap.optional_string ~long: "output" ~short: 'o' () in

  let _is_target_asm = Clap.flag ~set_short: 'S' ~description: "Produce an assembly file" false in

  let _is_without_link = Clap.flag ~set_short: 'c' ~description: "Produce an object file" false in

  let _files = Clap.list_string () in

  Clap.close ();

  let modules_opt = Kosu_lang.Cli.files_to_ast_program _files in

  match modules_opt with
  | Error e -> (match e with No_input_file -> raise (Invalid_argument "no Input file") | Cli.File_error (s, exn) -> Printf.printf "%s\n" (s); raise exn)
  | Ok modules -> 
    let { path; _module } = modules |> List.hd in
    let main =  _module 
    |> Kosu_lang.Asthelper.Module.retrieve_func_decl 
    |> List.find ( fun fn -> fn.fn_name = "main")
  in 
  print_endline path;
  (try
    typeof_kbody Env.create_empty_env path modules main.body
  with Ast.Error.Ast_error e -> Printf.printf "%s\n" (Asthelper.string_of_ast_error e);  failwith "" ) |> ignore
   

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
;;
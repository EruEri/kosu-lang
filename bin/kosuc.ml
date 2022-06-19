open Kosu_lang

let _ = 
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
;;
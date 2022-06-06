open Kosu_lang

let _ = 
  let file = open_in "test.kosu" in
  let source = file |> Lexing.from_channel in

  let program = Parser.prog  Lexer.main source in
  let enum_dels = Asthelper.Program.retrieve_enum_decl program in
  let struct_decls = Asthelper.Program.retrieve_struct_decl program in

  enum_dels |> List.iter (fun s -> Printf.printf "%s\n" (Astpp.PPEnum.string_of_enum_decl s));
  struct_decls |> List.iter (fun s -> Printf.printf "%s\n" (Astpp.PPStruct.string_of_struct_decl s));
  close_in file
;;
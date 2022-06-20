let () = 
  Clap.description "kosuc - The Kosu compiler";

  let _output = Clap.optional_string ~long: "output" ~short: 'o' () in

  let _is_target_asm = Clap.flag ~set_short: 'S' ~description: "Produce an assembly file" false in

  let _is_without_link = Clap.flag ~set_short: 'c' ~description: "Produce an object file" false in

  let _files = Clap.list_string () in

  Clap.close ()

(* let _ = 
  let file = open_in "test.kosu" in
  let source = file |> Lexing.from_channel in

  let program = Parser.prog  Lexer.main source in
  let enum_dels = Asthelper.Program.retrieve_enum_decl program in
  let struct_decls = Asthelper.Program.retrieve_struct_decl program in
  let external_fn_decls = Asthelper.Program.retrieve_external_func_decl program in

  enum_dels |> List.iter (fun s -> Printf.printf "%s\n" (Astpp.PPEnum.string_of_enum_decl s));
  struct_decls |> List.iter (fun s -> Printf.printf "%s\n" (Astpp.PPStruct.string_of_struct_decl s));
  external_fn_decls |> List.iter (fun s -> Printf.printf "%s\n" (Astpp.PPExternalFunc.string_of_external_func_decl s));
  close_in file *)
;;
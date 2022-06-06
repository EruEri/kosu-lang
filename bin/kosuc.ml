open Kosu_lang

let _ = 
  let file = open_in "test.kosu" in
  let source = file |> Lexing.from_channel in

  let program = Parser.prog  Lexer.main source in
  let Ast.Prog (nodes) = program in
  let enum_dels = nodes |> List.filter_map (fun node -> match node with Ast.NEnum e -> Some e | _ -> None) in

  enum_dels |> List.iter (fun s -> Printf.printf "%s\n" (Astpp.PPEnum.string_of_enum_decl s));
  close_in file
;;
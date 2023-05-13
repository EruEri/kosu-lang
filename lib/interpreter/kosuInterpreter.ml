(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
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

open KosuFrontend

module Make
    (Compilation_Files : Compilation_Files)
    (VRule : KosuValidationRule)
    (TyRule : Typecheck.TypeCheckerRuleS) =
struct 
  module FrontEnd = KosuFrontend.Make(Compilation_Files)(VRule)(TyRule)
  open KosuFrontend.Ast

  type ienv = {
    env: KosuFrontend.Ast.Env.t;
    modules: KosuFrontend.Ast.module_node list
  }

  let module_path_of_ienv ienv = 
    {
      _module = Mod ienv.modules;
      path = ""
    }::[]

  let empty_env = {
    env = Env.empty;
    modules = []
  }

  let print_welcome () = 
    Printf.printf "%s"
  let expression_prompt ktype () = 
    Printf.printf "- : %s\n%!" (Pprint.string_of_ktype ktype)

  let rec repl ienv () = 
    let () = Printf.printf "> %!" in
    let lexbuf = Lexing.from_channel stdin in
    let inode = KosuParser.kosu_repl_parse lexbuf (Parser.Incremental.iexpression_node lexbuf.lex_curr_p) in
    match inode with
    | Error e -> 
      let s = FrontEnd.Registerexn.string_of_lexer_error "" e in
      let () = Printf.printf "%s\n" s in
      repl ienv ()
    | Ok (None) -> Printf.printf "\n%!"
    | Ok (Some inode) -> begin 
      try
        match inode with
        | IExpression kexpression -> 
          let typeof = FrontEnd.Typecheck.typeof 
            ~generics_resolver:(Hashtbl.create 0)
            ienv.env 
            "" 
            (module_path_of_ienv ienv)
            kexpression
          in
          let () = expression_prompt typeof () in
          repl ienv ()
        | IStatement _stmt -> failwith ""
        | IModule_Node _node -> failwith ""   
      with 
        | FrontEnd.Astvalidation.VError.Validation_error (_, _) -> 
          let () = Printf.printf "Validation error\n" in
          repl ienv ()
        | FrontEnd.Ast.Error.Ast_error ast_error -> 
          let () = Printf.printf "%s\n" (FrontEnd.Pprinterr.string_of_ast_error ast_error) in
          repl ienv ()
    end

  let repl ~welcome () = 
    let () = Printf.printf "%s\n\n" welcome in
    repl empty_env ()

end
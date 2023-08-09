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
  module FrontEnd = KosuFrontend.Make (Compilation_Files) (VRule) (TyRule)
  open KosuFrontend.Ast

  type ienv = {
    env : KosuFrontend.Ast.Env.t;
    modules : KosuFrontend.Ast.module_node list;
  }

  let module_path_of_ienv ienv = { _module = Mod ienv.modules; path = "" } :: []
  let empty_ienv = { env = Env.empty; modules = [] }
  let replace_env env ienv = { ienv with env }
  let add_node node ienv = { ienv with modules = node :: ienv.modules }

  let string_of_parameters kts =
    let open Position in
    kts
    |> List.map (fun kt -> Pprint.string_of_ktype kt.v)
    |> String.concat ", "

  let string_of_node =
    let open Position in
    function
    | NExternFunc external_func_decl ->
        Printf.sprintf "external %s : (%s) : %s  = <external fun>"
          external_func_decl.sig_name.v
          (string_of_parameters external_func_decl.fn_parameters)
          (Pprint.string_of_ktype external_func_decl.r_type.v)
    | NFunction function_decl ->
        Printf.sprintf "fn %s : (%s) : %s = <fun>" function_decl.fn_name.v
          (function_decl.parameters
          |> List.map (fun (_, kt) -> Pprint.string_of_ktype kt.v)
          |> String.concat ", "
          )
          (Pprint.string_of_ktype function_decl.return_type.v)
    | NOperator operator_decl ->
        let operator =
          Position.value
          @@ Asthelper.ParserOperator.backticked_operator operator_decl
        in
        let return_type =
          Position.value @@ Asthelper.ParserOperator.return_ktype operator_decl
        in
        let parameters = Asthelper.ParserOperator.parameters operator_decl in
        Printf.sprintf "op ( %s ) : (%s) : %s = <operator fun>" operator
          (string_of_parameters parameters)
          (Pprint.string_of_ktype return_type)
    | NSyscall syscall_decl ->
        Printf.sprintf "syscall %s : (%s) : %s = <%Lu>"
          syscall_decl.syscall_name.v
          (string_of_parameters syscall_decl.parameters)
          (Pprint.string_of_ktype syscall_decl.return_type.v)
          syscall_decl.opcode.v
    | NStruct struct_decl ->
        Printf.sprintf "%s" (Pprint.string_of_struct_decl struct_decl)
    | NEnum enum_decl ->
        Printf.sprintf "%s" (Pprint.string_of_enum_decl enum_decl)
    | NConst const_decl ->
        Printf.sprintf "%s" (Pprint.string_of_const_decl const_decl)
    | NOpaque s ->
        Printf.sprintf "external opaque type %s" s.v

  let expression_prompt ~variable ktype () =
    let name = variable |> Option.value ~default:" - " in
    Printf.printf "%s : %s\n%!" name (Pprint.string_of_ktype ktype)

  let rec repl ienv () =
    let () = Printf.printf "> %!" in
    let lexbuf = Lexing.from_channel stdin in
    let inode =
      KosuParser.kosu_repl_parse lexbuf
        (Parser.Incremental.iexpression_node lexbuf.lex_curr_p)
    in
    match inode with
    | Error e ->
        let s = FrontEnd.Registerexn.string_of_lexer_error "" e in
        let () = Printf.printf "%s\n" s in
        repl ienv ()
    | Ok None ->
        Printf.printf "\n%!"
    | Ok (Some inode) -> (
        try
          match inode with
          | IExpression kexpression ->
              let typeof =
                FrontEnd.Typecheck.typeof ~constraint_type:None
                  ~generics_resolver:(Hashtbl.create 0) ienv.env ""
                  (module_path_of_ienv ienv) kexpression
              in
              let () = expression_prompt ~variable:None typeof () in
              repl ienv ()
          | IStatement stmt ->
              let new_env, typeof =
                FrontEnd.Typecheck.typeof_statement
                  ~generics_resolver:(Hashtbl.create 0) ienv.env ""
                  (module_path_of_ienv ienv) stmt
              in
              let variable =
                match stmt with
                | SDeclaration { variable_name; _ } ->
                    Some variable_name.v
                | _ ->
                    None
              in
              let () = expression_prompt ~variable typeof () in
              let env = replace_env new_env ienv in
              repl env ()
          | IModule_Node node ->
              let () =
                match
                  FrontEnd.Astvalidation.validate_module_node
                    (module_path_of_ienv ienv) "" node
                with
                | Ok () ->
                    ()
                | Error e ->
                    raise
                    @@ FrontEnd.Astvalidation.VError.Validation_error ("", e)
              in
              let ienv = add_node node ienv in
              let () = Printf.printf "%s\n%!" (string_of_node node) in
              repl ienv ()
        with
        | FrontEnd.Astvalidation.VError.Validation_error (_, _) ->
            let () = Printf.printf "Validation error\n" in
            repl ienv ()
        | FrontEnd.Ast.Error.Ast_error ast_error ->
            let () =
              Printf.printf "%s\n"
                (FrontEnd.Pprinterr.string_of_ast_error ast_error)
            in
            repl ienv ()
      )

  let repl ~welcome () =
    let () = Printf.printf "%s\n\n" welcome in
    repl empty_ienv ()
end

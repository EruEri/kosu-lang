(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022-2023 Yves Ndiaye                                                        *)
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

open KosuIrTyped.Asttyped

module MacOSAarch64AsmSpec : Aarch64AsmSpec.Aarch64AsmSpecification = struct
  open Printf

  let main = "_main"

  let function_directives fn_name =
    [ sprintf ".globl %s" fn_name; ".p2align 4" ]

  let constant_directives const_name = function
    | `IntVal (size, _) ->
        let _align_size = (KosuFrontend.Ast.Isize.size_of_isize size / 8) - 1 in
        [ Printf.sprintf ".globl %s" const_name; sprintf ".p2align %u" 2 ]
    | `StrVal _ ->
        [
          Printf.sprintf ".globl %s" const_name; Printf.sprintf ".p2align %u" 3;
        ]

  let comment_prefix = "#"
  let label_prefix = "_"

  let string_litteral_section_start =
    ".section\t__TEXT,__cstring,cstring_literals,"

  let string_litteral_section_end = ".subsections_via_symbols"
  let string_litteral_directive = ".asciz"

  let size_directive_of_size =
    let open KosuFrontend.Ast in
    function I8 -> "byte" | I16 -> "value" | I32 -> "long" | I64 -> "quad"

  (** Replace the `:` in the path name by `_` *)
  let asm_module_path = String.map (fun c -> if c = ':' then '_' else c)

  let label_of_constant ?module_path const_name =
    Printf.sprintf "%s%s"
      (module_path |> Option.map asm_module_path
      |> Option.map (Printf.sprintf "%s%s._" label_prefix)
      |> Option.value ~default:"")
      const_name

  let label_of_function ~label_prefix ~main ~module_path ~fn_name ~generics =
    if fn_name = "main" then main
    else
      Printf.sprintf "%s%s.%s%s" label_prefix
        (asm_module_path module_path)
        (if generics = [] then ""
        else generics |> String.concat "." |> Printf.sprintf "_%s_")
        fn_name

  let label_of_external_function rextern_func_decl =
    rextern_func_decl.c_name
    |> Option.value ~default:rextern_func_decl.rsig_name
    |> Printf.sprintf "%s%s" label_prefix

  let label_of_kosu_function ~module_path
      (rfunction_decl : KosuIrTyped.Asttyped.rfunction_decl) =
    label_of_function ~module_path ~main ~label_prefix
      ~fn_name:rfunction_decl.rfn_name ~generics:rfunction_decl.generics

  let label_of_tac_function ~module_path
      (tac_function_decl : KosuIrTAC.Asttac.tac_function_decl) =
    label_of_function ~module_path ~main ~label_prefix
      ~fn_name:tac_function_decl.rfn_name ~generics:tac_function_decl.generics

  let label_of_bin_operator (op : KosuFrontend.Ast.parser_binary_op) ktypes =
    Printf.sprintf "%s%s.%s" label_prefix
      (KosuFrontend.Asthelper.ParserOperator.string_name_of_parser_binary op)
      (ktypes
      |> List.map KosuIrTyped.Asttypprint.string_of_label_rktype
      |> String.concat "_")

  let label_of_unary_operator (op : KosuFrontend.Ast.parser_unary_op) ktypes =
    Printf.sprintf "%s%s.%s" label_prefix
      (KosuFrontend.Asthelper.ParserOperator.string_name_of_parser_unary op)
      (ktypes
      |> List.map KosuIrTyped.Asttypprint.string_of_label_rktype
      |> String.concat "_")

  let label_of_kosu_operator ~(module_path : string) =
    let open KosuIrTyped.Asttyped in
    let _ = module_path in
    function
    | RUnary { op; rfield; return_type; _ } ->
        label_of_unary_operator op [ snd rfield; return_type ]
    | RBinary { op; rfields; return_type; _ } ->
        label_of_bin_operator op
          [ snd @@ fst rfields; snd @@ snd rfields; return_type ]

  let label_of_tac_operator ~(module_path : string) =
    let open KosuIrTAC.Asttac in
    let _ = module_path in
    (* Silence unsued warning *)
    function
    | TacUnary { op; rfield; return_type; _ } ->
        label_of_unary_operator op [ snd rfield; return_type ]
    | TacBinary { op; rfields; return_type; _ } ->
        label_of_bin_operator op
          [ snd @@ fst rfields; snd @@ snd rfields; return_type ]
end

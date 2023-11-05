(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of San: A 3 address code language/compiler                               *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* San is free software: you can redistribute it and/or modify it under the terms             *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* San is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;           *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with San.          *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open SanTyAst
open SanUtil

module Module = struct
  let post_incr ref =
    let x = !ref in
    incr ref;
    x

  let make_label ref =
    let n = post_incr ref in
    if n = 0 then
      Printf.sprintf "l_str"
    else
      Printf.sprintf "l_str.%u" n

  let collect_string_litteral_atom ~count map atom () =
    match atom.atom with
    | String s ->
        let label = make_label count in
        Hashtbl.replace map s (StrLab label)
    | Int _ | Variable _ | Boolean _ ->
        ()

  let collect_string_litteral_rvalue ~count map rvalue () =
    match rvalue.san_rvalue with
    | TyRVExpr typed_atom | TYRVUnary { ty_atom = typed_atom; unop = _ } ->
        collect_string_litteral_atom ~count map typed_atom ()
    | TYRVBinary binary ->
        let () = collect_string_litteral_atom ~count map binary.tylhs () in
        collect_string_litteral_atom ~count map binary.tyrhs ()
    | TyRVFunctionCall fncall ->
        fncall.parameters
        |> List.iter (fun atom ->
               collect_string_litteral_atom ~count map atom ()
           )
    | TYRVLater _ | TyRVDiscard _ ->
        ()

  let collect_string_litteral_stmt ~count map stmt () =
    match stmt with
    | TySSDeclaration (_, rvalue) ->
        collect_string_litteral_rvalue ~count map rvalue ()

  let collect_string_litteral_ending ~count map ending () =
    match ending with
    | TySE_return atom | TYSE_If { expr = atom } ->
        collect_string_litteral_atom ~count map atom ()

  let collect_string_litteral_block ~count map block () =
    let () =
      block.statements
      |> List.iter (fun stmt -> collect_string_litteral_stmt ~count map stmt ())
    in

    block.ending
    |> Option.iter (fun ending ->
           collect_string_litteral_ending ~count map ending ()
       )

  let collect_string_litteral_function ~count map ty_san_function () =
    ty_san_function.san_basic_blocks
    |> List.iter (fun ty_san_basic_block ->
           collect_string_litteral_block ~count map ty_san_basic_block ()
       )

  let collect_string_litteral_module san_module () =
    let map = Hashtbl.create 7 in
    let () =
      san_module
      |> List.iter (function
           | TyDeclaration san_function ->
               collect_string_litteral_function ~count:(ref 0) map san_function
                 ()
           | TyExternal _ ->
               ()
           )
    in
    map
end

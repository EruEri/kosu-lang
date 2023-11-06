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

type suppoted_file = SF_Kosu | SF_C | SF_Object | SF_Assembly | SF_Archive

let extension_list =
  [
    (SF_C, ".c");
    (SF_Object, ".o");
    (SF_Kosu, ".kosu");
    (SF_Assembly, ".s");
    (SF_Archive, ".a");
  ]

let rev_extension_list =
  let swap (a, b) = (b, a) in
  List.map swap extension_list

let rec input_file ~kosu ~c ~co ~assembly ~arc = function
  | [] ->
      Ok
        (List.rev kosu, List.rev c, List.rev co, List.rev assembly, List.rev arc)
  | t :: q ->
      let ( let* ) = Result.bind in
      let filekind = List.assoc_opt (Filename.extension t) rev_extension_list in
      let* kind =
        match filekind with None -> Error t | Some kind -> Ok kind
      in
      let kosu, c, co, assembly, arc =
        match kind with
        | SF_C ->
            (kosu, t :: c, co, assembly, arc)
        | SF_Object ->
            (kosu, c, t :: co, assembly, arc)
        | SF_Kosu ->
            (t :: kosu, c, co, assembly, arc)
        | SF_Assembly ->
            (kosu, c, co, t :: assembly, arc)
        | SF_Archive ->
            (kosu, c, co, assembly, t :: arc)
      in
      input_file ~kosu ~c ~co ~assembly ~arc q

let input_file files =
  match input_file ~kosu:[] ~c:[] ~co:[] ~assembly:[] ~arc:[] files with
  | Ok (kosu, c, co, assembly, arc) ->
      Ok
        ( `KosuFile kosu,
          `CFile c,
          `ObjFile co,
          `AssemblyFile assembly,
          `ArchiveFile arc
        )
  | Error _ as e ->
      e

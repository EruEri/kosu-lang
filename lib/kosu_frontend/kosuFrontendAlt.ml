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

module ExitCode = struct
  let validation_error = 2
  let syntax_lexer_error = 3
  let unsported_file = 4
  let fatal_error = 5
end

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

let parse files =
  KosuError.Reporter.run ~emit:KosuError.Term.display ~fatal:(fun kosu_dig ->
      let () = KosuError.Term.display kosu_dig in
      exit ExitCode.fatal_error
  )
  @@ fun () ->
  let ( `KosuFile kosu_files,
        `CFile _c_files,
        `ObjFile _object_files,
        `AssemblyFile _assembly_files,
        `ArchiveFile _arc ) =
    match input_file files with
    | Ok e ->
        e
    | Error e ->
        let () = KosuReport.emitf @@ KosuError.Raw.unsupported_file e in
        exit ExitCode.unsported_file
  in
  let kosu_program =
    match KosuParsing.kosu_program kosu_files with
    | Ok kosu_program ->
        kosu_program
    | Error e ->
        let () = KosuReport.emitf @@ KosuError.Raw.analytics_error e in
        exit ExitCode.syntax_lexer_error
  in
  let () =
    match KosuValidation.validate kosu_program with
    | Ok () ->
        ()
    | Error (file, error) ->
        let () = KosuReport.emitf ~file error in
        exit ExitCode.validation_error
  in
  kosu_program

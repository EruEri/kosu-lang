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

(* module Ast = KosuAst
   module Env = KosuEnv
   module TypeChecking = KosuTypechecking
   module Type = KosuType
   module Parsing = KosuParsing

   module Error = struct
     include KosuError

     let register_exn () =
       Printexc.register_printer (function
         | KosuError.KosuRawErr kosu ->
             Option.some @@ KosuPrint.string_of_kosu_error String.empty kosu
         | KosuError.KosuErr (filename, kosu) ->
             Option.some @@ KosuPrint.string_of_kosu_error filename kosu
         | _ ->
             None
         )
   end

   module Reporter = KosuReport
   module Print = KosuPrint
   module Validation = KosuValidation *)

type suppoted_file = SF_Kosu | SF_C | SF_Object | SF_Assembly

let extension_list =
  [ (SF_C, ".c"); (SF_Object, ".o"); (SF_Kosu, ".kosu"); (SF_Assembly, ".s") ]

let rev_extension_list =
  let swap (a, b) = (b, a) in
  List.map swap extension_list

let rec input_file ~kosu ~c ~co ~assembly = function
  | [] ->
      Ok (List.rev kosu, List.rev c, List.rev co, List.rev assembly)
  | t :: q ->
      let ( let* ) = Result.bind in
      let filekind = List.assoc_opt (Filename.extension t) rev_extension_list in
      let* kind =
        match filekind with None -> Error t | Some kind -> Ok kind
      in
      let kosu, c, co, assembly =
        match kind with
        | SF_C ->
            (kosu, t :: c, co, assembly)
        | SF_Object ->
            (kosu, c, t :: co, assembly)
        | SF_Kosu ->
            (t :: kosu, c, co, assembly)
        | SF_Assembly ->
            (kosu, c, co, t :: assembly)
      in
      input_file ~kosu ~c ~co ~assembly q

let input_file files =
  match input_file ~kosu:[] ~c:[] ~co:[] ~assembly:[] files with
  | Ok (kosu, c, co, assembly) ->
      Ok (`KosuFile kosu, `CFile c, `ObjFile co, `AssemblyFile assembly)
  | Error _ as e ->
      e

let parse files =
  KosuError.Reporter.run ~emit:KosuError.Term.display ~fatal:(fun kosu_dig ->
      let () = KosuError.Term.display kosu_dig in
      exit 5
  )
  @@ fun () ->
  let ( `KosuFile kosu_files,
        `CFile _c_files,
        `ObjFile _object_files,
        `AssemblyFile _assembly_files ) =
    match input_file files with
    | Ok e ->
        e
    | Error e ->
        let () = KosuReport.fatalf @@ KosuError.Raw.unsupported_file e in
        exit 2
  in
  let kosu_program =
    match KosuParsing.kosu_program kosu_files with
    | Ok kosu_program ->
        kosu_program
    | Error e ->
        let () = KosuReport.fatalf @@ KosuError.Raw.analytics_error e in
        exit 3
  in
  let () =
    match KosuValidation.validate kosu_program with
    | Ok () ->
        ()
    | Error (file, error) ->
        let () = KosuReport.fatalf ~file error in
        exit 4
  in
  kosu_program

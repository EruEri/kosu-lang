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

open Cmdliner

let name = "kosu"
let kosu_doc = "The Kosu interpreter"

let kosu_man =
  [
    `S Manpage.s_description;
    `P
      "Kosu is (or will be at least I hope) a statically-typed, \
       expression-oriented language.";
    `P
      "The philosophy of Kosu is to have as control over memory as C (manual \
       memory management, pointers) while having some higher features like \
       generics or sum type.";
    `S Manpage.s_see_also;
    `P "$(b,kosuc)(1)";
    `Noblank;
    `P "Repository:  https://github.com/EruEri/kosu-lang";
    `S Manpage.s_authors;
    `P "Yves Ndiaye";
    `S "COPYRIGHT";
    `P "Yves Ndiaye";
    `S "LICENSE";
    `P "Kosuc is distributed under the GNU GPL-3.0";
  ]

module Cli = struct
  let kosu =
    let info =
      Cmd.info ~doc:kosu_doc ~man:kosu_man ~version:CliCommon.version name
    in
    Cmd.group info [ KosuRepl.repl; KosuCfgSubc.cfg; KosuRun.run ]

  let eval () = Cmd.eval @@ kosu
end

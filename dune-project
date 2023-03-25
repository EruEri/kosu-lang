(lang dune 3.1)

;**********************************************************************************************;
;*                                                                                            *;
;* This file is part of Kosu                                                                  *;
;* Copyright (C) 2022 Yves Ndiaye                                                             *;
;*                                                                                            *;
;* Kosu is free software: you can redistribute it and/or modify it under the terms            *;
;* of the GNU General Public License as published by the Free Software Foundation,            *;
;* either version 3 of the License, or (at your option) any later version.                    *;
;*                                                                                            *;
;* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          *;
;* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *;
;* PURPOSE.  See the GNU General Public License for more details.                             *;
;* You should have received a copy of the GNU General Public License along with Kosu.         *;
;* If not, see <http://www.gnu.org/licenses/>.                                                *;
;*                                                                                            *;
;**********************************************************************************************;

(using menhir 2.0)

(name kosu_lang)
(version "0.0.1")

(cram enable)

(generate_opam_files true)

(source
 (github EruEri/kosu-lang))

(authors "EruEri")

(maintainers "EruEri")

(license GPL-3.0)

; (documentation https://url/to/documentation)



(package
 (name kosu_lang)
 (synopsis "A programming language project")
 (description "A programming language that I hope will compile on arm64 and maybe one day on more platforms through the LLVM")
 (depends ocaml dune menhir cmdliner unix)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

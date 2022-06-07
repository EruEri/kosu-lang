(lang dune 3.1)

(using menhir 2.0)

(name kosu_lang)

(generate_opam_files true)

(source
 (github EruEri/kosu-lang))

(authors "EruEri")

(maintainers "EruEri")

(license LICENSE)

; (documentation https://url/to/documentation)



(package
 (name kosu_lang)
 (synopsis "A programming language project")
 (description "A programming language that I hope will compile on arm64 and maybe one day on all platform through the LLVM")
 (depends ocaml dune menhir)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

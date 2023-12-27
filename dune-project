(lang dune 3.9)

(name ocaml_autoexamples)

(generate_opam_files true)

(source
 (github jku20/ocaml_autoexamples))

(authors "Jeremy")

(maintainers "Jeremy")

(license LICENSE)

(package
 (name ocaml_autoexamples)
 (synopsis "A ppx to automatically generate example usage of functions")
 (description "A ppx to automatically generate example usage of functions")
 (depends ocaml dune ppxlib (core :with-test)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

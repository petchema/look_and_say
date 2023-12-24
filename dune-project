(lang dune 3.11)

(name look_and_say)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Pierre Etchemaite")

(maintainers "Pierre Etchemaite")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name look_and_say)
 (synopsis "Compute Look And Say sequence")
 (description "Output one item from Conway's Look And Say sequence https://oeis.org/A005150")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(lang dune 3.0)

(name ocaml-index-stats)

(generate_opam_files true)

(source
 (github Julow/ocaml-index-stats))

(authors "Jules Aguillon <jules@j3s.fr>")

(maintainers "Jules Aguillon <jules@j3s.fr>")

(license MIT)

(package
 (name ocaml-index-stats)
 (synopsis "Print occurrences of every items in signatures")
 (depends
  (ocaml
   (>= 5.3))
  cmdliner
  fpath
  (merlin-lib
   (>= 5.5))
  (ocaml-index :with-test)))

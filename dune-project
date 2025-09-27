(lang dune 3.19)

(name arrow)

(generate_opam_files true)

(source
 (github mtelvers/arrow))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name arrow)
 (synopsis "OCaml bindings for Apache Arrow")
 (description "OCaml bindings for Apache Arrow using standard OCaml libraries")
 (depends
  ocaml
  dune
  ctypes
  ctypes-foreign
  dune-configurator
  ptime
  (alcotest :with-test))
 (tags
  ("arrow" "data" "analytics")))

(package
 (name parquet)
 (synopsis "OCaml bindings to Apache Parquet columnar file format")
 (description "OCaml bindings to Apache Parquet, providing efficient columnar storage with compression support for analytical workloads.")
 (depends
  ocaml
  dune
  ctypes
  ctypes-foreign
  arrow
  cmdliner
  unix
  dune-configurator
  (alcotest :with-test))
 (tags
  ("parquet" "data" "analytics")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

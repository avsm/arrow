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
 (synopsis "Apache Arrow and Parquet in OCaml")
 (description "OCaml bindings for Apache Arrow and Parquet, providing efficient columnar storage with compression support for analytical workloads.")
 (depends
  ocaml
  dune
  ctypes
  ctypes-foreign
  dune-configurator
  ptime
  cmdliner
  (alcotest :with-test))
 (tags
  ("arrow" "data" "analytics")))

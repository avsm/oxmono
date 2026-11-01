(lang dune 3.21)
(name yamlt)

(generate_opam_files true)

(license ISC)
(authors "Anil Madhavapeddy")
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(source (tangled anil.recoil.org/ocaml-yamlt))

(package
 (name yamlt)
 (synopsis "YAML codec using Jsont type descriptions")
 (description "Allows the same Jsont.t codec definitions to work for both JSON and YAML")
 (depends
  (ocaml (>= 4.14.0))
  yamlrw
  jsont
  bytesrw
  (odoc :with-doc)))

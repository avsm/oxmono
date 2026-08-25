(lang dune 3.19)
(using ctypes 0.3)

(name proj)

(generate_opam_files true)

(source
 (github geocaml/ocaml-proj))

(authors "Patrick Ferris <patrick@sirref.org>")

(maintainers "Patrick Ferris <patrick@sirref.org>")

(license ISC)

(package
 (name proj)
 (synopsis "Platform-agnostic bindings to PROJ4")
 (description "C and Javascript OCaml bindings to the geospatial projection library PROJ4")
 (depends
   ocaml
   ctypes)
 (tags
  ("org:geocaml" "geospatial" "projections")))

(package
  (name proj_c)
  (synopsis "C bindings to PROJ4")
  (depends
    ocaml
    ctypes
    (alcotest :with-test))
   (tags ("org:geocaml" "geospatial" "projections")))

(package
  (name proj_js)
  (synopsis "Javascript bindings to PROJ4")
  (depends
    ocaml
    brr
    (alcotest :with-test))
   (tags ("org:geocaml" "geospatial" "projections")))


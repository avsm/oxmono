(lang dune 3.21)
(name root)
(generate_opam_files true)

(package
 (name root)
 (synopsis "OxCaml monorepo root package")
 (allow_empty)
 (depends
   httpz
 ))

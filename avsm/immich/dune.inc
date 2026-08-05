; Generated rules for OpenAPI code regeneration
; Run: dune build @gen --auto-promote

(rule
 (alias gen)
 (mode (promote (until-clean)))
 (targets immich.ml immich.mli)
 (deps immich-openapi-specs.json)
 (action
  (run openapi-gen generate -o . -n immich %{deps})))

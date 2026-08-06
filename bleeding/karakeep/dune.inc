; Generated rules for OpenAPI code regeneration
; Run: dune build @gen --auto-promote

(rule
 (alias gen)
 (mode (promote (until-clean)))
 (targets karakeep.ml karakeep.mli)
 (deps karakeep-openapi-spec.json)
 (action
  (run openapi-gen generate -o . -n karakeep %{deps})))

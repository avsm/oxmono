; Generated rules for OpenAPI code regeneration
; Run: dune build @gen --auto-promote

(rule
 (alias gen)
 (mode (promote (until-clean)))
 (targets ecosystems.ml ecosystems.mli)
 (deps ecosystems-openapi-spec.yaml)
 (action
  (run openapi-gen generate --code-only -o . -n ecosystems %{deps})))

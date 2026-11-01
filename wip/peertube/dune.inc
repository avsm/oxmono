; Generated rules for OpenAPI code regeneration
; Run: dune build @gen --auto-promote

(rule
 (alias gen)
 (mode (promote (until-clean)))
 (targets peer_tube.ml peer_tube.mli)
 (deps peertube-openapi.yaml)
 (action
  (run openapi-gen generate --code-only -o . -n peer_tube %{deps})))

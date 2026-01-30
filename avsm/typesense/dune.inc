(rule
 (alias gen)
 (mode (promote (until-clean)))
 (targets typesense.ml typesense.mli)
 (deps typesense-openapi.yaml)
 (action
  (run openapi-gen generate -o . -n typesense %{deps})))

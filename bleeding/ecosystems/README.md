# ecosystems

An OCaml client for the read-only [packages.ecosyste.ms][api] API. The core is
generated from the upstream OpenAPI 3.0.1 specification. A small library,
`ecosystems.client`, adds a constructor and a page iterator.

[api]: https://packages.ecosyste.ms/docs/api/v1/openapi.yaml

## Usage

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let c = Ecosystems_client.create ~sw env in
  Ecosystems_client.pages (fun ~page ~per_page ->
      Ecosystems.Registry.get_registries ~page ~per_page c ())
  |> Seq.iter (fun r -> print_endline (Ecosystems.Registry.T.name r))
```

An operation lives in the module of the schema it returns, as in
`Ecosystems.Registry.get_registries`. Operations that return strings or raw
JSON live in `Ecosystems.Client`. Each takes its path parameters as
labels and its query parameters as optional arguments.

- `page` and `per_page` are strings, because the generator types integer query
  parameters as strings. `Ecosystems_client.pages` converts for you.
- `bulk_lookup_packages` takes `~body:Jsont.json`. The generator leaves the
  spec's inline request object opaque.

## Regenerating

`ecosystems.ml` and `ecosystems.mli` are targets of the `@gen` rule, so every
build regenerates them. `dune build @bleeding/ecosystems/gen --auto-promote`
regenerates them on demand from `ecosystems-openapi-spec.yaml`. A hand edit is
overwritten. To detect drift, build and then run
`git diff --exit-code bleeding/ecosystems`.

## Pinned spec

The pinned file is upstream version 1.1.0 of
`https://packages.ecosyste.ms/docs/api/v1/openapi.yaml`, fetched on 2026-10-03
and patched in the places below. Each patch makes the schema accept a response
the live API sends. Test fixtures in `test/fixtures/` are recorded responses
that prove each one. No fixture covers `VersionWithPackage`, because
`/registries/{registryName}/versions` answered 500 for every registry on
2026-10-03.

- `Registry`: `downloads` and `purl_type` are no longer required.
- `Package`: `docker_dependents_count`, `docker_downloads_count`, `critical`
  and `issue_metadata` are nullable.
- `Version`: `codemeta_url` is no longer required.
- `Maintainer`: `total_downloads` and `role` are no longer required.
- `Namespace`: `uuid` is no longer required.
- `Keyword`: `packages_url` is no longer required.
- `KeywordWithPackages`: `packages_url` is no longer required and
  `packages_count` is nullable.
- `CodeMeta`: `dateCreated`, `dateModified` and `datePublished` have
  `format: date`.

To re-pin, fetch the new upstream file, reapply the list, run
`dune build @bleeding/ecosystems/all @bleeding/ecosystems/runtest --force`,
and re-record any fixture that no longer decodes. Do not edit a fixture to
make a test pass.

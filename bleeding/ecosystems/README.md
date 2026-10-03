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

## Command line

`oecosystems` queries the API from a shell. Run it with
`dune exec oecosystems -- COMMAND`. It is in the `ecosystems-cli` package.

| Command | Shows |
| --- | --- |
| `registries` | the registries |
| `package REGISTRY NAME` | one package |
| `versions REGISTRY NAME` | the versions of a package |
| `version REGISTRY NAME NUMBER` | one version and its dependencies |
| `dependents REGISTRY NAME` | the packages that depend on a package |
| `advisories REGISTRY NAME` | the advisories on a package |
| `lookup TARGET` | packages for a `pkg:` URL or a repository URL |
| `maintainer REGISTRY LOGIN` | one maintainer |
| `keyword NAME` | a keyword and some of its packages |

Output is a short summary. `--json` prints the decoded response instead, so
fields the spec does not describe are left out. The commands that list items
also take `--limit N`, which stops after N items. For `keyword` it limits the
packages shown. `dependents` stops at 100 items unless `--limit` says
otherwise. `--base-url` and `--user-agent` override the defaults. A failed
request prints one line on stderr and exits with code 1.

```
$ oecosystems advisories npmjs.org minimist
CRITICAL   Prototype Pollution in minimist (GHSA-xvch-5gv4-984h, CVE-2021-44906)
MODERATE   Prototype Pollution in minimist (GHSA-vh95-rmgr-6w4m, CVE-2020-7598)
MODERATE   Withdrawn: ESLint dependencies are vulnerable (ReDoS and Prototype Pollution) (GHSA-7fhm-mqm4-2wp7)

$ oecosystems lookup pkg:npm/minimist
npmjs.org      minimist                       1.2.8        parse argument options

$ oecosystems package crates.io itoa
name         itoa
ecosystem    cargo
latest       1.0.18
licenses     MIT OR Apache-2.0
description  Fast integer primitive to string conversion
homepage     -
downloads    1409199747 total
dependents   291 packages, 76076 repositories
advisories   0
purl         pkg:cargo/itoa
```

## Regenerating

`ecosystems.ml` and `ecosystems.mli` are generated from
`ecosystems-openapi-spec.yaml`. Any build of the library rebuilds them, so a
hand edit is overwritten. `dune build @bleeding/ecosystems/gen --auto-promote`
also copies the result into the source tree, which is how a new spec reaches
the checked-in files. To detect drift, build and then run
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
- `Package`: `docker_dependents_count`, `docker_downloads_count`, `critical`,
  `downloads` and `issue_metadata` are nullable.
- `Version`: `codemeta_url` is no longer required and `related_tag` is nullable.
- `VersionWithDependencies`: `related_tag` is nullable.
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

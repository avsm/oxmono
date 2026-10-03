# ecosystems: client for packages.ecosyste.ms

## Purpose

An OCaml client for the packages.ecosyste.ms API, version 1.1.0 of
`https://packages.ecosyste.ms/docs/api/v1/openapi.yaml`. It covers the
read-only endpoints for registries, packages, versions, dependents,
maintainers, namespaces, advisories and keywords. The API needs no
authentication.

## Layout

The package is `bleeding/ecosystems/`, modelled on `bleeding/karakeep`.

    ecosystems/
      dune-project, ecosystems.opam
      dune, dune.inc            @gen rule
      ecosystems-openapi-spec.yaml
      ecosystems.ml, .mli       generated, checked in
      lib/                      hand-written layer
      test/

## Generated core

`ecosystems-openapi-spec.yaml` is the upstream file, unmodified. The
`openapi-gen` tool reads YAML directly, so no conversion step is needed.
`dune build @gen --auto-promote` regenerates `ecosystems.ml` and
`ecosystems.mli`. Both are committed. The default base URL is
`https://packages.ecosyste.ms/api/v1`.

## Hand-written layer

`Ecosystems_client` (in `lib/`) wraps the generated `t`.

- `create ?session ?user_agent ~sw env` is a client with the base URL set.
  `user_agent` defaults to a library identifier.
- `pages f` is a `Seq.t` that walks an operation taking `page` and
  `per_page`, stopping at the first empty page. Forty operations in the spec
  take those parameters.

Nothing else is added until a real use shows the need.

## Dependencies

As karakeep: `openapi`, `fetch`, `fetch-curl`, `eio`, `jsont`, `ptime`.

## Testing

- A spec test checks that the generated files match what `openapi-gen`
  produces from the pinned YAML.
- Decode tests run the generated codecs over recorded JSON fixtures taken
  from the live API, one per schema.
- `pages` takes a plain function, so it is tested with a fake page source and
  needs no HTTP stack.
- No test reaches the network.

## Out of scope

A command-line tool, a `doc/` tree, the POST operation's write semantics
beyond what the generator emits, and caching.

## Repository obligations

Work on a branch. Build with `dune build @bleeding/ecosystems/all
@bleeding/ecosystems/runtest --force`. Add an entry to `CHANGES.md` grouped
by commit. Add no AI-disclosure tags. Commit generated output separately
from the hand-written layer.

## Risks

The generator may not handle every construct in the spec. If it fails, the
fix belongs in `bleeding/openapi` and becomes its own commit, or the
construct is patched in the pinned YAML with the patch recorded here.

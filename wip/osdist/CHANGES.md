## Unreleased

- Maintain this unreleased library under `wip/osdist`. Preserve its oi
  source provenance and package identity.

- Clarify opam descriptions of package functionality, upstream scope and local
  adaptations.

# Changes

- Declare the opam-core dependency used by metadata extraction.

- Attribute generated packaging files to osdist when used by other tools.

- Add an explicit Osdist interface with API documentation and a usage example.
  Correct packaging, source bundle and repository documentation.
- Give osdist its own project, package metadata and tests under `wip/osdist`.

- Import osdist from oi 0.14.2 with OxCaml-compatible Jsont codecs.

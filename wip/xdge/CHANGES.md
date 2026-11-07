## Unreleased

- Add the `xdge.testenv` library, which scopes environment bindings around a
  test, unsets variables for real and binds `HOME` and the XDG base
  directories to a throwaway root.

- Clarify opam descriptions of package functionality and interfaces.

v1.1.0 (dev)
------------

- Add native path helpers with the same application overrides as Eio contexts,
  without creating directories or starting a scheduler.

- Remove dependency on `eio_main` for library (@avsm).
  Thanks to @Alizter for the workaround in https://github.com/ocaml/dune/issues/12821).

v1.0.0 (2025-11-29)
-------------------

- Initial public release (@avsm)

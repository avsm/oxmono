# sqlite-vec

Unmodified amalgamation from sqlite-vec v0.1.9, upstream commit
`e9f598abfa0c06b328d8fe5da9c3760cce74be10`.

Source: https://github.com/asg017/sqlite-vec/releases/tag/v0.1.9
Asset: `sqlite-vec-0.1.9-amalgamation.tar.gz`.
SHA256: `3acd67cb4aff080c7050926fd3cf8227905fe5b7ee3829d8ee5024ab1283cf61`.

The local Dune file compiles against the repository SQLite header with
`SQLITE_CORE`, `SQLITE_VEC_STATIC` and `SQLITE_VEC_OMIT_FS`. Filesystem
vector readers are disabled. The OCaml binding registers the static extension
through `sqlite3_auto_extension`, so macOS needs no loadable extensions.
There are no patches to upstream C or header sources. The binding and build
files are local additions.

When refreshing, compare both amalgamation files byte for byte with the
release archive, record the version, commit and archive hash, then run
`opam exec --switch=5.2.0+ox -- dune runtest avsm/siftbot --force`.
Consumer tests check nearest neighbours, deletion, revision invalidation,
provider changes and dimensions. The vendored runtest alias is not a guard.

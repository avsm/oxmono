# sqlite-vec

Unmodified amalgamation from sqlite-vec v0.1.6, upstream commit
`639fca5739fe056fdc98f3d539c4cd79328d7dc7`.

Source: https://github.com/asg017/sqlite-vec/releases/tag/v0.1.6
Asset: `sqlite-vec-0.1.6-amalgamation.tar.gz`.
SHA256: `99b6ec36e9d259d91bd6cb2c053c3a7660f8791eaa66126c882a6a4557e57d6a`.

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

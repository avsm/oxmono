# An opam source and search tool for the agent

A tool that lets the agent search the opam package repository, inspect package
metadata, and download source code into a central cache it can then read with
the existing `read`, `grep` and `tree` tools. The tool runs the `opam` CLI,
which is already present on any machine that builds OCaml.

## What the model gets

Three operations in one tool, named `opam`:

- `opam search <query>` runs `opam list --search --short <query>`. It returns
  the package names that match. The query is a substring match against package
  names, which is what `opam search` does by default.
- `opam show <package>` runs `opam show <package> --field=name,version,synopsis,
  description,homepage,depends`. It returns the full metadata block, so the
  model can learn what a package is, what it depends on, and where its source
  lives without downloading anything.
- `opam source <package> <version?>` runs `opam source <package> --dir
  <cache>/opam/<name>.<version>`. It downloads the source archive, extracts it
  into the cache, and mints a capability to the extracted directory so the
  model can immediately read, grep and tree it.

The three operations are distinguished by the tool's first parameter, which
selects one of `search`, `show` or `source`. The tool name is `opam` and the
operation is the first argument, so a call reads `opam search "eio"`.

## The cache

Opam source downloads go to a central directory derived from the XDG cache
layout. The tool takes a `cache` parameter of type `Eio.Fs.dir_ty Eio.Path.t`
when it is constructed. The caller passes `Eio.Path.(Xdge.cache_dir xdg /
"opam")`. Each package is extracted to its own subdirectory named
`<name>.<version>`, so `opam source eio` extracts to
`$XDG_CACHE_DIR/ds4/opam/eio.1.4`.

The cache directory is created with `Eio.Path.mkdirs` on first use. A package
that is already in the cache is not downloaded again: the tool returns the
capability name for the existing directory and says it was already present.

## Capability discipline

The downloaded source is outside the workspace capability. The `opam source`
operation therefore mints a new capability to the extracted directory and
registers it in the `Caps.t` the tool was given. This is the one tool that
creates capabilities rather than only reading them.

The mint follows the same rules as `Toolbox.Caps.mint`: the new capability is
named by Eio's own printer, so it reads as `<...eio.1.4>` or similar, and the
model quotes that name back to `read`, `grep`, `tree`, `open_dir` and the
other filesystem tools exactly as it quotes any other capability. The `approve`
callback is not consulted, because the tool is not asking to widen anything: it
is opening a directory it created itself, inside the cache the caller already
granted by passing the `cache` path.

If the same package is requested twice, the second call returns the capability
name already registered and does not download again. `Caps.find` is consulted
before `opam source` is run, so the download is skipped when the capability is
already held.

## Where it lives

The tool belongs in `Deepseek.Toolbox` as a function taking `~proc` (the
process manager) and `~cache` (the cache root) and returning a `Tool.t`. It
sits beside `dns` and `bash`, which are the other tools that reach outside the
workspace. The `okit` split is not needed, because opam is a one-shot CLI
command rather than a persistent session. The dune and merlin tools go through
okitd because they hold a session that must survive the model's calls; opam
search and source are the same cost as a `bash` invocation either way.

## Output bounds

`opam search` can return hundreds of names. The tool clips the output at 200
lines and appends a truncation note, so a query for a common word does not fill
the model's context. `opam show` output is bounded by opam itself to the fields
requested and is passed through unchanged. `opam source` output is opam's own
two lines.

A command that fails (package not found, network error, no switch) reports
opam's standard error as the tool result, in the same style as `bash`: the
model reads what opam said and can correct its request.

## Humpty wiring

`bin/humpty.ml`'s `assemble` function gains the `opam` tool in its base list,
alongside `dns` and `bash`. It is constructed with the same `~proc` as `bash`
and with `Eio.Path.(Xdge.cache_dir xdg / "opam")` as `~cache`. The tool is
always present, since opam is a system-level tool rather than a workspace
feature. `humpty expect` picks it up automatically because it drives whatever
tool list `assemble` returns.

The system prompt gains nothing: the tool's own description says what it does,
and the model discovers it from the tool list. A paragraph about it is not
needed, since it does not replace or interact with any existing tool.

## Testing

- `test/test_opam.ml` exercises the tool against a mock process manager that
  scripts the `opam` binary's responses, and against a temporary cache
  directory. It checks that `search` passes the query through, that `show`
  requests the right fields, and that `source` mints a capability and returns
  its name.
- The capability-minting behaviour is tested with a real temporary directory
  and a real `Caps.t`, so the test verifies that a capability minted by the
  tool can be read by `Toolbox.read` and `Toolbox.list` with the name the tool
  returned.
- A negative test checks that requesting the same package twice does not call
  `opam source` a second time.

These tests do not need `DS4_LIVE` and do not need a real opam installation.

## Changes to the interface

`Deepseek.Toolbox.mli` gains:

```ocaml
val opam : proc:[> `Generic ] Eio.Process.mgr_ty Eio.Resource.t ->
          cache:[> Eio.Fs.dir_ty ] Eio.Path.t ->
          caps:Caps.t -> Tool.t
```

`Deepseek.Toolbox.ml` gains the implementation. No other module changes.

## Documentation

- `CHANGES.md` gains an entry: an `opam` tool that searches the package index
  and downloads source into the XDG cache.
- The module header of `lib/toolbox.ml` mentions that `opam` is the one tool
  that creates capabilities rather than only reading them, and that its `source`
  operation is the only one that reaches the network beside `dns`.

## Verification

`dune build`, `dune runtest` and `dune build @fmt` clean. `DS4_LIVE=1 dune
runtest` is not needed, since nothing here touches the FFI, the session
lifetime or the agent loop. The mock test covers the tool's behaviour without
a real opam.

# okit: OCaml tooling for humpty's agent

A library of tools that make humpty a specialist OCaml coding agent. The tools
shell out to the `dune` and `ocamlmerlin` binaries found in the project's
switch, so they always match the compiler the project builds with. Nothing new
is added to humpty's opam dependencies.

Three constraints shape the design. Tool results are clipped at about 4000
characters, so every tool returns dense, bounded output and says when it
truncates. The model is small and local, so a tool that saves a model turn is
worth more than one that saves a process spawn. The toolbox has an authority
ladder, and these tools sit on a new rung of it, described below.

## Placement

A new top-level directory `okit/` builds the library `humpty.okit` as part of
the `humpty` package. It depends on `deepseek`, for `Tool.t` and `Dsml.Json`,
and on `eio`. The csexp wire format is read and written by a small module of
our own, in keeping with how `dsml` hand-rolls its codecs.

## Modules

The module names below are the ones that landed. The plan split the protocol
in two, `Okit.Wire` over `Okit.Csexp`, and named the session `Okit.Dune_rpc`
rather than `Okit.Dune`, since the session is the only part a caller uses.

### Okit.Csexp

Canonical s-expression reading and writing over Eio flows. One parser serves
both the RPC wire format and the output of `dune describe --format csexp`. An
atom larger than the reader will buffer is refused rather than read, since the
length prefix is whatever the peer says it is.

### Okit.Pp_text

Dune sends a message as a tree of formatting instructions rather than as text.
This renders one to plain lines, which is what a tool result is.

### Okit.Wire

The RPC packets: the initialize handshake, the request and response frames, and
the payloads of the methods used. Diagnostics decode to a typed record:
severity, file, line and column range, the rendered message and the promotion a
diagnostic may offer.

The protocol client exists because dune's own CLI client reports only success
or failure. The structured error text is only available over the protocol.
Written against dune 3.24.1, and pinned to the version 2 payloads of `build`
and `diagnostics` and to `runtest`, which dune 3.24 is the first to serve. A
server offering less is refused by name at startup.

### Okit.Dune_rpc

The session. It spawns `dune build --passive-watch-mode` in the workspace root
through the process manager, or attaches to a server already serving the
workspace and leaves that one running. Passive mode means dune builds only when
asked, so a build result always corresponds to the tool call that requested it.
Before each requested build the session flushes the file watcher, so a write
that just happened is seen.

The version 2 `build` method answers with the outcome of the build itself, so
the session takes that answer and then fetches the diagnostics once, rather
than holding open the long poll the plan first described. A poll would have had
to be raced against the build to say which set of diagnostics belonged to it.

Dune's RPC resolves a target as a path alone. `build ~targets` therefore takes
paths such as `.` or `lib/foo.ml` and refuses an alias with the form to write
instead. The tests are an alias, so they are run by the separate `runtest`
method rather than by building `@runtest`. `promote ~path` accepts a built file
over RPC as well, rather than shelling out to `dune promote`.

### Okit.Project

`dune describe workspace --format csexp`, distilled into a project map: each
library and executable, its modules, its dependencies and its source paths. It
runs under a build directory of its own, with `DUNE_BUILD_DIR` set and
`INSIDE_DUNE` dropped, so it does not wait on the lock the passive server
holds.

### Okit.Merlin

One-shot queries through `ocamlmerlin single <command>`, with the file's
contents on stdin and the JSON reply parsed with `Dsml.Json`. The commands
used are `outline`, `type-enclosing`, `locate` and `errors`. Merlin discovers
its configuration from dune itself, so the session needs no configuration of
its own. If latency warrants it later, `ocamlmerlin server` gives the same
one-shot interface backed by a caching daemon.

Merlin was chosen over ocaml-lsp-server deliberately. LSP requires a
persistent server, a capabilities handshake and a synchronized document
lifecycle, which is mutable state that must be kept consistent with every
write the agent makes. Merlin's one-shot calls are stateless, which is exactly
the shape of a tool call.

## The tools

`Okit.Toolbox` provides the `Tool.t` values. All follow the existing toolbox
conventions: bounded output, truncation announced, errors reported rather than
silenced.

- `build` builds the paths it is given, or the whole workspace, and returns the
  first diagnostics, each as its location and message. The count is
  capped and the cap is stated when reached. An alias such as `@check` is
  refused with the form to write instead, since dune's RPC will not resolve
  one.
- `write` writes a file through the same `Caps` discipline as the plain write
  tool, and for `.ml`, `.mli` and `dune` files then flushes the watcher,
  requests a build, and appends the file's own diagnostics to the result,
  with a count of diagnostics elsewhere. The model cannot write broken code
  without hearing about it in the same tool result. This tool replaces
  `Toolbox.write` when okit is active.
- `test` calls the `runtest` method and returns failures as bounded
  diagnostics. A test whose output differs from what is recorded names the file
  `promote` would accept.
- `promote` accepts one source path over RPC.
- `project` returns the project map from `Okit.Project`. It replaces `tree`
  wandering for orientation inside the workspace.
- `outline` returns a file's structure with types, one bounded call instead
  of reading the file.
- `type_at` returns the type at a file, line and column.
- `locate` returns the definition site of an identifier as `path:line`, which
  composes with `read_lines`.

The merlin-backed tools are offered only when `ocamlmerlin` is on the path.
The rest are offered only when `dune` is, and a workspace `dune-project`
exists.

## Authority

These tools run exactly two fixed binaries, resolved at startup, always with
the workspace root as working directory. That is more authority than the
confined filesystem tools and less than `bash`. Granting `build` trusts the
project's own dune files, since build rules execute arbitrary commands. The
mli says this plainly.

## Humpty wiring

When `humpty agent` starts and `<workspace>/dune-project` exists, humpty starts
the okit session and adds the tools. The fused `write` replaces the plain one.
A session that cannot start, for want of dune or of a new enough one, is
reported once at warning level and leaves the plain tools as they were. The
system prompt tells the model to prefer `project` over `tree` inside the
workspace and to rely on the diagnostics that `write` returns rather than
running builds through `bash`, and that paragraph is added only when the tools
are there. `tree` remains available for capabilities opened outside the
workspace.

## Testing

Unit tests cover csexp round-trips and diagnostic decoding against fixture
payloads. An integration test spawns real `dune` on a fixture project in a
temporary directory and exercises session start, a clean build, a failing
build with diagnostics, the fused write, and `describe`. Merlin tests skip
when the binary is absent. None of this needs `DS4_LIVE`.

Such a workspace is made under `/tmp` and not under the `TMPDIR` dune gives a
test, because a unix socket address is limited to about 100 characters and
dune's own temporary directory is deep inside `_build`.

`CHANGES.md` records the user-visible change and `ARCH.md` gains the new
directory in its layout listing.

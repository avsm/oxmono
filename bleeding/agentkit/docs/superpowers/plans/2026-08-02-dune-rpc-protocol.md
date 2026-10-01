# Dune RPC wire protocol, as served by dune 3.24.1

This records what dune sends on the wire. No code in this repository encodes it
any more, since `dune-rpc-eio` builds on the `dune-rpc` package, so read this to
understand a trace rather than as a specification anything here implements.

A self-contained reference for implementing a client. Extracted from the dune
3.24.1 sources (`otherlibs/dune-rpc/`, `src/dune_rpc_impl/`, `src/rpc/`) and
verified against a live `dune build --passive-watch-mode` server. Csexp
examples are shown in pretty form. On the wire an atom is `<len>:<bytes>` and
a list is `(` items `)`, with no whitespace.

## Encoding rules

Everything on the wire is one canonical s-expression per message.

| Value | Sexp |
|---|---|
| string, int, float | an atom |
| unit | `()` |
| list, pair, triple | a list |
| sum constructor | `(Name <arg>)`, always exactly two elements; nullary is `(Name ())` |
| option | `(Some <x>)` or `(None ())` |
| record | list of `(<field-name> <value>)` pairs |

Record rules, all enforced by the server:

- Fields are serialised in **alphabetical order** of the field name. Decoding
  is order-insensitive.
- An optional field with no value is **omitted entirely**.
- Duplicate fields are an error. **Unknown fields are an error** — never send
  a field the server does not expect.

Ids (`Id.t`) are arbitrary sexps chosen by the client, unique among in-flight
requests. Dune's own client uses `(auto 0)`, `(auto 1)`, …

## Transport

Connect to the unix socket. **There is no preamble**: no greeting bytes, no
framing beyond csexp itself. The server says nothing until the client sends
`initialize`. Multiple csexps may arrive in one read, so the reader must be a
streaming csexp parser.

### Socket discovery

1. `DUNE_RPC` env var, a D-Bus style address: `unix:path=/abs/path` or
   `tcp:host=127.0.0.1,port=8587`.
2. Otherwise stat `<root>/_build/.rpc/dune`. A socket: connect to it (the
   normal case on unix). A regular file: its contents are a D-Bus address
   string (Windows). Missing: no server is running.

The socket file is unlinked when the server exits.

## Packet framing

A packet is a record with optional fields `id`, `method`, `params`, `result`
(alphabetical wire order):

- **Request**: `id`, `method`, `params` present.
- **Notification**: `method`, `params` present, no `id`.
- **Response**: `id`, `result` present.

`method` and `params` must both be present or both absent. `result` is
`(ok <payload>)` or `(error <error>)`. The error object has fields
(alphabetical) `kind`, `message`, `payload` (optional): `kind` is the atom
`Invalid_request` or `Code_error`, `message` is a string.

```
((id (auto 0)) (method ping) (params ()))
((id (auto 0)) (result (ok ())))
((method shutdown) (params ()))
((id 1) (result (error ((kind Invalid_request) (message "invalid method")))))
```

Requests flow both ways in principle, but dune 3.24.1 only sends the client
notifications: `notify/log` and `notify/abort`, both with `Message` params,
fields (alphabetical) `message` (string, required), `payload` (optional).
`notify/abort` means the server is closing the connection and can arrive at
any phase, including before negotiation completes. Responses can arrive out
of order for concurrent requests; match on `id`.

## Handshake

Strictly sequential, client-driven, three phases.

### 1. initialize

Method `initialize`. Params record, fields alphabetical
`dune_version`, `id`, `protocol_version`:

- `dune_version`: `(3 24)` (pair of ints, informational).
- `id`: a free-form client-name sexp, shown by `dune rpc status`. Not the
  packet id.
- `protocol_version`: **must be the atom `0`**. On mismatch the server sends
  `notify/abort` and closes.

```
((id (initialize))
 (method initialize)
 (params ((dune_version (3 24)) (id okit) (protocol_version 0))))
```

Response payload is `()`:

```
((id (initialize)) (result (ok ())))
```

### 2. version menu

Method `version_menu` (not `negotiate_version`). Params:
`list (pair method-name (list version-int))` — every method the client may
use, with the versions it supports. Include `notify/abort` and `notify/log`
so the server may send them.

```
((id (menu))
 (method version_menu)
 (params ((build (2)) (diagnostics (2)) (flush-file-watcher (1))
          (notify/abort (1)) (notify/log (1)) (ping (1)) (promote (1))
          (shutdown (1)))))
```

Response: one selected version per method, `max` of the common versions.
Methods the server does not know are silently dropped from the reply, and a
method absent from the negotiated menu is unusable: calling it returns
`(error ((kind Invalid_request) (message "remote and local have no common
version for method") (payload ((method X)))))`. If the intersection is
entirely empty the server aborts.

```
((id (menu)) (result (ok ((build 2) (diagnostics 2) …))))
```

### 3. Session

After the menu response, requests and notifications flow freely.

## Procedures served by dune 3.24.1

| Method | Kind | Versions | Params | Response payload |
|---|---|---|---|---|
| `ping` | request | 1 | `()` | `()` |
| `build` | request | 1, 2 | `(<target> …)` | `(Success ())` / `(Failure …)` |
| `diagnostics` | request | 1, 2 | `()` | `(<diagnostic> …)` |
| `runtest` | request | 1 | `(<path> …)` | as build |
| `flush-file-watcher` | request | 1 | `()` | `(ok ())` / `(not_in_watch_mode ())` |
| `promote` | request | 1 | `<path>` (one atom) | `()` |
| `promote_many` | request | 1, 2 | see source | outcome |
| `format-dune-file` | request | 1 | `((contents <s>) (path <s>))` | `<string>` |
| `status`, `build_dir`, `format` | request | 1 | `()` | various |
| `shutdown` | notification | 1 | `()` | — |
| `poll/diagnostic` | request | 1, 2 | `<sub-id sexp>` | `(Some (<event> …))` / `(None ())` |
| `poll/progress` | request | 1, 2 | `<sub-id sexp>` | `(Some <progress>)` / `(None ())` |
| `cancel-poll/…` | notification | 1 | `<sub-id sexp>` | — |
| `notify/abort`, `notify/log` | notification (server→client) | 1 | `Message` | — |

Notes:

- A `build` target is one of three forms. A path relative to the workspace
  root, such as `.`, `lib` or `src/foo.exe`. `(alias <path>)`, the alias in
  exactly the directory named and in the root when the path has no directory,
  which is `@@name` in dune's CLI. `(alias_rec <path>)`, the alias in that
  directory and every directory below it, which is `@name` in dune's CLI.
  Paths and alias forms mix freely in one request. The literal string
  `@check` is read as a path and fails with `Don't know how to build @check`,
  and `dune rpc build @check` fails the same way, while `dune rpc build .`
  succeeds. A malformed s-expression such as `(alias` is answered with a
  `Code_error`, dune's own internal failure, rather than an
  `Invalid_request`, and it names nothing useful, so a client validates a
  target before sending it. These forms were verified against a live dune
  3.24.2 passive server. In passive watch mode the response arrives only when
  the requested build completes.
- The build v2 `Failure` payload carries the errors as compound user
  messages, but the one-shot `diagnostics` request returns the same
  information as proper `Diagnostic` records, so a sequential client can
  ignore the `Failure` payload and follow up with `diagnostics`.
- `flush-file-watcher` response is nested: `(ok (ok ()))` inside the packet's
  own result wrapper.
- The polls long-poll: the first poll for a subscription id answers
  immediately with the current state, later polls block until it changes. A
  sequential client that uses one-shot `diagnostics` never needs them.
- `promote` params: the path is a single atom, not a list. Paths are as the
  diagnostic's `promotion.in_source` reports them.

## Diagnostic encoding (version 2)

Record fields, alphabetical: `directory`, `id`, `loc`, `message`,
`promotion`, `related`, `severity`, `targets`. `directory`, `loc`,
`severity` are optional and omitted when absent. The rest always appear,
possibly `()`.

- `id`: int atom, stable per error.
- `severity`: atom `error` or `warning`.
- `directory`: absolute path of the rule's directory.
- `targets`: always `()` in 3.24.1.
- `promotion`: list of records, fields `in_build`, `in_source`, both required
  absolute-path strings.
- `related`: list of records, fields `loc`, `message`, both required.
- `message`: a serialised `Pp.t`, see below.

`loc` is a record, fields `start`, `stop`, each a `Lexing.position` record
with fields (alphabetical) `pos_bol`, `pos_cnum`, `pos_fname`, `pos_lnum`.
Line is `pos_lnum` (1-based), column is `pos_cnum - pos_bol` (0-based).
`pos_fname` is absolute. The `File "…", line N` header dune prints is not in
`message`; synthesise it from `loc`.

Full example:

```
((directory /home/me/proj)
 (id 0)
 (loc ((start ((pos_bol 0) (pos_cnum 0) (pos_fname /home/me/proj/src/foo.ml) (pos_lnum 1)))
       (stop  ((pos_bol 0) (pos_cnum 5) (pos_fname /home/me/proj/src/foo.ml) (pos_lnum 1)))))
 (message (Vbox (0 (Concat ((Break (("" 0 "") ("" 0 "")))
                            ((Box (0 (Text "Error: Unbound value foo")))))))))
 (promotion ())
 (related ())
 (severity error)
 (targets ()))
```

`diagnostics` v1 differs only in the `Tag` node of messages (below).

## Message trees (`Pp.t`)

`message` is a pretty-printing AST with semantic tags, not a string. Every
node is `(Name <arg>)`:

| Sexp | Meaning |
|---|---|
| `(Nop ())` | nothing |
| `(Verbatim <s>)` | literal text |
| `(Char <c>)` | one character |
| `(Text <s>)` | wrappable text |
| `(Newline ())` | hard newline |
| `(Seq (<a> <b>))` | `a` then `b` |
| `(Concat (<sep> (<t> …)))` | items joined by `sep` (first element is the separator) |
| `(Box (<n> <t>))`, `(Hvbox …)`, `(Hovbox …)` | layout box, indent `n` |
| `(Hbox <t>)` | single argument, no pair |
| `(Vbox (<n> <t>))` | every break inside becomes a newline |
| `(Break ((<l> <n> <r>) (<l'> <n'> <r'>)))` | first triple when on one line, second when broken |
| `(Tag (<style> <t>))` | v2 form; v1 encodes `(Tag <t>)` with no style |

`Pp.space` is `(Break (("" 1 "") ("" 0 "")))`, `Pp.cut` is
`(Break (("" 0 "") ("" 0 "")))`. Styles are nullary constructors
(`(Error ())`, `(Hint ())`, …) or `(Ansi_styles (…))`.

Dune builds each diagnostic as `Vbox(0, Concat(cut, [Box(0,p); …]))`, one box
per paragraph. Rendering to plain text: concatenate `Text`/`Verbatim`/`Char`;
`Newline` is a newline; a `Break` inside a `Vbox` renders as a newline plus
`n'` spaces, elsewhere as `l` plus `n` spaces plus `r`; `Concat` joins items
with the rendered separator; boxes other than `Vbox` reset the vbox flag;
`Tag` renders its subtree, dropping the style.

## Failure modes worth handling

- Server error messages seen in practice: `"invalid method"`, `"remote and
  local have no common version for method"`, `"initialize request expected"`,
  `"version negotiation request expected"`, `"missing required field"`,
  `"unexpected fields"`, `"invalid constructor name"`.
- `notify/abort` before or after negotiation: report its `message` and stop.
- EOF at any point: the server exited or was replaced.
- A second dune cannot serve the same build dir: it fails to start with a
  lock error, so a pre-existing socket means some other server owns the
  workspace.

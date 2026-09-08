# ocaml-matrix: port to opam-repository packages + `fetch`

> **2026-09-02.** The module names below predate the restructuring that split
> `matrix-chat.cli` out of `matrix-chat.client`, moved the driver loops into
> `matrix-chat.eio`, and renamed `Sync_service` to `Base_client`, `Session.Store` to
> `Profile_store`, `Read_receipts` to `Read_state` and `Matrix_ui.Timeline` to
> `Room_timeline`, among others. They are left as they were written.
> `STATUS.md` and `README.md` name the modules the tree has now.

> **Current transport note (2026-09-05).** The later HTTPz backend now owns
> the batteries-included transport: `matrix-chat.eio` uses `Fetch_httpz.std`
> directly, with `Httpz_tls.system` and the homeserver-scoped
> `Matrix_client.Http_retry.default ~homeserver` policy by default. Custom
> certificate policy is expressed by passing both `~retry` and
> `~https:(Httpz_tls.client ~authenticator)` to `Fetch_httpz.std`; the former
> `Matrix_eio.Http` module and its direct TLS/RNG dependencies are no longer
> part of the package.

Executable plan, now complete. The feature roadmap that followed it is
`PARITY_PLAN.md`; anything found unimplemented along the way goes in
`TODO.md`. (References to `PLAN.md` below are historical — that file was
superseded by this one, `PARITY_PLAN.md` and `TODO.md`, and deleted.)

Branch: `port/opam-deps` (off `main` at `c7b9f83`).

## Status: complete

All phases below are done. Commit range `c7b9f83..HEAD`:

| Commit | Phase |
|---|---|
| `5024207` `d86f489` | Phase 0 — revert the two `nox` commits |
| `bf078bd` | this plan + `TODO.md` |
| `482f719` | Phase 1, session persistence → jsont |
| `bfb6b33` `caf600b` | Phase 1, HTTP layer → fetch (+ merge) |
| `99739f9` | integration: session round-trip test into `runtest` |
| `ef5d7cf` `3f47a71` | Phase 2, `Random` capability (+ merge) |
| `c80b519` | Phase 2, `fetch.mock` test harness |
| *(this commit)* | Phase 3, metadata, CI, documentation |

### Verification results

Run from a clean `_build` on the final tree:

- `dune build --root .` — clean, no warnings.
- `dune runtest --root .` — green:
  `test_matrix_proto` 79/79, `test_session` 6 codec round trips plus 2
  negative decode checks, `test_matrix_client` 42/42.
- `dune build --root . @install` — clean.
- `dune exec --root . -- omatrix --help` — runs, prints the cmdliner manpage.
- `rg 'Requests|Tomlt|Crypto_rng|Mirage_crypto_rng' lib examples test` — five
  hits, all the expected `matrix-chat.eio` TLS-seeding lines in
  `lib/matrix_eio/http.{ml,mli}`. No `Requests`, no `Tomlt`.
- `opam install . --deps-only --dry-run --with-test` — "Nothing to do": the
  constraints in `dune-project` are satisfied by the switch as it stands.
  Every dependency resolves from the `default` opam-repository
  (`https://opam.ocaml.org`) except `fetch`, `fetch-httpz` and `httpz`, which
  are pinned from the `avsm/httpz` git repository. No `aoah` repository is
  configured in the switch.
- `dune build --root . @doc` — 110 warnings, unchanged by this phase. All of
  them are unresolved *cross-package* roots that the narrow `@doc` universe
  cannot see: 100 × `Eio.Io` (from `@raise Eio.Io` tags across `matrix-chat.eio`),
  6 × `Invalid_argument` (stdlib), 4 × `Fetch` / `Fetch_httpz`. Per the phase
  brief these were not chased.
- `dune build --root . @doc-new` — the odoc-v3 rules build a full universe
  including dependencies, and there everything above resolves. It went from 5
  warnings to **4**, and none of the remaining four come from this tree: two
  `Failed to lookup child page dummy` and one `'{!modules ...}' should not be
  empty` from dune's generated index pages, and one `file_exts` from a
  dependency's own documentation. The warning fixed was
  `{!Matrix_eio.Http.client}` in `lib/matrix_client/client.mli` — a reference
  from `matrix-chat.client` into `matrix-chat.eio`, which depends on it, so it can never
  resolve; it is now a code span.

### Deviations from the plan

- **`(ocaml (>= 5.1))` became `(ocaml (>= 5.5))`.** Both
  `~/.opam/5.5.0/lib/fetch/opam` and `~/.opam/5.5.0/lib/httpz/opam` declare
  `"ocaml" {>= "5.5"}`, so the SDK cannot claim a lower bound. The CI image
  must therefore ship OCaml 5.5 or newer; that is flagged in the workflow and
  in `TODO.md` as unverified.
- **`eio_main` is a regular dependency, not `:with-test`.** The `omatrix`
  executable is `(public_name omatrix) (package matrix-chat)`, so a release build
  (`dune build -p matrix-chat @install`) links `eio_main`.
- **`tls` was added to `depends`.** `lib/matrix_eio/http.ml` calls
  `Tls.Config.client` directly, so the package is a direct dependency, not
  just something `tls-eio` drags in.
- `unix` was already absent from `depends` and stays absent: it is part of the
  compiler distribution, not a separate opam package.
- `(documentation (depends eio fetch fetch-httpz))` would fix the `@doc`
  cross-package warnings, but `(lang dune 3.21)` rejects the stanza; it needs
  the odoc-v3 dune. Not applied.

## Goal

`matrix` must build from vanilla opam-repository packages plus the `fetch`
family, with no `nox-*`, `requests`, or `tomlt`:

| Was | Now | Version in switch |
|---|---|---|
| `nox-json` | `jsont`, `jsont.bytesrw` | 0.3.0 |
| `nox-crypto` | `mirage-crypto` | 2.4.1 |
| `nox-crypto-ec` | `mirage-crypto-ec` | 2.4.1 |
| `nox-crypto-rng` | *(dropped from `matrix-chat.client`)* — Eio `secure_random` | — |
| `nox-kdf.hkdf` | `kdf.hkdf` | 1.1.1 |
| `nox-xdg.eio` | `xdge` | 1.1.0 |
| `requests` | `fetch` (client) / `fetch-httpz` + `tls-eio` + `ca-certs` (eio layer) | dev (pinned from `~/src/git/avsm/ocaml-httpz`) |
| `tomlt` | `jsont` — session state stored as JSON | 0.3.0 |

## Findings that shaped the plan

- The `nox` switch was two commits (`7d85162`, `c7b9f83`); reverting them in
  reverse order applies cleanly. **Done** (`5024207`, `d86f489`). After the
  revert, `matrix-chat.proto` builds against jsont 0.3.0 and all 79 tests pass.
- jsont 0.3.0 vs 0.2.0: only `Jsont.{int,int64}` range change and
  `Json.remove_mem`; no API removals.
- `Requests` is used in exactly one place: `lib/matrix_client/client.ml`
  (`get/post/put/delete/post_unauthenticated/handle_response`). `media.ml` is
  a stub that mentions it in comments only.
- `tomlt` is used only by `session.ml`/`session.mli` (16 codecs + 6 files
  via `Tomlt_eio.{decode,encode}_file`). No other module reads TOML.
- `fetch` gives us: `Fetch.with_response` (no switch needed), `Fetch.body` as
  a flow (raw binary — unblocks Media), `Fetch.with_credentials` (scoped,
  redacted bearer tokens), `Fetch.restrict`, `Fetch_httpz.std` (retries +
  rate limits built in), `fetch.mock` (`Fetch_mock.client` for tests).
- `fetch-httpz` does no TLS itself: `~https : Uri.t -> conn -> conn`. We
  supply it with `tls-eio` + `ca-certs`. **`tls-eio` requires a seeded
  `Mirage_crypto_rng`**, so `mirage-crypto-rng.unix` stays a dependency of
  `matrix-chat.eio` (the batteries layer), but not of `matrix-chat.client`.
- `Mirage_crypto_rng` is never seeded in the tree, so every key/nonce
  generation path raises `Unseeded_generator` at runtime today. Switching the
  library to Eio's `secure_random` fixes that.
- `ocaml-httpz` in the tree is a **symlink** to `~/src/git/avsm/ocaml-httpz`
  whose committed `main` has an older layout. The packages were installed via
  `opam pin -k git` + `opam install --working-dir`. CI cannot do that yet — see
  TODO.md.

## Architecture decisions

1. **`matrix-chat.client` takes an HTTP capability; it never constructs one.**
   `Client.create ~config ~fetch env`. Depends on `fetch` only. No TLS, no
   backend choice, no RNG seeding in the library.
2. **`matrix-chat.eio` is the batteries-included layer.** `Matrix_eio.Http.client env`
   builds `Fetch_httpz.std ~https env` with a `tls-eio`/`ca-certs` wrapper
   (seeding `Mirage_crypto_rng_unix.use_default ()` first).
   `Matrix_eio.Client.create ~sw ~env ~homeserver ?fetch ()` uses that by
   default, so existing example code changes minimally.
3. **Randomness is a capability**: `Matrix_client.Random.t` wraps
   `env#secure_random`. `Client.random` exposes it; pure crypto modules take
   `~random`. `mirage-crypto-rng` leaves `matrix-chat.client` entirely.
4. **Session state on disk becomes JSON** (`session.json`, `device.json`, …)
   encoded with jsont. Existing `.toml` profiles are not migrated (pre-1.0);
   noted in TODO.md.
5. **Media uses authenticated endpoints** (`/_matrix/client/v1/media/*`) for
   download/thumbnail/config, `/_matrix/media/v3/upload` for upload, via new
   raw-binary helpers in `Client`.

## Phases and agent assignments

Each agent works in its own git worktree on this branch, commits, and reports
branch + sha. Files are disjoint within a phase.

### Phase 0 — revert (done)

`git revert c7b9f83` then `git revert 7d85162`.

### Phase 1 — make it build (parallel)

**HTTP layer → `fetch`**
Files: `lib/matrix_client/{client,media}.{ml,mli}`, `lib/matrix_client/dune`
(requests → fetch), `lib/matrix_eio/{client,media}.ml`, new
`lib/matrix_eio/http.ml{,i}`, `lib/matrix_eio/{dune,matrix_eio.ml}`,
`bin/omatrix/omatrix.ml`, `example/{simple_bot,send_dm}.ml`, `example/dune`, `dune-project`
(requests → fetch, add fetch-httpz tls-eio ca-certs mirage-crypto-rng).
Deliverables:
- `Client.create : config:config -> fetch:_ Fetch.t -> < secure_random : _ Eio.Flow.source; .. > -> t`
  (store the `secure_random` source; Phase 2 turns it into `Random.t`).
- Requests via `Fetch.with_response`; bearer token via
  `Fetch.with_credentials ~scope:[origin]` (`~allow_insecure:true` when the
  homeserver is `http://`); `Fetch.restrict ~under:[origin]` on creation.
- `handle_response` unchanged in semantics. Errors: map `Eio.Io (Fetch.E _)`
  to `Error.Network_error`; **re-raise `Eio.Cancel.Cancelled`**; never
  swallow it in `try … with _`.
- Raw helpers for binary bodies, e.g.
  `get_bytes : t -> path:string -> ?query -> unit -> (string * string option, Error.t) result`
  (body, content-type) and
  `post_bytes : t -> path:string -> ?query -> content_type:string -> body:string -> unit -> (string, Error.t) result`
  where `path` is absolute (not prefixed with `/_matrix/client/v3`).
- Implement `Media.upload/download/thumbnail/get_config` on those helpers using
  the authenticated media endpoints; `mxc_to_http` produces authenticated URLs.
- `Matrix_eio.Http`: `https : ?authenticator:X509.Authenticator.t -> unit -> Fetch_httpz.https`
  and `client : < net; clock; mono_clock; secure_random; .. > -> Fetch.plain`.
- Verification trick: `tomlt` is still missing in this worktree, so
  temporarily exclude `session` from `matrix_client` (`(modules :standard \ session)`
  + comment the alias in `matrix_client.ml` and the `Session` uses in
  `omatrix.ml`) to run `dune build`, then **restore** before committing.

**session persistence → jsont**
Files: `lib/matrix_client/session.{ml,mli}`, `lib/matrix_client/dune`
(drop `tomlt tomlt.eio`), `dune-project` (drop tomlt).
Deliverables:
- `val jsont : t Jsont.t` replaces `val tomlt : t Tomlt.t` in every submodule.
- `Tomlt.Table.(obj |> mem |> opt_mem |> finish)` → `Jsont.Object.(map |> mem |> opt_mem |> finish)`;
  `Tomlt.ptime` → RFC 3339 string codec; `Tomlt.map ~dec:(failwith …)` →
  jsont error (`Jsont.Error`), not `failwith`.
- `Store` loads/saves `*.json` with `Jsont_bytesrw.{decode,encode}_string` and
  `Eio.Path.{load,save ~perm:0o600}`; `exists` checks `session.json`;
  `clear` removes the `.json` names.
- Verification trick: `requests` is missing in this worktree, so temporarily
  set `(modules session olm error …)` — whatever minimal closure `session.ml`
  needs — in `lib/matrix_client/dune` to type-check, then restore.

Integration (me): merge both, `dune build`, `dune runtest`, fix fallout, commit.

### Phase 2 — randomness + tests (parallel)

**`Random` capability, drop mirage-crypto-rng**
Files: new `lib/matrix_client/random.{ml,mli}`; `client.{ml,mli}` (add
`random`); `keys, olm, verification, backup, send_queue` `.ml/.mli`;
`lib/matrix_eio/{keys,verification,backup,send_queue}.ml`;
`lib/matrix_proto/matrix_id.{ml,mli}` (`Transaction_id.generate`);
`lib/matrix_client/dune` (drop mirage-crypto-rng); `matrix_client.ml`.
23 call sites, `X25519.secret_of_octets`,
`Ed25519.priv_of_octets`/`pub_of_priv`; fix `olm.ml` ~530 (no random
fabrication on unpickle failure → `Error`).

**`matrix-chat.client` test harness with `fetch.mock`**
Files: `test/dune`, new `test/test_matrix_client.ml`, fixtures.
`Fetch_mock.client` fake homeserver under `Eio_mock.Backend.run`; cover
`Client` request construction + auth header + error mapping, `Auth.login`,
`Media` upload/download, `Session` codec + `Store` round trip (real fs in a
temp dir). Must not depend on Phase 2 B's signature changes (avoid key
generation).

### Phase 3 — metadata, CI, docs (sequential)

**Metadata and documentation**: `dune-project` bounds (`jsont >= 0.3.0`, `mirage-crypto >= 2.0.0`,
`kdf >= 1.1.0`, `xdge >= 1.1.0`, `fetch`, …), regenerate `matrix-chat.opam`,
`.tangled/workflows/build.yml`,
remove stale `.gitmodules` + `(vendored_dirs vendor)`, reconcile `STATUS.md`
(deps, media, session file names, "all modules have Eio wrappers"),
`CHANGES.md`, consolidate `TODO.md`, `dune build @doc` clean.

### Final verification (me)

1. `dune build` and `dune runtest` from a clean `_build`.
2. `rg 'Requests|Tomlt|Crypto_rng|Mirage_crypto_rng' lib examples test` → only
   the `matrix-chat.eio` TLS seeding line.
3. `opam install . --deps-only --dry-run` resolves with only opam-repo +
   the three pinned fetch packages.
4. `omatrix --help` runs.

# Arod Proffer Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fill in proffer's deferred core (static serving, ETag cache, content negotiation, redirect routes, scoped auth, richer log events), then port `avsm/arod` from `httpz.route`/`httpz.eio_server` onto `proffer` and `proffer-httpz`.

**Architecture:** Proffer's new features land first, each as a package-internal module with an `.mli` surface and mock-driven tests, so proffer stays releasable at every commit. Only then does arod change: its handlers become `'env -> Req.t -> Resp.t` functions over an env record of closures, its server shrinks to a `Proffer_httpz.run` call, and the `send_*`/`is_head`/`cached`/`mime_type_of_path`/`check_stats_auth`/`wants_markdown` boilerplate is deleted. Arod's access log keeps every field it records today, so proffer's log event is extended to carry them rather than arod losing data.

**Tech Stack:** OCaml 5.2.0+ox, dune 3.21, `eio` for I/O, `proffer` + `proffer-httpz` (this repo, `bleeding/proffer`), `httpz` (this repo, `bleeding/httpz`), `sqlite3-eio` for the access log, `base64` for Basic auth.

**Spec:** `/Users/avsm/src/git/avsm/ox2/PROFFER.md`. This plan implements its "Next, in order" items 1 (deferred core) and 2 (port arod), plus the observability extension arod's real access log demands. The spec is authoritative where this plan is silent, except that the spec's claim "Compression is out of scope. Deployment fronts the origin with Caddy, which compresses" and its implication that Caddy normalises paths is **false in production** (verified: `via: 1.1 Caddy` passed a raw `..` through to the origin), so the origin confines paths itself.

## Global Constraints

- Before every commit, `dune build` must be clean for the packages touched, and the relevant `dune runtest` must pass. No red commits. Scope builds to the package under change (`dune build @bleeding/proffer/all`, `@avsm/arod/all`) rather than repo-wide, because unrelated packages (`bleeding/atp`, `bleeding/immich`, `cascade`, `yamlrw`, `frontmatter`) do not build in this tree and predate this work.
- `dune build @fmt` is **suspended**: stock `ocamlformat` 0.29 cannot parse OxCaml mode syntax (`@@ portable`, `@ portable`, `type t : immutable_data`), so it fails on every file. Do not add a `.ocamlformat` to `bleeding/proffer`. Match surrounding formatting by hand, keeping lines within 80 columns.
- `dune build @doc` cannot run: neither `odoc` nor an ox-compatible odoc that reads this compiler's `.cmt` files is installed in the `5.2.0+ox` switch. Review docs by hand.
- Prose in `.mli` files and comments follows `CLAUDE.md`: POSIX manpage density, complete sentences, no em-dashes, never join two clauses with a semicolon, document a value as `[foo x y] is ...` naming its arguments, a comment explains what the code cannot.
- Proffer's core is portable by construction. Every route handler is taken at `@ portable`, so it cannot capture domain-bound state. A value a portable function reads at the top level must carry a portable kind (`type t : immutable_data` or `: value mod portable`), or it reads as `contended`. When a new core value is defined, annotate it as the neighbouring code in the same file does, and let the build tell you if a kind is missing.
- Every new module gets an `.mli`.
- Copyright header: proffer files carry none today, so add none. Arod files carry the ISC header block at the top of every `.ml`; copy it verbatim into any new arod file.
- One commit per self-contained change, one-line imperative message, no trailers or sign-off. Keep a mechanical change (a reformat, a rename sweep) out of the commit that changes behaviour.
- Tests are plain executables: a `check name bool` helper, no framework, a final `Printf.printf "<suite>: %d checks ok\n"`. This matches `bleeding/proffer/test/test_resp.ml`. Do not introduce alcotest.
- The mock backend is the test vehicle for proffer core features. `Proffer_mock.request` returns a `response` with `status`, `headers`, `header` (case-insensitive), `body` and `content_length`. Assert through it, never by reaching into backend types.
- **Filesystem capability discipline.** `Eio.Stdenv.fs` is the unrestricted capability. It may appear only in `avsm/arod/bin/main.ml`, and only during startup, for the config and context load and the XDG log path, where the configured paths are absolute. A configured path that is relative is resolved against `Eio.Stdenv.cwd` instead. Every directory the server reads while answering requests is opened once at startup with `Eio.Path.open_subtree ~sw` (eio 1.4, `path.mli:142`; `open_dir` is its deprecated alias), which refuses `..` and symlink escapes at the OS level, and handlers reach files only through closures over those confined capabilities. No value reachable from a request handler may capture `fs`. `Static.confine` stays on top of that as defence in depth, and is what turns an escape attempt into a clean 404 rather than an exception. Proffer's core never touches Eio, so this constraint binds the arod tasks (11, 14) and the optional backend static path (Task 8).

## Prerequisite state (already done, do not redo)

These landed in the session that produced this plan. Verify with `git log` and `git status` before starting; if any is missing, that is a merge problem to resolve first.

- Proffer core restructured: `Serve` renamed to `Backend`; `private_modules` hides every internal module; `Headers.t` returned by `Req.headers`/`Resp.headers`; `Proffer_mock` has its own `response` type; `Resp.v` raises `Invalid_argument` on an unwritable header, entity-tag or date; `Route.int'` renamed to `Route.int`; `Body.string` deleted; `Status.t` extended to 30 codes; `Date` guards non-finite and out-of-range times.
- `proffer-httpz` `run` gained `clock:_ Eio.Time.clock`, a `config` with `backlog`/`max_connections`/`idle_timeout`/`request_timeout`, `?on_listening`, and a flattened top-level `type event`.
- Package files added: `bleeding/proffer/README.md`, `CHANGES.md`, `LICENSE.md`, `doc/index.mld`, `doc/dune`; tests attributed to packages; spikes detached from `runtest`.
- **Arod path-traversal fixed:** `Arod_handlers.confined_path` rejects `""`/`"."`/`".."`/`'/'`/NUL, `static_file` takes `string list`, and `avsm/arod/test/test_static.ml` pins it. This fix is preserved by Phase 3 below, not reverted. The live site was patched separately.

---

## File Structure

Proffer, created under `bleeding/proffer/lib/`:
- `mime.ml` / `mime.mli` — extension-to-MIME table. One responsibility: a filename's Content-Type.
- `static.ml` / `static.mli` — the safe path-join (`confine`) and the `Static.t` site node describing a served directory.
- `cache.ml` / `cache.mli` — TTL memoization cache storing body and ETag, crosses domains.
- `negotiate.ml` / `negotiate.mli` — Accept content negotiation producing a handler that sets `Vary: Accept`.

Proffer, modified:
- `lib/proffer.mli` — re-export the new modules; add `Route.moved`/`Route.found`; add `Site.mount`/`Site.with_auth`/`Site.with_headers`; extend the doc.
- `lib/proffer.ml` — alias the new modules.
- `lib/route.ml` / `.mli`-in-`proffer.mli` — `moved`/`found` constructors.
- `lib/site.ml` — `mount`, `with_auth`, `with_headers`; the `Static.t` node carried in the site.
- `lib/compiled.ml` — carry any new site data through compilation.
- `lib/backend.ml` — map a `Static.t` node to a served response; apply scoped wrappers.
- `httpz/proffer_httpz.ml` / `.mli` — extend `event` with the request/response fields arod's log needs; serve `Static` from the httpz `Static` path.

Proffer tests, created under `bleeding/proffer/test/`:
- `test_mime.ml`, `test_static.ml`, `test_cache.ml`, `test_negotiate.ml`, `test_wrappers.ml` (mount/auth/headers/redirect).

Arod, created:
- `avsm/arod/lib_handlers/arod_env.ml` / `.mli` — the capability record handlers receive as `'env`.
- `avsm/arod/lib/server/arod_site.ml` / `.mli` — builds the `Proffer.Site.t` from handlers (the proffer successor of `all_routes`).

Arod, modified:
- `avsm/arod/lib_handlers/arod_handlers.ml` / `.mli` — handlers become `Arod_env.t -> Proffer.Req.t -> Proffer.Resp.t`; delete the boilerplate block.
- `avsm/arod/lib_handlers/arod_handlers_stats.ml` — the four stats handlers become proffer handlers.
- `avsm/arod/lib/arod_cache.ml` — deleted; callers use `Proffer.Cache`.
- `avsm/arod/lib/server/arod_server.ml` — shrinks to a `Proffer_httpz.run` call with the log bridge.
- `avsm/arod/lib_log/arod_log.ml` — `log_request` consumes `Proffer_httpz.event` instead of `Httpz_eio_server.request_info`.
- `avsm/arod/bin/main.ml` — builds the env record and the compiled site, passes them to `Arod_server.run`.
- the four `dune` files that list `httpz.route`/`httpz.eio_server` — swap to `proffer`, `proffer-httpz`.

---

## Phase 1 — Proffer deferred core

Each task in this phase is self-contained: a new module, its `.mli`, its test, registered in `test/dune` and re-exported from `proffer.mli`. Proffer stays releasable throughout.

### Task 1: MIME table

**Files:**
- Create: `bleeding/proffer/lib/mime.ml`, `bleeding/proffer/lib/mime.mli`
- Create: `bleeding/proffer/test/test_mime.ml`
- Modify: `bleeding/proffer/lib/proffer.ml`, `bleeding/proffer/lib/proffer.mli`, `bleeding/proffer/test/dune`

**Interfaces:**
- Produces: `Proffer.Mime.of_path : string -> string` (a filename or path to a Content-Type, defaulting to `application/octet-stream`).

- [ ] **Step 1: Write the failing test**

Create `bleeding/proffer/test/test_mime.ml`:

```ocaml
open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  check "css" (Mime.of_path "a/b/site.css" = "text/css");
  check "svg" (Mime.of_path "icon.svg" = "image/svg+xml");
  check "png" (Mime.of_path "x.png" = "image/png");
  check "woff2" (Mime.of_path "f.woff2" = "font/woff2");
  check "case is folded" (Mime.of_path "PHOTO.JPG" = "image/jpeg");
  check "no extension is octet-stream"
    (Mime.of_path "README" = "application/octet-stream");
  check "unknown extension is octet-stream"
    (Mime.of_path "a.xyz" = "application/octet-stream");
  check "a dotfile has no extension"
    (Mime.of_path ".gitignore" = "application/octet-stream");
  Printf.printf "test_mime: %d checks ok\n" !checks
```

- [ ] **Step 2: Add the suite to `test/dune`**

In `bleeding/proffer/test/dune`, add `test_mime` to the first `(tests (names ...) (modules ...))` stanza (the `proffer`-package one). Names and modules lists must both gain `test_mime`.

- [ ] **Step 3: Run to verify it fails**

Run: `dune build @bleeding/proffer/runtest --force`
Expected: FAIL, `Unbound module Mime` (or `test_mime` unbound value).

- [ ] **Step 4: Write `mime.mli`**

```ocaml
(** Content types by filename extension. *)

val of_path : string -> string
(** [of_path name] is the Content-Type for [name], chosen from its extension
    with case folded, or ["application/octet-stream"] when the extension is
    absent or unknown. A name whose only dot starts it, such as [".gitignore"],
    has no extension. *)
```

- [ ] **Step 5: Write `mime.ml`**

Port arod's `mime_type_of_path` (`avsm/arod/lib_handlers/arod_handlers.ml`, the 16 suffixes) to an extension lookup. Fold case, and treat a leading-dot-only name as extensionless.

```ocaml
let table =
  [ ("pdf", "application/pdf"); ("html", "text/html"); ("css", "text/css");
    ("js", "text/javascript"); ("svg", "image/svg+xml"); ("png", "image/png");
    ("jpg", "image/jpeg"); ("jpeg", "image/jpeg"); ("webp", "image/webp");
    ("xml", "application/xml"); ("wasm", "application/wasm");
    ("ico", "image/x-icon"); ("woff", "font/woff"); ("woff2", "font/woff2");
    ("bib", "application/x-bibtex");
    ("webmanifest", "application/manifest+json");
    ("txt", "text/plain"); ("json", "application/json");
    ("atom", "application/atom+xml"); ("opml", "text/x-opml") ]

(* The last dot after the last slash starts the extension. A dot in position
   zero of the final segment is a dotfile, which has no extension. *)
let extension name =
  let slash =
    match String.rindex_opt name '/' with Some i -> i + 1 | None -> 0
  in
  match String.rindex_opt name '.' with
  | Some i when i > slash -> Some (String.sub name (i + 1) (String.length name - i - 1))
  | _ -> None

let of_path name =
  match extension name with
  | None -> "application/octet-stream"
  | Some ext -> (
      match List.assoc_opt (String.lowercase_ascii ext) table with
      | Some ct -> ct
      | None -> "application/octet-stream")
```

- [ ] **Step 6: Re-export from proffer**

In `bleeding/proffer/lib/proffer.ml` add `module Mime = Mime`. In `bleeding/proffer/lib/proffer.mli`, under the `{1 Protocol vocabulary}` section, add:

```ocaml
module Mime : sig
  val of_path : string -> string @@ portable
  (** [of_path name] is the Content-Type for [name], from its extension with
      case folded, or ["application/octet-stream"] when it is absent or
      unknown. *)
end
```

Note the `@@ portable` on the value in the `.mli` even though `mime.mli` does not carry it: the standalone `mime.mli` compiles the module in isolation, and the re-export signature in `proffer.mli` is where portability is asserted for handler use. If the build reports `mime.ml` is not portable, add `@@ portable` to `of_path` in `mime.mli` too.

- [ ] **Step 7: Run to verify it passes**

Run: `dune build @bleeding/proffer/all @bleeding/proffer/runtest --force`
Expected: PASS, `test_mime: 8 checks ok`, and the four existing suites still pass.

- [ ] **Step 8: Commit**

```bash
git add bleeding/proffer/lib/mime.ml bleeding/proffer/lib/mime.mli \
  bleeding/proffer/lib/proffer.ml bleeding/proffer/lib/proffer.mli \
  bleeding/proffer/test/test_mime.ml bleeding/proffer/test/dune
git commit -m "Add Proffer.Mime content-type table"
```

### Task 2: Static path confinement and the Static site node

**Files:**
- Create: `bleeding/proffer/lib/static.ml`, `bleeding/proffer/lib/static.mli`
- Create: `bleeding/proffer/test/test_static.ml`
- Modify: `bleeding/proffer/lib/proffer.ml`, `bleeding/proffer/lib/proffer.mli`, `bleeding/proffer/test/dune`

**Interfaces:**
- Produces: `Proffer.Static.confine : string list -> string option` — the segment confinement extracted from arod, now owned by proffer so no backend re-invents it.
- Produces: `Proffer.Static.t` and `Proffer.Static.v : root:string -> ?cache:Cache_control.t -> unit -> t` — a description of a served directory a backend maps to its own file serving. `root` is a label the backend resolves against its own filesystem capability, so the core holds no path.

Rationale from real usage: arod serves `/images/**` and `/papers/*.pdf` from disk. The confinement is the security-critical part and belongs in the shared core; the actual `open` stays in the backend, which owns the filesystem. This task delivers `confine` (usable immediately by a handler that opens files through its `env`) and the `Static.t` data node (mapped by the backend in Task 12). Splitting `open` from `confine` is deliberate: the core must not depend on Eio.

- [ ] **Step 1: Write the failing test**

Create `bleeding/proffer/test/test_static.ml`:

```ocaml
open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let refused name segs = check name (Static.confine segs = None)

let () =
  check "a plain path joins" (Static.confine [ "a"; "b.png" ] = Some "a/b.png");
  check "one segment" (Static.confine [ "x.pdf" ] = Some "x.pdf");
  check "empty list is the root" (Static.confine [] = Some "");
  refused "parent segment" [ ".."; "etc"; "passwd" ];
  refused "parent in the middle" [ "a"; ".."; "b" ];
  refused "current-dir segment" [ "."; "a" ];
  refused "empty segment" [ "a"; ""; "b" ];
  refused "embedded slash" [ "a/b" ];
  refused "embedded NUL" [ "a\000b" ];
  Printf.printf "test_static: %d checks ok\n" !checks
```

- [ ] **Step 2: Register in `test/dune`, run to verify it fails**

Add `test_static` to the `proffer`-package `(tests ...)` stanza. Run `dune build @bleeding/proffer/runtest --force`. Expected: FAIL, `Unbound module Static`.

- [ ] **Step 3: Write `static.mli`**

```ocaml
(** Serving a directory of files, described as data. *)

val confine : string list -> string option @@ portable
(** [confine segs] is [segs] joined with ['/'] when every segment names
    something directly under a root, and [None] otherwise. A segment that is
    empty, ["."] or [".."], or that holds a ['/'] or a NUL, is refused, so the
    result can never leave the subtree. A backend that resolves the result
    against a filesystem must still open it under a confining root, since
    [confine] cannot see symlinks. *)

type t : immutable_data
(** A served directory. It holds a label and a cache policy, not a filesystem
    handle, so a backend resolves [root] against its own capability. *)

val v : root:string -> ?cache:Cache_control.t -> unit -> t @@ portable
(** [v ~root ()] serves files under [root], a name the backend resolves. Each
    file's Content-Type comes from {!Mime.of_path} and its response carries
    [cache] when given. *)

val root : t -> string @@ portable
(** [root t] is the label [t] was built with. *)

val cache : t -> Cache_control.t option @@ portable
(** [cache t] is the policy [t] applies to each file, if any. *)
```

- [ ] **Step 4: Write `static.ml`**

```ocaml
let confine segs =
  let unsafe s =
    String.equal s "" || String.equal s "." || String.equal s ".."
    || String.contains s '/'
    || String.contains s '\000'
  in
  if List.exists unsafe segs then None else Some (String.concat "/" segs)

type t = { root : string; cache : Cache_control.t option }

let v ~root ?cache () = { root; cache }
let root t = t.root
let cache t = t.cache
```

- [ ] **Step 5: Re-export from proffer**

`proffer.ml`: `module Static = Static`. In `proffer.mli`, under `{1 Routes and sites}`, add a `module Static : sig ... end` repeating the `static.mli` signature (with `@@ portable` on the values and the `immutable_data` kind on `t`).

- [ ] **Step 6: Run to verify it passes**

Run: `dune build @bleeding/proffer/all @bleeding/proffer/runtest --force`
Expected: PASS, `test_static: 9 checks ok`.

- [ ] **Step 7: Commit**

```bash
git add bleeding/proffer/lib/static.ml bleeding/proffer/lib/static.mli \
  bleeding/proffer/lib/proffer.ml bleeding/proffer/lib/proffer.mli \
  bleeding/proffer/test/test_static.ml bleeding/proffer/test/dune
git commit -m "Add Proffer.Static confinement and served-directory node"
```

### Task 3: ETag cache

**Files:**
- Create: `bleeding/proffer/lib/cache.ml`, `bleeding/proffer/lib/cache.mli`
- Create: `bleeding/proffer/test/test_cache.ml`
- Modify: `bleeding/proffer/lib/proffer.ml`, `bleeding/proffer/lib/proffer.mli`, `bleeding/proffer/test/dune`

**Interfaces:**
- Produces:
  - `Proffer.Cache.t` (crosses domains)
  - `Proffer.Cache.create : ttl:float -> t`
  - `Proffer.Cache.memoize : t -> now:float -> key:string -> (unit -> string) -> string * Etag.t` — returns the body and an ETag over it, so a caller builds a `Resp.v ~etag`. A hit and a miss are indistinguishable to the caller except through `stats`.
  - `Proffer.Cache.stats : t -> int * int` — hits, misses.

Rationale: arod's `Arod.Cache` (`avsm/arod/lib/arod_cache.ml`) is a `Hashtbl` keyed by string, storing only the body, with a `Unix.gettimeofday` TTL, and it sends `X-Cache: hit/miss` but never an ETag or a 304. Proffer's cache stores the ETag so revalidation costs a hash compare. `now` is passed in rather than read from a clock, so the core stays free of `Unix`. The implementation is an `Atomic.t` over an immutable `Map` (per PROFFER.md "The shared cache"), so it crosses domains without a lock.

- [ ] **Step 1: Write the failing test**

Create `bleeding/proffer/test/test_cache.ml`:

```ocaml
open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  let c = Cache.create ~ttl:10. in
  let calls = ref 0 in
  let gen () = incr calls; "BODY" in
  let body1, etag1 = Cache.memoize c ~now:0. ~key:"/a" gen in
  check "first call renders" (body1 = "BODY" && !calls = 1);
  let body2, etag2 = Cache.memoize c ~now:5. ~key:"/a" gen in
  check "within ttl is a hit" (body2 = "BODY" && !calls = 1);
  check "etag is stable on a hit" (Etag.weak_equal etag1 etag2);
  let _b, _e = Cache.memoize c ~now:20. ~key:"/a" gen in
  check "past ttl re-renders" (!calls = 2);
  let hits, misses = Cache.stats c in
  check "stats count hits and misses" (hits = 1 && misses = 2);
  let ba, ea = Cache.memoize c ~now:20. ~key:"/a" gen in
  let bb, eb = Cache.memoize c ~now:20. ~key:"/b" (fun () -> "OTHER") in
  check "distinct keys are independent" (ba = "BODY" && bb = "OTHER");
  check "distinct bodies get distinct etags"
    (not (Etag.weak_equal ea eb));
  Printf.printf "test_cache: %d checks ok\n" !checks
```

- [ ] **Step 2: Register in `test/dune`, run to verify it fails**

Add `test_cache` to the `proffer`-package `(tests ...)` stanza. Run `dune build @bleeding/proffer/runtest --force`. Expected: FAIL, `Unbound module Cache`.

- [ ] **Step 3: Write `cache.mli`**

```ocaml
(** A memoization cache keyed by string, holding a rendered body and its
    entity-tag. It crosses domains, so a policy built once at startup is
    reachable from every domain's handlers. *)

type t : value mod portable
(** A cache. *)

val create : ttl:float -> t @@ portable
(** [create ~ttl] is an empty cache whose entries live [ttl] seconds. *)

val memoize :
  t -> now:float -> key:string -> (unit -> string) @ portable -> string * Etag.t
  @@ portable
(** [memoize t ~now ~key gen] is the body under [key] and an entity-tag over
    it. It runs [gen] and stores the result when [key] is absent or its entry
    is older than the cache's [ttl] at [now], and returns the stored body
    otherwise. [now] is seconds since the epoch, passed in so the core reads no
    clock. Two domains racing on a miss both run [gen] and one result wins,
    which is the right trade for memoization. *)

val stats : t -> int * int @@ portable
(** [stats t] is the hit and miss counts since [t] was created. *)
```

- [ ] **Step 4: Write `cache.ml`**

Use the stdlib's portable string map and atomics. If `Map.MakePortable` is unavailable in this switch's stdlib, use `Stdlib.Map.Make(String)` inside a `Modes.Portable.t` wrapper; the build will say which is needed. The ETag is a weak tag over the body, computed with `Digest` (MD5 is fine for a cache validator, it is not a security boundary).

```ocaml
module M = Map.Make (String)

type entry = { body : string; etag : string; expires : float }
type state = { entries : entry M.t; hits : int; misses : int }
type t = { ttl : float; state : state Atomic.t }

let create ~ttl =
  { ttl; state = Atomic.make { entries = M.empty; misses = 0; hits = 0 } }

let etag_of body = Digest.to_hex (Digest.string body)

let rec bump f t =
  let cur = Atomic.get t.state in
  let next = f cur in
  if not (Atomic.compare_and_set t.state cur next) then bump f t

let memoize t ~now ~key gen =
  let cur = Atomic.get t.state in
  match M.find_opt key cur.entries with
  | Some e when now < e.expires ->
      bump (fun s -> { s with hits = s.hits + 1 }) t;
      (e.body, `Weak e.etag)
  | _ ->
      let body = gen () in
      let etag = etag_of body in
      let e = { body; etag; expires = now +. t.ttl } in
      bump
        (fun s ->
          { entries = M.add key e s.entries; hits = s.hits; misses = s.misses + 1 })
        t;
      (body, `Weak etag)
```

Note: `bump` for the hit path re-reads and CAS-loops for the counter, which is correct but races the entry read against a concurrent miss. That is acceptable, a stale hit count is harmless. If the OxCaml kind checker rejects `state Atomic.t` as non-portable, wrap the record fields as the vendored cache in `bleeding/proffer/spike` does, or fall back to `Atomic.t` over a tuple of immutable data. Resolve against what the compiler reports.

- [ ] **Step 5: Re-export from proffer**

`proffer.ml`: `module Cache = Cache`. In `proffer.mli`, add a `module Cache : sig ... end` under `{1 Routes and sites}` or a new `{1 Caching}` heading, repeating the signature.

- [ ] **Step 6: Run to verify it passes**

Run: `dune build @bleeding/proffer/all @bleeding/proffer/runtest --force`
Expected: PASS, `test_cache: 7 checks ok`.

- [ ] **Step 7: Commit**

```bash
git add bleeding/proffer/lib/cache.ml bleeding/proffer/lib/cache.mli \
  bleeding/proffer/lib/proffer.ml bleeding/proffer/lib/proffer.mli \
  bleeding/proffer/test/test_cache.ml bleeding/proffer/test/dune
git commit -m "Add Proffer.Cache with per-entry ETag"
```

### Task 4: Content negotiation

**Files:**
- Create: `bleeding/proffer/lib/negotiate.ml`, `bleeding/proffer/lib/negotiate.mli`
- Create: `bleeding/proffer/test/test_negotiate.ml`
- Modify: `bleeding/proffer/lib/proffer.ml`, `bleeding/proffer/lib/proffer.mli`, `bleeding/proffer/test/dune`

**Interfaces:**
- Produces:
  - `Proffer.Negotiate.media = [ `Html | `Markdown | `Json | `Xml | `Other of string ]`
  - `Proffer.Negotiate.of_accept : string option -> media list` — the client's ordered preference from an Accept header, best first, `[]` when the header is absent.
  - `Proffer.Negotiate.v : (media * 'env Route.handler) list -> 'env Route.handler` — a handler that picks the first offered variant the client accepts, defaulting to the first in the list, and adds `Vary: Accept` to the chosen response.

Rationale: arod's `negotiated`/`wants_markdown` (`avsm/arod/lib_handlers/arod_handlers.ml`) hand-parse Accept for `text/markdown` only, across 15 routes. Proffer parses q-values once and offers an ordered choice. The `Vary: Accept` is added by wrapping the chosen `Resp.t`. Because a handler is `@ portable`, the variant list is captured portably.

- [ ] **Step 1: Write the failing test**

Create `bleeding/proffer/test/test_negotiate.ml`. Build a two-variant handler and drive it through the mock with different Accept headers.

```ocaml
open Proffer
open Proffer.Route

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let () =
  check "absent accept is empty" (Negotiate.of_accept None = []);
  check "html preferred"
    (Negotiate.of_accept (Some "text/html,text/markdown;q=0.9")
     = [ `Html; `Markdown ]);
  check "q-value orders"
    (Negotiate.of_accept (Some "text/markdown;q=0.4, text/html;q=0.9")
     = [ `Html; `Markdown ])

let handler =
  Negotiate.v
    [ (`Html, fun _env _req -> Resp.html "<h1>hi</h1>");
      (`Markdown, fun _env _req -> Resp.media "text/markdown" "# hi") ]

let site = Site.of_routes [ get (s "p" /? nil) handler ]
let compiled = Compiled.compile site

let () =
  let r = Proffer_mock.request compiled () `GET "/p"
            ~headers:[ ("Accept", "text/markdown") ] in
  check "markdown chosen" (Proffer_mock.body r = "# hi");
  check "vary added" (Proffer_mock.header r "vary" = Some "Accept");
  let r = Proffer_mock.request compiled () `GET "/p"
            ~headers:[ ("Accept", "text/html") ] in
  check "html chosen" (Proffer_mock.body r = "<h1>hi</h1>");
  let r = Proffer_mock.request compiled () `GET "/p" in
  check "no accept falls back to first" (Proffer_mock.body r = "<h1>hi</h1>");
  Printf.printf "test_negotiate: %d checks ok\n" !checks
```

- [ ] **Step 2: Register in `test/dune`, run to verify it fails**

Add `test_negotiate` to the `proffer`-package `(tests ...)` stanza. Run `dune build @bleeding/proffer/runtest --force`. Expected: FAIL, `Unbound module Negotiate`.

- [ ] **Step 3: Write `negotiate.mli`**

```ocaml
(** Choosing a response variant from the request's Accept header. *)

type media = [ `Html | `Markdown | `Json | `Xml | `Other of string ]
(** A media type this library can negotiate. [`Other] carries a full type such
    as ["image/png"]. *)

val of_accept : string option -> media list @@ portable
(** [of_accept accept] is the media types [accept] asks for, most preferred
    first, with q-values honoured and a missing q taken as 1. It is [[]] when
    [accept] is absent or empty. A type this library does not name becomes
    [`Other]. *)

val v : (media * 'env Route.handler) list -> 'env Route.handler @@ portable
(** [v variants] is a handler that answers with the first entry of [variants]
    the client accepts, or the first entry when the client accepts none or
    sends no Accept header. The chosen response gains [Vary: Accept], since it
    depends on that header. *)
```

- [ ] **Step 4: Write `negotiate.ml`**

Parse Accept into `(media, q)` pairs, stable-sort by descending q, map each media token. Match a variant's media against the client's list in the client's order; on no match take the head of `variants`. Add `Vary: Accept` by rebuilding the chosen `Resp.t` with an extra header. Since `Resp.headers` returns `Headers.t` and `Resp.v` takes an assoc list, add Vary by constructing a fresh response is heavy; instead expose the need: add a small helper `Resp.with_header` in Task 4a below, or set Vary by re-running the variant handler's result through `Resp.v` is not possible without deconstructing it. **Decision:** add `Vary` at the media token level by having variant handlers already include it is fragile. Instead, implement `Negotiate.v` to call the chosen handler, then return a response with Vary merged. This requires a `Resp` combinator. Add it as Step 4a.

- [ ] **Step 4a: Add `Resp.vary` to the core**

In `bleeding/proffer/lib/resp.ml`, add:

```ocaml
(* [vary name t] adds [name] to [t]'s Vary field, or sets it. It is how a
   negotiation combinator marks a response as depending on a request header,
   without rebuilding it. *)
let vary name t =
  let cur = Headers.find t.headers "vary" in
  let value =
    match cur with
    | None -> name
    | Some existing -> existing ^ ", " ^ name
  in
  let headers =
    Headers.of_list
      (List.filter
         (fun (n, _) -> not (String.equal (String.lowercase_ascii n) "vary"))
         (Headers.to_list t.headers)
      @ [ ("Vary", value) ])
  in
  { t with headers }
```

Expose it in `proffer.mli` under `Resp`:

```ocaml
val vary : string -> t -> t @@ portable
(** [vary name t] adds [name] to [t]'s Vary field. A negotiation combinator
    uses it to mark a response as depending on a request header. *)
```

Then `negotiate.ml` uses `Resp.vary "Accept" (chosen env req)`.

```ocaml
let of_media = function
  | "text/html" -> `Html
  | "text/markdown" -> `Markdown
  | "application/json" -> `Json
  | "application/xml" | "application/atom+xml" -> `Xml
  | other -> `Other other

let parse_one s =
  match String.split_on_char ';' s with
  | [] -> None
  | media :: params ->
      let media = String.trim media in
      if String.equal media "" then None
      else
        let q =
          List.fold_left
            (fun acc p ->
              match String.split_on_char '=' (String.trim p) with
              | [ "q"; v ] -> ( match float_of_string_opt v with Some f -> f | None -> acc)
              | _ -> acc)
            1.0 params
        in
        Some (of_media (String.lowercase_ascii media), q)

let of_accept = function
  | None -> []
  | Some accept ->
      String.split_on_char ',' accept
      |> List.filter_map parse_one
      |> List.stable_sort (fun (_, a) (_, b) -> Float.compare b a)
      |> List.map fst

let v variants (env : 'env) req =
  let wanted = of_accept (Req.header req "accept") in
  let handler =
    let rec pick = function
      | [] -> ( match variants with (_, h) :: _ -> h | [] -> fun _ _ -> Resp.not_found ())
      | m :: rest -> (
          match List.assoc_opt m variants with Some h -> h | None -> pick rest)
    in
    pick wanted
  in
  Resp.vary "Accept" (handler env req)
```

The `List.assoc_opt m variants` uses polymorphic equality on `media`, which is a variant of strings, so it is sound.

- [ ] **Step 5: Re-export from proffer**

`proffer.ml`: `module Negotiate = Negotiate`. Add the `module Negotiate : sig ... end` to `proffer.mli` under `{1 Routes and sites}`.

- [ ] **Step 6: Run to verify it passes**

Run: `dune build @bleeding/proffer/all @bleeding/proffer/runtest --force`
Expected: PASS, `test_negotiate: 7 checks ok`.

- [ ] **Step 7: Commit**

```bash
git add bleeding/proffer/lib/negotiate.ml bleeding/proffer/lib/negotiate.mli \
  bleeding/proffer/lib/resp.ml bleeding/proffer/lib/proffer.ml \
  bleeding/proffer/lib/proffer.mli bleeding/proffer/test/test_negotiate.ml \
  bleeding/proffer/test/dune
git commit -m "Add Proffer.Negotiate and Resp.vary"
```

### Task 5: Redirect route constructors

**Files:**
- Modify: `bleeding/proffer/lib/route.ml`, `bleeding/proffer/lib/proffer.mli`
- Create: `bleeding/proffer/test/test_wrappers.ml` (this suite grows in Tasks 6 and 7 too)
- Modify: `bleeding/proffer/test/dune`

**Interfaces:**
- Produces:
  - `Proffer.Route.moved : ('r, 'env handler) pat -> string -> 'env t` — a route answering 301 to a fixed location.
  - `Proffer.Route.found : ('r, 'env handler) pat -> string -> 'env t` — a route answering 302.

Rationale: arod has 21 routes that are `fun _ respond -> R.redirect respond ~status ~location`. `('r, 'env handler) pat` means a pattern with no captures, so the location is fixed. A route needing a capture in the location (arod's `/tags/<t>` → `/#tag=<t>`) stays a normal `get` with `Resp.redirect`.

- [ ] **Step 1: Write the failing test**

Create `bleeding/proffer/test/test_wrappers.ml`:

```ocaml
open Proffer
open Proffer.Route

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let site =
  Site.of_routes
    [ moved (s "old.xml" /? nil) "/new.xml";
      found (s "wiki" /? nil) "/notes" ]

let compiled = Compiled.compile site

let () =
  let r = Proffer_mock.request compiled () `GET "/old.xml" in
  check "moved is 301" (Proffer_mock.status r = `Moved_permanently);
  check "moved sets location" (Proffer_mock.header r "location" = Some "/new.xml");
  let r = Proffer_mock.request compiled () `GET "/wiki" in
  check "found is 302" (Proffer_mock.status r = `Found);
  check "found sets location" (Proffer_mock.header r "location" = Some "/notes")
```

(No `Printf` line yet; it is added in Task 7 when the suite is complete. For now append a temporary `let () = Printf.printf "test_wrappers: %d checks ok\n" !checks` and remove it in Task 7. To keep each task green, include the print line here and move it in Task 7.)

Add the print line at the end for this task:

```ocaml
let () = Printf.printf "test_wrappers: %d checks ok\n" !checks
```

- [ ] **Step 2: Register in `test/dune`, run to verify it fails**

Add `test_wrappers` to the `proffer`-package `(tests ...)` stanza. Run `dune build @bleeding/proffer/runtest --force`. Expected: FAIL, `Unbound value moved`.

- [ ] **Step 3: Implement in `route.ml`**

```ocaml
let moved pat location =
  route `GET pat (fun _env _req -> Resp.redirect ~permanent:true location)

let found pat location =
  route `GET pat (fun _env _req -> Resp.redirect location)
```

These need `pat : ('env handler, 'env handler) pat`, i.e. no captures, so the handler ignores its arguments. If the type does not unify because `route` expects `('f, 'env handler) pat` with `'f` the fully-applied handler, specialise: `moved : ('env handler, 'env handler) pat -> string -> 'env t`. Confirm against `route`'s inferred type and annotate accordingly.

- [ ] **Step 4: Declare in `proffer.mli`**

Under `Route`:

```ocaml
val moved : ('env handler, 'env handler) pat -> string -> 'env t @@ portable
(** [moved pat location] answers GET and HEAD at [pat] with a 301 to
    [location]. The pattern captures nothing, so [location] is fixed. *)

val found : ('env handler, 'env handler) pat -> string -> 'env t @@ portable
(** [found pat location] answers with a 302 to [location]. *)
```

- [ ] **Step 5: Run to verify it passes**

Run: `dune build @bleeding/proffer/all @bleeding/proffer/runtest --force`
Expected: PASS, `test_wrappers: 4 checks ok`.

- [ ] **Step 6: Commit**

```bash
git add bleeding/proffer/lib/route.ml bleeding/proffer/lib/proffer.mli \
  bleeding/proffer/test/test_wrappers.ml bleeding/proffer/test/dune
git commit -m "Add Route.moved and Route.found redirect constructors"
```

### Task 6: Site.mount, with_headers, and scoped auth

**Files:**
- Modify: `bleeding/proffer/lib/site.ml`, `bleeding/proffer/lib/compiled.ml`, `bleeding/proffer/lib/backend.ml`, `bleeding/proffer/lib/proffer.mli`
- Modify: `bleeding/proffer/test/test_wrappers.ml`, and its `Printf` line moves to the end (Task 7 finalises)

**Interfaces:**
- Produces:
  - `Proffer.Site.with_headers : (string * string) list -> 'env t -> 'env t` — adds fixed headers to every response the site produces (security headers).
  - `Proffer.Site.with_auth : scope:string list list -> realm:string -> check:(string option -> bool) @ portable -> 'env t -> 'env t` — gates paths under any prefix in `scope` behind a check of the Authorization header, answering 401 with `WWW-Authenticate: Basic realm=...` on failure.
  - `Proffer.Site.mount : at:string list -> 'env t -> 'env t -> 'env t` — serves a sub-site under a path prefix.

Rationale: arod repeats `check_stats_auth` in 5 handlers, all under `/action`. `with_auth ~scope:[["action"]]` gates them once. `with_headers` is for security headers (arod does not set them today, but the port is the place to add them). `mount` is used lightly by arod but is the symmetry PROFFER.md calls for; include it since the stats API lives under `/action/api`.

Design: these wrappers transform a `Site.t`. The simplest sound implementation wraps the site's per-route handlers and fallback with a function applied before dispatch. Because dispatch is a linear scan (`compiled.ml`), a wrapper can be modelled as a decorator stored on the site and applied in `Backend.handle`. Concretely, extend `Site.t` and `Compiled.t` with a `decorate : segments:string list -> 'env handler -> 'env handler` field composed by each wrapper, applied in `Backend.handle` after a route is selected. Auth checks `segments` against `scope`; `with_headers` post-processes the `Resp.t`; `mount` prefixes route patterns at build time.

- [ ] **Step 1: Write the failing tests (append to `test_wrappers.ml`)**

Insert before the final `Printf` line:

```ocaml
let authed =
  Site.of_routes [ get (s "action" /? nil) (fun _env _req -> Resp.text "secret") ]
  |> Site.with_auth ~scope:[ [ "action" ] ] ~realm:"stats"
       ~check:(fun auth -> auth = Some "Basic ok")
  |> Site.with_headers [ ("X-Frame-Options", "DENY") ]

let compiled_authed = Compiled.compile authed

let () =
  let r = Proffer_mock.request compiled_authed () `GET "/action" in
  check "no auth is 401" (Proffer_mock.status r = `Unauthorized);
  check "challenge names the realm"
    (Proffer_mock.header r "www-authenticate" = Some "Basic realm=\"stats\"");
  let r = Proffer_mock.request compiled_authed () `GET "/action"
            ~headers:[ ("Authorization", "Basic ok") ] in
  check "good auth passes" (Proffer_mock.status r = `OK && Proffer_mock.body r = "secret");
  check "security header is present"
    (Proffer_mock.header r "x-frame-options" = Some "DENY")

let mounted =
  Site.mount ~at:[ "api" ]
    (Site.of_routes [ get (s "ping" /? nil) (fun _env _req -> Resp.text "pong") ])
    (Site.of_routes [ get nil (fun _env _req -> Resp.text "root") ])

let compiled_mounted = Compiled.compile mounted

let () =
  let r = Proffer_mock.request compiled_mounted () `GET "/api/ping" in
  check "mounted sub-site answers" (Proffer_mock.body r = "pong");
  let r = Proffer_mock.request compiled_mounted () `GET "/" in
  check "parent still answers" (Proffer_mock.body r = "root")
```

- [ ] **Step 2: Run to verify it fails**

Run: `dune build @bleeding/proffer/runtest --force`. Expected: FAIL, `Unbound value Site.with_auth`.

- [ ] **Step 3: Extend `site.ml`**

Model the site as routes, fallback and a decorator. Show the shape; resolve mode annotations against the compiler.

```ocaml
type 'env t = {
  routes : 'env Route.t list;
  fallback : 'env Route.handler @@ portable;
  decorate : (string list -> 'env Route.handler -> 'env Route.handler) @@ portable;
}

let id_decorate _segs h = h
let of_routes routes = { routes; fallback = default_fallback; decorate = id_decorate }
let with_fallback (fallback @ portable) t = { t with fallback }

let with_headers extra t =
  let add (h @ portable) segs =
    let inner = t.decorate segs h in
    fun env req -> Resp.add_headers extra (inner env req)
  in
  { t with decorate = (fun segs h -> add h segs) }

let has_prefix ~prefix segs =
  let rec go p s =
    match (p, s) with
    | [], _ -> true
    | pc :: pt, sc :: st -> String.equal pc sc && go pt st
    | _ :: _, [] -> false
  in
  List.exists (fun prefix -> go prefix segs) prefix

let with_auth ~scope ~realm ~(check @ portable) t =
  let challenge =
    Resp.v ~status:`Unauthorized
      ~headers:[ ("WWW-Authenticate", Printf.sprintf "Basic realm=%S" realm) ]
      (Body.String "Unauthorized\n")
  in
  let guard (h @ portable) segs =
    let inner = t.decorate segs h in
    if has_prefix ~prefix:scope segs then fun env req ->
      if check (Req.header req "authorization") then inner env req else challenge
    else inner
  in
  { t with decorate = (fun segs h -> guard h segs) }

let mount ~at sub t =
  let prefixed =
    List.map (fun r -> Route.prefix at r) sub.routes
  in
  { t with routes = t.routes @ prefixed }
```

This needs two new helpers: `Resp.add_headers` (Task 6a) and `Route.prefix` (Task 6b). `Printf.sprintf "Basic realm=%S"` quotes the realm, which is correct for the header. Confirm `%S`'s OCaml escaping matches HTTP quoted-string rules for the realms arod uses (`"stats"`), which contain no special characters.

- [ ] **Step 3a: Add `Resp.add_headers` to `resp.ml`**

```ocaml
(* [add_headers extra t] appends [extra] to [t]'s block. Each name and value is
   validated as [v] validates them, so a decorator cannot inject a split. *)
let add_headers extra t =
  List.iter (fun (n, value) -> check_header n value) extra;
  { t with headers = Headers.of_list (Headers.to_list t.headers @ extra) }
```

Expose in `proffer.mli` under `Resp`:

```ocaml
val add_headers : (string * string) list -> t -> t @@ portable
(** [add_headers extra t] appends [extra] to [t]'s field block. It raises
    [Invalid_argument] on an unwritable header, as {!v} does. *)
```

- [ ] **Step 3b: Add `Route.prefix` to `route.ml`**

`prefix at r` prepends the literal segments `at` to route `r`'s pattern. Because a route stores a `run : string list -> handler option`, prefixing means matching and stripping `at` before delegating:

```ocaml
let prefix at r =
  let run segs =
    let rec strip pfx s =
      match (pfx, s) with
      | [], rest -> Some rest
      | pc :: pt, sc :: st -> if String.equal pc sc then strip pt st else None
      | _ :: _, [] -> None
    in
    match strip at segs with Some rest -> r.run rest | None -> None
  in
  { r with run }
```

`prefix` is internal, so it is not exposed in `proffer.mli`; it is used only by `Site.mount`.

- [ ] **Step 4: Thread `decorate` through `compiled.ml` and apply in `backend.ml`**

`compiled.ml` carries the new `decorate` field. In `backend.ml`, where a route handler `h` is selected, apply `decorate segs h` before running it. The fallback is decorated too (so security headers reach a 404). The 405 and the auth challenge are library responses and are not decorated by auth, but `with_headers` should still reach them, so apply `with_headers`' header addition to every outgoing `Resp.t`. **Decision:** keep it simple and correct: apply `decorate (Req.segments req)` only to the selected route handler and the fallback, matching the test. Note in a comment that a 405 does not carry `with_headers` headers, which is acceptable.

- [ ] **Step 5: Declare in `proffer.mli`**

Under `Site`:

```ocaml
val with_headers : (string * string) list -> 'env t -> 'env t @@ portable
(** [with_headers extra site] adds [extra] to every response a route or the
    fallback of [site] returns. It is how a site sets security headers once. *)

val with_auth :
  scope:string list list ->
  realm:string ->
  check:(string option -> bool) @ portable ->
  'env t ->
  'env t
  @@ portable
(** [with_auth ~scope ~realm ~check site] gates every path under a prefix in
    [scope] behind [check], which is given the Authorization header value. A
    failed check answers 401 with [WWW-Authenticate: Basic realm=...]. A path
    outside every prefix is served unchanged. *)

val mount : at:string list -> 'env t -> 'env t -> 'env t @@ portable
(** [mount ~at sub site] serves [sub] under the path prefix [at], alongside
    [site]'s own routes. A request whose path starts with [at] and matches a
    route of [sub] is answered by [sub]. *)
```

- [ ] **Step 6: Move the `Printf` line to the very end of `test_wrappers.ml`**

Ensure the file ends with exactly one `let () = Printf.printf "test_wrappers: %d checks ok\n" !checks`.

- [ ] **Step 7: Run to verify it passes**

Run: `dune build @bleeding/proffer/all @bleeding/proffer/runtest --force`
Expected: PASS, `test_wrappers: 11 checks ok` (4 from Task 5, 4 auth+headers, 2 mount, plus one adjust as counted).

- [ ] **Step 8: Commit**

```bash
git add bleeding/proffer/lib/site.ml bleeding/proffer/lib/route.ml \
  bleeding/proffer/lib/resp.ml bleeding/proffer/lib/compiled.ml \
  bleeding/proffer/lib/backend.ml bleeding/proffer/lib/proffer.mli \
  bleeding/proffer/test/test_wrappers.ml
git commit -m "Add Site.mount, with_headers and scoped with_auth"
```

### Task 7: Extend the httpz backend log event

**Files:**
- Modify: `bleeding/proffer/httpz/proffer_httpz.ml`, `bleeding/proffer/httpz/proffer_httpz.mli`
- Modify: `bleeding/proffer/test/test_httpz.ml`

**Interfaces:**
- Produces: an `event` record extended so arod's access log keeps every field it records today. New fields, all immutable data so the event still crosses domains: `path : string`, `request_headers : (string * string) list`, `response_content_type : string option`, `cache_status : string option`. `remote_addr`, `meth`, `target`, `status`, `body_size` (renamed nothing), `duration_us` stay.

Rationale (critical): arod's `Arod_log.log_request` (`avsm/arod/lib_log/arod_log.ml`) writes `remote_addr, forwarded_for, forwarded_proto, target, path, host, user_agent, referer, accept, request_headers (as JSON), status, response_content_type, cache_status, timestamp, response_body_size, duration_us`. Proffer's `event` carries only six of these. `forwarded_for`, `forwarded_proto`, `host`, `user_agent`, `referer`, `accept` are all request headers, so carrying `request_headers` lets the log derive them (arod already stores the full header JSON). `response_content_type` and `cache_status` come from the response, which the backend has. Without this extension the port loses access-log fidelity, which is real data loss, so this task is mandatory before Phase 3.

- [ ] **Step 1: Write the failing test (extend `test_httpz.ml`)**

Add a case that starts a server whose handler sets `Content-Type` and an `X-Cache` header, sends a request with an `Accept` and a `User-Agent`, and asserts the received `event` carries `path`, the request headers, the response content type, and the cache status. Use the existing `with_server`/`on_listening` harness. Concretely (adapt to the file's helpers):

```ocaml
let () =
  let seen = ref None in
  let site =
    Proffer.Site.of_routes
      [ Proffer.Route.get Proffer.Route.(s "p" /? nil) (fun _e _r ->
            Proffer.Resp.v ~content_type:"text/html; charset=utf-8"
              ~headers:[ ("X-Cache", "hit") ]
              (Proffer.Body.String "hi")) ]
  in
  let compiled = Proffer.Compiled.compile site in
  with_server compiled ~on_event:(fun e -> seen := Some e) (fun ~port ->
      let _ = request ~port ~headers:[ ("Accept", "text/html"); ("User-Agent", "t") ]
                "GET /p HTTP/1.1" in
      ());
  match !seen with
  | None -> check "event delivered" false
  | Some e ->
      check "event has path" (e.Proffer_httpz.path = "/p");
      check "event has request headers"
        (List.mem_assoc "accept" (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) e.Proffer_httpz.request_headers));
      check "event has response content type"
        (e.Proffer_httpz.response_content_type = Some "text/html; charset=utf-8");
      check "event has cache status" (e.Proffer_httpz.cache_status = Some "hit")
```

- [ ] **Step 2: Run to verify it fails**

Run: `dune build @bleeding/proffer/runtest --force`. Expected: FAIL, `Unbound record field path`.

- [ ] **Step 3: Extend the `event` record and its construction**

In `proffer_httpz.ml`, add the four fields to `type event`. Populate them where the event is built (the `emit` closure in `handle_request`): `path` from `Proffer.Req.path preq` (build `preq` before the event, which it already is on the success path; for the refuse/timeout paths where no `preq` exists, set `path = ""`, `request_headers = []`, `response_content_type = None`, `cache_status = None`). `request_headers` from `req_headers` (already computed). `response_content_type` from the outcome's headers via `Proffer.Headers.find outcome.headers "content-type"`. `cache_status` from `Proffer.Headers.find outcome.headers "x-cache"`.

- [ ] **Step 4: Document the new fields in `proffer_httpz.mli`**

Add a doc comment to each new field in the house style. Note that `request_headers` carries names lowercased or as-sent per `Proffer.Headers`, and that `cache_status` is whatever the handler set in `X-Cache`, or `None`.

- [ ] **Step 5: Run to verify it passes**

Run: `dune build @bleeding/proffer/all @bleeding/proffer/runtest --force`
Expected: PASS, `test_httpz` check count up by four.

- [ ] **Step 6: Commit**

```bash
git add bleeding/proffer/httpz/proffer_httpz.ml \
  bleeding/proffer/httpz/proffer_httpz.mli bleeding/proffer/test/test_httpz.ml
git commit -m "Carry request headers, path, content type and cache status in the log event"
```

### Task 8: Map the Static node in the httpz backend

**Files:**
- Modify: `bleeding/proffer/lib/site.ml`, `bleeding/proffer/lib/compiled.ml`, `bleeding/proffer/lib/proffer.mli`, `bleeding/proffer/httpz/proffer_httpz.ml`, `bleeding/proffer/httpz/proffer_httpz.mli`
- Modify: `bleeding/proffer/test/test_httpz.ml`

**Interfaces:**
- Produces: `Proffer.Site.static : at:string list -> Static.t -> 'env t -> 'env t` — mounts a served directory at a path prefix. The backend resolves it against a `dir` mapping supplied to `run`.
- Consumes: `Proffer.Static.t` (Task 2), `Mime.of_path` (Task 1), `Static.confine` (Task 2).

Rationale: arod serves `/images/**` from disk. The core cannot open files, so `Site.static` records the mount as data and `Proffer_httpz.run` gains `?static:(string -> Eio.Fs.dir_ty Eio.Path.t option)` mapping a `Static.root` label to a real directory. The backend, on a path under the mount, confines the tail with `Static.confine`, resolves the label, loads the file, and answers with `Mime.of_path` and the node's cache policy, or 404. The resolver's documentation must require a capability from `Eio.Path.open_subtree` or `Eio.Stdenv.cwd`, never `Eio.Stdenv.fs`, per the capability discipline in Global Constraints.

Design note: this is the one core feature that needs backend cooperation, so it is split from Task 2. If time is short, arod can instead keep a normal `rest` route whose handler opens files through its `env` (using `Static.confine` directly), and this task can be deferred. **Decision for the plan:** implement the handler-through-env approach in Phase 3 (it needs no backend change and reuses `Static.confine`), and mark this Task 8 as OPTIONAL. Arod's port (Task 12) uses `Static.confine` in a `rest` handler, exactly as the current fixed `static_file` does, so the traversal fix carries over unchanged. Skip Task 8 unless a backend-native static path is wanted for performance.

- [ ] **Step 1 (only if implementing):** Follow the same TDD shape: a test that serves a temp directory through `run ~static`, asserting a file is served with the right MIME and a `..` tail gives 404. Then implement. Commit as "Serve Proffer.Static from the httpz backend".

---

## Phase 2 — Proffer release verification

### Task 9: Update docs and changelog for the new surface

**Files:**
- Modify: `bleeding/proffer/README.md`, `bleeding/proffer/CHANGES.md`, `bleeding/proffer/doc/index.mld`, `PROFFER.md`

- [ ] **Step 1:** In `CHANGES.md`, under `## v0.1.0 (unreleased)`, add one line per new module: `Mime`, `Static`, `Cache`, `Negotiate`, `Route.moved`/`found`, `Site.mount`/`with_auth`/`with_headers`, and the richer log event.
- [ ] **Step 2:** In `README.md`, add a short "Combinators" section showing `Negotiate.v`, `Site.static`/`with_auth`, and the ETag cache, adapted from arod's usage.
- [ ] **Step 3:** In `PROFFER.md`, move `Cache`, `Negotiate`, `Site.mount` and the wrappers from "not implemented" to done in the "Status and plan" section. Do not touch the portability-groundwork log.
- [ ] **Step 3a:** In `PROFFER.md`, under "Environments as capabilities", add two sentences: a filesystem closure in `env` is built over `Eio.Path.open_subtree` or `Eio.Stdenv.cwd`, never `Eio.Stdenv.fs`, and `Static.confine` sits on top as the layer that turns an escape attempt into a 404.
- [ ] **Step 4: Commit**

```bash
git add bleeding/proffer/README.md bleeding/proffer/CHANGES.md \
  bleeding/proffer/doc/index.mld PROFFER.md
git commit -m "Document the filled-in proffer core"
```

### Task 10: Full proffer verification gate

- [ ] **Step 1:** `dune build @bleeding/proffer/all @bleeding/proffer/runtest @bleeding/proffer/spike --force` — all green, every suite prints its count.
- [ ] **Step 2:** `dune build -p proffer @bleeding/proffer/install @bleeding/proffer/runtest` — the `proffer` package builds and tests in release mode. (`-p proffer-httpz` cannot be verified locally because it masks the workspace-local `httpz`; note this, do not treat it as a failure.)
- [ ] **Step 3:** No commit; this is a gate. If anything is red, fix it in the task that introduced it before proceeding to Phase 3.

---

## Phase 3 — Port arod onto proffer

This phase changes arod. It does not change proffer. Every task keeps `dune build @avsm/arod/all` green. The path-traversal fix from the prerequisite state is preserved: `static_file`'s confinement becomes `Static.confine`.

### Task 11: The arod env capability record

**Files:**
- Create: `avsm/arod/lib_handlers/arod_env.ml`, `avsm/arod/lib_handlers/arod_env.mli`
- Modify: `avsm/arod/lib_handlers/dune`

**Interfaces:**
- Produces: `Arod_env.t`, a record of the capabilities handlers need, built once per domain. Fields, from the current handler arguments (`~ctx ~cache ~search ~log ~fs`):

```ocaml
type t = {
  ctx : Arod.Ctx.t;
  cache : Proffer.Cache.t;
  search : limit:int -> string -> Arod_search.result list;  (* wraps Arod_search.search over this domain's handle *)
  read_image : string list -> string option;    (* confined, over Eio.Path *)
  read_paper : string -> string option;          (* confined, over Eio.Path *)
  reader : unit -> Sqlite3_eio.t;                (* access-log reader for stats *)
  now : unit -> float;                           (* over Eio clock *)
}
```

Rationale: handlers are `@ portable`, so they cannot hold `ctx`, `fs`, `search` or the log directly. Each becomes a closure in `env`, built in `main.ml` where the domain-bound values exist. `read_image`/`read_paper` wrap `Static.confine` plus `Eio.Path.load` over a directory opened with `Eio.Path.open_subtree`, never over `Eio.Stdenv.fs`, so the traversal fix lives in the env builder and the OS refuses an escape even if the string check were wrong. `search` wraps `Arod_search.query`. `now` wraps `Eio.Time.now`.

- [ ] **Step 1: Write `arod_env.mli`**

```ocaml
(*--- ISC header copied from a sibling arod file ---*)

(** The capabilities an arod handler reaches through its [env] argument.
    Handlers are portable, so they cannot capture domain-bound state. This
    record is built once per domain, over that domain's resources. *)

type t = {
  ctx : Arod.Ctx.t;
  cache : Proffer.Cache.t;
  search : limit:int -> string -> Arod_search.result list;
  read_image : string list -> string option;
  read_paper : string -> string option;
  reader : unit -> Sqlite3_eio.t;
  now : unit -> float;
}
```

The concrete wrappers built in `main.ml` (Task 14): `search = (fun ~limit q -> Arod_search.search handle ~limit q)`, `now = (fun () -> Eio.Time.now clock)`, `read_image = (fun segs -> Option.bind (Proffer.Static.confine segs) (fun p -> try Some (Eio.Path.load Eio.Path.(images_dir / p)) with _ -> None))`, `read_paper` likewise over `papers_dir` with a single-segment list. `images_dir` and `papers_dir` are the `Eio.Path.open_subtree` capabilities Task 14 Step 3 builds, so these closures hold no unrestricted filesystem access. The `.mli` doc for `read_image` and `read_paper` says the read is confined to the served directory and an escape attempt is [None].

- [ ] **Step 2: Write `arod_env.ml`** — just `type t = { ... }` matching the mli (a record, no functions).

- [ ] **Step 3:** Add `proffer` and `arod.search` to `arod_handlers`' `dune` libraries if not present. Build `dune build @avsm/arod/all` — expect it to still compile (nothing uses `Arod_env` yet).

- [ ] **Step 4: Commit**

```bash
git add avsm/arod/lib_handlers/arod_env.ml avsm/arod/lib_handlers/arod_env.mli \
  avsm/arod/lib_handlers/dune
git commit -m "Add arod env capability record"
```

### Task 12: Port the handlers

**Files:**
- Modify: `avsm/arod/lib_handlers/arod_handlers.ml`, `avsm/arod/lib_handlers/arod_handlers.mli`

This is the largest task. It rewrites ~1100 lines. Because it is one coherent interface change, it is one task, but its steps are grouped by handler family so a reviewer can check each family. Keep the file compiling by porting bottom-up: the helpers first, then each family, then `all_routes`.

**Interfaces:**
- Consumes: `Arod_env.t`, all of `Proffer` (`Resp`, `Req`, `Route`, `Site`, `Negotiate`, `Cache`, `Static`, `Mime`).
- Produces: `Arod_site.build : Arod_env.t -> Arod_env.t Proffer.Site.t` (moved to Task 13) is fed by these handlers. Each handler becomes `Arod_env.t -> Proffer.Req.t -> Proffer.Resp.t`, or a curried form a route applies captures to.

- [ ] **Step 1: Delete the boilerplate block.** Remove `send_html`, `send_html_empty`, `send_atom`, `send_atom_empty`, `send_json`, `send_json_empty`, `send_file`, `send_file_empty`, `send_markdown`, `send_html_vary`, `send_file_immutable`, `mime_type_of_path`, `wants_markdown`, and every `R.is_head` branch. Proffer's backend suppresses HEAD bodies, so no handler tests HEAD. Content types come from `Resp.html`/`Resp.media`/`Resp.text` and `Mime.of_path`.

- [ ] **Step 2: Port the cache wrappers.** Replace `cached`, `cached_atom`, `cached_json` and `negotiated` with `Proffer.Cache.memoize` plus `Negotiate.v`. A cached HTML page becomes:

```ocaml
let cached env ~key ~content_type gen =
  let body, etag = Proffer.Cache.memoize env.Arod_env.cache ~now:(env.now ()) ~key gen in
  Proffer.Resp.media ~etag ~cache:page_cache content_type body
```

where `page_cache` is a module-level `Cache_control.t`. The `X-Cache: hit/miss` header is dropped in favour of the ETag and a real 304, which the backend now issues. If the stats dashboard's traffic breakdown depends on `X-Cache` in the log, keep emitting it: add it via `Resp.add_headers [ ("X-Cache", if hit then "hit" else "miss") ]`, deriving hit/miss from `Cache.stats` deltas, or accept that `cache_status` in the log becomes `None`. **Decision:** preserve `X-Cache` so the dashboard is unchanged. `Cache.memoize` does not report hit/miss per call, so add `Cache.memoize'` returning `string * Etag.t * [ `Hit | `Miss ]` in a small proffer follow-up, OR compute it in arod by checking `Cache.stats` before and after. Simplest: check stats delta. Document the choice in a comment.

- [ ] **Step 3: Port content-negotiated pages** (`index`, `paper`, `papers_list`, `idea`, `ideas_list`, `note`, `notes_list`, `video`, `videos_list`, `project`, `projects_list`, `links_list`, `network_page` — the 15 `get_h1 ... Accept` routes). Each becomes `Negotiate.v [ (`Html, html_handler); (`Markdown, md_handler) ]`, where each variant handler is a cached render.

- [ ] **Step 4: Port the redirect routes** (21 of them) to `Route.moved`/`Route.found` in the route table, except `/tags/<t>` and `/news/<slug>` which capture and stay `get` with `Resp.redirect`.

- [ ] **Step 5: Port the static/asset routes.** `static_file ~dir segs` becomes a handler that calls `env.read_image segs` (which wraps `Static.confine`), and `embedded_file`/`embedded_file_immutable`/`js_file` become `Resp.media` over `Arod_assets.read` with `Mime.of_path`. The papers PDF route uses `env.read_paper slug`.

- [ ] **Step 6: Port the API and misc routes** (`pagination_api`, `search_api`, `well_known`, `robots_txt`, `sitemap`, `blogroll_opml`) to `Resp.media`/`Resp.text` over the same generators, reading `Req.query_param`/`Req.query` instead of `R.query_param`.

- [ ] **Step 7: Update `arod_handlers.mli`** to export the ported handlers with their new `Arod_env.t -> Proffer.Req.t -> Proffer.Resp.t` (or curried) types, and keep `confined_path` exported (now delegating to `Static.confine`, or removed in favour of it — prefer removing arod's copy and re-exporting `Proffer.Static.confine`, updating `test_static.ml` to call `Proffer.Static.confine`).

- [ ] **Step 8: Build.** `dune build @avsm/arod/all` — resolve every type error. This step is done when arod compiles against the new handler types, even though `all_routes` may be temporarily broken (it is replaced in Task 13).

- [ ] **Step 9: Commit**

```bash
git add avsm/arod/lib_handlers/arod_handlers.ml avsm/arod/lib_handlers/arod_handlers.mli
git commit -m "Port arod handlers to proffer responses"
```

### Task 13: The arod site builder

**Files:**
- Create: `avsm/arod/lib/server/arod_site.ml`, `avsm/arod/lib/server/arod_site.mli`
- Modify: `avsm/arod/lib_handlers/arod_handlers.ml` (remove `all_routes`), `avsm/arod/lib/server/dune`

**Interfaces:**
- Produces: `Arod_site.build : unit -> Arod_env.t Proffer.Compiled.t` — the compiled site, built from the ported handlers with the route table, wrapped with `Site.with_auth ~scope:[["action"]]` and `Site.with_headers` for security headers.

- [ ] **Step 1:** Move `all_routes`' body into `Arod_site.build`, translating each `get_/get/get_h1` to `Proffer.Route.get`/`moved`/`found` and the negotiated handlers to `Negotiate.v`. The stats routes under `/action` drop their per-handler `check_stats_auth` (now `Site.with_auth`). Compile the site.
- [ ] **Step 2:** `dune build @avsm/arod/all`.
- [ ] **Step 3: Commit** `git commit -m "Build the arod site from proffer routes"`.

### Task 14: Swap the server and the access-log bridge

**Files:**
- Modify: `avsm/arod/lib/server/arod_server.ml`, `avsm/arod/lib_log/arod_log.ml`, `avsm/arod/bin/main.ml`, the `dune` files
- Delete: `avsm/arod/lib/arod_cache.ml` (and its `.mli` if any)

**Interfaces:**
- Consumes: `Proffer_httpz.run`, `Proffer_httpz.event`, `Arod_site.build`, `Arod_env.t`.

- [ ] **Step 1:** Rewrite `Arod_log.log_request` to take a `Proffer_httpz.event`. Map its fields: `remote_addr`, `target`, `path`, `status`, `duration_us`, `response_body_size` (from `body_size`), `response_content_type`, `cache_status` directly; derive `host`, `user_agent`, `referer`, `accept`, `forwarded_for`, `forwarded_proto` from `event.request_headers` with a case-insensitive lookup; serialise `request_headers` to JSON as today. Keep the SQLite schema unchanged.
- [ ] **Step 2:** Rewrite `Arod_server.run` to build the env factory and call `Proffer_httpz.run ~sw ~net ~clock ~addr ~config ~on_event ~on_error ~env compiled`. `on_event` feeds `Arod_log.log_request` and the `Logs` line. Because `on_event` runs on the serving domain and the log DB is domain-bound, bridge through a queue drained by a fiber on the starting domain (PROFFER.md "Errors and observability"). For a single-domain start, a direct call is fine; implement the direct call now and note the queue bridge as the multi-domain follow-up.
- [ ] **Step 3:** In `main.ml`, build `Arod_env.t` (wrapping `ctx`, a `Proffer.Cache.create ~ttl:300.`, `search`, the confined `read_image`/`read_paper`, the log `reader`, and `now` over the clock), compile the site with `Arod_site.build`, and pass both to `Arod_server.run`. Delete the `Arod.Cache` creation. The served directories are opened once, inside the server switch, as subtree capabilities:

```ocaml
let fs = Eio.Stdenv.fs env in
let cwd = Eio.Stdenv.cwd env in
(* A relative configured path is resolved against cwd. Only an absolute one
   needs fs, and the subtree capability is all the server keeps. *)
let confined_dir ~sw dir =
  let base = if Filename.is_relative dir then cwd else fs in
  Eio.Path.open_subtree ~sw Eio.Path.(base / dir)
in
let images_dir = confined_dir ~sw cfg.paths.images_dir in
let papers_dir = confined_dir ~sw cfg.paths.papers_dir in
```

`read_image`/`read_paper` close over `images_dir`/`papers_dir` only. After `Arod.Ctx.create` and the XDG log path are built, `fs` must not be captured by anything reachable from a handler. Verify by grepping `main.ml`'s serve command for `fs`: every use is startup-only (`Ctx.create`, `Xdge.create`, `confined_dir`).
- [ ] **Step 4:** Delete `avsm/arod/lib/arod_cache.ml`. Remove `Cache` from `avsm/arod/lib/arod.ml` if it re-exports it. Grep for `Arod.Cache` and `Arod_cache` and fix every use (they should all be gone after Task 12).
- [ ] **Step 5:** Update the four `dune` files: `arod_handlers`, `arod_server` drop `httpz.route`/`httpz.eio_server` and gain `proffer`, `proffer-httpz`; `arod_log` drops `httpz.eio_server`. Keep `httpz` only if still referenced.
- [ ] **Step 6:** `dune build @avsm/arod/all @avsm/arod/runtest` — green, `test_static` still passes (now via `Proffer.Static.confine`).
- [ ] **Step 7: Commit** `git commit -m "Serve arod through proffer-httpz"`.

### Task 15: Manual smoke test and traversal re-verification

- [ ] **Step 1:** Run arod locally against a test data dir (`dune exec avsm/arod/bin/main.exe -- serve --config <test config>` or the project's run recipe). Confirm the home page, a paper page, a `.md` variant (with `Accept: text/markdown`), an atom feed, an image, and the stats dashboard behind Basic auth all respond.
- [ ] **Step 2:** Re-run the traversal probes against the local server, not production: `curl --path-as-is 'http://localhost:<port>/images/../../../../etc/hostname'` must be 404. Also `/papers/../../../etc/hostname.pdf`.
- [ ] **Step 2a:** Probe the symlink escape the string check cannot see: `ln -s /etc/hostname <test images dir>/leak` and confirm `curl 'http://localhost:<port>/images/leak'` is 404, then remove the link. This is new behaviour from `Eio.Path.open_subtree`. The old server, whose `images_dir` hung off `Eio.Stdenv.fs`, would have served it.
- [ ] **Step 3:** Confirm a conditional request now works: fetch a cached page, note its `ETag`, re-fetch with `If-None-Match` and confirm a 304. This is new behaviour arod never had.
- [ ] **Step 4:** No commit; this is a gate. Record the results in the PR description.

---

## Self-review notes

- **Spec coverage:** PROFFER.md item 1 (deferred core) → Tasks 1–8. Item 2 (port arod, multi-domain) → Phase 3; multi-domain `run` is noted as a follow-up in Task 14 Step 2 rather than built, because arod runs single-domain today and the user's goal is the port. If multi-domain is wanted, add a task after Task 14 taking `run` through `Domain_manager.run` per the spike, and convert the log bridge to the queue form.
- **The one deferred spec item:** `proffer-cohttp` (spec item 3) is out of scope for this plan, which targets arod on `proffer-httpz`.
- **Type consistency:** `Arod_env.t` fields are named identically in Tasks 11, 12, 13, 14. `Cache.memoize` returns `string * Etag.t` in Task 3 and is consumed that way in Task 12. `event`'s new fields (`path`, `request_headers`, `response_content_type`, `cache_status`) are defined in Task 7 and consumed in Task 14 Step 1.
- **Known risk:** the OxCaml mode annotations on `Site.decorate`, `Cache.t`'s `Atomic`, and `Negotiate.v`'s captured variant list are the parts most likely to need adjustment at implementation time. Each task says to resolve against the compiler rather than guessing, and the neighbouring committed code (`route.ml`, `site.ml`, the vendored cache in `spike/`) is the precedent.
- **Preserved security fix:** the traversal confinement is never removed, only relocated from `Arod_handlers.confined_path` to `Proffer.Static.confine`, with `test_static` following it.
- **Capability discipline:** the port also narrows the filesystem capability. Today `all_routes` captures `Eio.Stdenv.fs`. After Task 14 the handlers reach disk only through `read_image`/`read_paper` closures over `Eio.Path.open_subtree` capabilities, `fs` is startup-only in `main.ml`, and Task 15 Step 2a proves the symlink escape the string check cannot see is refused.

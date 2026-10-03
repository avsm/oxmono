# ecosystems Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** An OCaml client for packages.ecosyste.ms, generated from its OpenAPI spec with a thin hand-written layer.

**Architecture:** `bleeding/ecosystems/` follows `bleeding/karakeep`. `openapi-gen` turns the pinned upstream YAML into a checked-in `ecosystems.ml` and `ecosystems.mli`. A small library `ecosystems.client` adds a base-URL and User-Agent constructor and a page iterator.

**Tech Stack:** OxCaml `5.2.0+ox`, dune 3.21, `openapi`, `fetch`, `fetch-curl`, `eio`, `jsont`.

**Spec:** `docs/superpowers/specs/2026-10-03-ecosystems-design.md`

## Global Constraints

- Package and library name `ecosystems`, directory `bleeding/ecosystems/`.
- The pinned spec is `ecosystems-openapi-spec.yaml`, the unmodified upstream 1.1.0 file.
- Default base URL `https://packages.ecosyste.ms/api/v1`.
- No test reaches the network. Fixtures are recorded once and committed.
- Work on a branch. Generated output is committed apart from hand-written code.
- Build with `dune build @bleeding/ecosystems/all @bleeding/ecosystems/runtest --force`. `ocamlformat` is absent, so match formatting by hand within 80 columns.
- Prose follows CLAUDE.md: no em-dashes, no semicolons joining clauses, `[foo x] is ...` doc style.
- No `ai_disclosure` attributes and no `x-ai` opam fields.
- Never regenerate a golden file to make a test pass.

## Review Focus

- A fixture field that is `null` where the schema is not nullable. The decoder must accept what the live API returns.
- `pages` over an empty first page. It yields nothing and calls `f` once.
- `pages` over a short final page. It must stop without a further call.
- `page` and `per_page` are `string` in the generated signatures although the spec says integer. `pages` owns the conversion.
- A 404 from the API. It surfaces as the generated client's error, not a decode exception.
- `bulk_lookup_packages` takes `body:Jsont.json`, because the generator leaves the inline object schema opaque. The doc must say so.

---

## Task 1: Scaffold and generated core

**Files:**
- Create: `bleeding/ecosystems/{dune-project,ecosystems.opam,dune,dune.inc,ecosystems-openapi-spec.yaml,ecosystems.ml,ecosystems.mli,test/dune,test/test.ml}`

**Interfaces:**
- Produces: library `ecosystems` exposing `Ecosystems.t`, `Ecosystems.create`, `Ecosystems.of_fetch`, and modules `Registry`, `Package`, `Version`, `Maintainer`, `Namespace`, `Advisory`, `Keyword`, `Dependency`, `CodeMeta`, `PackageWithRegistry`, `VersionWithPackage`, `VersionWithDependencies`, `VersionLookup`, `KeywordWithPackages`, and `Client`. Operations such as `Client.get_registries : ?ecosystem:string -> ?page:string -> ?per_page:string -> t -> unit -> Registry.T.t list` live in `Client`.

- [ ] **Step 1: Branch**

```bash
git checkout -b ecosystems
mkdir -p bleeding/ecosystems/test
```

- [ ] **Step 2: Pin the spec**

```bash
curl -sL -o bleeding/ecosystems/ecosystems-openapi-spec.yaml \
  https://packages.ecosyste.ms/docs/api/v1/openapi.yaml
head -3 bleeding/ecosystems/ecosystems-openapi-spec.yaml
```

Expected: `openapi: 3.0.1`.

- [ ] **Step 3: Write `dune-project`**

```
(lang dune 3.21)
(name ecosystems)

(generate_opam_files true)

(license ISC)
(authors "Anil Madhavapeddy")
(maintainers "anil@recoil.org")
(source (tangled anil.recoil.org/ocaml-ecosystems))

(package
 (name ecosystems)
 (synopsis "Client for the packages.ecosyste.ms API")
 (description
  "An OCaml client for the read-only packages.ecosyste.ms API, generated
   from its OpenAPI specification. Covers registries, packages, versions,
   dependents, maintainers, namespaces, advisories and keywords.")
 (depends
  (ocaml (>= "5.2.0"))
  (openapi (>= "0.4.0"))
  fetch
  fetch-curl
  (eio (>= "1.2"))
  eio_main
  (jsont (>= "0.2.0"))
  bytesrw
  (ptime (>= "1.0.0"))
  (odoc :with-doc)))
```

- [ ] **Step 4: Write `dune` and `dune.inc`**

`dune`:

```
(library
 (name ecosystems)
 (public_name ecosystems)
 (libraries openapi jsont jsont.bytesrw fetch fetch-curl ptime eio)
 (wrapped true))

(include dune.inc)
```

`dune.inc`:

```
; Generated rules for OpenAPI code regeneration
; Run: dune build @gen --auto-promote

(rule
 (alias gen)
 (mode (promote (until-clean)))
 (targets ecosystems.ml ecosystems.mli)
 (deps ecosystems-openapi-spec.yaml)
 (action
  (run openapi-gen generate --code-only -o . -n ecosystems %{deps})))
```

- [ ] **Step 5: Generate**

Run: `dune build @bleeding/ecosystems/gen --auto-promote`
Expected: `ecosystems.ml` (about 1700 lines) and `ecosystems.mli` (about 1060 lines) appear. A scratch run on 2026-10-03 parsed the spec and emitted both files without error.

- [ ] **Step 6: Compile the generated code**

Run: `dune build @bleeding/ecosystems/all`

Expected: success. If the generator emitted code that does not compile, fix the generator in `bleeding/openapi/lib/`, add a regression test beside the existing ones in `bleeding/openapi/test/`, rerun Step 5, and commit the generator fix on its own before continuing. The user pre-authorised this.

- [ ] **Step 7: Smoke test**

`test/dune`:

```
(test
 (name test)
 (modules test)
 (libraries ecosystems eio_main))
```

`test/test.ml`:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let c =
    Ecosystems.create ~sw env ~base_url:"https://packages.ecosyste.ms/api/v1"
  in
  assert (Ecosystems.base_url c = "https://packages.ecosyste.ms/api/v1")
```

Run: `dune build @bleeding/ecosystems/runtest --force`
Expected: PASS.

- [ ] **Step 8: Commit the generated core**

```bash
git add bleeding/ecosystems
git commit -m "Add ecosystems client generated from the packages.ecosyste.ms spec"
```

## Task 2: Spec and generated code stay in sync

**Files:**
- Modify: `bleeding/ecosystems/dune.inc`

**Interfaces:**
- Consumes: Task 1 spec and generated files.

- [ ] **Step 1: Add the sync rule to `dune.inc`**

```
(rule
 (alias runtest)
 (deps ecosystems-openapi-spec.yaml ecosystems.ml ecosystems.mli)
 (action
  (progn
   (run openapi-gen generate --code-only -o regen -n ecosystems
    ecosystems-openapi-spec.yaml)
   (diff ecosystems.ml regen/ecosystems.ml)
   (diff ecosystems.mli regen/ecosystems.mli))))
```

- [ ] **Step 2: Run it, expecting pass**

Run: `dune build @bleeding/ecosystems/runtest --force`
Expected: PASS.

- [ ] **Step 3: Prove it detects drift**

```bash
echo "(* drift *)" >> bleeding/ecosystems/ecosystems.mli
dune build @bleeding/ecosystems/runtest --force 2>&1 | head
git checkout bleeding/ecosystems/ecosystems.mli
```

Expected: the first run FAILS with a diff, and the checkout restores a passing tree.

- [ ] **Step 4: Commit**

```bash
git add bleeding/ecosystems/dune.inc
git commit -m "Check generated ecosystems code against the pinned spec"
```

## Task 3: Page iterator

**Files:**
- Create: `bleeding/ecosystems/lib/{dune,ecosystems_client.ml,ecosystems_client.mli}`, `bleeding/ecosystems/test/test_pages.ml`
- Modify: `bleeding/ecosystems/test/dune`

**Interfaces:**
- Produces: `Ecosystems_client.pages : ?per_page:int -> (page:string -> per_page:string -> 'a list) -> 'a Seq.t`. It yields the items of each page in order, starting at page 1. It stops after a page with no items or with fewer than `per_page` items. `per_page` defaults to 100.

- [ ] **Step 1: Write the failing test** `test/test_pages.ml`

```ocaml
let fake ~total ~calls ~page ~per_page =
  incr calls;
  let page = int_of_string page and n = int_of_string per_page in
  let first = ((page - 1) * n) + 1 in
  List.init (max 0 (min n (total - first + 1))) (fun i -> first + i)

let run total per_page =
  let calls = ref 0 in
  let items =
    Ecosystems_client.pages ~per_page (fake ~total ~calls)
    |> List.of_seq
  in
  (items, !calls)

let () =
  assert (run 0 10 = ([], 1));
  assert (run 7 10 = (List.init 7 succ, 1));
  assert (run 20 10 = (List.init 20 succ, 3));
  assert (run 25 10 = (List.init 25 succ, 3));
  let calls = ref 0 in
  let seq = Ecosystems_client.pages ~per_page:10 (fake ~total:100 ~calls) in
  ignore (Seq.take 5 seq |> List.of_seq);
  assert (!calls = 1)
```

Add to `test/dune`:

```
(test
 (name test_pages)
 (modules test_pages)
 (libraries ecosystems.client))
```

- [ ] **Step 2: Run, expecting failure**

Run: `dune build @bleeding/ecosystems/runtest --force 2>&1 | head`
Expected: FAIL, unbound library `ecosystems.client`.

- [ ] **Step 3: Implement**

`lib/dune`:

```
(library
 (name ecosystems_client)
 (public_name ecosystems.client)
 (libraries ecosystems eio fetch fetch-curl))
```

`lib/ecosystems_client.mli`:

```ocaml
val pages :
  ?per_page:int -> (page:string -> per_page:string -> 'a list) -> 'a Seq.t
(** [pages ?per_page f] is the items of every page of [f], in order.
    [f ~page ~per_page] fetches one page. Pages are numbered from 1. The
    sequence is lazy and stops after a page with fewer than [per_page] items.
    [per_page] defaults to 100. The arguments are strings because the
    generated operations take them as strings. *)
```

`lib/ecosystems_client.ml`:

```ocaml
let pages ?(per_page = 100) f =
  let rec from page () =
    match f ~page:(string_of_int page) ~per_page:(string_of_int per_page) with
    | [] -> Seq.Nil
    | items ->
        let rest =
          if List.length items < per_page then Seq.empty else from (page + 1)
        in
        Seq.append (List.to_seq items) rest ()
  in
  from 1
```

- [ ] **Step 4: Run, expecting pass**

Run: `dune build @bleeding/ecosystems/runtest --force`
Expected: PASS. The last assertion in the test proves laziness.

- [ ] **Step 5: Commit**

```bash
git add bleeding/ecosystems
git commit -m "Add a page iterator to the ecosystems client"
```

## Task 4: Constructor with base URL and User-Agent

**Files:**
- Modify: `bleeding/ecosystems/lib/ecosystems_client.{ml,mli}`, `bleeding/ecosystems/test/test.ml`

**Interfaces:**
- Produces: `Ecosystems_client.default_base_url : string` and `Ecosystems_client.create : ?user_agent:string -> ?base_url:string -> sw:Eio.Switch.t -> env -> Ecosystems.t`, where `env` has the same object type as `Ecosystems.create`.

- [ ] **Step 1: Extend the smoke test** in `test/test.ml`

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let c = Ecosystems_client.create ~sw env in
  assert (Ecosystems.base_url c = Ecosystems_client.default_base_url);
  let c = Ecosystems_client.create ~user_agent:"t/1" ~base_url:"http://x/" ~sw env in
  assert (Ecosystems.base_url c = "http://x/")
```

Add `ecosystems.client` to `libraries` of the `test` stanza in `test/dune`.

- [ ] **Step 2: Run, expecting failure**

Expected: FAIL, unbound value `Ecosystems_client.create`.

- [ ] **Step 3: Implement**

Add to the `.mli`:

```ocaml
val default_base_url : string
(** [default_base_url] is ["https://packages.ecosyste.ms/api/v1"]. *)

val create :
  ?user_agent:string ->
  ?base_url:string ->
  sw:Eio.Switch.t ->
  < clock : _ Eio.Time.clock
  ; mono_clock : _ Eio.Time.Mono.t
  ; secure_random : _ Eio.Flow.source
  ; .. > ->
  Ecosystems.t
(** [create ?user_agent ?base_url ~sw env] is a client for the public API.
    [user_agent] defaults to ["ocaml-ecosystems"]. [base_url] defaults to
    {!default_base_url}. *)
```

Add to the `.ml`:

```ocaml
let default_base_url = "https://packages.ecosyste.ms/api/v1"

let create ?(user_agent = "ocaml-ecosystems") ?(base_url = default_base_url)
    ~sw env =
  let session = Fetch_curl.v ~sw ~user_agent () in
  Ecosystems.create ~session ~sw env ~base_url
```

If `Fetch_curl.v` returns a type `Ecosystems.create ~session` rejects, read `bleeding/fetch/curl/fetch_curl.mli` and use its documented conversion. Do not guess.

- [ ] **Step 4: Run, expecting pass, then commit**

```bash
dune build @bleeding/ecosystems/all @bleeding/ecosystems/runtest --force
git add bleeding/ecosystems
git commit -m "Add a constructor to the ecosystems client with a User-Agent"
```

## Task 5: Decode fixtures

**Files:**
- Create: `bleeding/ecosystems/test/fixtures/*.json`, `bleeding/ecosystems/test/test_decode.ml`
- Modify: `bleeding/ecosystems/test/dune`

**Interfaces:**
- Consumes: `Ecosystems.<M>.T.jsont` for each schema `<M>`.

- [ ] **Step 1: Record one live response per schema, once**

```bash
cd bleeding/ecosystems/test && mkdir -p fixtures && cd fixtures
B=https://packages.ecosyste.ms/api/v1
curl -s "$B/registries?per_page=1"                          -o registries.json
curl -s "$B/registries/crates.io/packages/serde"            -o package.json
curl -s "$B/registries/crates.io/packages/serde/versions?per_page=1" -o versions.json
curl -s "$B/registries/crates.io/packages/serde/versions/1.0.0" -o version.json
curl -s "$B/registries/crates.io/maintainers?per_page=1"    -o maintainers.json
curl -s "$B/registries/crates.io/namespaces?per_page=1"     -o namespaces.json
curl -s "$B/keywords?per_page=1"                            -o keywords.json
curl -s "$B/keywords/rust?per_page=1"                       -o keyword.json
curl -s "$B/registries/crates.io/packages/serde/versions/1.0.0/codemeta" -o codemeta.json
for f in *.json; do head -c 1 $f | grep -q '[\[{]' || echo "BAD $f"; done
```

Expected: no `BAD` lines. If an endpoint returns a non-JSON error, choose another package, and record the choice in the commit message. Open each file and read it before committing it.

- [ ] **Step 2: Write the failing test** `test/test_decode.ml`

```ocaml
let read p = In_channel.with_open_bin ("fixtures/" ^ p) In_channel.input_all

let ok name = function
  | Ok _ -> ()
  | Error e -> failwith (name ^ ": " ^ e)

let list codec = Jsont.list codec
let dec = Openapi.Runtime.Json.decode

let () =
  ok "registries" (dec (list Ecosystems.Registry.T.jsont) (read "registries.json"));
  ok "package" (dec Ecosystems.Package.T.jsont (read "package.json"));
  ok "versions" (dec (list Ecosystems.Version.T.jsont) (read "versions.json"));
  ok "version" (dec Ecosystems.Version.T.jsont (read "version.json"));
  ok "maintainers" (dec (list Ecosystems.Maintainer.T.jsont) (read "maintainers.json"));
  ok "namespaces" (dec (list Ecosystems.Namespace.T.jsont) (read "namespaces.json"));
  ok "keywords" (dec (list Ecosystems.Keyword.T.jsont) (read "keywords.json"));
  ok "keyword" (dec Ecosystems.KeywordWithPackages.T.jsont (read "keyword.json"));
  ok "codemeta" (dec Ecosystems.CodeMeta.T.jsont (read "codemeta.json"))
```

`test/dune` addition:

```
(test
 (name test_decode)
 (modules test_decode)
 (deps (glob_files fixtures/*.json))
 (libraries ecosystems openapi jsont))
```

- [ ] **Step 3: Run**

Run: `dune build @bleeding/ecosystems/runtest --force`

Expected: each decode either passes or names the failing fixture. The `Error` payload type of `Openapi.Runtime.Json.decode` may not be `string`. If the compiler objects, match the real type from `bleeding/openapi/lib/` and adjust `ok`.

- [ ] **Step 4: Fix any decode failure at its cause**

A failure on a `null` the schema forbids is a spec or generator issue, as in karakeep commit `760abc77c`. Fix it in the generator with a regression test, or record a minimal patch in the pinned YAML with its provenance in the README and update the sync test. Never edit a fixture to hide it, because fixtures are recorded API output.

- [ ] **Step 5: Commit**

```bash
git add bleeding/ecosystems/test
git commit -m "Test ecosystems decoders against recorded API responses"
```

## Task 6: Docs, changelog, final verification

**Files:**
- Create: `bleeding/ecosystems/README.md`
- Modify: `CHANGES.md`

- [ ] **Step 1: README.** Say what the package is, show `Ecosystems_client.create` and `pages` in one example, state that `page` and `per_page` are strings, that `bulk_lookup_packages` takes a raw `Jsont.json` body, and how to regenerate (`dune build @bleeding/ecosystems/gen --auto-promote`) and re-pin the spec.

- [ ] **Step 2: CHANGES.md.** Add one or two lines under the commit group for this work, naming the new `ecosystems` client and its page iterator.

- [ ] **Step 3: Verify**

```bash
dune build @bleeding/ecosystems/all @bleeding/ecosystems/runtest --force
dune build @bleeding/ecosystems/doc 2>&1 | head
```

Expected: both clean. `@fmt` for OCaml cannot run here, so check formatting by eye.

- [ ] **Step 4: Commit**

```bash
git add bleeding/ecosystems/README.md CHANGES.md
git commit -m "Document the ecosystems client"
```

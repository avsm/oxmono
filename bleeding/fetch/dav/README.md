# fetch.dav

`Fetch_dav` is a WebDAV client built over Fetch and `Httpz_dav`. Link
`fetch.dav`; choose a Fetch backend when constructing the application.
The library provides file transfers, collection and property operations,
COPY/MOVE, REPORT, RFC 6578 collection synchronization, RFC 5689 extended MKCOL,
RFC 6764 principal discovery and explicit lock creation, refresh and release.
Proffer applications use `Fetch_dav` for outgoing DAV operations. The separate
[`proffer.dav`](../../proffer/dav/README.md) library serves explicitly mounted
DAV exports and has no outbound client authority.

For example, with `fetch.dav`, `httpz.dav`, `fetch`, `fetch-httpz` and `eio_main`
in the executable's Dune libraries:

```ocaml
let () = Eio_main.run @@ fun env ->
  let root = Sys.getenv "DAV_ROOT" in
  let http = Fetch_httpz.std ~retry:(Fetch.Retry.v ~max_retries:0 ()) env
    |> Fetch.with_credentials ~scope:[root]
         [Fetch.Credential.basic
            ~user:(Sys.getenv "DAV_USER")
            ~password:(Sys.getenv "DAV_PASSWORD")] in
  let client = Fetch_dav.v ~root http in
  let listing = Fetch_dav.propfind ~depth:`One client ""
    (Httpz_dav.Prop [Httpz_dav.dav "resourcetype"; Httpz_dav.dav "getetag"])
  in
  List.iter (fun (response : Httpz_dav.response) ->
    List.iter print_endline response.hrefs;
    match Httpz_dav.property (Httpz_dav.dav "getetag") response with
    | Some (Ok property) ->
        (match Httpz_dav.text property with Ok tag -> print_endline tag | Error _ -> ())
    | Some (Error status) -> Printf.eprintf "ETag failed: %d\n" status
    | None -> ()) listing.responses
```

`DAV_ROOT` is an HTTPS collection URL ending in `/`. Credentials and trust
remain Fetch configuration. Targets resolve relative to the root, and
`child ~collection name` constructs an encoded child from a decoded filename.
Both source and destination are checked for COPY/MOVE, and redirects stop.
Use `Overwrite: T` explicitly via `~overwrite:true` to replace a destination.

A 207 is a structured result, including failures. Inspect every relevant
resource and property status; `Complete` and `Multi` distinguish ordinary
mutation responses from recursive results. `Http_error` retains a bounded
diagnostic and parsed DAV errors. `Protocol_error` indicates invalid or oversized
XML; transport and cancellation exceptions retain their Fetch/Eio identity.
Use `Httpz_dav.property_results` for repeated PROPPATCH property names; it preserves
every status, while `Httpz_dav.property` rejects ambiguity. A successful patch report
does not contain the final value: use PROPFIND to read that back.

Uploads accept `Fetch.String` or `Fetch.stream`, with optional known length.
`with_download` provides headers and a streaming body inside a callback and
closes the response when the callback returns or raises. Its 304 response is
passed to the callback for cache handling. XML responses materialize within
configurable limits (8 MiB, depth 64 and 100,000 nodes/attributes by default).

`If_match` requires a strong Fetch ETag. `If_absent` sends `If-None-Match: *`
to request creation only when absent. Enforcement depends on the server; the
live smoke test checks both creation and stale-validator rejection.
For locks, pass `lock_condition lease` as `~if_` when modifying the resource,
overwriting a locked destination or creating a child of a locked collection.
It tags the condition with the lease URL; `refresh_lock` uses the untagged form
required for refresh. Use `refresh_lock` and `unlock` explicitly. The granted
timeout may differ from
the requested timeout. The integration adds no retries and cannot remove retry
middleware already present in a supplied capability. The example disables
retries; a lost mutation reply leaves an uncertain outcome.

From the repository root, select the compiler and layout once:

```sh
DAV_SWITCH=5.2.0+ox  # use 5.5.0 for stock OCaml
DAV_PREFIX=          # use bleeding/ in OxMono
```

Run local protocol, client-policy and Proffer integration tests:

```sh
opam exec --switch="$DAV_SWITCH" -- dune build --force \
  "@${DAV_PREFIX}httpz/dav/runtest" "@${DAV_PREFIX}fetch/dav/runtest" \
  "@${DAV_PREFIX}proffer/dav/runtest"
```

Run the real client against the isolated Apache Docker fixture:

```sh
python3 "${DAV_PREFIX}fetch/test/webdav/run.py" -- \
  opam exec --switch="$DAV_SWITCH" -- dune exec \
  "${DAV_PREFIX}fetch/dav/integration/test_docker.exe"
```

The runner first checks the server independently, then tests the client over
HTTP and HTTPS with an explicitly trusted test CA. It removes the fixture on
success or failure and retains logs. See the
[Docker strategy](../../fetch/test/webdav/README.md),
[specification](../../httpz/dav/SPEC.md) and [API](fetch_dav.mli).
Fetch does not refresh leases automatically. Server handlers live in
`proffer.dav`. CalDAV
and CardDAV clients are built on this library in the `idk` repository.

Synchronization keeps one token per collection:

```ocaml
let page = Fetch_dav.sync ?token client "" in
List.iter (function
  | Httpz_dav.Sync.Changed r -> print_endline (Httpz_dav.href r)
  | Removed href -> print_endline ("gone " ^ href)
  | Unsupported (href, _) -> print_endline ("cannot sync " ^ href)) page.changes;
(* Store page.token with the applied changes; repeat while page.truncated. *)
```

Discovery from a service's well-known path follows RFC 6764: `context_path`
resolves the redirect, `principal` reads `DAV:current-user-principal` from it
and `home_set` the hrefs of a home set property on the principal. The root of
the client bounds every URL these return.

Coverage includes generated namespace-preservation cases and resource bounds in
`httpz.dav`, malformed responses and request-policy assertions through Fetch mocks,
and Set/Set, Set/Remove and Remove/Set patches plus source, destination and
parent-collection locks against Apache. The fixture also runs an independent
Python HTTP oracle before the OCaml client. The protocol test alias includes
direct checks of the private XML codec.
OxMono also tests its shared vendor with `dune build @@vendor/xmlm/test/runtest`.

An opt-in live smoke test defaults to authentication, depth-one discovery and
bounded reads of small immediate child files. Credentials are read inside the
process, never passed as a command-line password or printed:

```sh
opam exec --switch="$DAV_SWITCH" -- dune exec \
  "${DAV_PREFIX}fetch/dav/integration/test_live.exe" -- \
  --root https://dav.example/files/ --user USER --password-file /path/to/password
```

Add `--scratch` to exercise writes in a new collection named with 128 random
bits. Only a confirmed successful MKCOL authorizes cleanup. All subsequent
mutations use a client restricted to that collection; creation and COPY/MOVE
request collision rejection, which the test verifies. Cleanup refuses unexpected children, deletes individual
test files, verifies the collection is empty, then removes it. Existing resource
names, file validators, lengths and downloaded bytes are compared afterwards.
Existing collections are not traversed. Each test phase has a 45-second timeout,
and no retries or redirects are enabled. A lost creation reply prints the
candidate collection name for inspection and leaves it alone.

Validate this same smoke test and its cleanup against the fixture first:

```sh
python3 "${DAV_PREFIX}fetch/test/webdav/run.py" -- \
  opam exec --switch="$DAV_SWITCH" -- dune exec \
  "${DAV_PREFIX}fetch/dav/integration/test_live.exe" -- --fixture --scratch
```

Fixture mode also injects a failure after upload to verify cleanup on that path.
The test records conditional-request and optional-feature deviations, continues
independent checks inside the scratch collection, and exits unsuccessfully after
cleanup if any were found. A completed phase can therefore contain a reported
server deviation.
See the [recorded Fastmail results](integration/INTEROP.md) for observed differences
from the Apache fixture, including ignored PUT preconditions.

## Collection mirroring

`Fetch_dav.Mirror.run` synchronizes a collection into a dedicated directory.
It uses sync-collection when advertised and falls back to depth-one PROPFIND
and ETag comparison. Refused tokens trigger a rebuild. Member filenames and
the index are validated, hrefs stay within the collection, and basename
collisions are rejected. Downloads and the index use atomic replacement.
An interrupted rebuild retains an empty token until stale files are pruned.

Keep other local writers and concurrent mirror runs out of this directory.
The mirror can replace existing member files. It does not fsync publications
or bound downloaded bytes. It caps the index at 16 MiB and runs at 1024 pages.
Infinite-depth mirroring uses a flat directory and rejects name collisions.

`Fetch_dav.v ~lenient_hrefs:true` explicitly permits percent-encoding repair
of malformed server hrefs. Strict parsing remains the default. This option
does not relax Fetch's origin or path restrictions.

## Sessions

`Fetch_dav.Session` is the ground a CardDAV or CalDAV client stands on. It
connects to one server as one principal, following the well-known path of
RFC 6764 to the context path, reading the current user principal and then
the home set named by the caller. HTTP, DAV, XML and Fetch transport errors
are returned as results for composition with `Result.bind`. Cancellation and
unexpected provider exceptions propagate. Invalid configuration can raise
`Invalid_argument`. Credentials are attached to the origin of the URL given.
An origin root returning 404, 405 or a redirect is retried once through the
service's well-known path. Explicit resource paths and authentication failures
are not retried. Discovery remains within the configured origin.
The switch a session is connected under scopes `Session.download`, whose
body streams until closed with `Fetch.close` or until that switch ends.
Rejected downloads close immediately, including when reading their error
body fails. Session downloads retain the DAV token-redaction policy.

```ocaml
Eio.Switch.run @@ fun sw ->
match Fetch_dav.Session.connect ~sw
        ~credentials:[Fetch.Credential.basic ~user ~password]
        ~service:`Carddav ~home_set:(Httpz_dav.carddav "addressbook-home-set")
        http "https://contacts.example.com/.well-known/carddav" with
| Error e -> prerr_endline (Fetch_dav.Session.error_to_string e)
| Ok session -> List.iter print_endline (Fetch_dav.Session.home_sets session)
```

# proffer.dav

`Proffer_dav` is a WebDAV client built over Fetch and `Davz`. Link
`proffer.dav`; choose a Fetch backend when constructing the application.
The library provides file transfers, collection and property operations,
COPY/MOVE and explicit lock creation, refresh and release. Its name follows the
requested Proffer integration; it contains client operations.

For example, with `proffer.dav`, `davz`, `fetch`, `fetch-httpz` and `eio_main`
in the executable's Dune libraries:

```ocaml
let () = Eio_main.run @@ fun env ->
  let root = Sys.getenv "DAV_ROOT" in
  let http = Fetch_httpz.std ~retry:(Fetch.Retry.v ~max_retries:0 ()) env
    |> Fetch.with_credentials ~scope:[root]
         [Fetch.Credential.basic
            ~user:(Sys.getenv "DAV_USER")
            ~password:(Sys.getenv "DAV_PASSWORD")] in
  let client = Proffer_dav.v ~root http in
  let listing = Proffer_dav.propfind ~depth:`One client ""
    (Davz.Prop [Davz.dav "resourcetype"; Davz.dav "getetag"])
  in
  List.iter (fun (response : Davz.response) ->
    List.iter print_endline response.hrefs;
    match Davz.property (Davz.dav "getetag") response with
    | Some (Ok property) ->
        (match Davz.text property with Ok tag -> print_endline tag | Error _ -> ())
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
Use `Davz.property_results` for repeated PROPPATCH property names; it preserves
every status, while `Davz.property` rejects ambiguity. A successful patch report
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
required for refresh. Use `refresh_lock` and `unlock` explicitly. The granted timeout may differ from
the requested timeout. The integration adds no retries and cannot remove retry
middleware already present in a supplied capability. The example disables
retries; a lost mutation reply leaves an uncertain outcome.

Run local protocol and policy tests:

```sh
opam exec --switch=5.2.0+ox -- dune build \
  @bleeding/davz/runtest @bleeding/proffer/dav/test/runtest
```

Run the real client against the isolated Apache Docker fixture:

```sh
python3 bleeding/fetch/test/webdav/run.py -- \
  opam exec --switch=5.2.0+ox -- dune exec \
  bleeding/proffer/dav/test/test_docker.exe
```

The runner first checks the server independently, then tests the client over
HTTP and HTTPS with an explicitly trusted test CA. It removes the fixture on
success or failure and retains logs. See the
[Docker strategy](../../fetch/test/webdav/README.md),
[specification](../../davz/SPEC.md) and [API](proffer_dav.mli).
Collection sync, automatic lock leases, CalDAV/CardDAV and server handlers are
outside this initial version.

Coverage includes generated namespace-preservation cases and resource bounds in
`davz`, malformed responses and request-policy assertions through Fetch mocks,
and Set/Set, Set/Remove and Remove/Set patches plus source, destination and
parent-collection locks against Apache. The fixture also runs an independent
Python HTTP oracle before the OCaml client. Run the vendored Xmlm regressions
after parser changes with `dune build @@vendor/xmlm/test/runtest` under the same
opam switch.

An opt-in live smoke test defaults to authentication, depth-one discovery and
bounded reads of small immediate child files. Credentials are read inside the
process, never passed as a command-line password or printed:

```sh
opam exec --switch=5.2.0+ox -- dune exec \
  bleeding/proffer/dav/test/test_live.exe -- \
  --root https://dav.example/files/ --user USER --password-file /path/to/password
```

Add `--scratch` to exercise writes in a new collection named with 128 random
bits. Only a confirmed successful MKCOL authorizes cleanup. All subsequent
mutations use a client restricted to that collection; creation and COPY/MOVE
reject collisions. Cleanup refuses unexpected children, deletes individual
test files, verifies the collection is empty, then removes it. Existing resource
names, file validators, lengths and downloaded bytes are compared afterwards.
Existing collections are not traversed. Each test phase has a 45-second timeout,
and no retries or redirects are enabled. A lost creation reply prints the
candidate collection name for inspection and leaves it alone.

Validate this same smoke test and its cleanup against the fixture first:

```sh
python3 bleeding/fetch/test/webdav/run.py -- \
  opam exec --switch=5.2.0+ox -- dune exec \
  bleeding/proffer/dav/test/test_live.exe -- --fixture --scratch
```

Fixture mode also injects a failure after upload to verify cleanup on that path.
The test records conditional-request and optional-feature deviations, continues
independent checks inside the scratch collection, and exits unsuccessfully after
cleanup if any were found. A completed phase can therefore contain a reported
server deviation.
See the [recorded Fastmail results](test/INTEROP.md) for observed differences
from the Apache fixture, including ignored PUT preconditions.

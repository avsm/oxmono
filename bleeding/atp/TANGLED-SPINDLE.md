# Tangled spindle: compatibility audit and local testbed

Historical design note, reviewed 2026-09-09. The implemented
[spindle](../spindle/README.md), [CLI](bin/tangled/README.md) and
[current lexicon manifest](lexicons/tangled/README.md) supersede its status
and refresh plan. The machine-readable audit now describes the refreshed set.

Original checkout comparison:

| Source | Revision |
| --- | --- |
| oxmono | `2decb59f8014d5d9726aca5ff442fdda744d6239` |
| tangled-core | `338719d7d4f1e1e32becc4f4d3ef04f3c7daeb32` |
| tack | `8e3bd108617fdd83514b2955f47f045029e5577f` |

The spindle is feasible on Proffer, Fetch and ATP. The refresh is substantial
at the Tangled application layer. It does not require implementing an OCaml PDS
or using the unfinished MST mutation code. The local ATP foundation is running
and tested. The full Tangled CI testbed remains the next layer. No live ATP
service was contacted.

## Lexicon refresh

The [machine-readable audit](tangled-lexicon-audit.json) compares JSON objects
by NSID, ignoring file layout and whitespace. Structural comparisons exclude
string-valued `description` annotations, but retain field schemas, formats,
required fields and other constraints.
Reproduce it without network access from the repository root:

```sh
python3 bleeding/atp/tools/audit_tangled.py ../tangled-core
```

| `sh.tangled.*` | Count |
| --- | ---: |
| Initial oxmono | 55 |
| Initial tangled-core | 187 |
| Added upstream | 133 |
| Removed upstream | 1 |
| Changed existing definitions | 25 |
| Changes beyond descriptions | 24 |
| Identical existing definitions | 29 |

The removed NSID is `sh.tangled.repo.forkStatus`. The description-only change
is `sh.tangled.pipeline.status`. Upstream also supplies 43
`org.tangled.temp.*` lexicons for application-specific APIs and one
`com.atproto.repo.strongRef`, identical to our ATProto copy. There is no evidence
in this checkout comparison that the whole Bluesky or ATProto lexicon bundle
needs replacing. Keep the temporary Tangled APIs in a separate namespace or
library if imported. They are not all required for a CI spindle.

The eight missing CI lexicons are `ci.pipeline`, `ci.trigger`,
`ci.queryPipelines`, `ci.getPipeline`, `ci.triggerPipeline`,
`ci.cancelPipeline`, `ci.subscribePipelineLogs` and
`ci.describeWorkflowDefinition`, all under `sh.tangled`.

### Breaking changes that affect the implementation

* Repository DIDs become the stable repository identity. `sh.tangled.repo`
  gains optional `repoDid`, makes `name` optional and changes its record key
  from `tid` to `any`. Keep owner DID, repository DID and record AT-URI distinct.
  Several existing fields remain optional for migration compatibility.
* `git.refUpdate` replaces required `repoDid` and `repoName` with required
  `repo`, a repository DID. It adds `ownerDid`, `changedFiles` and `pushOptions`.
  Trigger matching needs the new identity and CI skip options.
* Repository creation now requires `name` and returns `repoDid` and `key`.
  Deletion requires `repo`. Merge, merge-check, fork-sync, collaboration,
  branch, issue and pull operations move from owner/name or AT-URI to repo DID.
* Pulls require `rounds`, containing dated gzip patch blobs. The old top-level
  patch fields disappear. Pull source SHA disappears. Source and target repo
  fields become DIDs. Issue-state and pull-status records require `createdAt`.
* Pipeline trigger metadata gains repository DIDs and `sourceRepo`. Manual
  triggers require SHA. Clone options require `tags`. Workflow placement gains
  `runsOn`. Pull action becomes optional and gains an enum constraint.
* Stars change from a string subject to a closed repo/string union. Profiles
  gain avatars and preferred handles. Nullable languages and secrets expose
  an existing generator defect.

Our [Tangled API](bin/tangled/lib/tangled_api.ml) still builds owner/name Git
URLs, writes the old create/delete bodies and lists spindle pipeline records
through the configured PDS. Regeneration alone cannot fix those behaviors.
Pipeline reads should address the spindle's current `sh.tangled.ci.*` API.
Keep legacy lookup as an explicit conversion, not an ambiguous string accepted
by every function.

### Generator probe

Built Hermest with `5.2.0+ox --profile release-check` and generated the whole
upstream lexicon directory into scratch storage. Two documents are rejected:

```text
ci/subscribePipelineLogs.json: Missing member encoding in body_def object
knot/subscribeRepos.json: Missing member encoding in body_def object
```

Subscription `message` has a schema, not the required encoding of an HTTP
input/output body. Hermest currently reuses the latter parser. Its CLI prints
the errors, omits the documents and exits successfully. The resulting partial
bindings compiled as a temporary private library. This proves neither that all
schemas were imported nor that the generated representations validate them.
The production JSON and generated bindings were left unchanged.

Refresh in this order:

1. Give subscription messages their own schema representation. Make any parse
   error fail generation before publishing output. Add both real subscription
   schemas as regression fixtures.
2. Fix required/nullable/absent semantics and discriminator validation. The
   [existing ATP review](REVIEW.md) documents both generation paths and their
   weaknesses. Unknown union members must remain explicit and lossless.
3. Import the 187 stable Tangled NSIDs with revision provenance, remove the
   obsolete definition and regenerate ML and MLI together. Include and test
   strong-ref dependencies without duplicating the ATProto implementation.
4. Repair the Tangled library and CLI around repository DIDs and spindle XRPC.
   Validate decoded requests at the server boundary. Generated Jsont types
   currently do not enforce formats, bounds, enums or all union rules.

## Spindle implementation boundary

Tack is a useful reference for durable ingestion and provider dispatch. Its
`go.mod` uses core `v1.16.1-alpha`, not this core checkout. Its repository
verification already carries a compatibility patch for newer `TangledKnot`
service declarations. The replacement should implement the current core
contract, including cancellation and workflow-definition descriptions, which
Tack does not currently register in `xrpc.go`.

| Component | Reuse and remaining work |
| --- | --- |
| HTTP server | Proffer routes, typed responses, bounded request bodies and its connection-upgrade handoff. |
| HTTP client | `Xrpc.Client.of_fetch` over a restricted `Fetch_httpz` capability. Inject every endpoint, TLS root and dial policy. |
| WebSockets | `httpz.websocket` supplies version-13 handshake validation, bounded messages, fragmentation, UTF-8, masking and control frames. Bind its byte transport to Proffer's upgraded socket. An outbound HTTP/TLS upgrade connector remains to be wired for ATP subscriptions. |
| Stream formats | Decode knot `/events` JSON and Jetstream JSON separately. CI log subscriptions use binary WebSocket messages containing the XRPC CBOR header and body, not SSE or ordinary JSON. Add vectors against the current Go encoder. |
| Identity | `Atp.Did` checks syntax only. Add injectable PLC/did:web resolution, PDS service extraction and repository verification through `TangledKnot` plus `repo.describeRepo`. Preserve the documented legacy service fallback if compatibility is desired. |
| Authentication | `Xrpc.Jwt` only decodes expiration. Add signature verification with DID verification keys and checks for issuer, audience, time and `lxm`. `Jsonwt` has ES256, but no ES256K constructor. secp256k1 support and did:key/multikey conversion need a deliberate implementation and fixtures. Never substitute decode-only JWT handling. |
| State | SQLite transactions for records, per-source cursors, membership, verified repos, trigger deduplication, pipelines, jobs and logs. Commit cursor advancement with the resulting state. |
| Jobs | Trusted OCaml definitions produce a serializable job plan from repository facts. Proffer serves its status, logs and analysis. Execute repository code and builds in a separate worker process/container. |

Use a small job interface: stable job ID, dependencies, input digests, resource
limits, execution description and typed result/artifact metadata. Keep analysis
as ordinary OCaml functions over immutable repository facts. Memoization keys
must include the commit, definition digest, compiler/toolchain image and job
inputs. Persist the resulting plan before scheduling it.

For definitions managed in the spindle's own OCaml configuration,
`describeWorkflowDefinition` returns `derived=false`. If definitions come from
the repository, return `derived=true` with the effective source fingerprint
and workflow names. This controls the appview's workflow-change warnings.
Do not dynlink arbitrary repository OCaml into the HTTP service. Compilation
and evaluation of repository-supplied definitions belong in the worker too.

Keep request parsing and temporary analysis values local where they do not
escape. Queued plans, logs and worker messages need owned storage. Preserve
borrowed byte slices through framing and copy at persistence boundaries.
Proffer route closures must be portable. Mutable schedulers should be owned by
a fiber/domain and reached through an explicit queue or synchronized handle.

## Docker testbed

Use a new Compose project here with a pinned checkout snapshot of core as its
build input. Do not extend and start upstream Compose unchanged. Baseline
services should be PLC + PostgreSQL, PDS, Jetstream, knot, OCaml spindle, a
bootstrap/test driver, local DNS and a TLS gateway. A UI profile adds appview,
Redis, knotmirror and its Tap service. Search needs its own optional Zoekt
profile. Browser tests run on the Compose network for local hostname routing.

```text
local PLC <---- PDS ----subscribeRepos----> Jetstream
    ^            ^                            |
    |            | records/auth              | repo/member/pull events
    |            |                            v
    +---------- knot ----refUpdate----> OCaml spindle ----> job worker
                 ^                         ^       |
                 | Git push/fetch          |       | status + CBOR logs
                 +---- test driver     appview <----+
```

The worker needs neither KVM nor the host Docker socket for the first slice.
Use one disposable container with a fixed toolchain, read-only source inputs,
scratch storage, bounded CPU/memory/PIDs and `network_mode: none`. Exchange
plans/results through dedicated volumes or a narrow local control channel.
If later jobs need container creation, use a dedicated isolated engine, with
no host daemon socket mounted into jobs or the spindle.

### Local development, without federation dependencies

The development target is a self-contained ATP network, not an air-gapped
machine. Image pulls and build-time dependency downloads are normal. Runtime
identity resolution, record writes, event subscriptions and service
verification must use the local services.

The [working Compose setup](testbed/README.md) uses its own PLC, PostgreSQL,
PDS, Jetstream and TLS gateway. It has local accounts, `*.tangled.test` service
aliases and a generated CA. It uses an ordinary private Docker bridge and
publishes development ports on loopback. PDS crawlers and public Bluesky
appview/report/moderation destinations are unset. No public DNS records or
live account credentials are needed.

The separate `check_isolation.py` experiment verifies Docker's stricter
isolated-gateway mode with local canaries. It passed, but it is optional and
is not a startup requirement. Docker's [gateway-mode documentation](https://docs.docker.com/engine/network/port-publishing/#gateway-modes)
explains the distinction from an ordinary internal bridge.

For the Tangled services, retain explicit local endpoint configuration. Prefer
a normal private subnet. Core's knot `KNOT_SERVER_DEV` also disables signature
verification, so use a targeted local-address allowance when testing real
service authentication rather than using that flag as an auth test shortcut.

### Upstream setup changes required

| Source | Change required before starting it |
| --- | --- |
| `docker-compose.yml` | Replace the public-looking `11.0.0.0/24` subnet with a private subnet and bind development ports to loopback. Knot sets `APPVIEW_ENDPOINT=https://tangled.org`, which needs a local override. |
| `localinfra/pds.env` | Set crawler and Bluesky service configuration explicitly after checking the pinned PDS image's actual defaults. A commented-out crawler setting does not disable defaults. Generate disposable secrets locally. |
| `localinfra/scripts/init-accounts.sh` | Bake curl/jq into a bootstrap image. Compose currently runs `apk add` at startup. Extend bootstrap to SSH keys, spindle membership, repo creation, collaborator grants and fixture commits. |
| `localinfra/appview.Dockerfile` | Build appview ahead of time. The current image runs Air and compiles at startup, potentially downloading modules/toolchains. Core now declares Go 1.26 while this Dockerfile starts from Go 1.25. |
| `localinfra/scripts/appview-static-files.sh` | Stage all browser assets. The script fetches an actor-typeahead repository from live `tangled.org`, which is forbidden for this task. Obtain a supplied/local snapshot or serve a local replacement. |
| Zoekt build context | Replace `https://tangled.org/boltless.me/zoekt.git#tngl` with a local pinned source archive or omit search. |
| `localinfra/ncps.yaml` | It forwards cache misses to `cache.nixos.org`. This build dependency is compatible with the development setup. Preseed the toolchain if individual jobs need to run without network access. |
| Tack `main.go`, `pipeline.go`, `xrpc.go` | Besides the public Jetstream default, repository verification and service auth construct public PLC resolvers directly. A reference Tack image needs a small endpoint-injection patch in a local build snapshot. Its fake provider alone does not make it offline. |
| Appview config | Override public PDS and ATP service destinations. Use local assets or disable optional avatar, camo, image, mail and analytics integrations for the test profile. |

The live-network references in these source/build scripts still need local
snapshots or replacements when adding the complete Tangled UI profile. The
current ATP setup does not run them.

Docker Engine 29.1.3 and Compose 2.40.3 are available. Downloaded and pinned the
PLC, PDS and Jetstream images plus PostgreSQL. Existing Docker credentials
caused GHCR pulls to fail. Anonymous pulls with a temporary client config
succeeded, without modifying the user's Docker configuration. The pinned
PDS image reports package version 0.5.27 despite its image tag being `0.4`.
The local PDS issued an ES256K service-auth token, confirming that verifier
support is required. Its inspected runtime configuration defaults crawlers
to an empty list and appview/report services to disabled when their URL
settings are absent.

The local smoke test passed: Alice and Bob were created, their DID documents
came from local PLC, HTTPS handle discovery returned their DIDs, and a
`sh.tangled.repo` write appeared on local Jetstream. The stack remains running
with persistent development volumes. Recreating it on the development bridge
preserved both accounts, and the event test passed again. Host access passed
over HTTP and HTTPS with the local CA. A full knot push through the replacement
spindle and appview is the next integration layer, not a completed test.

### Acceptance sequence

1. Verify local endpoint configuration and TLS/hostname resolution.
   Create fresh Alice, Bob and an unauthorized user on the local PDS/PLC.
2. Register the knot and spindle, publish SSH keys and owner-authorized spindle
   membership, create a repository DID and assign that spindle. Wait for
   ingestion barriers rather than fixed sleeps.
3. Push a fixture commit over Git SSH. Verify that the knot event creates
   exactly one pipeline. Run a deterministic OCaml analysis job, then expose
   success, failure, artifacts and streamed stdout/stderr through CI XRPC.
4. Exercise branch/changed-file matching, skip push options, pull rounds,
   explicit-commit manual triggers, workflow fingerprints and cancellation.
   Read paginated results and logs through both a test client and appview.
5. Reject forged membership, collaborator grants and knot events. Reject
   wrong signatures, audience, method and expiration in service JWTs. Check
   fixture key rotation and repository DID ownership changes.
6. Restart the spindle and event sources. Replay duplicates and reorder
   records. Verify cursor persistence, idempotent scheduling, crash recovery,
   membership revocation and bounded reconnect/log buffering.
7. For disposable CI runs, remove the run's containers, networks and volumes
   on success, failure or interruption. Keep failure logs and the exact lock
   manifest. Interactive development retains volumes until an explicit reset.

The first useful deliverable is the real PLC/PDS/Jetstream/knot path ending in
one OCaml job and Proffer CI responses. Add the appview profile once this path
passes. Neither a fake event server alone nor a healthy Compose dashboard is
evidence of Tangled interoperability.

# Spindle

A small Tangled CI spindle using Proffer, HTTPz, Fetch, JSONWT and Eio on
OxCaml.
The default `inspect` job checks out the requested commit, prints the spindle
request metadata and executes `ls -la --`. Metadata is JSON in both the job
log and service stdout. Authorization headers and service tokens are excluded.

Run the Docker testbed from the oxmono root:

```sh
python3 bleeding/spindle/testbed/run.py test
python3 bleeding/spindle/testbed/demo.py
```

The testbed uses the [local ATP stack](../atp/testbed/README.md), Alice's real
PDS-issued service token and a local Git fixture. Build preparation downloads
container images and Ubuntu packages. Runtime uses the local PDS and HTTPS
PLC gateway with its test CA.
The spindle remains running at `http://127.0.0.1:9000` with persistent state.
`demo.py` dispatches another job and prints its metadata and directory listing.
`run.py down` removes the spindle containers and retains `.state`.

Build and run directly:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/spindle/all
_build/default/bleeding/spindle/bin/main.exe \
  --owner=did:plc:OWNER --repo=did:web:REPOSITORY \
  --source=/absolute/path/to/git/repo --plc=http://127.0.0.1:2582
```

OpenSSL 3 development headers are required by JSONWT's ES256K verification
binding. Git must be installed at runtime. The Docker runtime matches the
Ubuntu 26.04 host ABI and runs as the invoking user's UID. It has a read-only
root, a read-only Git fixture, resource limits and writable state storage.

For a persistent native service, HTTPS setup and PDS authentication, see
[deployment instructions](DEPLOYMENT.md). The service initializes its own
state directory and needs no PDS password or spindle signing key.

## OCaml jobs

Pass a trusted job definition to `Spindle.run`:

```ocaml
let job : Spindle.Job.t = {
  name = "inspect";
  steps = [Metadata; Command ["ls"; "-la"; "--"]];
}
```

Commands receive argument vectors without a shell. Each runs in a fresh
checkout of the requested commit. The environment supplies `TANGLED_REPO`,
`TANGLED_COMMIT_SHA`, `TANGLED_PIPELINE_ID` and `SPINDLE_REQUEST` JSON.
Repository-supplied OCaml and workflow files are not evaluated. Definitions
are managed by the service, so workflow discovery returns `derived=false`.

## API

All methods are under `/xrpc/`. The implementation follows the CI lexicons
in tangled-core revision `338719d7d4f1e1e32becc4f4d3ef04f3c7daeb32`.

| Method | Behaviour |
| --- | --- |
| `sh.tangled.owner` | Return the configured owner DID. |
| `sh.tangled.ci.triggerPipeline` | Authenticate and queue a manual trigger at an explicit SHA. |
| `sh.tangled.ci.getPipeline` | Return workflow status and timestamps by pipeline TID. |
| `sh.tangled.ci.queryPipelines` | Filter by commit or trigger kind and paginate newest first. |
| `sh.tangled.ci.describeWorkflowDefinition` | Describe the external OCaml job definition. |
| `sh.tangled.ci.cancelPipeline` | Cancel a queued or running job. |
| `sh.tangled.ci.subscribePipelineLogs` | Stream binary WebSocket messages containing XRPC header and body CBOR values. |
| `_health` | Report readiness after loading persistent state. |

Trigger and cancellation require an ES256K JWT for the configured owner,
`did:web:<hostname>` audience and exact method in `lxm`. Verification resolves
the owner's current `#atproto` key through the explicitly configured PLC.
Tokens must have `typ=JWT`, an absent `kid` or `#atproto`, integral `iat` and
`exp`, and a scalar audience. Issuance may be at most 30 seconds in the future,
expiry at most one hour after receipt, and `iat` cannot exceed `exp`. Optional
`nbf` is honored. JSONWT rejects duplicate members, malformed registered claims,
noncanonical base64url and critical JOSE extensions before verification.

The service does not yet require `jti` or track accepted IDs to prevent token
replay. Valid tokens can be reused until expiry. Its exact configured bare DID
audience and ES256K key support form a limited ATProto profile. See the
[JWT RFC review](../jsonwt/spec/REVIEW.md) for the suitability assessment and
remaining service-authentication work.

The owner/repository/source mapping is operator configuration for this first
service. It does not yet ingest membership records or knot push/pull events,
discover repository ownership, serve appview, or support other JWT algorithms.
These endpoints use explicit codecs and validation. The existing ATP generated
Tangled bindings have not been refreshed by this project.

Two jobs may run concurrently, with at most 32 outstanding jobs and 1000
retained pipelines. A job has a 60-second deadline and 1 MiB log limit.
Pipeline transitions and step boundaries atomically replace a JSON checkpoint.
A restart marks interrupted jobs failed and removes their checkouts. One
process holds the state-directory lock. Checkpoints provide process-restart
recovery, without a power-loss durability guarantee or automatic retention.
The worker is a child process in the service container. Use trusted command
definitions until a separate worker with filesystem quotas is added.

## Verification

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/spindle/test
```

The native tests cover signed-token validity, expiration, audience, issuer,
method, malformed signatures and unsupported headers. Fixture signing uses
OpenSSL and retains no private key. The Docker test covers real authentication,
checkout, metadata, directory listing, CBOR logs, failed checkout, cancellation,
filtering, pagination and recovery of completed and interrupted jobs.

`testbed/interop.go` checks captured log frames with the current Tangled Go
decoder and encoder. From the sibling tangled-core checkout:

```sh
GOPROXY=https://proxy.golang.org GOTOOLCHAIN=local go run -mod=readonly \
  ../oxmono/bleeding/spindle/testbed/interop.go \
  ../oxmono/bleeding/spindle/testbed/.state/last-run.json
```

JSONWT's ES256K verification uses OpenSSL's
[EVP public-key import](https://docs.openssl.org/3.0/man3/EVP_PKEY_fromdata/)
and [digest verification](https://docs.openssl.org/3.0/man3/EVP_DigestVerifyInit/).

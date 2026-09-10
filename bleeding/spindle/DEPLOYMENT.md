# Running spindle as a service

Spindle runs continuously, discovers assigned Tangled repositories and handles
pushes and pull requests. Configure its hostname, owner DID, PLC origin,
Jetstream URL and writable state directory. SQLite initialization and migration
happen automatically. The service needs no PDS login or spindle signing key.

## PDS and network requirements

The owner and authorized callers need ATProto accounts with ES256K or ES256
`#atproto` signing keys. Their PDS issues short-lived service JWTs. The spindle
resolves current DID documents, verifies signatures and consumes each token's
`jti` once. Cancellation requires its own method-specific token. PDS passwords
and session tokens are never stored by the spindle.

For automatic operation, supply `--jetstream=wss://YOUR_JETSTREAM/subscribe`.
That Jetstream must cover the owner/member PDSes and Tangled collections.
The spindle bootstraps existing owner and member repository assignments from
their PDSes, then follows changes. Outbound access includes the configured PLC,
account PDSes, canonical knots, knot WebSockets and HTTPS Git clones.

Use HTTPS/WSS for remote services. TLS verifies hostnames and system trust;
`SSL_CERT_FILE` can select a private CA bundle. `--allow-http` enables HTTP/WS
for a development network. No PLC or Jetstream endpoint defaults to the live
network. Point them at the local Docker setup for independent operation.

## Register in Tangled

1. Serve the configured hostname over HTTPS, forwarding HTTP and WebSocket
   upgrades to the spindle. Both `/xrpc/sh.tangled.owner` and
   `/.well-known/did.json` must be reachable.
2. Sign in to Tangled as the configured owner and register the hostname in
   spindle settings. This creates the owner's `sh.tangled.spindle` record,
   whose record key is the hostname. Tangled verifies the owner endpoint.
3. Select this spindle in the repository's pipeline settings. The owner's
   `sh.tangled.repo` record must name the spindle and canonical knot/repo DID.
4. To host another account's repositories, add that account as a spindle
   member. The owner's `sh.tangled.spindle.member` record names the account in
   `subject` and this hostname in `instance`.
5. Push a commit. The default workflow prints metadata and the directory
   listing; pipeline status and logs appear in Tangled.

Knot membership and repository collaborators are managed by the knot's own
settings/procedures. Spindle membership permits repository assignment; it does
not grant access to other members' repositories. The current Tangled appview
queries spindle CI endpoints directly; no separate spindle event stream is
needed for pipeline display.

## Keep the local Docker service running

From the oxmono root:

```sh
python3 bleeding/spindle/testbed/parity.py up
python3 bleeding/spindle/testbed/parity.py test
curl --fail http://127.0.0.1:9000/xrpc/_health
curl --fail http://127.0.0.1:9000/xrpc/_ready
```

`up` builds the executable and image, prepares the local identity/Git fixtures
and leaves the services running. `demo.py` obtains Alice's PDS service JWT and
prints the inspection job's metadata and directory listing. State is retained
under `bleeding/spindle/testbed/.state/data`. The API binds to host loopback.
The testbed Compose service has no reboot restart policy. Use the persistent
service setup below for a daemon managed by the host.

## Install the native service

On a host matching the build system's ABI, install Git, CA certificates and
the shared libraries required by the executable. This tree's Docker runtime
uses Ubuntu 26.04, OpenSSL 3, GMP and zstd. Inspect `ldd` before moving a binary
between hosts. Build from the oxmono root with the existing OxCaml switch:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/spindle/all
sudo install -m 0755 _build/default/bleeding/spindle/bin/main.exe \
  /usr/local/bin/ocaml-spindle
```

Create an unprivileged `ocaml-spindle` system account with no interactive
login:

```sh
sudo useradd --system --user-group --home-dir /var/lib/ocaml-spindle \
  --shell /usr/sbin/nologin ocaml-spindle
```

Install Git and `gzip` alongside the executable. Automatic jobs clone the
canonical knot by repository DID. A manual-only installation may instead use
`--repo=did:plc:REPOSITORY --source=/absolute/path/to/repo` and omit Jetstream.
This static mapping accepts only its configured owner's mutations.

Save this as `/etc/systemd/system/ocaml-spindle.service`, replacing every
uppercase placeholder. `--hostname` is a DNS hostname without a scheme, path
or trailing slash. `--repo` is the repository DID used in requests, not the
owner DID or a Git URL. Use the actual Tangled repository DID when connecting
an existing repository. `--plc` is an origin without a trailing slash.

```ini
[Unit]
Description=OCaml Tangled spindle
Wants=network-online.target
After=network-online.target

[Service]
User=ocaml-spindle
Group=ocaml-spindle
StateDirectory=ocaml-spindle
StateDirectoryMode=0700
WorkingDirectory=/var/lib/ocaml-spindle
ExecStart=/usr/local/bin/ocaml-spindle \
  --addr=127.0.0.1 --port=9000 \
  --hostname=ci.YOUR_DOMAIN \
  --owner=did:plc:OWNER \
  --jetstream=wss://YOUR_JETSTREAM/subscribe \
  --plc=https://YOUR_PLC_HOST \
  --state-dir=/var/lib/ocaml-spindle
Restart=on-failure
RestartSec=5
KillMode=control-group
UMask=0077
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict

[Install]
WantedBy=multi-user.target
```

Systemd's `StateDirectory` creates the writable service-owned directory and
exempts it from `ProtectSystem=strict`. Jobs and checkouts run as the service
account.
The default job only prints request metadata and runs `ls -la --`.

```sh
sudo systemctl daemon-reload
sudo systemctl enable --now ocaml-spindle
sudo systemctl status ocaml-spindle
journalctl -u ocaml-spindle -f
curl --fail http://127.0.0.1:9000/xrpc/_health
curl --fail http://127.0.0.1:9000/xrpc/sh.tangled.owner
```

`/xrpc/_health` reports liveness and diagnostics. Use `/xrpc/_ready` or
`/readyz` for readiness, including observer connections and pending recovery.
Submit an authenticated job to check execution and caller authorization.

## Expose HTTPS and submit a job

Point `ci.YOUR_DOMAIN` at the host. Terminate HTTPS at a reverse proxy that
forwards HTTP and WebSocket upgrades to port 9000. For example, Caddy can use:

```caddyfile
ci.YOUR_DOMAIN {
    reverse_proxy 127.0.0.1:9000
}
```

Caddy's [reverse proxy](https://caddyserver.com/docs/caddyfile/directives/reverse_proxy)
handles WebSocket upgrades. Keep the `/xrpc/` prefix intact and preserve
Authorization headers. Queries and log subscriptions are public in the
current spindle, so request metadata and job output are public too.

From the oxmono checkout, run:

```sh
python3 bleeding/spindle/tools/trigger.py \
  --pds=https://YOUR_PDS_HOST \
  --spindle=https://ci.YOUR_DOMAIN \
  --hostname=ci.YOUR_DOMAIN \
  --identifier=YOUR_OWNER_HANDLE \
  --repo=did:plc:REPOSITORY \
  --sha=FULL_GIT_COMMIT_SHA
```

It prompts for a PDS app password, obtains a service JWT and prints the new
pipeline URI without printing credentials. Take the final path component as
`PIPELINE_TID` and query its status:

```sh
curl --fail --get \
  https://ci.YOUR_DOMAIN/xrpc/sh.tangled.ci.getPipeline \
  --data-urlencode pipeline=PIPELINE_TID
```

The log endpoint is
`wss://ci.YOUR_DOMAIN/xrpc/sh.tangled.ci.subscribePipelineLogs?pipeline=PIPELINE_TID`.
It sends binary Tangled CBOR frames. See the local demo for decoding them.

## State and operational limits

The state directory contains SQLite state and temporary checkouts. Back it up
while the service is stopped, or use SQLite's backup API. Preserve `spindle.db`
with its WAL when copying live storage. Deleting the database also deletes
pipeline URLs, event cursors and accepted JWT nonces.

There are two concurrent workflows, 32 outstanding pipelines, a 60-second
execution deadline and a 1 MiB log limit per workflow. Pending work resumes
after restart. Interrupted workflows become failed. One process may own a
state directory.

Membership and repository changes trigger fresh PDS reads. Affected mutations
return `503 CatalogPending` while reconciliation is pending. Fetch failures
back off up to one minute and keep the affected catalog unavailable. Event
processing continues for other repositories.

Each command gets a process group. Cancellation, deadlines and normal exit
kill remaining group members. This cleans up shell children but does not
isolate jobs that deliberately detach or access the service account's files.

## Retention and readiness

Automatic maintenance runs on startup and every minute. Override these CLI
defaults as needed, or pass `~operations:(Spindle.Operations.v ... ())` to
`Spindle.run` in an OCaml service:

| Flag | Default | Scope |
| --- | --- | --- |
| `--history-days` | 30 | Age since the last completed pipeline update |
| `--history-limit` | 1000 | Completed pipelines |
| `--history-megabytes` | 1024 | Completed summaries and logs, in MiB |
| `--receipt-days` | 7 | Completed event receipts and inactive dispatch keys |
| `--receipt-limit` | 100000 | Completed receipts and inactive dispatch keys, each |
| `--inbox-limit` | 10000 | Pending stream events |
| `--inbox-megabytes` | 64 | Pending event payload, in MiB |
| `--replay-hours` | 24 | Maximum age of a cursor to replay |
| `--reconcile-seconds` | 300 | PDS and current Git ref reconciliation interval |
| `--maintenance-seconds` | 60 | Storage cleanup interval |

All limits must be positive. Receipt retention must cover the replay window
and exceed the reconciliation interval. Cleanup removes the oldest completed
pipelines first until all history budgets hold. It never evicts active
pipelines, pending inbox work or unexpired JWT nonces. A full inbox applies
backpressure without advancing the stream cursor. Expired receipts retain a
per-source replay floor, preventing blind redispatch of old events.

These are payload budgets, not physical disk quotas. SQLite reuses freed pages
and checkpoints its WAL. Database overhead, pending work and original JSON
migration files are outside the completed-history budget. Monitor filesystem
capacity and use SQLite maintenance during planned downtime if the file must
shrink. Deleted pipeline URLs return not found.

`/xrpc/_health` returns HTTP 200 with a `ready` flag and diagnostics. The same
report at `/xrpc/_ready` or `/readyz` returns HTTP 503 while degraded. It includes
observer connections, reconnect attempts, activity and event ages, cursors,
queue counts/bytes/ages, discovery errors, replay gaps and maintenance results.
Readiness fails for disconnected sources, 120 seconds without transport
activity, inbox work older than 60 seconds, pending catalog/recovery tasks or
maintenance failure. Ping/pong traffic keeps a quiet stream healthy. Readiness
can briefly fail during periodic reconciliation. Connection errors also go
to the service journal.

## When upstream replay is unavailable

Set `--replay-hours` no higher than the history guaranteed by the configured
Jetstream and knots. Their protocols do not advertise an earliest retained
cursor, so the spindle cannot prove that every historical event is available.
An expired cursor, pruned receipt floor or HTTP 410 creates a durable recovery
task before the cursor moves forward. Ref checks also detect surviving pushes
missing from a journal even when no replay error is returned.

Startup, reconnect and periodic reconciliation read current PDS assignments,
member-owned pull records and canonical Git refs. Missing ref heads dispatch
jobs with `request.recovery.mode="current_refs"`, the spindle DID as actor,
and `committerKnown=false` and `changedFilesKnown=false`. Custom predicates
can inspect these fields to decide how to handle incomplete context. Stream
pushes and recovered pushes share a repository/ref/SHA key. Re-pushing the
same SHA shares the existing dispatch while its key is retained. Checkpoints
prevent unchanged refs from running on every scan.

The health report's `replay` entries record the first missing cursor, reason,
and `pending` or `reconciled` status. Readiness returns after current-state
recovery finishes, while `historicalEventsComplete=false` remains visible.
Deleted refs, intermediate commits and unknown authors' PR records may be
unrecoverable. Reconciled reports expire with the history age policy. Pending
reports and recovery tasks remain until resolved.

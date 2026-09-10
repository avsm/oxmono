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

Readiness confirms state loading and the listener. It does not probe the PLC
or Git source. Submit an authenticated job to check those dependencies.

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
execution deadline and a 1 MiB log limit per workflow. Completed histories
remain on disk without automatic retention. Pending work resumes after
restart; interrupted workflows become failed. One process may own a state
directory. The HTTP health endpoint reports service availability; observer
connection failures are reported in the service journal and retried.

Membership and repository changes trigger fresh PDS reads. Affected mutations
return `503 CatalogPending` while reconciliation is pending. Fetch failures
back off up to one minute and keep the affected catalog unavailable. Event
processing continues for other repositories. Stream catch-up requires the
upstream knot and Jetstream to retain the saved cursor. Monitor disk use,
since history and event queues have no automatic quota or retention policy.

Each command gets a process group. Cancellation, deadlines and normal exit
kill remaining group members. This cleans up shell children but does not
isolate jobs that deliberately detach or access the service account's files.

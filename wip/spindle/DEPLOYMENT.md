# Running spindle as a service

Spindle runs continuously and runs CI for the Tangled repositories whose
manifests name it. Configure its hostname, owner DID, PLC origin, Jetstream URL
and writable state directory. SQLite initialization and migration happen
automatically. The service needs no PDS login or spindle signing key.

## Network requirements

Knots and users reach the spindle at `https://HOSTNAME`. A knot notifies a
spindle only if its `/.well-known/did.json` names a `#tangled_spindle` service
of type `TangledSpindle` at an HTTPS endpoint, which this service serves.

The owner and callers need ATProto accounts with ES256K or ES256 `#atproto`
signing keys. Their PDS issues short-lived service JWTs. A knot signs its push
notifications with the key in its own `did:web` document. The spindle resolves
current DID documents, verifies signatures and consumes each token's `jti`
once. PDS passwords and session tokens are never stored by the spindle.

Outbound access includes the configured PLC directory, knots, owners' PDSes,
`did:web` hosts and HTTPS Git clones from knots. `--jetstream` adds a
Jetstream that carries the knots' `org.tangled.repo.manifest` and
`org.tangled.review.patch` records. Without it, pushes and manual runs still
work, but pull requests do not run and manifest changes take effect only when
cached acceptance expires.

Use HTTPS/WSS for remote services. TLS verifies hostnames and system trust;
`SSL_CERT_FILE` can select a private CA bundle. `--allow-http` enables HTTP/WS
for a development network, but knots refuse a spindle without HTTPS. No PLC or
Jetstream endpoint defaults to the live network. Tangled's public Jetstream is
`wss://index.tangled.network/subscribe`.

Tangled's repository settings pages call the spindle from the browser.
`--cors-origin=https://tangled.org` lets them. Without it, browsers refuse the
responses and the pages report `Load failed`.

## Jetstream version

This spindle implements the legacy `/subscribe` protocol with
`wantedCollections` and microsecond timestamp cursors. Do not point it at the
Jetstream v2 path `/xrpc/network.bsky.jetstream.subscribeEvents`. The
Jetstream must follow a relay that crawls the knots, or a knot's
`com.atproto.sync.subscribeRepos` directly, as the Docker test does. Tangled's
own deployments use hydrant, a filtering indexer with a compatible
`/subscribe` endpoint. `index.tangled.network` serves this protocol.

## Serve repositories

1. Serve the configured hostname over HTTPS, forwarding HTTP and WebSocket
   upgrades to the spindle. `/.well-known/did.json` and `/xrpc/` must be
   reachable.
2. Assign the spindle to a repository. In Tangled this is the repository's
   pipeline setting. With the CLI:

   ```sh
   tangled repo set-spindle ci.YOUR_DOMAIN --repo alice.example/demo
   ```

   This sets the `spindle` field of the repository's manifest on its knot.
3. Admit the repository's owner. `--admission=closed`, the default, serves
   the `--owner`, any `--admin` DIDs and the allow-list. Admins manage the
   allow-list:

   ```sh
   tangled spindle member add ci.YOUR_DOMAIN bob.example
   tangled spindle member list ci.YOUR_DOMAIN
   ```

   `--admission=open` serves anyone.
4. Push a commit. The knot notifies the spindle, and the default executor
   logs a description of each workflow. Pipeline status and logs appear
   in Tangled and through `tangled pipeline`.

Collaborators are the knot's `org.tangled.graph.collaborator` records. They may
start, cancel, retry and approve pipelines. Allow-list membership admits an
owner's repositories. It grants no access to other owners' repositories.

## Repository workflows, secrets and limits

The spindle hands each repository's `.tangled/workflows` YAML, and the OCaml
workflows, to its executor. It runs no workflow commands on this host. The
`spindle` command's executor only logs each workflow and succeeds. To run
builds, supply an executor through `Spindle.config.executor` and raise
`--workflow-timeout` above its 60-second default.

Secrets are kept in `spindle.db` in the state directory, which only the
service account can read. Webhooks refuse private and loopback addresses
unless `--private-webhooks` is given, which suits only a development network.

`--quota-repo` and `--quota-user` set default limits, such as
`--quota-repo=workflows=2` or `--quota-user=ci_minutes_monthly=600`.
Administrators override them with `tangled spindle quota`, ban with
`tangled spindle ban`, and admit new owners with `tangled spindle invite`.

## Keep the local Docker service running

From the oxmono root, with Tangled core checked out in `../core`:

```sh
python3 wip/spindle/testbed/stack.py up
python3 wip/spindle/testbed/stack.py test
```

`up` builds the executables and images and leaves knot2, its Jetstream and
the spindle running on the local ATP network. State is kept in Docker volumes
and under `wip/spindle/testbed/.state/stack-spindle`. The stack has no
reboot restart policy. Use the persistent service setup below for a daemon
managed by the host.

## Install the native service

On a host matching the build system's ABI, install CA certificates and the
shared libraries required by the executable: OpenSSL 3, GMP, zstd and Brotli. The glibc must be at least as new as the build host's. Inspect `ldd`
before moving a binary between hosts. Build from the oxmono root with the
existing OxCaml switch:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @wip/spindle/all
sudo install -m 0755 _build/default/wip/spindle/bin/main.exe \
  /usr/local/bin/ocaml-spindle
```

Create an unprivileged `ocaml-spindle` system account with no interactive
login:

```sh
sudo useradd --system --user-group --home-dir /var/lib/ocaml-spindle \
  --shell /usr/sbin/nologin ocaml-spindle
```

Jobs clone the knot by repository DID. A manual-only installation may instead
use `--repo=did:plc:REPOSITORY --source=/absolute/path/to/repo` and omit
Jetstream. This static mapping accepts only its configured owner's mutations.

Save this as `/etc/systemd/system/ocaml-spindle.service`, replacing every
uppercase placeholder. The other endpoints shown are Tangled's live network.
`--hostname` is a DNS hostname without a scheme, path or trailing slash.
`--plc` is an origin without a trailing slash.

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
  --jetstream=wss://index.tangled.network/subscribe \
  --cors-origin=https://tangled.org \
  --plc=https://plc.directory \
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
exempts it from `ProtectSystem=strict`. The spindle starts no process for a
workflow.
Eio uses io_uring, so a seccomp filter must allow it.

```sh
sudo systemctl daemon-reload
sudo systemctl enable --now ocaml-spindle
sudo systemctl status ocaml-spindle
journalctl -u ocaml-spindle -f
curl --fail http://127.0.0.1:9000/xrpc/_health
curl --fail http://127.0.0.1:9000/xrpc/org.tangled.spindle.describeSpindle
```

`/_health` answers `{"status":"ok"}` while the process runs, as the
reference spindle's does. `/xrpc/_health` reports liveness and diagnostics.
Use `/xrpc/_ready` or
`/readyz` for readiness. Submit an authenticated job to check execution and
caller authorization.

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
Authorization headers. Queries and log subscriptions are public, so request
metadata and job output are public too.

Run a job with the Tangled CLI:

```sh
tangled pipeline trigger --repo alice.example/demo --commit FULL_COMMIT_ID
tangled pipeline logs PIPELINE --repo alice.example/demo
```

`wip/spindle/tools/trigger.py` does the same without the CLI. It prompts
for a PDS app password, obtains a service JWT and prints the spindle's answer
without printing credentials:

```sh
python3 wip/spindle/tools/trigger.py \
  --pds=https://YOUR_PDS_HOST \
  --spindle=https://ci.YOUR_DOMAIN \
  --hostname=ci.YOUR_DOMAIN \
  --identifier=YOUR_OWNER_HANDLE \
  --repo=did:plc:REPOSITORY \
  --sha=FULL_COMMIT_ID
```

## State and operational limits

The state directory contains SQLite state. Back it up
while the service is stopped, or use SQLite's backup API. Preserve `spindle.db`
with its WAL when copying live storage. Deleting the database also deletes
pipeline IDs, the event cursor, the allow-list and accepted JWT nonces.

There are two concurrent workflows unless `--concurrency` allows more, 32
outstanding pipelines, a 60-second
default deadline and a 1 MiB log limit per workflow. Pending and blocked
work resumes after restart. Interrupted workflows run again from the start
about a minute after the spindle is back, up to three times, from the Duraml
store `jobs.sqlite` in the state directory. Back it up with its WAL, as the
main database. One process may own a state directory.

Output streams one line per frame, as the reference spindle sends it. A line
waits for its newline, the end of its step or 16 KiB. Valid UTF-8, including
characters split across reads, is preserved. Malformed bytes become
replacement characters. Each event commits to SQLite before log subscribers
can see it. Step transitions atomically fold acknowledged journal entries into
the pipeline snapshot. Restart restores any remaining journal entries, so
already published output survives an interrupted step.

A knot that cannot reach the spindle keeps the push in its outbox and retries
for three days. The spindle answers 503 while a repository's knot, PDS or DID
host is unreachable, so such a push is retried too. A retried push starts no
second pipeline while its deduplication key is retained.

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
| `--inbox-limit` | 10000 | Pending Jetstream events |
| `--inbox-megabytes` | 64 | Pending event payload, in MiB |
| `--replay-hours` | 24 | Maximum age of a Jetstream cursor to resume from |
| `--reconcile-seconds` | 300 | How long a repository's acceptance is cached |
| `--maintenance-seconds` | 60 | Storage cleanup interval |

All limits must be positive. Receipt retention must cover the replay window
and exceed the cache lifetime. Cleanup removes the oldest completed pipelines
first until all history budgets hold. It never evicts active pipelines,
pending inbox work or unexpired JWT nonces. A full inbox applies backpressure
without advancing the stream cursor.

These are payload budgets, not physical disk quotas. SQLite reuses freed pages
and checkpoints its WAL. Database overhead and pending work are outside the
completed-history budget. Monitor filesystem capacity and use SQLite
maintenance during planned downtime if the file must shrink. Deleted pipeline
IDs return not found.

`/xrpc/_health` returns HTTP 200 with a `ready` flag and diagnostics. The same
report at `/xrpc/_ready` or `/readyz` returns HTTP 503 while degraded. It
includes the Jetstream connection, reconnect attempts, activity and event
ages, the cursor, inbox counts, bytes and ages, and maintenance results.
Readiness fails for a disconnected Jetstream, 120 seconds without transport
activity, inbox work older than 60 seconds or maintenance failure. Ping/pong
traffic keeps a quiet stream healthy. Connection errors also go to the
service journal.

A Jetstream cursor older than `--replay-hours` restarts from the present.
Patches published while the spindle was away for longer are not run. Set
`--replay-hours` no higher than the history the Jetstream retains.

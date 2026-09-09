# Running spindle as a service

Spindle can run persistently now and accept manual inspection jobs. It needs
an owner DID, repository DID, Git source, PLC origin, hostname and writable
state directory. It creates and locks its state directory on first start.
There is no database migration, PDS login or spindle signing key to initialize.

## PDS and network requirements

The configured owner must have an account whose `did:plc` document contains
an ES256K/secp256k1 `#atproto` key. Spindle reads that document from `--plc` on
each authenticated request. Use an HTTPS PLC origin for a remote deployment.
TLS verifies the host and system CA store. Set `SSL_CERT_FILE` before startup
only when using a private CA bundle, as the Docker testbed does.

The caller logs in to the owner's PDS and requests
`com.atproto.server.getServiceAuth`, with `aud=did:web:YOUR_HOSTNAME` and
`lxm=sh.tangled.ci.triggerPipeline`. It sends the returned JWT directly to
spindle. Cancellation requires a token for `sh.tangled.ci.cancelPipeline`.
The service stores neither the PDS password nor its session tokens.

The current authentication profile supports ES256K, the `#atproto` key, one
owner and a bare DID audience. It does not support P-256 owner keys, require
jti, or prevent reuse of a valid token before expiry. The current
[ATProto specification](https://atproto.com/specs/xrpc#inter-service-authentication-jwt)
requires jti and recommends replay prevention and short lifetimes. Those gaps
remain service work before claiming full ATProto authentication conformance.

Outbound traffic consists of PLC lookups and Git access if `--source` is a
remote URL. Spindle does not need a relay, Jetstream or a local PDS process.
A private installation can instead use the existing local PLC, PDS and Git
fixture, without any dependency on the wider ATP network.

## Keep the local Docker service running

From the oxmono root:

```sh
python3 bleeding/spindle/testbed/run.py up
python3 bleeding/spindle/testbed/demo.py
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

Create a mirror owned by that account. Replace `HTTPS_GIT_URL` with the
repository's clone URL. The requested commit must exist in the mirror.

```sh
sudo install -d -o ocaml-spindle -g ocaml-spindle /srv/git/ocaml-spindle
sudo -u ocaml-spindle git clone --mirror HTTPS_GIT_URL \
  /srv/git/ocaml-spindle/project.git
```

An existing bare or ordinary repository also works if owned by the service
account. Git may reject local clones from a different owner's repository.
Git children run with a fixed environment, disable global/system Git config
and allow only file, HTTP and HTTPS transports. Use a local mirror for private
repositories. SSH and ambient credential-helper configuration are unsupported.
Update the mirror outside the service when new commits are needed.

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
  --repo=did:plc:REPOSITORY \
  --source=/srv/git/ocaml-spindle/project.git \
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

## Registration and current limits

No PDS record or resolvable spindle DID document is required for the direct
manual-dispatch path tested here. The configured `did:web` name is the exact
JWT audience. Spindle does not serve `/.well-known/did.json`, and this guide
uses direct requests rather than PDS service proxying.

The sibling tangled-core appview registers a spindle through its settings UI.
It writes `sh.tangled.spindle` in the owner's PDS repository, using the hostname
as its record key, and verifies the owner through `sh.tangled.owner`.
The OCaml service implements that owner endpoint. Registration alone will not
make this prototype a complete Tack replacement. It does not consume spindle
membership records, discover repositories, follow knot push/pull events or
publish an event stream for appview indexing. Automatic CI and complete UI
integration need that work. The repository/source mapping is currently static.

There are two concurrent workers, 32 outstanding jobs, 1000 retained pipelines,
a 60-second job deadline and a 1 MiB log limit. History does not prune itself.
Archive the state directory while stopped and start with an empty one when
rotating history. Old pipeline URLs then cease to resolve. Back up state while
the service is stopped. Interrupted jobs become failed after restart.
Only one process may use a state directory. Custom OCaml jobs must be trusted
because child processes share the service account's filesystem permissions.

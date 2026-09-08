# Live Zulip integration tests

The live suite uses the official Zulip Server 12.2 Docker image and its
PostgreSQL, RabbitMQ, Redis and memcached dependencies.  Every image is pinned
by digest in `compose.yaml`.  It binds only the Zulip HTTP service to loopback;
all dependency ports remain inside the Compose network.

The setup is a small local derivative of the official
[docker-zulip Compose stack](https://github.com/zulip/docker-zulip) and follows
the server-in-container precedent in the
[python-zulip-api CI workflow](https://github.com/zulip/python-zulip-api/blob/main/.github/workflows/zulip-ci.yml).

```sh
test/integration/zulip.sh up
eval "$(test/integration/zulip.sh env)"
dune build @integration
test/integration/zulip.sh down --purge
```

`test/integration/zulip.sh run` uses a fresh Compose project and an ephemeral
loopback port, runs the authentication smoke check and then invokes `dune build
@integration` (or a command passed after `run`). It tears down its own
containers, volumes and marked temporary directory. On failure it preserves
Compose logs under `/tmp/ocaml-zulip-logs` (or `ZULIP_TEST_LOG_DIR`) before
cleanup. `up` is for interactive work and keeps the server until `down` is
called. `smoke` is a useful first-run diagnostic:

```sh
test/integration/zulip.sh smoke
```

The wrapper creates random local secrets and an isolated Docker configuration
directory. The latter avoids a stale authenticated `ghcr.io` entry preventing
the public Zulip image from being pulled. Its data marker records the exact
Compose project and canonical data path; all management commands reject
unmarked or mismatched directories and symlinked secrets/output. The generated
API keys are temporary credentials for `127.0.0.1` only and are removed by
`down --purge`.

After `up`, `env` exports the contract consumed by OCaml scenarios:

| Variable | Meaning |
| --- | --- |
| `ZULIP_TEST_SERVER` | `http://127.0.0.1:<allocated-port>` |
| `ZULIP_TEST_FIXTURES` | Absolute path to the fixture JSON file |

The fixture is schema version 1.  It contains `server` (`url`, `api_url`,
`version`), `realm`, `users`, `channels`, `topics` and `group_dm`.  Users
`admin`, `alice`, `bob`, and bots `echo`/`store` each provide numeric `id`,
API-authentication `email`, `delivery_email`, `api_key` and `full_name`.
Channels `public` and `private` provide an ID and name.  The private channel is
subscribed by admin, Alice and echo; the public channel by every fixture
principal.

`seed.py` runs inside the pinned image through `manage.py shell`.  It calls the
server's realm, user, channel and subscription actions rather than inserting
database rows, and writes the JSON file atomically.  Topics and a group DM are
described by stable names/participants; scenarios create their own messages so
they remain independent when rerun.

The normal `@runtest` alias remains offline. `@integration` requires the exported fixture, server and restart-helper variables and fails if the fixture cannot be read; it never silently
reports a live test as skipped. It deliberately has an `universe` dependency,
so each explicit `dune build @integration` runs the live suite again.

The suite has 19 API/runtime scenarios plus a tutorial CLI check. They cover
fixture authentication, private-channel access, channel and group-DM delivery,
rendering/flags/narrows, uploads/downloads, custom emoji, bot storage, event
projection, and one-to-one/group-DM/mentioned-channel bot replies. Expanded
scenarios exercise scheduled messages and drafts, typed message history, narrow
flags and read receipts, attachment URLs/thumbnail status, channel folders and
permissions, groups/subgroups, profile fields, linkifiers, settings and presence.
The saved-snippet scenario uses a generic bot account to create, list, edit and
delete reusable Markdown content. Separate title and content edits verify that
omitted fields stay unchanged.

The restart scenario keeps a collector running while `zulip.sh restart` restarts
this project's Zulip container. It then explicitly invalidates the old queue
(Zulip can preserve queues during a graceful restart), verifies replacement
registration, and receives a new message. Fixture data stays intact.

The CLI check imports a private profile in temporary XDG directories, runs the
preflight recipe, verifies explicit HTTP opt-in, exchanges a DM with the echo
tutorial, sends SIGTERM and checks its exit, and checks failure with an invalid
API key. It never uses a developer's hosted profile. Both suites run even if
one fails, and the wrapper returns failure if either fails.

On the development host used to validate this harness, a cold database
initialization, server startup, and fixture seed took about 67 seconds. The
five running containers used about 6.0 GiB RSS in total, dominated by the
Zulip server process (about 5.1 GiB). The pinned images occupied about 3.5 GB
before writable volumes. These are observed figures, not minimum requirements;
leave several GiB free in Docker's storage area and memory headroom before a
fresh run.

Each isolated `run` removes its five containers, four volumes, network and
marked temporary data directory. Failure logs are retained separately under
`/tmp/ocaml-zulip-logs`.

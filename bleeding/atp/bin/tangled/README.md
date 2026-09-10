# Tangled CLI

Build from the monorepo root:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  bleeding/atp/bin/tangled/cli/main.exe
```

The binary is `_build/default/bleeding/atp/bin/tangled/cli/main.exe`.
Examples below call it `tangled`.

## Authentication and routing

```sh
tangled auth login --pds https://your-pds.example --handle alice.example
tangled auth status
tangled auth profile --help
```

Login prompts for the password without terminal echo. `TANGLED_PASSWORD` can
supply an app password. Sessions use XDG directories and the selected auth
profile. `TANGLED_PDS` selects the PDS for subsequent commands. An override
never forwards a saved session to a different origin.

PDS record operations read the configured PDS. To read a user on another PDS,
set `TANGLED_PDS` to that user's PDS and supply `--user`. PDS discovery is not
automatic. Public knot, spindle and appview queries work without login.
`SSL_CERT_FILE` selects a CA bundle for HTTPz HTTP and WebSocket connections.

Service mutations obtain a fresh PDS-issued JWT with the destination's
`did:web` audience, a 60-second expiry and the exact method in `lxm`.
`--audience DID` overrides the audience for local test gateways. PDS bearer
tokens stay on the PDS origin. Authenticated bearer requests require HTTPS.

## Repositories and access

```sh
tangled repo create demo --knot knot.example --branch main
tangled repo create fork --knot knot.example --source alice.example/demo
tangled repo list --json
tangled repo info alice.example/demo
tangled repo info did:plc:REPO --knot knot.example
tangled repo clone alice.example/demo ./demo
tangled repo spindle alice.example/demo spindle.example
tangled repo collaborators alice.example/demo
tangled repo add-collaborator alice.example/demo bob.example
tangled repo remove-collaborator alice.example/demo bob.example
tangled knot members knot.example
tangled knot add-member knot.example bob.example
tangled knot remove-member knot.example bob.example
tangled spindle members spindle.example --user did:plc:SPINDLE_OWNER
tangled spindle add-member spindle.example bob.example
tangled spindle remove-member spindle.example bob.example
```

Repository names are cosmetic. CI, cloning and knot operations use repository
DIDs. Record URIs identify the owner's PDS record. A bare repo DID needs
`--knot` when resolving owner metadata. Repository creation requires a current
knot that returns a repo DID.

Knot membership and collaborators use the knot's authoritative API. Spindle
membership commands publish or remove grants in the logged-in user's PDS.
Only records published by the configured spindle owner grant access.

`repo spindle REPO` with no hostname removes the assignment. Updates retain
unknown metadata and use a CID precondition. Deletion removes the PDS record
before requesting knot teardown:

```sh
tangled repo delete alice.example/demo
```

Creation and deletion span two services and are not atomic. Failed publication
reports the minted DID and rkey so the record can be published with `record
create`. After failed knot teardown, retry `sh.tangled.repo.delete` through
`api call` using the repo DID. Do not recreate the PDS record during teardown.

## CI

```sh
tangled pipeline list --repo did:plc:REPO --spindle spindle.example \
  --limit 20 --kind push --commit COMMIT --json
tangled pipeline show PIPELINE --spindle spindle.example
tangled pipeline trigger --repo did:plc:REPO --spindle spindle.example \
  --sha COMMIT --workflow inspect --input message=hello
tangled pipeline logs PIPELINE --spindle spindle.example
tangled pipeline retry PIPELINE --spindle spindle.example
tangled pipeline cancel PIPELINE --repo did:plc:REPO --spindle spindle.example
tangled pipeline definition --repo did:plc:REPO --spindle spindle.example \
  --sha COMMIT
```

`--cursor` continues a pipeline query. `--commit`, `--kind`, `--workflow` and
`--input` can repeat. Owner/name and record-URI forms of `--repo` can discover
the assigned spindle from the configured PDS.

Logs use the current CBOR WebSocket API. Text mode preserves stdout, stderr,
newlines and partial lines. `--json` includes control events and metadata.
The command reads until the server closes the stream. It does not reconnect
automatically. Retry creates a new pipeline at the original commit and
retains the original workflow selection. Manual and pull-request trigger
metadata is retained. Push retries become manual dispatches at that commit.

## Issues, pulls, keys and stars

```sh
tangled issue list did:plc:REPO --appview https://tangled.org --state open
tangled issue show at://did:plc:AUTHOR/sh.tangled.repo.issue/RKEY
tangled issue create did:plc:REPO --title "Build fails" --body-file issue.md
tangled issue close at://did:plc:AUTHOR/sh.tangled.repo.issue/RKEY
tangled issue reopen at://did:plc:AUTHOR/sh.tangled.repo.issue/RKEY
tangled pull list did:plc:REPO --state open
tangled pull show at://did:plc:AUTHOR/sh.tangled.repo.pull/RKEY
tangled key add ~/.ssh/id_ed25519.pub --name laptop
tangled key list
tangled key remove RKEY
tangled star add did:plc:REPO
tangled star list
tangled star remove did:plc:REPO
```

Issue state records are subject to appview authority checks. Advanced pull
revisions and comments use the record interface below. Keys print complete
public material. Private key files are rejected.

## Complete lexicon access

All 231 vendored documents are available offline. `api call` handles queries,
procedures, binary responses and public CBOR subscriptions:

```sh
tangled api list org.tangled.temp.spindle
tangled api schema org.tangled.temp.spindle.quota.set
tangled api call sh.tangled.repo.listIssues --service https://tangled.org \
  -q subject=did:plc:REPO -q limit=20
tangled api call org.tangled.temp.spindle.quota.set \
  --service https://spindle.example --auth service --input quota.json
```

Choose the service implementing the method. New lexicons do not imply that
every knot, appview or spindle implements them. Authentication modes are
`none` (default), `service` and `pds`. The latter requires the configured PDS
origin. Browser-session-only APIs do not gain browser authentication here.
Parameters repeat with `-q KEY=VALUE`. JSON bodies come from `--input FILE`
or `--input -`. Binary results are written unchanged to stdout.

All vendored record collections have CRUD access:

```sh
tangled record list sh.tangled.repo.issue --user did:plc:AUTHOR
tangled record get sh.tangled.repo.issue RKEY --user did:plc:AUTHOR
tangled record create sh.tangled.feed.comment --input comment.json
tangled record put sh.tangled.repo.issue RKEY --cid CURRENT_CID --input issue.json
tangled record delete sh.tangled.repo.issue RKEY
```

Records must include the matching `$type`. Use `--rkey self` when creating a
literal-key record such as a profile. Replacements require the previous CID.
Lists follow all PDS pages and report malformed records and server errors.

## Verification

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/atp/atp/test bleeding/atp/hermest/test bleeding/atp/bin/tangled/test

python3 bleeding/spindle/testbed/parity.py up
python3 bleeding/atp/bin/tangled/testbed/run.py
```

The first suite uses temporary XDG directories and loopback TLS fixtures.
The Docker test reuses the local PLC/PDS/knot/spindle stack, creates disposable
repositories and exercises service JWTs, memberships, Git clone/push, CI,
logs, retry, cancellation and deletion. Its ATP endpoints are local Docker
aliases. Updating Docker images or building Tangled may download dependencies.
No test contacts a live ATP service.

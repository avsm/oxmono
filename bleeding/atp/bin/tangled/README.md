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
tangled auth login --oauth --handle alice.example
tangled auth login --pds https://your-pds.example --handle alice.example
tangled auth status
tangled auth profile --help
```

`--oauth` signs in through the PDS's OAuth server. The CLI prints an
authorization URL and opens it in a browser unless `--no-browser` is given.
The browser may run on any machine. After approval it shows a
`code=...&state=...` line to paste at the CLI's prompt. The whole URL of that
page is accepted too. The page and the client metadata are the ocaml-atp
CLI's, published at `https://www.cl.cam.ac.uk/~avsm2/`. Their scope is
`atproto transition:generic`, the access an app password has.
`--client-id` and `--redirect-uri` select other published copies.

`--oauth --loopback` instead returns the browser to a listener on
`127.0.0.1`, so the browser must run on the same host. It needs no published
metadata and requests only write access to the `org.tangled.*` record
collections and service tokens for the methods the CLI calls. `--scope`
overrides either default. The tokens are bound to a key saved with the
session and refresh automatically.

Password login prompts for the password without terminal echo.
`TANGLED_PASSWORD` can supply an app password. Sessions use XDG directories and the selected auth
profile. `TANGLED_PDS` selects the PDS for subsequent commands. An override
never forwards a saved session to a different origin. `TANGLED_PLC` selects
the PLC directory, `https://plc.directory` by default.
`SSL_CERT_FILE` selects a CA bundle for HTTPS and WebSocket connections.

The logged-in account's records are read from the configured PDS. Another
account's records are read from the PDS its DID document names. Public knot,
spindle and PDS reads work without login.

Every service call carries a fresh PDS-issued JWT with a 60-second expiry and
the exact method in `lxm`. Knots accept each token once. Knot-level calls
such as repository creation and membership are addressed to the knot's
`did:web`. Writes into a repository account are addressed to the repository
DID. Spindle calls are addressed to the spindle's `did:web`. `--audience DID`
overrides the knot or spindle audience for local test gateways. PDS bearer
tokens stay on the PDS origin.

## Repositories and access

Each repository has its own `did:plc`, minted and hosted by its knot. The
knot holds the repository's `org.tangled.repo.manifest`, which names the
owner and repository name. The owner's PDS holds an
`org.tangled.repo.declaration` keyed by the lowercased name, naming the
repository DID back. Commands resolve a repository only while both agree.

```sh
tangled repo create demo --knot knot.example --branch main --spindle ci.example
tangled repo create fork --knot knot.example --source alice.example/demo
tangled repo list --json
tangled repo info alice.example/demo
tangled repo info did:plc:REPO
tangled repo clone alice.example/demo ./demo
tangled repo spindle alice.example/demo spindle.example
tangled repo collaborators alice.example/demo
tangled repo add-collaborator alice.example/demo bob.example
tangled repo remove-collaborator alice.example/demo bob.example
tangled knot describe knot.example
tangled knot members knot.example
tangled knot add-member knot.example bob.example
tangled knot remove-member knot.example bob.example
tangled spindle describe spindle.example
tangled spindle members spindle.example
tangled spindle add-member spindle.example bob.example
tangled spindle remove-member spindle.example bob.example
```

A repository is named by `owner/name`, a bare name of the logged-in account,
its declaration AT-URI, or its DID. The knot comes from the repository DID's
`#tangled_knot` service unless `--knot` overrides it. Clones use the DID form
of the URL, which survives renames.

Collaborators are `org.tangled.graph.collaborator` records in the repository
account, keyed by the collaborator's DID. Only the owner may change them.
Knot members are `org.tangled.knot.member` records in the knot's own
account, and need a knot admin. Spindle membership is the spindle's
allow-list, which needs a spindle admin.

`repo spindle REPO` with no hostname removes the assignment. Manifest updates
retain unknown fields and use a CID precondition. Deletion removes the
declaration first, because the knot refuses to delete a repository that is
still declared:

```sh
tangled repo delete alice.example/demo
```

Creation and deletion span two services and are not atomic. If the
declaration cannot be written after the knot mints a repository, the error
names the DID and rkey so that the declaration can be written with `record
create`. After a failed knot deletion, retry `org.tangled.repo.deleteRepo`
through `api call`. Do not recreate the declaration in between.

## CI

```sh
tangled pipeline list --repo alice.example/demo --limit 20 \
  --trigger org.tangled.event.push --commit COMMIT --json
tangled pipeline show PIPELINE --repo alice.example/demo
tangled pipeline trigger --repo alice.example/demo --commit COMMIT \
  --ref refs/heads/main --workflow inspect --input message=hello
tangled pipeline logs PIPELINE --repo alice.example/demo
tangled pipeline retry PIPELINE --repo alice.example/demo
tangled pipeline cancel PIPELINE --repo alice.example/demo
tangled pipeline approve PIPELINE --repo alice.example/demo
tangled pipeline definitions --repo alice.example/demo
```

The spindle is the one named in the repository manifest unless `--spindle`
names another. A repository DID together with `--spindle` needs no knot or
PDS lookup. Pipeline IDs are local to their spindle and are always paired
with the repository DID. `--cursor` continues a query. `--commit`,
`--trigger`, `--workflow` and `--input` can repeat.

`trigger` sends an `org.tangled.ci.trigger.manual` event through
`org.tangled.event.notifyEvent` and prints the new pipeline IDs. The account
must be able to push to the repository. `retry` asks the spindle to run a
pipeline again and prints the new ID. `approve` releases a pipeline that is
blocked waiting for a collaborator.

Logs use the CBOR WebSocket API. Text mode preserves stdout, stderr, newlines
and partial lines. `--json` includes control events. The command reads until
the server closes the stream. It does not reconnect.

## Issues, pulls, keys and stars

```sh
tangled issue list alice.example/demo --state open
tangled issue show alice.example/demo 7
tangled issue create alice.example/demo --title "Build fails" --body-file issue.md
tangled issue close alice.example/demo 7
tangled issue complete alice.example/demo 7
tangled issue reopen alice.example/demo 7
tangled pull list alice.example/demo
tangled pull create alice.example/demo --title "Fix" --target main
tangled pull submit alice.example/demo 8 --base BASE --head HEAD --branch fix
tangled key add ~/.ssh/id_ed25519.pub --name laptop --sign
tangled key list
tangled key remove RKEY
tangled star add did:plc:REPO
tangled star list
tangled star remove did:plc:REPO
```

Issues and pull requests are `org.tangled.track.ticket` records in the
repository account. A pull request is a ticket with a target branch. The knot
writes them, mints their keys and checks who may change their state. `pull
submit` adds a revision. The knot fetches its head commit from `--source`, the
target repository by default.

Keys are `org.tangled.key.sshKey` records. A key is published for push
authentication, and `--sign` also publishes it for signature checks. Keys
print complete public material. Private key files are rejected. Stars name a
repository by its DID.

## Complete lexicon access

All 418 vendored documents are available offline. `api call` handles queries,
procedures, binary responses and public CBOR subscriptions:

```sh
tangled api list org.tangled.ci
tangled api schema org.tangled.temp.spindle.quota.set
tangled api call org.tangled.git.listBranches --service https://knot.example \
  -q repo=did:plc:REPO -q limit=20
tangled api call org.tangled.temp.spindle.quota.set \
  --service https://spindle.example --auth service --input quota.json
```

Choose the service implementing the method. A vendored lexicon does not imply
that every knot or spindle implements it. Authentication modes are `none`
(default), `service` and `pds`. The latter requires the configured PDS origin.
Parameters repeat with `-q KEY=VALUE`. JSON bodies come from `--input FILE` or
`--input -`. Binary results are written unchanged to stdout.

All vendored record collections have CRUD access on the configured PDS:

```sh
tangled record list org.tangled.feed.star --user did:plc:AUTHOR
tangled record get org.tangled.actor.profile self
tangled record create org.tangled.graph.follow --rkey did:plc:BOB --input follow.json
tangled record put org.tangled.actor.profile self --cid CURRENT_CID --input profile.json
tangled record delete org.tangled.feed.star RKEY
```

Records must include the matching `$type`. Replacements require the previous
CID. Lists follow all pages and report malformed records and server errors.

## Verification

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/atp/atp/test bleeding/atp/hermest/test bleeding/atp/bin/tangled/test

python3 bleeding/spindle/testbed/stack.py test
```

The first suite uses temporary XDG directories and loopback TLS fixtures for
the PDS, PLC directory, knot and spindle. The Docker test runs a local PLC,
PDS, knot2 built from a sibling Tangled checkout, and the OCaml spindle. It
drives this CLI through SSH keys, repository creation, Git pushes, CI, logs,
collaborators, issues, pull requests and deletion. See the [spindle
README](../../../spindle/README.md#local-docker-integration). No test
contacts a live ATP service.

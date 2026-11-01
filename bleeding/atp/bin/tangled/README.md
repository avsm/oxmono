# Tangled CLI

Build from the monorepo root:

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  bleeding/atp/bin/tangled/cli/main.exe
```

The binary is `_build/default/bleeding/atp/bin/tangled/cli/main.exe`.
Examples below call it `tangled`.

## Output

On a terminal, commands draw tables, panels and trees in colour, show a
spinner while they wait on the network, and confirm each change. Accounts
show by handle, where the handle resolves back to the account. Pipes get the
plain lines or JSON that scripts read, unchanged. `--json` prints JSON on a
terminal too. Workflow logs keep their colours on a terminal, but lose
escapes that would move the cursor or retitle the window.

Every command takes these options:

```sh
tangled repo list -v                       # trace each HTTP exchange
tangled repo list -v -v                    # with request and response fields
tangled repo list --color=always | less -R  # keep tables and colour in a pager
tangled repo list --theme=dracula
```

`-v` prints one line per exchange on standard error, with its status, time,
host and XRPC method. Secrets are redacted. `--color` and `--theme` also
read `TANGLED_COLOR` and `TANGLED_THEME`. `NO_COLOR` turns colour off.

## Authentication and routing

```sh
tangled auth login --oauth --handle alice.example
tangled auth login --pds https://your-pds.example --handle alice.example
tangled auth status
tangled auth account list
```

`--oauth` signs in through the PDS's OAuth server. The CLI prints an
authorization URL and opens it in a browser unless `--no-browser` is given.
The browser may run on any machine. After approval it shows a
`code=...&state=...` line to paste at the CLI's prompt. The whole URL of that
page is accepted too. The CLI requests write access to the `org.tangled.*`
record collections and service tokens for the methods it calls, and no more.
Its client metadata, `web/tangled-oauth-client.json`, is published at
`https://www.cl.cam.ac.uk/~avsm2/` beside the paste page. `--client-id` and
`--redirect-uri` select other published copies, such as the ocaml-atp CLI's
`oauth-client.json` with `--scope "atproto transition:generic"` for the
access an app password has.

`--oauth --loopback` instead returns the browser to a listener on
`127.0.0.1`, so the browser must run on the same host. It needs no published
metadata. `--scope` overrides either default. The tokens are bound to a key saved with the
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
DID. Spindle calls are addressed to the spindle's `did:web`.
`TANGLED_AUDIENCE`, or the unlisted `--audience DID`, overrides the knot or
spindle audience for local test gateways. PDS bearer tokens stay on the PDS
origin. Writes into a repository account, such as tickets and comments, are
then attested by an `org.tangled.graph.attestation` record in the author's
own account, as the web client does.

## Choosing a repository

Commands that act in a repository, such as `issue list` or `repo log`, take
it with `--repo` or `-r`. Commands about a repository, such as `repo info`,
take it as an argument. Both default to the repository that the current
directory is a clone of, found from its remotes, `origin` first. A
repository is named by `owner/name`, a bare name of the logged-in account,
its declaration AT-URI, or its DID.

```sh
tangled repo clone alice.example/demo && cd demo
tangled issue list                  # the issues of alice.example/demo
tangled pipeline logs               # the pipeline of the checked-out commit
tangled issue list -r bob.example/other
```


## Repositories and access

Each repository has its own `did:plc`, minted and hosted by its knot. The
knot holds the repository's `org.tangled.repo.manifest`, which names the
owner and repository name. The owner's PDS holds an
`org.tangled.repo.declaration` keyed by the lowercased name, naming the
repository DID back. Commands resolve a repository only while both agree.

```sh
tangled repo create demo --knot knot.example --branch main --spindle ci.example
tangled repo create fork --knot knot.example --fork alice.example/demo
tangled repo create mirror --knot knot.example \
  --import https://example.com/x.git
tangled repo list --json
tangled repo info alice.example/demo
tangled repo clone alice.example/demo ./demo
tangled repo set-spindle spindle.example
tangled repo unset-spindle
tangled repo collaborator list
tangled repo collaborator add bob.example
tangled repo collaborator remove bob.example
tangled knot describe knot.example
tangled knot member list knot.example
tangled knot member add knot.example bob.example
tangled knot invite knot.example --uses 5
tangled knot join knot.example CODE
tangled knot ban add knot.example mallory.example
tangled spindle member add spindle.example bob.example
tangled spindle quota set spindle.example bob.example vcpus 4
```

`TANGLED_KNOT` names the knot for `repo create` when `--knot` is absent. An
import runs on the knot after creation, and `repo info` shows its progress.
The knot comes from the repository DID's `#tangled_knot` service unless
`--knot` overrides it. Clones use the DID form
of the URL, which survives renames.

Collaborators are `org.tangled.graph.collaborator` records in the repository
account, keyed by the collaborator's DID. Only the owner may change them.
Knot members are `org.tangled.knot.member` records in the knot's own
account, and need a knot admin. Spindle membership is the spindle's
allow-list, which needs a spindle admin. Invite codes admit members to
knots and spindles alike. Knot bans are `org.tangled.knot.block` records.
Spindle bans, quotas and invite codes use `org.tangled.temp` methods, which
may change.

Manifest updates
retain unknown fields and use a CID precondition. Deletion removes the
declaration first, because the knot refuses to delete a repository that is
still declared:

```sh
tangled repo delete alice.example/demo
```

Creation and deletion span two services and are not atomic. If the
declaration cannot be written after the knot mints a repository, the error
names the DID and rkey so that the declaration can be written with `api
record create`. After a failed knot deletion, retry
`org.tangled.repo.deleteRepo` through `api call`. Do not recreate the declaration in between.

## Browsing

The knot serves a repository's Git data, so these need no clone:

```sh
tangled repo branch list
tangled repo branch default develop
tangled repo branch delete old-work
tangled repo tags
tangled repo log v1.0 --path src -n 50
tangled repo ls src --ref v1.0
tangled repo cat README.md > README.md
tangled repo diff v1.0 v1.1 --stat
```

A diff prints in unified form. The knot names the changed lines, and the
CLI reads both versions of each file to print them.

## Releases and webhooks

```sh
tangled repo release upload v1.0 demo-1.0.tbz demo-1.0.tbz.sig
tangled repo release list
tangled repo release download v1.0 --dir dist
tangled repo release delete v1.0 demo-1.0.tbz.sig
tangled repo webhook add https://ci.example/hook --event push --secret S
tangled repo webhook deliveries WEBHOOK
```

A release artifact is an `org.tangled.repo.artifact` record that attaches a
file of at most 50 MiB to the commit of a tag. Push the tag first. Webhooks
are kept by the repository's spindle.

## CI

```sh
tangled pipeline list --limit 20 --trigger org.tangled.event.push --json
tangled pipeline show
tangled pipeline logs
tangled pipeline trigger --workflow inspect --input message=hello
tangled pipeline retry PIPELINE
tangled pipeline cancel
tangled pipeline approve PIPELINE
tangled pipeline workflows
tangled pipeline workflows inspect
tangled pipeline secret set DEPLOY_TOKEN < token.txt
tangled pipeline secret list
```

A command that takes a pipeline ID uses, without one, the latest pipeline
for the checked-out commit, or else the latest pipeline. `trigger` runs the
checked-out commit and branch unless `--commit` and `--ref` say otherwise.
The commit must be on the knot.

The spindle is the one named in the repository manifest unless `--spindle`
names another. A repository DID together with `--spindle` needs no knot or
PDS lookup. Pipeline IDs are local to their spindle and are always paired
with the repository DID. `--cursor` continues a query. `--commit`,
`--trigger`, `--workflow` and `--input` can repeat.

`trigger` sends an `org.tangled.ci.trigger.manual` event through
`org.tangled.event.notifyEvent` and prints the new pipeline IDs. The account
must be able to push to the repository. `retry` asks the spindle to run a
pipeline again and prints the new ID. `approve` releases a pipeline that is
blocked waiting for a collaborator. `secret set` reads the value from
standard input unless `--value` gives it, so that it stays out of the shell
history. Secrets cannot be read back.

Logs use the CBOR WebSocket API. Text mode preserves stdout, stderr, newlines
and partial lines. `--json` includes control events. The command reads until
the server closes the stream. It does not reconnect.

## Issues, pulls, keys and stars

```sh
tangled issue list --state open
tangled issue show 7
tangled issue create --title "Build fails" --body-file issue.md
tangled issue comment 7 --body "Fixed in v1.1."
tangled issue edit 7 --assign bob.example --label bug
tangled issue list --label "good first issue"
tangled repo label add bug "#e11d48"
tangled issue history 7
tangled issue complete 7
tangled pull create --title "Fix the build"
tangled pull update 8
tangled pull diff 8 --stat
tangled pull checkout 8
tangled key add ~/.ssh/id_ed25519.pub --name laptop --sign
tangled key list --user bob.example
tangled star add alice.example/demo
tangled profile bob.example
```

Issues and pull requests are `org.tangled.track.ticket` records in the
repository account. A pull request is a ticket with a target branch. The knot
writes them, mints their keys and checks who may change their state.
Comments are `org.tangled.feed.comment` records beside them. `show` lists
them, and pipes get the ticket, its comments and its revisions as JSON.

Labels such as `bug` are defined per repository by its owner, as
`org.tangled.label.definition` records keyed by a slug of the name.
`repo label` lists, adds, renames and removes them. `--label` on `issue
create`, `pull create` and `edit` applies one by name, and `--unlabel` takes
it off. The key of a removed label cannot be used again.

`pull create` proposes the current branch, or `--branch`, into the default
branch, or `--target`. Push the branch first. The head is the branch's tip
on the knot, and the base is where it left the target, found in the clone
when it has both commits. `--source` names a fork that holds the branch.
`--base` and `--head` override both. `pull update` submits a new revision the
same way, from the branch of the last one. `pull diff` shows a revision, the
latest by default, and `pull checkout` fetches it into the branch `pull/ID`.

Keys are `org.tangled.key.sshKey` records. A key is published for push
authentication, and `--sign` also publishes it for signature checks. Keys
print complete public material. Private key files are rejected. Stars name a
repository by its DID.

`auth account` lists and switches between saved logins.

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
tangled api record list org.tangled.feed.star --user did:plc:AUTHOR
tangled api record get org.tangled.actor.profile self
tangled api record create org.tangled.graph.follow --rkey did:plc:BOB \
  --input follow.json
tangled api record put org.tangled.actor.profile self --cid CURRENT_CID \
  --input profile.json
tangled api record delete org.tangled.feed.star RKEY
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
collaborators, issues, comments, pull requests from a clone, and deletion.
See the [spindle
README](../../../spindle/README.md#local-docker-integration). No test
contacts a live ATP service.

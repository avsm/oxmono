# Oracle tests

The unit tests in `test/` check the library against fixtures. The tests in
this directory check it against a *real JMAP server*: a
[Cyrus IMAP](https://www.cyrusimap.org/) test image that speaks JMAP over
HTTP and accepts mail over LMTP. Cyrus is the oracle — where the RFCs are
ambiguous, what a shipping server actually does is the tiebreaker, and these
tests record that answer.

Nothing here runs unless the oracle is configured, so a plain `dune runtest`
stays hermetic and offline: every test calls `Oracle_harness.test_case`,
which skips when `JMAP_ORACLE_URL` is unset.

## Starting the oracle

```sh
scripts/oracle-up.sh                  # docker run, waits until /jmap answers
eval "$(scripts/oracle-env.sh)"       # exports JMAP_ORACLE_URL and _LMTP
scripts/oracle-down.sh                # stop and remove the container
```

`oracle-up.sh` prints the two exports itself, so
`eval "$(scripts/oracle-up.sh)"` also works. The container is named
`jmap-oracle` and publishes these ports on loopback only:

| port    | service                                          |
| ------- | ------------------------------------------------ |
| `18080` | HTTP; JMAP session at `/jmap`, `/.well-known/jmap` redirects there |
| `18024` | LMTP, used to inject test messages                |
| `18001` | Cyrus master management port                      |

Users `user1` … `user5` exist in domain `example.com`, and Cyrus accepts any
password.

The startup script sets the fixture's `jmap_max_size_upload` to 51200 KiB
(50 MiB). The image's original unqualified value can overflow to an advertised
`maxSizeUpload=0`, causing the client to correctly reject nonempty uploads.
Cyrus interprets unqualified values as KiB; see its
[configuration reference](https://www.cyrusimap.org/imap/reference/manpages/configs/imapd.conf.html#jmap-max-size-upload).

## Running the tests

```sh
JMAP_ORACLE_URL=http://localhost:18080/.well-known/jmap \
JMAP_ORACLE_LMTP=localhost:18024 \
  dune build @test/oracle/sync/runtest --force
```

`--force` matters: without it dune caches the last successful run and the
tests never touch the server again. Replace `sync` with `smoke`, `email`,
`submission`, `push`, … to run one area, or use `@test/oracle/runtest` for
all of them.

### Environment variables

| variable                | default                                    | meaning |
| ----------------------- | ------------------------------------------ | ------- |
| `JMAP_ORACLE_URL`       | *unset* — tests skip                       | session or well-known URL |
| `JMAP_ORACLE_USER`      | `user1`                                    | login name |
| `JMAP_ORACLE_PASSWORD`  | `x`                                        | password (Cyrus accepts anything) |
| `JMAP_ORACLE_DOMAIN`    | `example.com`                              | mail domain of the users |
| `JMAP_ORACLE_LMTP`      | `localhost:18024`                          | `host:port` of the LMTP listener |
| `JMAP_ORACLE_HTTP_PORT` | `18080`                                    | read by `oracle-up.sh` only |
| `JMAP_ORACLE_IMAGE`     | `ghcr.io/cyrusimap/cyrus-docker-test-server:bookworm` | image to run |

## How the harness is structured

`oracle_harness.mli` is the whole interface; `test/oracle/dune` builds it as
the library `oracle_harness`. It has four parts.

**Connecting.** `Oracle_harness.test_case name f` is an Alcotest case that
skips when the oracle is not configured, and otherwise runs `f` inside
`Eio_main.run` with a fresh, authenticated client:

```ocaml
type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;  (* the switch the client was connected under *)
  client : Jmap_eio.Client.t;
  account_id : Jmap.Proto.Id.t;
  user : string;      (* "user1" *)
  address : string;   (* "user1@example.com" *)
}
```

The client is built by `Oracle_harness.connect_with ~sw env`, which is
`Jmap_eio.Client.connect_env ~sw ~auth:(Jmap_eio.Auth.basic ~user ~password)
~allow_insecure:true`: Cyrus answers 401 to a bearer token whatever it holds,
and `~allow_insecure` is needed because the oracle speaks plain HTTP on
localhost. `account_id` is the primary account for
`urn:ietf:params:jmap:mail`. `connect_with` takes an optional `?auth` and
`?timeout`, so a test can vary the credential (or connect as a second user,
as `submission/` does for the recipient) and inspect the failure rather than
aborting on it; `t.sw` is the switch to hand it, and the one a
`Jmap_eio.Push.subscribe` fiber belongs to.

**Requests.** `call t chain` builds and sends a typed chain with
`Oracle_harness.capabilities` (core, mail, submission), then decodes its one
result. `run_all t chain` does the same for several handles. `request t req`
sends a caller-built request and returns the raw response. These helpers fail
the test on transport errors; the chain helpers also report method and decode
errors.

**Test mail.** There is no SMTP submission path in the sandbox, so messages
arrive over LMTP:

- `unique prefix` — a string unique to this process and call, for subjects
  and mailbox names that must not collide between runs;
- `message ?from ?to_ ?subject ?body ?headers ()` — `(subject, raw)` for an
  RFC 5322 message with CRLF line endings and a unique `Message-ID`;
- `deliver t raw` — a minimal LMTP client (RFC 2033) that injects it, with
  dot stuffing;
- `wait_for_email t ~subject ()` — polls `Email/query` until the message is
  indexed (default timeout 45 s; Cyrus can take tens of seconds under load);
- `deliver_and_wait t ()` — the two together, returning `(email_id, subject)`.

**Session helpers.** ``mailbox_with_role t `Inbox`` fetches the Mailbox with the
`Inbox` role.

## Adding a suite

Each area gets its own executable so that a failure in one does not hide the
others, and so you can run just the area you are working on.

1. `mkdir test/oracle/<area>` and write `test/oracle/<area>/dune`:

   ```
   (test
    (name test_<area>)
    (package jmap)
    (libraries oracle_harness jmap jmap.eio alcotest))
   ```

2. Write `test/oracle/<area>/test_<area>.ml`, ending with

   ```ocaml
   let () =
     Oracle_harness.run "oracle-<area>"
       [ ("group", [ Oracle_harness.test_case "what it checks" f ]) ]
   ```

3. Run it with `dune build @test/oracle/<area>/runtest --force`, and check
   that `dune runtest` with the environment unset still passes (it should
   report the cases as skipped, not failed).

Guidelines that keep these tests useful:

- **Assert what the RFC says, not what Cyrus happens to do.** Where a value
  is `X|null`, assert that *both* shapes decode, and accept either.
- **Make the data unique.** Filter on a subject from `unique`/`message`
  rather than on "the newest message"; suites run concurrently against one
  account.
- **Never assume an empty account.** Other suites and the examples deliver
  mail to the same user.
- **Cite the section.** A comment naming the RFC section under test is what
  makes a failure actionable a year later.

## Current suites

| directory | what it covers |
| --------- | -------------- |
| `smoke/`      | session, capabilities, mailbox roles, delivery, `Email/get` |
| `sync/`       | `Email/changes`, `Mailbox/changes` + `updatedProperties`, `Email/queryChanges`, `SearchSnippet/get`, keyword comparators, `Thread/get` |
| `email/`      | `Email/import`, `Email/parse`, `Email/set` patches, `Mailbox/set` |
| `submission/` | `Identity/*` and `EmailSubmission/*` |
| `push/`       | EventSource push (RFC 8620 §7.3) |
| `release/`    | `Thread/changes`, `Mailbox/queryChanges`, submission query capabilities |

## Known oracle behaviour

Recorded here so a surprising assertion elsewhere makes sense:

- The session lives at `/jmap`; `/.well-known/jmap` returns a 301 to it. All
  session URLs are **relative** (`/jmap/`, `/jmap/upload/{accountId}/`, …),
  so they must be resolved against the session URL.
- Session-level capability objects are `{}`, and the account-level mail
  capability sends `"maxMailboxDepth": null`.
- `Mailbox/queryChanges` and `Email/queryChanges` work with a current query
  state in the image tested for the release review. Earlier images answered
  `cannotCalculateChanges` for mailboxes; callers must still handle it.
  `EmailSubmission/query` advertises `canCalculateChanges=false`, so the
  release suite checks that capability and skips its incremental query path.
- `SearchSnippet/get` sends `"notFound": null` and no `state`.
- `/set` responses carry explicit nulls for every unused map
  (`"created": null`, …).
- `VacationResponse/*` and `PushSubscription/*` are `unknownMethod`, and
  `urn:ietf:params:jmap:vacationresponse` in `using` is rejected at the
  request level with an `unknownCapability` problem+json (HTTP 400).
- Blob upload returns HTTP 201 with an extra `expires` member.
- Delivery latency is unpredictable: under load an LMTP `DATA` can take
  30 s to be acknowledged. Prefer `wait_for_email` over sleeping.
- **An open event source locks the user.** While a `closeafter=no` stream is
  held open for `user1`, everything else that account does waits for it: an
  LMTP `DATA` is not acknowledged, and a JMAP API POST — even `Core/echo` —
  gets no answer until the stream closes. Another user is unaffected, so the
  lock is per-account rather than server-wide. Measured against the image
  above: with a stream open, `Core/echo` as `user1` was still unanswered
  after 12 s and returned in 0.00 s as soon as the stream closed, while
  `Core/echo` as `user2` answered in 0.02 s throughout.

  This is why `push/` runs `Push.subscribe` in its polling mode. With
  `~poll:1.` each connection asks for `closeafter=state`, is held for at most
  a second and then dropped, and the next one starts a second later quoting
  `Last-Event-ID`, so the changes made in the gap are replayed rather than
  lost. `~last_event_id:"1"` is what makes the first connection answer at
  once: a request quoting a `Last-Event-ID` is answered with the state that
  followed it and the connection is closed immediately (measured: 406 bytes
  in 0.01 s), while a request with no id is held open until something
  changes — and Cyrus only looks every `jmap_pushpoll` seconds, 60 by
  default. Nothing in `push/` delivers mail while a connection is open.

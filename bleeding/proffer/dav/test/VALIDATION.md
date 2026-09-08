# DAV implementation validation

Validated locally on 2026-09-08 with disposable test identities and stores.
No production DAV server or credential file was accessed for these tests.

| Tree | Compiler | Unit and regression tests | Docker interoperability |
| --- | --- | --- | --- |
| OxMono | `5.2.0+ox`, `release-check` | Full HTTPz, Fetch and Proffer suites passed | Passed |
| oxcaml-httpz | `5.2.0+ox`, `release-check` | Full repository suite passed | Passed |
| ocaml-httpz | `5.5.0` | Full repository suite passed | Passed |

Tests were forced. Installation targets built in all three trees. The DAV
parity checker passed for shared sources, tests, fixtures and documentation.
Backend changes were ported separately for each compiler and exercised by
these suites. Unrelated existing standalone work was preserved.

Each Docker run passed 10 disabled-default checks, 2 forged-transport checks,
133 independent Python checks, 7 Fetch-to-Proffer checks, 8 persistence checks
and 7 verified TLS checks. The runner also verified exclusive writer ownership,
orphan cleanup, unsafe filesystem entry refusal and corrupt-store refusal.
The Python container uses both `webdavclient3` and raw HTTP/XML assertions.
Its image and Python dependencies are pinned. Token-redaction failures stop
the fixture process, so the runner cannot silently accept a failing assertion.

The scoped DAV suites include 40 server policy checks, 25 server codec checks,
77 protocol checks, 101 extension checks, 658 namespace checks, 100 Fetch
client checks and 30 mirror checks, plus the private XML codec regressions.
The session checks include immediate cleanup of rejected downloads, body-read
exceptions and cancellation, caller ownership of 200/206/304 responses, and
redaction of DAV tokens in request diagnostics.
The mirror regressions include malicious index paths, foreign deletions,
filename collisions, interrupted downloads, incomplete rebuilds and stalled
sync tokens. Unexpected sync response statuses cannot become an empty listing.

The ordinary Proffer regression suite passed after adding request admission
and streaming input. OxCaml's release build checked existing zero-allocation
and portability annotations. The new DAV operations themselves allocate.

The repository's complete OCaml formatting alias could not pass with the
available stock formatter, which does not parse OxCaml modes. Existing Dune
formatting differences also remain outside this change. New Dune stanzas
were formatted and whitespace checks passed. The arods before/after render
capture could not run because `~/.config/arod/config.toml` is absent.

This is interoperability evidence, not complete RFC conformance or crash
certification. The [remaining work](../README.md#remaining-work) includes
filesystem fault injection, power-loss testing, platform and client coverage,
and unsupported server extensions. See [the runner](run_docker.py) for a
repeatable local test, and [the review ledger](../../DAV_REVIEW.md) for scope.

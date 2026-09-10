# Changes

## Unreleased

- Recover refs despite unrelated catalog failures, tolerate notes and avoid
  duplicate annotated-tag jobs. Stream and durably journal partial log output.

- Bound retained history and event queues, report observer readiness, and
  reconcile current Git/PDS state when upstream replay is unavailable.

- Reconcile catalog changes from current PDS state, reject stale snapshots and
  persist retry delays. Bound history reads and clean up job descendants.

- Discover Tangled repositories and run push, pull and multi-workflow CI.
  Persist event recovery and JWT replay protection, with a full Docker testbed.

- Resolve PLC keys over verified HTTPS and test the local TLS gateway.
  Add persistent-service deployment instructions and a manual trigger client.

- Use JSONWT for ES256K verification and reject malformed JWT claims, encodings
  and key headers, with signed regressions and local PDS integration tests.

- Run OCaml inspection jobs from authenticated Tangled manual triggers, with
  commit checkout, request metadata, persistent status and WebSocket logs.

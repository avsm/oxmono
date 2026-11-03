# ocluster-client

An Eio client for [OCluster](https://github.com/ocurrent/ocluster)
schedulers. It connects with a submission capability file, queues OBuilder
jobs on a pool, streams their logs and reads their results.

```ocaml
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let submission = Ocluster_client.connect ~sw env#net "submission.cap" in
let ticket =
  Ocluster_client.submit submission ~pool:"linux-x86_64" ~cache_hint:"demo"
    ~src:("https://example.org/repo.git", [ commit ])
    spec
in
let job = Ocluster_client.job ticket in
Ocluster_client.stream_log job print_string;
Ocluster_client.result job
```

`ocluster-submit --cap FILE --pool POOL SPEC` runs one job from the command
line. `OCLUSTER_LOG=info` shows the connection.

`Job.log` waits until a worker takes the job, which can take a while on a busy
pool. A job whose ticket is released is cancelled by the scheduler.

## Schema

`lib/schema.capnp` is `api/schema.capnp` from OCluster at
`8905eb66b8c2491ca625c752a5b993a379460008`, unchanged. `lib/schema.ml` and
`lib/schema.mli` are generated from it and checked in, as described in
[capnp's notes](../../vendor/capnp/OXMONO.md). Regenerate them when the
schema changes.

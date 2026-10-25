# Eioext

Filesystem and subprocess extensions being exercised locally before proposing
them upstream to Eio. The library targets native Unix Eio backends.

- `Eioext.Path` provides bounded private-file reads, durable atomic replacement,
  exclusive creation and file locking through Eio directory capabilities.
- `Eioext.Process` provides bounded capture, streaming stdout and scoped process
  groups. It resolves commands against the child environment's `PATH`.
- `Eioext.Native` supports existing synchronous Unix stores. These operations
  may block. Eio applications should use `Path` or offload native calls to a
  worker thread. This compatibility module is not an upstream API proposal.

`Path` never converts directory capabilities to strings for filesystem access.
Reads reject symlinks and special files and check the opened descriptor. Private
files must be owned by the current user with no group or other permissions.
Atomic replacement requires a private parent directory and creates mode-0600
files. It fsyncs the file and parent directory. Cancellation before publication
leaves the original file intact. Rename and directory sync run under cancellation
protection. Exclusive creation protects its write but is not crash-atomic.

Locks coordinate fibers, domains and processes. The native and Eio entry points
share the same process-local registry. Unused registry entries are removed.
Lock files must not be linked, renamed or unlinked while in use. Their parent
directories must be trusted. Contention raises `Busy`, unless a `wait` callback
is supplied. Eio callbacks should yield or sleep so waiting remains cancellable.

Each process operation owns a process group and kills remaining descendants
when its leader exits, a consumer fails or the caller cancels. Commands that
launch persistent daemons must use another API. Captures default to 16 MiB of
stdout and 64 KiB of stderr. Invalid limits are rejected before spawning. A
stream consumer must consume stdout to EOF or raise. Cleanup preserves the
original failure. Descendants that leave the group cannot be cleaned up by it.

The process implementation periodically wakes Eio's child-status condition while
a call is active, to tolerate other libraries replacing the SIGCHLD handler. It
does not replace signal handlers or reap unrelated children. This workaround is
kept behind the process interface for review with the Eio backends.

The workspace's vendored Eio supplies no-follow opens and the process environment
API used here. Upstreaming requires reconciling those additions too. Tests run
without network access or model downloads:

```
dune build --force @bleeding/eioext/all @bleeding/eioext/runtest
```

Hugging Face invocation and retry policy live in the separate `hf` adapter.
Secret encryption and reference resolution live in `sekrit`.

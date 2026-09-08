# WebDAV interoperability fixture

Run from the repository root, with Docker Engine, Compose v2 and Python 3:

```sh
python3 bleeding/fetch/test/webdav/run.py
```

The runner builds an Apache 2.4.68 image pinned by digest, starts a unique
Compose project, and checks the server independently using Python's HTTP and
XML libraries. It then runs an optional client command before collecting logs
and removing the project's containers, network and image tag. Each run uses
new tmpfs data and loopback-only dynamic ports, so concurrent runs are isolated.
The first build needs registry access; subsequent builds can use Docker's cache.

Pass a command after `--` to test a client against the verified server:

```sh
python3 bleeding/fetch/test/webdav/run.py -- \
  opam exec --switch=5.2.0+ox -- dune exec \
  bleeding/proffer/dav/test/test_docker.exe
```

The command inherits these fixture settings:

| Variable | Meaning |
| --- | --- |
| `WEBDAV_URL` | HTTPS collection URL, including trailing slash |
| `WEBDAV_HTTP_URL` | Plain HTTP equivalent, for local transport tests |
| `WEBDAV_CA_FILE` | Public test CA to trust explicitly |
| `WEBDAV_USER`, `WEBDAV_PASSWORD` | Alice's fixture credentials |
| `WEBDAV_OTHER_USER`, `WEBDAV_OTHER_PASSWORD` | Bob's fixture credentials |

Both users are authorized; Bob tests that authentication alone does not confer
Alice's lock token. These are fixed test passwords. The image generates a CA
and a separate localhost certificate; HTTPS is checked both with and without
the CA. No host trust store is modified. The server runs as `www-data`, with a
read-only root filesystem and disposable writable data and lock directories.

Artifacts are printed at startup and retained under `_build/webdav-docker/`:
build/startup output, response bodies, a JSON exchange log, the test CA, exported
environment, server logs and cleanup output. `--artifacts DIR` chooses a new
directory. Request authorization values are not logged. A failing server probe
prevents the client command from running. A failing command or interrupted run
still triggers cleanup; inspect `cleanup.log` if Docker becomes unavailable.

The baseline covers authentication, advertised classes/methods, MKCOL failures,
PUT/GET/HEAD, ranges, ETags and conditional writes, explicit-property PROPFIND,
depth zero/one, mixed 200/404 propstat groups, dead properties, atomic PROPPATCH
rollback, encoded names, COPY/MOVE overwrite policy, DELETE, exclusive locks,
token enforcement for both users, refresh, unlock and chunked uploads. The
same checks run over HTTP and verified HTTPS. This establishes an
interoperability target, not RFC conformance of either the server or client.

Known behavior of this pinned Apache version matters to clients:

- `DAV` capabilities span multiple field lines.
- Newly modified files may temporarily have weak ETags. Weak tags cannot
  satisfy If-Match; the probe waits for a strong tag without changing its text.
- Returned href escapes use lowercase hex; literal path case remains significant.
- Changing protected `getetag` returns property status 409 and dependent 424;
  RFC 4918 recommends 403 with `cannot-modify-protected-property` instead.
- Refused infinite-depth PROPFIND returns an HTML 403 diagnostic.
- Ordered PROPPATCH instructions can produce repeated property names in a
  successful response. Query again to check the final stored value.
- Creating a child of a depth-zero locked collection without its token returns
  a 207 with the parent's 423 status. Tagging the If condition with the parent
  collection URL permits creation.

Keep three testing layers. Ordinary `dune runtest` covers protocol fixtures and
Fetch mocks, including malformed XML, resource limits, partial 207 failures,
redirect policy and uncertain mutation outcomes. Run this Docker suite explicitly
in an integration CI job, save its artifacts on failure and impose a job timeout.
Add a separately pinned Nextcloud fixture when implementing collection sync or
Nextcloud extensions: Apache's filesystem provider does not establish RFC 6578
support. A controlled local faulty HTTP peer should cover truncated bodies and
lost replies, which an ordinary DAV server cannot reliably produce on demand.

References: [Apache mod_dav](https://httpd.apache.org/docs/2.4/mod/mod_dav.html),
[mod_dav_fs](https://httpd.apache.org/docs/2.4/mod/mod_dav_fs.html),
[official image sources](https://github.com/docker-library/httpd),
[RFC 4918](https://www.rfc-editor.org/rfc/rfc4918.html) and the
[client investigation](../../WEBDAV.md).

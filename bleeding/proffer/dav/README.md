# proffer.dav

`Proffer_dav` re-exports the [`Fetch_dav`](../../fetch/dav/README.md) WebDAV client
for Proffer applications. Link `proffer.dav`. Client values, conditions, leases
and exceptions are shared with `fetch.dav`; there is one implementation.
The protocol types belong to [`Httpz_dav`](../../httpz/dav/README.md), provided
by `httpz.dav`. This integration provides client operations.

See the [client guide](../../fetch/dav/README.md) for API examples, credential
configuration, response ownership and local/Docker/live testing commands.
The explicit Docker and live executables live under `test/` so the Proffer test
closure can select `fetch-httpz` without creating a Fetch package dependency
cycle. The ordinary alias checks that this facade preserves client type and
exception identity. [Recorded live observations](test/INTEROP.md) describe the
Fastmail compatibility results.

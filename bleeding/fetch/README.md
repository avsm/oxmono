# Fetch

HTTP clients, middleware and backends for Eio and OxCaml. See
[HTTPZ_RELEASE.md](../../HTTPZ_RELEASE.md) for the shared stack contract.

## Selective POST retries

Some applications use `POST` for a read-only lookup. If the application has
established that a particular endpoint is safe to repeat, `retry_request` can
approve that route while vetoing retries for other POSTs. Adding `POST` to
`allowed_methods` alone is unsafe: it makes every replayable POST eligible
for status and connection-failure retries, including requests whose reply
was lost after the server acted on them.

For example, suppose the application's `/api/catalog/search` endpoint is a
read-only search accepting a replayable `Fetch.String` JSON body:

```ocaml
let retry_request (req : Fetch.Middleware.request) =
  match req.meth with
  | `POST ->
      (match List.rev (Fetch.Middleware.Url.path_segments req.url) with
       | "search" :: "catalog" :: "api" :: _ -> true
       | _ -> false)
  | _ -> true

let retry =
  Fetch.Retry.v
    ~allowed_methods:(`POST :: Fetch.Retry.default.allowed_methods)
    ~retry_request
    ()
```

Pass this policy to `Fetch_httpz.std ~retry env`,
`Fetch_curl.std ~sw ~retry env`, or `Fetch.with_retry ~config:retry`.
Route classification belongs to the application. This example compares a
suffix of decoded, dot-resolved path segments, so it also permits a deployment
prefix such as `/tenant/api/catalog/search`, but excludes nearby routes such
as `/api/catalog/search-all` or `/api/catalog/update`.

The predicate narrows every retry reason, including built-in statuses,
connection failures, and the custom `retry_response` and `retry_exception`
hooks. Returning `false` still permits the first exchange and skips both
custom hooks. Returning `true` still requires an allowed method, a replayable
body, and a remaining retry budget; streaming bodies are never retried.
Omitting the predicate preserves the default behaviour.

It runs once per request at the retry middleware boundary, before the first
attempt; each rewritten redirect hop gets its own evaluation. It is skipped
when `max_retries` is zero, the body is streamed, or the method is disallowed.
Keep it pure and fast, and avoid diagnostics that expose request credentials.
If it raises, the exception propagates before the inner client is called.

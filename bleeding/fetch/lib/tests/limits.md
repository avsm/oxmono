# Rate limiting and retry

```ocaml
# #require "fetch";;
# #require "fetch.mock";;
# #require "eio.mock";;
# #require "mtime";;
```

`Eio_mock.Backend.run_full` provides a mock monotonic clock that starts at
0 and advances automatically when all fibers are sleeping — time below is
virtual and deterministic.

```ocaml
open Fetch

let run fn = Eio_mock.Backend.run_full @@ fun env -> fn env#mono_clock

let now_s clock =
  Int64.to_float (Mtime.to_uint64_ns (Eio.Time.Mono.now clock)) /. 1e9

(* with_retry draws its jitter from a flow — env#secure_random in an
   application; any deterministic source in a test. The tests below
   either disable jitter or answer with Retry-After, so this one is
   never actually read. *)
let random = Eio.Flow.string_source (String.make 64 '\000')
```

## Pacing: with_limits

Sequential requests to one origin are spaced `min_interval` apart:

```ocaml
# run @@ fun clock ->
  let t = Fetch_mock.client (Fetch_mock.respond "ok")
    |> Fetch.with_limits ~clock ~min_interval:5. in
  List.iter (fun i ->
      ignore (Fetch.read t "https://api.example/x");
      Fmt.pr "request %d done at t=%.0f@." i (now_s clock))
    [ 1; 2; 3 ];;
request 1 done at t=0
+mock time is now 5
request 2 done at t=5
+mock time is now 10
request 3 done at t=10
- : unit = ()
```

Concurrent fibers reserve successive slots — they queue at the configured
rate instead of stampeding:

```ocaml
# run @@ fun clock ->
  let t = Fetch_mock.client (Fetch_mock.respond "ok")
    |> Fetch.with_limits ~clock ~min_interval:2. in
  Eio.Fiber.all
    (List.map (fun i () ->
         ignore (Fetch.read t "https://api.example/x");
         Fmt.pr "fiber %d done at t=%.0f@." i (now_s clock))
       [ 1; 2; 3 ]);;
fiber 1 done at t=0
+mock time is now 2
fiber 2 done at t=2
+mock time is now 4
fiber 3 done at t=4
- : unit = ()
```

`max_concurrent` caps in-flight requests per origin — here the mock server
takes 3s per request, so with a cap of 2 the third request waits:

```ocaml
# run @@ fun clock ->
  let server req = Eio.Time.Mono.sleep clock 3.; Fetch_mock.respond "done" req in
  let t = Fetch_mock.client server
    |> Fetch.with_limits ~clock ~max_concurrent:2 in
  Eio.Fiber.all
    (List.map (fun i () ->
         ignore (Fetch.read t "https://api.example/x");
         Fmt.pr "fiber %d done at t=%.0f@." i (now_s clock))
       [ 1; 2; 3 ]);;
+mock time is now 3
fiber 1 done at t=3
fiber 2 done at t=3
+mock time is now 6
fiber 3 done at t=6
- : unit = ()
```

## Retry: with_retry respects server backoff requests

A 429 with `Retry-After` is retried after exactly the requested delay:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    Fmt.pr "> attempt %d at t=%.0f@." !attempts (now_s clock);
    if !attempts < 3 then
      Fetch_mock.respond ~status:429
        ~headers:(Http.Header.of_list [ "Retry-After", "7" ]) "slow down" req
    else Fetch_mock.respond "ok" req
  in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random in
  Fetch.read t "https://api.example/data";;
> attempt 1 at t=0
+mock time is now 7
> attempt 2 at t=7
+mock time is now 14
> attempt 3 at t=14
- : string = "ok"
```

A server-requested delay is capped at `backoff_max`, so a hostile server
cannot park the client:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    Fmt.pr "> attempt %d at t=%.0f@." !attempts (now_s clock);
    if !attempts = 1 then
      Fetch_mock.respond ~status:429
        ~headers:(Http.Header.of_list [ "Retry-After", "100000" ]) "" req
    else Fetch_mock.respond "ok" req
  in
  let t = Fetch_mock.client server
    |> Fetch.with_retry ~clock ~random ~config:(Retry.v ~backoff_max:60. ()) in
  Fetch.read t "https://api.example/data";;
> attempt 1 at t=0
+mock time is now 60
> attempt 2 at t=60
- : string = "ok"
```

Without `Retry-After`, exponential backoff applies (jitter disabled here
for a deterministic transcript):

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    Fmt.pr "> attempt %d at t=%.1f@." !attempts (now_s clock);
    Fetch_mock.respond ~status:503 "unavailable" req
  in
  let t = Fetch_mock.client server
    |> Fetch.with_retry ~clock ~random ~config:(Retry.v ~jitter:false ~max_retries:2 ()) in
  Fetch.read t "https://api.example/data";;
> attempt 1 at t=0.0
+mock time is now 0.5
> attempt 2 at t=0.5
+mock time is now 1.5
> attempt 3 at t=1.5
- : string = "unavailable"
```

Jitter multiplies each backoff by a uniform draw from `random`, 8
little-endian bytes per delay — so a crafted flow pins it. Both draws
below decode to exactly 0.5 (2⁶¹ of 2⁶²), halving the 0.5s and 1s
backoffs:

```ocaml
# run @@ fun clock ->
  let random = Eio.Flow.string_source
      "\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x20" in
  let attempts = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    Fmt.pr "> attempt %d at t=%.2f@." !attempts (now_s clock);
    Fetch_mock.respond ~status:503 "unavailable" req
  in
  let t = Fetch_mock.client server
    |> Fetch.with_retry ~clock ~random ~config:(Retry.v ~max_retries:2 ()) in
  Fetch.read t "https://api.example/data";;
> attempt 1 at t=0.00
+mock time is now 0.25
> attempt 2 at t=0.25
+mock time is now 0.75
> attempt 3 at t=0.75
- : string = "unavailable"
```

Non-idempotent methods are not retried:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    Fetch_mock.respond ~status:503 "no" req
  in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random in
  Eio.Switch.run @@ fun sw ->
  let resp = Fetch.post ~sw t ~body:(String "x") "https://api.example/submit" in
  (status resp, !attempts);;
- : int * int = (503, 1)
```

A connection failure is retried by default, and the last one propagates:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 in
  let flaky (_ : Middleware.request) : response =
    incr attempts;
    raise (Fetch.err (Connection_failure Timeout))
  in
  let t = Fetch_mock.client flaky
    |> Fetch.with_retry ~clock ~random ~config:(Retry.v ~jitter:false ~max_retries:2 ()) in
  (try ignore (Fetch.read t "https://api.example/data" : string) with
   | Eio.Io (E (Connection_failure _), _) -> ());
  !attempts;;
+mock time is now 0.5
+mock time is now 1.5
- : int = 3
```

`retry_exception` sees exceptions that are not `Eio.Io` at all — a
middleware's own failure, say — while cancellation and the errors a
re-issue cannot fix are never retried:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 in
  let failing (_ : Middleware.request) : response = incr attempts; failwith "boom" in
  let config =
    Retry.v ~jitter:false ~max_retries:2
      ~retry_exception:(function Failure _ -> true | _ -> false) () in
  let t = Fetch_mock.client failing |> Fetch.with_retry ~clock ~random ~config in
  (try ignore (Fetch.read t "https://api.example/data" : string) with
   | Failure _ -> ());
  !attempts;;
+mock time is now 0.5
+mock time is now 1.5
- : int = 3
```

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 in
  let denier (_ : Middleware.request) : response =
    incr attempts;
    raise (Fetch.err (Denied "nope"))
  in
  let config =
    Retry.v ~jitter:false ~retry_exception:(fun _ -> true) () in
  let t = Fetch_mock.client denier |> Fetch.with_retry ~clock ~random ~config in
  (try ignore (Fetch.read t "https://api.example/data" : string) with
   | Eio.Io (E (Denied _), _) -> ());
  !attempts;;
- : int = 1
```

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
let () = Printexc.record_backtrace false
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

`Retry-After` may name an instant rather than a delay. That form needs a
wall clock to subtract from, so it is honoured when one is passed and
ignored otherwise. `Eio_mock`'s wall clock tracks its monotonic one and
both start at the epoch, so a date two seconds ahead is
`Thu, 01 Jan 1970 00:00:02 GMT`:

```ocaml
# let dated wall =
    Eio_mock.Backend.run_full @@ fun env ->
    let clock = env#mono_clock in
    let attempts = ref 0 in
    let server (req : Middleware.request) =
      incr attempts;
      Fmt.pr "> attempt %d at t=%.1f@." !attempts (now_s clock);
      if !attempts = 1 then
        Fetch_mock.respond ~status:503
          ~headers:(Http.Header.of_list
                      [ "Retry-After", "Thu, 01 Jan 1970 00:00:02 GMT" ]) "" req
      else Fetch_mock.respond "ok" req
    in
    let wall = if wall then Some env#clock else None in
    let t = Fetch_mock.client server
      |> Fetch.with_retry ~clock ~random ?wall
           ~config:(Retry.v ~jitter:false ()) in
    Fetch.read t "https://api.example/data";;
val dated : bool -> string = <fun>
# dated true;;
> attempt 1 at t=0.0
+mock time is now 2
> attempt 2 at t=2.0
- : string = "ok"
```

Without a clock the field is unusable, so the configured backoff applies
instead — here the first 0.5s step:

```ocaml
# dated false;;
> attempt 1 at t=0.0
+mock time is now 0.5
> attempt 2 at t=0.5
- : string = "ok"
```

A date already past asks for no wait at all, and one far in the future is
capped by `backoff_max` like any other server-requested delay:

```ocaml
# Eio_mock.Backend.run_full @@ fun env ->
  let clock = env#mono_clock in
  let attempts = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    Fmt.pr "> attempt %d at t=%.0f@." !attempts (now_s clock);
    match !attempts with
    | 1 ->
      Fetch_mock.respond ~status:503
        ~headers:(Http.Header.of_list
                    [ "Retry-After", "Thu, 01 Jan 1970 00:00:00 GMT" ]) "" req
    | 2 ->
      Fetch_mock.respond ~status:503
        ~headers:(Http.Header.of_list
                    [ "Retry-After", "Fri, 01 Jan 2100 00:00:00 GMT" ]) "" req
    | _ -> Fetch_mock.respond "ok" req
  in
  let t = Fetch_mock.client server
    |> Fetch.with_retry ~clock ~random ~wall:env#clock
         ~config:(Retry.v ~jitter:false ~backoff_max:60. ()) in
  Fetch.read t "https://api.example/data";;
> attempt 1 at t=0
> attempt 2 at t=0
+mock time is now 60
> attempt 3 at t=60
- : string = "ok"
```

A discarded response is closed without reading its body, so a response whose
body would raise on read cannot interfere with the retry:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 in
  let truncated =
    let module S = struct
      type t = unit
      let read_methods = []
      let single_read () buf =
        Cstruct.blit_from_string "part" 0 buf 0 4;
        raise (Fetch.err (Protocol_error "connection reset by peer"))
    end in
    Eio.Resource.T ((), Eio.Flow.Pi.source (module S))
  in
  let server (req : Middleware.request) =
    incr attempts;
    if !attempts = 1 then
      Middleware.Pi.response ~close:(fun () -> ()) ~status:503 ~headers:(Http.Header.init ())
        ~version:`HTTP_1_1 ~body:truncated ~url:req.url ()
    else Fetch_mock.respond "ok" req
  in
  let t = Fetch_mock.client server
    |> Fetch.with_retry ~clock ~random ~config:(Retry.v ~jitter:false ()) in
  let body = Fetch.read t "https://api.example/data" in
  (body, !attempts);;
+mock time is now 0.5
- : string * int = ("ok", 2)
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

## Request-level retry policy

Omitting the request predicate preserves the GET status and connection-failure
retries above:

```ocaml
# Option.is_none Retry.default.retry_request, Option.is_none (Retry.v ()).retry_request;;
- : bool * bool = (true, true)
```

The request predicate narrows a method allowlist for both response and
connection retries. A false predicate still permits the first exchange:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 in
  let server (req : Middleware.request) =
    incr attempts; Fetch_mock.respond ~status:503 "busy" req
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0.
    ~allowed_methods:[`POST]
    ~retry_request:(fun _ -> incr policy_calls; false) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  Eio.Switch.run @@ fun sw ->
    let r = Fetch.post ~sw t ~body:(String "x") "https://api.example/submit" in
    let result = (status r, !attempts, !policy_calls) in
    close r; result;;
- : int * int * int = (503, 1, 1)
```

The same veto applies to the built-in connection-failure classifier:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 in
  let server (_ : Middleware.request) : response =
    incr attempts; raise (Fetch.err (Connection_failure Timeout))
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0.
    ~allowed_methods:[`POST]
    ~retry_request:(fun _ -> incr policy_calls; false) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  Eio.Switch.run @@ fun sw ->
    (try ignore (Fetch.post ~sw t ~body:(String "x")
      "https://api.example/submit") with
     | Eio.Io (E (Connection_failure _), _) -> ());
    (!attempts, !policy_calls);;
- : int * int = (1, 1)
```

An approved replayable POST retries either failure kind:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    if !attempts = 1 then Fetch_mock.respond ~status:503 "busy" req
    else Fetch_mock.respond "ok" req
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0.
    ~allowed_methods:[`POST]
    ~retry_request:(fun _ -> incr policy_calls; true) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  Eio.Switch.run @@ fun sw ->
    let r = Fetch.post ~sw t ~body:(String "x") "https://api.example/submit" in
    let result = (status r, !attempts, !policy_calls) in
    close r; result;;
- : int * int * int = (200, 2, 1)
```

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    if !attempts = 1 then raise (Fetch.err (Connection_failure Timeout))
    else Fetch_mock.respond "ok" req
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0.
    ~allowed_methods:[`POST]
    ~retry_request:(fun _ -> incr policy_calls; true) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  Eio.Switch.run @@ fun sw ->
    let r = Fetch.post ~sw t ~body:(String "x") "https://api.example/submit" in
    let result = (status r, !attempts, !policy_calls) in
    close r; result;;
- : int * int * int = (200, 2, 1)
```

The body and method checks happen before the request predicate. A stream and a
disallowed PUT therefore make one exchange and never call it; zero retry
budget does the same for an otherwise eligible GET:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 in
  let server (req : Middleware.request) =
    incr attempts; Fetch_mock.respond ~status:503 "busy" req
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0.
    ~allowed_methods:[`POST]
    ~retry_request:(fun _ -> incr policy_calls; true) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  Eio.Switch.run @@ fun sw ->
    let body = Stream { length = Some 1L;
      flow = (Eio.Flow.string_source "x" :> Eio.Flow.source_ty Eio.Resource.t) } in
    let stream_r = Fetch.fetch ~sw ~body t `POST "https://api.example/stream" in
    let stream_result = (status stream_r, !attempts, !policy_calls) in
    close stream_r;
    let put_r = Fetch.fetch ~sw ~body:(String "x") t `PUT
      "https://api.example/put" in
    let put_result = (status put_r, !attempts, !policy_calls) in
    close put_r; (stream_result, put_result);;
- : (int * int * int) * (int * int * int) = ((503, 1, 0), (503, 2, 0))
```

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 in
  let server (req : Middleware.request) =
    incr attempts; Fetch_mock.respond ~status:503 "busy" req
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0. ~max_retries:0
    ~retry_request:(fun _ -> incr policy_calls; true) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  let r = Fetch.read t "https://api.example/get" in
  (r, !attempts, !policy_calls);;
- : string * int * int = ("busy", 1, 0)
```

The existing response and exception hooks remain additive inside the outer
gate. With a false gate, neither hook sees a non-built-in retry candidate:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 and response_hook = ref 0 in
  let server (req : Middleware.request) =
    incr attempts; Fetch_mock.respond ~status:418 "teapot" req
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0.
    ~retry_request:(fun _ -> incr policy_calls; false)
    ~retry_response:(fun _ _ -> incr response_hook; true) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  let body = Fetch.read t "https://api.example/status" in
  (body, !attempts, !policy_calls, !response_hook);;
- : string * int * int * int = ("teapot", 1, 1, 0)
```

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 and exception_hook = ref 0 in
  let server (_ : Middleware.request) : response =
    incr attempts; failwith "middleware failure"
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0.
    ~retry_request:(fun _ -> incr policy_calls; false)
    ~retry_exception:(fun _ -> incr exception_hook; true) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  (try ignore (Fetch.read t "https://api.example/exception") with
   | Failure msg when String.equal msg "middleware failure" -> ());
  (!attempts, !policy_calls, !exception_hook);;
- : int * int * int = (1, 1, 0)
```

An approving gate still permits both custom classifiers, and the gate is
evaluated once for all attempts:

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 and response_hook = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    if !attempts < 3 then Fetch_mock.respond ~status:418 "teapot" req
    else Fetch_mock.respond "ok" req
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0. ~max_retries:2
    ~retry_request:(fun _ -> incr policy_calls; true)
    ~retry_response:(fun _ _ -> incr response_hook; true) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  let body = Fetch.read t "https://api.example/status" in
  (body, !attempts, !policy_calls, !response_hook);;
- : string * int * int * int = ("ok", 3, 1, 2)
```

```ocaml
# run @@ fun clock ->
  let attempts = ref 0 and policy_calls = ref 0 and exception_hook = ref 0 in
  let server (req : Middleware.request) =
    incr attempts;
    if !attempts = 1 then failwith "middleware failure"
    else Fetch_mock.respond "ok" req
  in
  let config = Retry.v ~jitter:false ~backoff_factor:0.
    ~retry_request:(fun _ -> incr policy_calls; true)
    ~retry_exception:(fun _ -> incr exception_hook; true) () in
  let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
  let body = Fetch.read t "https://api.example/exception" in
  (body, !attempts, !policy_calls, !exception_hook);;
- : string * int * int * int = ("ok", 2, 1, 1)
```

A request predicate exception escapes before the backend is entered, even
when it is itself a normally retryable connection failure:

```ocaml
# run @@ fun clock ->
  let probe failure =
    let backend_calls = ref 0 and policy_calls = ref 0 and hook_calls = ref 0 in
    let server req = incr backend_calls; Fetch_mock.respond "unexpected" req in
    let config = Retry.v ~backoff_factor:0.
      ~retry_request:(fun _ -> incr policy_calls; raise failure)
      ~retry_exception:(fun _ -> incr hook_calls; true) () in
    let t = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
    let escaped =
      try ignore (Fetch.read t "https://api.example/policy"); false with
      | Failure msg when String.equal msg "policy failure" -> true
      | Eio.Io (E (Connection_failure _), _) -> true
    in
    (escaped, !policy_calls, !backend_calls, !hook_calls)
  in
  List.map probe [Failure "policy failure"; Fetch.err (Connection_failure Timeout)];;
- : (bool * int * int * int) list = [(true, 1, 0, 0); (true, 1, 0, 0)]
```

Application route policies can match canonical decoded segments, including a
deployment prefix, without accepting a near miss or a decoded separator. This
example application approves only POSTs ending in `/api/keys/query`:

```ocaml
# let ends_with suffix xs =
    let rec drop n = function
      | xs when n = 0 -> xs
      | _ :: xs -> drop (n - 1) xs
      | [] -> []
    in
    let lx = List.length xs and ls = List.length suffix in
    lx >= ls && List.equal String.equal (drop (lx - ls) xs) suffix
  let route_allows (req : Middleware.request) =
    req.meth = `POST && ends_with ["api"; "keys"; "query"]
      (Middleware.Url.path_segments req.url);;
val ends_with : string list -> string list -> bool = <fun>
val route_allows : Middleware.request -> bool = <fun>
# Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let probe path =
      let attempts = ref 0 and policy_calls = ref 0 in
      let server (req : Middleware.request) =
        incr attempts;
        if !attempts = 1 then Fetch_mock.respond ~status:503 "busy" req
        else Fetch_mock.respond "ok" req
      in
      let config = Retry.v ~jitter:false ~backoff_factor:0.
        ~allowed_methods:[`POST]
        ~retry_request:(fun req ->
          incr policy_calls; route_allows req) () in
      let client = Fetch_mock.client server
        |> Fetch.with_retry ~clock:env#mono_clock
             ~random:(Eio.Flow.string_source "") ~config in
      let r = Fetch.fetch ~sw ~body:(String "x") client `POST path in
      let result = (status r, !attempts, !policy_calls) in
      close r; result
    in
    List.map probe
      [ "https://api.example/api/keys/query";
        "https://api.example/deploy/api/keys/query";
        "https://api.example/deploy/api/keys/query-all";
        "https://api.example/deploy/api/keys/query/extra";
        "https://api.example/deploy/api/keys/claim";
        "https://api.example/deploy/x/../api/keys/query";
        "https://api.example/deploy/api/keys/%71uery";
        "https://api.example/deploy/api/keys%2Fquery" ];;
- : (int * int * int) list =
[(200, 2, 1); (200, 2, 1); (503, 1, 1); (503, 1, 1); (503, 1, 1);
 (200, 2, 1); (200, 2, 1); (503, 1, 1)]
```

Each canonical redirect hop gets one request-policy evaluation. A 303
rewrites a replayable POST body to an empty GET before the next hop. Both
hops retry a 503, but the predicate sees each canonical request only once:

```ocaml
# Eio_mock.Backend.run_full @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let attempts = ref 0 and seen = ref [] in
    let server (req : Middleware.request) =
      incr attempts;
      match !attempts, Middleware.Url.path_segments req.url with
      | (1 | 3), _ -> Fetch_mock.respond ~status:503 "busy" req
      | 2, ["start"] -> Fetch_mock.respond ~status:303
          ~headers:(Http.Header.of_list ["Location", "/unused/../%66inish"])
          "redirect" req
      | 4, ["finish"] -> Fetch_mock.respond "done" req
      | _ -> Fetch_mock.respond ~status:404 "missing" req
    in
    let config = Retry.v ~jitter:false ~backoff_factor:0. ~max_retries:1
      ~allowed_methods:[`GET; `POST]
      ~retry_request:(fun req ->
        seen := (req.meth, Middleware.Url.path_segments req.url, req.body) :: !seen;
        true) () in
    let client = Fetch_mock.client server
      |> Fetch.with_retry ~clock:env#mono_clock
           ~random:(Eio.Flow.string_source "") ~config in
    let r = Fetch.fetch ~sw client ~body:(String "payload") `POST
      "https://api.example/unused/../start" in
    let result = (status r, !attempts,
      match List.rev !seen with
      | [(`POST, ["start"], String "payload"); (`GET, ["finish"], Empty)] -> true
      | _ -> false) in
    close r; result;;
- : int * int * bool = (200, 4, true)
```

A 307 preserves the method and body, but the redirected route gets its own
decision. A vetoed destination is attempted once even after an approved hop:

```ocaml
# run @@ fun clock ->
  Eio.Switch.run @@ fun sw ->
    let attempts = ref 0 and decisions = ref [] in
    let server (req : Middleware.request) =
      incr attempts;
      assert (req.meth = `POST && req.body = String "payload");
      if Middleware.Url.path_segments req.url = ["api"; "keys"; "query"] then
        Fetch_mock.respond ~status:307
          ~headers:(Http.Header.of_list ["Location", "claim"]) "redirect" req
      else Fetch_mock.respond ~status:503 "busy" req
    in
    let config = Retry.v ~jitter:false ~backoff_factor:0.
      ~allowed_methods:[`POST]
      ~retry_request:(fun req ->
        let approved = route_allows req in
        decisions := approved :: !decisions;
        approved) () in
    let client = Fetch_mock.client server |> Fetch.with_retry ~clock ~random ~config in
    let r = Fetch.post ~sw ~body:(String "payload") client
      "https://api.example/api/keys/query" in
    let result = (status r, !attempts, List.rev !decisions) in
    close r; result;;
- : int * int * bool list = (503, 2, [true; false])
```

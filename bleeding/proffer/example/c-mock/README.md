# `c-mock`

<br>

A Proffer site is just a value, so it can be tested without starting a server.
The `proffer.mock` library sends synthetic requests through exactly the same
routing and response handling that the real backend uses, and gives back the
response for inspection. This example is a small program that exercises a site
and prints what it gets:

```ocaml
type env = { greeting : string }

let site =
  Site.of_routes
    [ get root (fun env _request respond ->
        Resp.text respond (env.greeting ^ ", world!\n"));
      get (s "echo" / str) (fun word _env _request respond ->
        Resp.text respond (word ^ "\n"));
      post (s "greet") (fun _env request respond ->
        match Req.form_param request "name" with
        | Some name -> Resp.see_other respond ("/hello/" ^ name)
        | None -> Resp.bad_request respond ()) ]

let env = { greeting = "Good evening" }

let show name response =
  Printf.printf "%s: %d %s\n" name
    (Status.code (Proffer_mock.status response))
    (String.escaped (Proffer_mock.body response))

let () =
  show "GET /" (Proffer_mock.request site env Get "/");
  show "GET /echo/hi" (Proffer_mock.request site env Get "/echo/hi");
  show "GET /missing" (Proffer_mock.request site env Get "/missing");
  show "HEAD /" (Proffer_mock.request site env Head "/");
  let response =
    Proffer_mock.request site env Post "/greet"
      ~headers:[ ("Content-Type", "application/x-www-form-urlencoded") ]
      ~body:"name=alice"
  in
  show "POST /greet" response;
  Printf.printf "  Location: %s\n"
    (Option.get (Proffer_mock.header response Location))
```

<pre><code><b>$ cd proffer/example/c-mock</b>
<b>$ dune exec ./mock.exe</b>
GET /: 200 Good evening, world!\n
GET /echo/hi: 200 hi\n
GET /missing: 404 Not Found\n
HEAD /: 200
POST /greet: 303
  Location: /hello/alice
</code></pre>

<br>

`Proffer_mock.request` takes the site, the application state, a method and a
target, with optional headers and a body. The result can be
asked for its status, its body, its headers, and its declared content length.
Nothing is written to a socket, and no port is needed, so these calls are fast
enough to run in any test suite.

Notice that the `env` here is different from anything a server would use. The
site was written without knowing its state, as described in
[**`4-counter`**](../4-counter#readme), so a test can supply whatever state
suits it. The `HEAD` request shows that the mock applies the same rules as the
real backend: the status is 200 but the body has been left out.

`Proffer_mock.describe` goes one step further down. It takes a function that
calls `respond`, with no site or route at all, which is a convenient way to
test a single handler in isolation.

<br>

**Next steps:**

- [**`d-config`**](../d-config#readme) sets connection limits and timeouts.

<br>

[Up to the tutorial index](../#readme)

# `c-mock`

<br>

Code that makes requests should be testable without a network. Because a
client is just a value, a test can pass in a fake one that answers from a
function. This example has a function `greeting` that fetches a page, and
tests it against a mock client:

```ocaml
let greeting client = String.trim (Fetch.read client "https://example.com/hello")

let canned (request : Fetch.Middleware.request) =
  Printf.printf "The mock saw %s %s\n"
    (Http.Method.to_string request.meth)
    (Fetch.Middleware.Url.to_string request.url);
  Fetch_mock.respond "Hello from the mock!" request

let () =
  Eio_mock.Backend.run @@ fun () ->
  let client = Fetch_mock.client canned in
  Printf.printf "greeting returned %S\n" (greeting client)
```

<pre><code><b>$ cd fetch/example/c-mock</b>
<b>$ dune exec ./mock.exe</b>
The mock saw GET https://example.com/hello
greeting returned "Hello from the mock!"
</code></pre>

<br>

`Fetch_mock.client`, from the `fetch.mock` library, turns a function into a
client. The function receives each request, with its method, URL, headers
and body, and returns whatever response it likes. `Fetch_mock.respond`
builds one from a body string, with optional `status` and `headers`. A test
can check what was asked for, as `canned` does by printing, and can answer
differently depending on the URL to exercise each path through the code
under test.

`greeting` takes its client as an argument, which is the habit that makes
this possible. Code written against `Fetch` never needs to know whether the
client it holds is real, mocked, or narrowed by the wrappers in the earlier
examples, and all of those wrappers work on a mock client too. That means a
test can check that a library never reaches outside its `restrict` scope, or
that a retry policy gives up when it should.

The test runs under `Eio_mock.Backend.run` instead of `Eio_main.run`. It is
Eio's own test harness, which needs no operating system resources and, with
`run_full`, provides clocks that advance on demand so that tests of retries
and rate limits do not have to wait in real time.

<br>

**Next steps:**

- [**`d-curl`**](../d-curl#readme) switches to the libcurl backend.
- [**`e-https`**](../e-https#readme) adds HTTPS to the pure OCaml backend.

<br>

[Up to the tutorial index](../#readme)

# `7-credentials`

<br>

Many sites need a token before they will answer. This example asks for a
protected page, is turned away, then attaches a token to the client and asks
again. Finally it adds a couple of headers to every request and looks at what
the server receives:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in

  print_string (Fetch.read client (base ^ "/secret"));

  let trusted =
    Fetch.with_credentials ~scope:[ base ] ~allow_insecure:true
      Fetch.Credential.[ Bearer (fun () -> "letmein") ]
      client
  in
  print_string (Fetch.read trusted (base ^ "/secret"));

  let polite =
    Fetch.with_headers
      Fetch.Header.[ user_agent, "fetch-tutorial/1.0"; raw "X-Example" "7-credentials" ]
      trusted
  in
  print_string (Fetch.read polite (base ^ "/headers"))
```

<pre><code><b>$ cd fetch/example/7-credentials</b>
<b>$ dune exec ./credentials.exe</b>
server: GET /secret -> 401
Who are you?
server: GET /secret -> 200
The secret is 42.
server: GET /headers -> 200
User-Agent: fetch-tutorial/1.0
X-Example: 7-credentials
Authorization: Bearer letmein
Accept-Encoding: gzip
Host: 127.0.0.1:39997
</code></pre>

<br>

`Fetch.with_credentials` returns a client that adds a credential to every
request within `scope`, a list of URL prefixes written just as for
`restrict` in [**`6-restrict`**](../6-restrict#readme). `Bearer` sets the
`Authorization` header, which is what most web APIs expect. It takes a
function rather than a string so that a token which expires can be refreshed
each time. `Header` sets a header of your own naming, for APIs that use an
`X-Api-Key` or similar, and `Query` adds parameters to the URL.

The scope is what makes this safe. The token is only ever sent to addresses
under the prefix, so a redirect to another site never carries it, and code
holding `trusted` cannot read the token back or send it elsewhere. Combined
with `restrict`, this is how a program hands a library exactly the access it
needs to one API and nothing more.

Credentials are only sent over `https://` unless you say otherwise. The local
server speaks plain `http://`, so the example passes `allow_insecure:true`.
Leave that out in real code.

<br>

`Fetch.with_headers` adds ordinary headers, such as a `User-Agent` that
identifies your program. The `/headers` page shows everything the server
received: the two headers just added, the token from `trusted` underneath,
and two that Fetch supplies itself. If a request sets a header that
`with_headers` also sets, the wrapper's value wins by default. Pass
`` ~mode:`If_absent `` to let the request's value win instead, or
`` `Add `` to send both.

`with_headers` refuses `Authorization` and `Cookie`, which belong to
`with_credentials` and to a cookie jar, so that secrets always travel with a
scope.

<br>

**Next steps:**

- [**`8-cookies`**](../8-cookies#readme) remembers cookies between requests.
- [**`9-retry`**](../9-retry#readme) tries again after a temporary failure.

<br>

[Up to the tutorial index](../#readme)

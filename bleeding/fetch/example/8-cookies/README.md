# `8-cookies`

<br>

Web sites hand out cookies and expect to see them again on later requests. A
*cookie jar* does the remembering. This example logs in to the local server,
looks inside the jar, and then visits the account page, once with the jar
and once with a client that keeps no cookies:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->

  let jar = Fetch_cookies.Jar.in_memory ~clock:(Eio.Stdenv.clock env) () in
  let client = Fetch_cookies.with_jar jar (Fetch_httpz.v (Eio.Stdenv.net env) ()) in
  print_string (Fetch.read client (base ^ "/login"));
  Printf.printf "The jar now holds: %s\n"
    (Option.value (Fetch_cookies.Jar.header_for jar base) ~default:"nothing");
  print_string (Fetch.read client (base ^ "/account"));

  let forgetful = Fetch_httpz.std ~cookies:`Off env in
  print_string (Fetch.read forgetful (base ^ "/account"))
```

<pre><code><b>$ cd fetch/example/8-cookies</b>
<b>$ dune exec ./cookies.exe</b>
server: GET /login -> 303
server: GET /account -> 200
Logged in with cookie session=abc123
The jar now holds: session=abc123
server: GET /account -> 200
Logged in with cookie session=abc123
server: GET /account -> 401
Not logged in.
</code></pre>

<br>

The `/login` page sets a session cookie and redirects to `/account`. The
server's log shows that the redirect was already made with the cookie: the
jar stored it from the redirect response and sent it on the very next
request. Later requests to the same site carry it too.

`Fetch_cookies.with_jar` attaches a jar to any client, and
`Fetch_cookies.Jar.in_memory` makes a jar that lasts as long as the program.
The jar applies the same rules a browser does about which cookies are sent to
which addresses, honours expiry times using the clock it was given, and is
bounded in size. `Jar.header_for` shows what would be sent to an address,
which is handy when debugging a login flow.

<br>

You rarely need to build this yourself. The `std` client from
[**`1-read`**](../1-read#readme) already has an in-memory jar, so cookies just
work. Its `cookies` argument chooses otherwise: `` `Off `` keeps none, which
is why the last request above is turned away, and `` `File path `` saves the
jar to a file in the format curl uses, so that a login survives between runs
of the program.

Build the stack by hand, as this example does, when you want to share one jar
between several clients or inspect it directly.

<br>

**Next steps:**

- [**`9-retry`**](../9-retry#readme) tries again after a temporary failure.
- [**`a-limits`**](../a-limits#readme) spaces requests out.

<br>

[Up to the tutorial index](../#readme)

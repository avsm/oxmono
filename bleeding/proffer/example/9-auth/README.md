# `9-auth`

<br>

This example keeps everything under `/admin` behind a username and password,
while the rest of the site stays open to everyone:

```ocaml
(* "alice:secret" in the Base64 form that browsers send. *)
let alice = "Basic YWxpY2U6c2VjcmV0"

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Anyone can read this.\n");
      get (s "admin") (fun () _request respond ->
        Resp.text respond "Welcome, alice.\n");
      get (s "admin" / s "settings") (fun () _request respond ->
        Resp.text respond "Settings for alice.\n") ]
  |> Site.with_auth ~scope:[ [ "admin" ] ] ~realm:"tutorial"
       ~check:(fun authorization -> authorization = Some alice)
```

<pre><code><b>$ cd proffer/example/9-auth</b>
<b>$ dune exec ./auth.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

`Site.with_auth` wraps a site so that every path under one of the prefixes in
`scope` needs to pass `check` first. A prefix is written as a list of path
segments, so `[ "admin" ]` covers `/admin` and everything below it, and the
empty list `[]` would cover the whole site.

`check` is given the value of the request's `Authorization` header, or `None`
when there is none, and returns whether the request may proceed. Here it
compares against one fixed credential. A real application would look the user
up and compare a password hash, but the shape is the same.

When the check fails, the response is `401 Unauthorized` with a challenge that
makes browsers show their login dialog:

<pre><code><b>$ curl -i http://localhost:8765/admin</b>
HTTP/1.1 401 Unauthorized
Date: Wed, 02 Sep 2026 09:32:02 GMT
WWW-Authenticate: Basic realm="tutorial"
Content-Type: text/plain; charset=utf-8
Content-Length: 13
Connection: keep-alive

Unauthorized
<b>$ curl -u alice:secret http://localhost:8765/admin/settings</b>
Settings for alice.
</code></pre>

The gate answers before any routing happens, so a visitor without credentials
cannot discover which paths under `/admin` exist. Asking for `/admin/nothing`
without a password gets the same 401 rather than a 404.

<br>

The realm is the name shown in the browser's login dialog. Because
`WWW-Authenticate` is a Basic challenge, browsers send the credentials in the
Base64 form used above, which is why the comparison is against that string.
This scheme sends passwords in the clear, so use it only behind HTTPS.

<br>

**Next steps:**

- [**`a-negotiate`**](../a-negotiate#readme) serves HTML, JSON or text depending
  on what the client accepts.
- [**`b-mount`**](../b-mount#readme) builds a site from smaller sites.

<br>

[Up to the tutorial index](../#readme)

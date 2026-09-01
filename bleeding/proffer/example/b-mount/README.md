# `b-mount`

<br>

A larger site is easier to write as several smaller ones. This example keeps
an API as its own site and mounts it under `/api/v1` of the main site:

```ocaml
let api =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.media respond "application/json" "{\"version\": 1}\n");
      get (s "users" / str) (fun name () _request respond ->
        Resp.media respond "application/json"
          (Printf.sprintf "{\"name\": %S}\n" name)) ]

let site =
  Site.of_routes
    [ get root (fun () _request respond ->
        Resp.text respond "Good morning, world!\n") ]
  |> Site.mount ~at:[ "api"; "v1" ] api
  |> Site.with_headers [ ("Server", "proffer-tutorial") ]
```

<pre><code><b>$ cd proffer/example/b-mount</b>
<b>$ dune exec ./mount.exe</b>
Running at http://localhost:8765
</code></pre>

<pre><code><b>$ curl http://localhost:8765/api/v1</b>
{"version": 1}
<b>$ curl http://localhost:8765/api/v1/users/alice</b>
{"name": "alice"}
</code></pre>

<br>

The routes of `api` are written relative to its own root, so `get root` there
answers `/api/v1`. `Site.mount` adds them to the main site beneath the given
prefix. The main site's own routes stay first, so they take precedence if two
patterns overlap. Only the routes are mounted: the sub-site's fallback, if it
had one, is not carried across.

Because both halves must share the same kind of `env`, a sub-site is usually
written in its own module and given the application's state type.

<br>

Wrappers such as `Site.with_headers` and `Site.with_auth` should be applied
after mounting, as the `Server` header is here. Proffer refuses to mount a
site that has already been wrapped, since the wrapper would otherwise be
silently lost.

<br>

**Next steps:**

- [**`c-mock`**](../c-mock#readme) tests a site without opening a network
  connection.
- [**`d-config`**](../d-config#readme) sets connection limits and timeouts.

<br>

[Up to the tutorial index](../#readme)

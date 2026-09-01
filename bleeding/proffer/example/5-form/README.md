# `5-form`

<br>

This example shows an HTML form, reads what the visitor typed into it, and
sends them on to a page that greets them. It also reads a query string, and a
second form uploads a file.

```ocaml
let page =
  {|<!doctype html>
<form method="post" action="/greet">
  <label>Your name: <input name="name"></label>
  <button>Greet me</button>
</form>

<form method="post" action="/upload" enctype="multipart/form-data">
  <label>Your name: <input name="name"></label>
  <label>A file: <input type="file" name="file"></label>
  <button>Upload it</button>
</form>
|}

let site =
  Site.of_routes
    [ get root (fun () _request respond -> Resp.html respond page);

      post (s "greet") (fun () request respond ->
        match Req.form_param request "name" with
        | Some name -> Resp.see_other respond ("/hello/" ^ name)
        | None -> Resp.bad_request respond ());

      post (s "upload") (fun () request respond ->
        match Multipart.of_req request with
        | Error e -> Resp.text respond ~status:Bad_request
            (Media.error_to_string e ^ "\n")
        | Ok parts ->
          match Multipart.file parts "file" with
          | None -> Resp.text respond ~status:Bad_request "No file was sent.\n"
          | Some file ->
            let name =
              Option.value (Multipart.field request parts "name")
                ~default:"stranger"
            in
            Resp.text respond
              (Printf.sprintf "Thank you %s, I got %s (%d bytes).\n" name
                 (Option.value file.filename ~default:"a file") file.len));

      get (s "hello" / str) (fun name () _request respond ->
        Resp.text respond ("Good morning, " ^ name ^ "!\n"));

      get (s "search") (fun () request respond ->
        match Req.query_param request "q" with
        | Some q -> Resp.text respond ("You searched for: " ^ q ^ "\n")
        | None -> Resp.text respond "Add ?q=something to the URL.\n") ]
```

<pre><code><b>$ cd proffer/example/5-form</b>
<b>$ dune exec ./form.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

Open [http://localhost:8765](http://localhost:8765), type a name, and press the
button. The browser sends a `POST` request to `/greet`, the `post` route reads
the `name` field with `Req.form_param`, and `Resp.see_other` answers with a
redirect to the greeting page. The browser follows it, so you end up at
`/hello/` followed by the name.

Redirecting after a form is submitted is a common pattern. It means reloading
the greeting page does not resubmit the form.

With curl you can watch the redirect happen, or follow it with `-L`:

<pre><code><b>$ curl -i http://localhost:8765/greet --data name=alice</b>
HTTP/1.1 303 See Other
Date: Wed, 02 Sep 2026 09:31:36 GMT
Location: /hello/alice
Content-Length: 0
Connection: keep-alive

<b>$ curl -L http://localhost:8765/greet --data name=alice</b>
Good morning, alice!
</code></pre>

If the field is missing, `Resp.bad_request` sends a `400 Bad Request` with a
small page. Both it and `Resp.not_found` accept your own HTML instead.

<br>

`Req.form_param` reads fields from a body in the usual form encoding,
`application/x-www-form-urlencoded`. `Req.form` returns all of the fields as a
list. Values are decoded for you, so `+` and `%20` both arrive as a space. If
you need to tell a body of another media type from a form with nothing in it,
`Req.form_result` says which it was.

Query strings work the same way through `Req.query_param` and `Req.query`:

<pre><code><b>$ curl 'http://localhost:8765/search?q=ocaml+eio'</b>
You searched for: ocaml eio
</code></pre>

<br>

A form that carries a file is sent in the other form encoding,
`multipart/form-data`, which is what `enctype` on the second form selects. The
browser packs each control into its own part, and `Multipart.of_req` unpacks
them. `Multipart.file` finds the part a file input filled in, `Multipart.field`
the content of an ordinary one, and `Multipart.fields` all of the ordinary ones
at once:

<pre><code><b>$ curl http://localhost:8765/upload -F name=alice -F file=@hello.txt</b>
Thank you alice, I got hello.txt (13 bytes).
</code></pre>

The filename comes from the client and is not a path you may use. Treat it as a
label; choose your own name for anything you write to disk.

The whole request, head and body together, is held in memory, so the backend
decides how large an upload may be. `proffer-httpz` caps a request at about
32 KiB and answers a larger one with `413 Payload Too Large`, which is room for
a small text file and no more. An upload of real size needs a backend that
streams.

<br>

The greeting page uses `Resp.text` rather than `Resp.html` on purpose. The
name comes from the visitor, and Proffer does not escape it for you, so
putting it straight into HTML would let a visitor inject markup. Use a
templating library, or escape the value yourself, before building HTML from
user input.

<br>

**Next steps:**

- [**`6-error`**](../6-error#readme) customises error pages and adds a header
  to every response.
- [**`7-cache`**](../7-cache#readme) tells browsers what they may keep.

<br>

[Up to the tutorial index](../#readme)

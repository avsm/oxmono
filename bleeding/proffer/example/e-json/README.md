# `e-json`

<br>

Most APIs speak JSON. Rather than build strings by hand, this example
describes its data once with [Jsont](https://erratique.ch/software/jsont) and
turns that description into a *codec*, which Proffer then uses to read
request bodies and write responses. The site keeps a list of to-do items:

```ocaml
type todo = { id : int; title : string; done_ : bool }

let todo_jsont =
  Jsont.Object.map ~kind:"Todo" (fun id title done_ -> { id; title; done_ })
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun t -> t.id)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Jsont.Object.mem "done" Jsont.bool ~enc:(fun t -> t.done_) ~dec_absent:false
  |> Jsont.Object.finish

type env = {
  store : (int, todo) Hashtbl.t;
  todo : todo Media.t;
  todos : todo list Media.t;
  todo_lines : todo Media.seq;
}

let all env =
  Hashtbl.to_seq_values env.store
  |> List.of_seq
  |> List.sort (fun a b -> compare a.id b.id)

let site =
  Site.of_routes
    [ get (s "todos") (fun env _request respond ->
        Resp.encode respond env.todos (all env));

      get (s "todos" / s "export") (fun env _request respond ->
        Resp.encode_seq respond env.todo_lines (List.to_seq (all env)));

      get (s "todos" / int) (fun id env _request respond ->
        match Hashtbl.find_opt env.store id with
        | Some t -> Resp.encode respond env.todo t
        | None -> Resp.not_found respond ());

      post (s "todos") (with_body (fun env -> env.todo) (fun t env _request respond ->
        Hashtbl.replace env.store t.id t;
        Resp.encode respond ~status:Created env.todo t)) ]
```

<pre><code><b>$ cd proffer/example/e-json</b>
<b>$ dune exec ./json.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

`todo_jsont` says what a to-do looks like as JSON: an object with an `id`, a
`title` and a `done` flag, the last of which may be left out. Jsont reads and
writes from that one description. At startup, `Json.v` wraps it as a codec for
`application/json`, and the same call over `Jsont.list` gives a codec
for a whole list. The codecs live in the server environment alongside the
store, so each server domain owns the library closures it invokes.

`Resp.encode` responds with a value through a codec. It sets the
`Content-Type` from the codec, so the handler never names a media type:

<pre><code><b>$ curl -i http://localhost:8765/todos</b>
HTTP/1.1 200 OK
Date: Wed, 02 Sep 2026 10:24:02 GMT
Content-Type: application/json
Content-Length: 51
Connection: keep-alive

[{"id":1,"title":"write the tutorial","done":true}]
</code></pre>

<br>

`with_body` takes a function that selects the codec from the environment and
wraps a handler so that the request body is decoded first. The decoded value
arrives as an extra leading argument, just as a path capture does in
[**`2-router`**](../2-router#readme). The handler only ever sees a well-formed
to-do:

<pre><code><b>$ curl -i http://localhost:8765/todos -H 'Content-Type: application/json' --data '{"id": 2, "title": "ship it"}'</b>
HTTP/1.1 201 Created
Date: Wed, 02 Sep 2026 10:24:02 GMT
Content-Type: application/json
Content-Length: 39
Connection: keep-alive

{"id":2,"title":"ship it","done":false}
</code></pre>

A body of the wrong type gets `415 Unsupported Media Type`, and one that is
JSON but does not fit the description gets `400 Bad Request` with Jsont's
explanation, so a client is told exactly what was wrong:

<pre><code><b>$ curl -i http://localhost:8765/todos -H 'Content-Type: application/json' --data '{"id": "two"}'</b>
HTTP/1.1 400 Bad Request
Date: Wed, 02 Sep 2026 10:24:02 GMT
Content-Type: text/plain; charset=utf-8
Content-Length: 143
Connection: keep-alive

Bad Request: String "two" does not parse to OCaml int value
File "-":
File "-": in member id of
File "-", line 1, characters 0-13: Todo object
</code></pre>

To answer such mistakes differently, call `Req.decode` yourself instead of
using `with_body`. It returns the value or the reason, and the handler
decides what to send.

<br>

The export route sends the list as [JSON Lines](https://jsonlines.org), one
item per line, through `Resp.encode_seq`. `Json.lines` makes a
sequence codec from the same description. The items are written as the
sequence produces them, so a long export streams rather than being built in
memory first, as in [**`8-stream`**](../8-stream#readme):

<pre><code><b>$ curl -i http://localhost:8765/todos/export</b>
HTTP/1.1 200 OK
Date: Wed, 02 Sep 2026 10:24:02 GMT
Content-Type: application/jsonl
Transfer-Encoding: chunked
Connection: keep-alive

{"id":1,"title":"write the tutorial","done":true}
{"id":2,"title":"ship it","done":false}
</code></pre>

<br>

Codecs are not tied to JSON. `Media.of_strings` makes one from any pair of
functions between a type and a string, and `Markdown` in the next example
provides codecs for Markdown. Both adapters are part of the main Proffer
library, ready for use without selecting another library.

<br>

**Next steps:**

- [**`f-markdown`**](../f-markdown#readme) serves one document as HTML or as
  Markdown source, whichever the client prefers.
- The Fetch tutorial's [**`f-json`**](../../../fetch/example/f-json#readme)
  is the client side of this API, using the same description.

<br>

[Up to the tutorial index](../#readme)

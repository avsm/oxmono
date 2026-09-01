# `f-json`

<br>

APIs answer in JSON, and reading it by hand is tedious and easy to get
wrong. This example describes a to-do item once with
[Jsont](https://erratique.ch/software/jsont), turns the description into a
*codec*, and lets Fetch do the reading and writing:

```ocaml
type todo = { id : int; title : string; done_ : bool }

let todo_jsont =
  Jsont.Object.map ~kind:"Todo" (fun id title done_ -> { id; title; done_ })
  |> Jsont.Object.mem "id" Jsont.int ~enc:(fun t -> t.id)
  |> Jsont.Object.mem "title" Jsont.string ~enc:(fun t -> t.title)
  |> Jsont.Object.mem "done" Jsont.bool ~enc:(fun t -> t.done_) ~dec_absent:false
  |> Jsont.Object.finish

let todo = Fetch.Json.v todo_jsont
let todo_lines = Fetch.Json.lines todo_jsont

let show t =
  Printf.printf "Todo %d: %s%s\n" t.id t.title (if t.done_ then " (done)" else "")

let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in

  (match Fetch.read_as client todo (base ^ "/todo") with
   | Ok t -> show t
   | Error r -> Printf.printf "The server said %d\n" (Fetch.status r));

  (match Fetch.read_as client todo (base ^ "/todo/2") with
   | Ok t -> show t
   | Error r ->
     Printf.printf "The server said %d: %s" (Fetch.status r)
       (Fetch.decode Fetch.Media.text r));

  Eio.Switch.run @@ fun sw ->
  let headers, body = Fetch.encode todo { id = 2; title = "ship it"; done_ = false } in
  let response = Fetch.post ~sw ~headers ~body client (base ^ "/echo") in
  print_string "Echoed back: ";
  show (Fetch.decode todo response);

  let response = Fetch.get ~sw client (base ^ "/todos.jsonl") in
  Fetch.decode_seq todo_lines response
  |> Seq.filter (fun t -> not t.done_)
  |> Seq.iter (fun t -> Printf.printf "Still to do: %s\n" t.title)
```

<pre><code><b>$ cd fetch/example/f-json</b>
<b>$ dune exec ./json.exe</b>
server: GET /todo -> 200
Todo 1: write the tutorial
server: GET /todo/2 -> 404
The server said 404: Not Found
server: POST /echo -> 200
Echoed back: Todo 2: ship it
server: GET /todos.jsonl -> 200
Still to do: write the tutorial
Still to do: ship it
</code></pre>

<br>

`todo_jsont` says what a to-do looks like as JSON, and `Fetch.Json.v` turns it
into a codec for `application/json`. From then on the program deals in
`todo` values and never sees a JSON string.

`Fetch.read_as` is `Fetch.read` with a codec. It sends an `Accept` header
naming the codec's media type, and when the server answers with a success
status it decodes the body and returns `Ok`. Any other status comes back as
`Error` with the whole response, because a "not found" or "forbidden" reply
is the server's answer rather than a failure, and its body often explains
itself. Here the second request decodes that body as plain text; an API that
returns JSON errors would decode it with a second codec.

What the server *gets wrong* is a different matter. A body that is not JSON,
or JSON that does not fit the description, means the codec cannot produce a
value at all. That raises an exception, the same `Eio.Io` that
[**`5-errors`**](../5-errors#readme) showed for a failed connection, carrying
`Fetch.Decode_failure` with the media type expected and what was found. The
two kinds of problem therefore never get confused: a status is matched on
the `Error` branch, and a broken body is caught wherever the program handles
failures.

<br>

`Fetch.encode` goes the other way. It gives the `Content-Type` header and
the body for a value, ready to pass to `Fetch.post`, and `Fetch.decode` reads
a value out of any response you already hold.

The last request reads [JSON Lines](https://jsonlines.org), one item per
line, through `Fetch.decode_seq`. It returns a sequence that decodes each
line as it is consumed, so a large export is handled item by item without
waiting for the whole body. `Fetch.encode_seq` sends a sequence the same
way, as a streamed body.

<br>

**Next steps:**

- [**`g-markdown`**](../g-markdown#readme) fetches a Markdown document and
  renders it.
- The Proffer tutorial's [**`e-json`**](../../../proffer/example/e-json#readme)
  is the server side of this API, using the same description.

<br>

[Up to the tutorial index](../#readme)

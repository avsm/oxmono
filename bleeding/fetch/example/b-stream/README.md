# `b-stream`

<br>

A body need not fit in memory. This example downloads four megabytes straight
into a file, then uploads a body that is read from a source as it is sent:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Localhost.run env @@ fun base ->
  let client = Fetch_httpz.std env in
  Eio.Switch.run @@ fun sw ->

  let path = Eio.Path.(Eio.Stdenv.fs env / Filename.get_temp_dir_name () / "fetch-tutorial.bin") in
  let response = Fetch.get ~sw client (base ^ "/big") in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) path (fun file ->
    Eio.Flow.copy (Fetch.body response) file;
    Printf.printf "Saved %s bytes to %s\n"
      (Optint.Int63.to_string (Eio.File.size file)) (snd path));

  let source = Eio.Flow.string_source (String.make 8192 'u') in
  let response =
    Fetch.post ~sw client (base ^ "/upload") ~body:(Fetch.stream ~length:8192L source)
  in
  Eio.Flow.copy (Fetch.body response) (Eio.Stdenv.stdout env)
```

<pre><code><b>$ cd fetch/example/b-stream</b>
<b>$ dune exec ./stream.exe</b>
server: GET /big -> 200
Saved 4194304 bytes to /tmp/fetch-tutorial.bin
server: POST /upload -> 200
Received 8192 bytes.
</code></pre>

<br>

As [**`2-response`**](../2-response#readme) showed, `Fetch.body` is a flow
that is read on demand. `Eio.Flow.copy` moves it into the file a piece at a
time, so the whole download never sits in memory. If the file were on a slow
disk, the download would simply slow down to match. The same copy works to
any destination Eio can write to, including another network connection.

<br>

Uploads work the other way round. `Fetch.stream` makes a body from any flow
that can be read, such as an open file, and the backend reads from it while
the request is being sent. Here the source is a string, to keep the example
short, but `Eio.Path.with_open_in` would supply a file in the same position.
Give `length` when you know the size, so the server is told up front how much
to expect; leave it out and the body is sent in chunks until the source runs
dry.

A streamed body can only be sent once. If a redirect or a retry would need to
send it again, the request fails with `Body_not_replayable` rather than
sending something incomplete. `String` bodies do not have this restriction.
The tutorial's local server accepts only small uploads, which is why this one
is eight kilobytes.

<br>

**Next steps:**

- [**`c-mock`**](../c-mock#readme) tests code that makes requests.
- [**`d-curl`**](../d-curl#readme) switches to the libcurl backend.

<br>

[Up to the tutorial index](../#readme)

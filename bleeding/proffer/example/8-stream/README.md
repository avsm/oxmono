# `8-stream`

<br>

Some responses are too large to build in memory, or take a while to produce
and are better sent as they go. `Resp.stream` takes a function that writes the
body a piece at a time. This example counts down with a pause between numbers,
and writes the alphabet one letter at a time:

```ocaml
type env = { clock : float Eio.Time.clock_ty Eio.Resource.t }

let site =
  Site.of_routes
    [ get (s "countdown") (fun env _request respond ->
        Resp.stream respond "text/plain" @@ fun sink ->
        for i = 5 downto 1 do
          Body.Sink.write sink (string_of_int i ^ "\n");
          Eio.Time.sleep env.clock 1.0
        done;
        Body.Sink.write sink "Liftoff!\n");

      get (s "alphabet") (fun _env _request respond ->
        Resp.stream respond ~length:27L "text/plain" @@ fun sink ->
        for c = Char.code 'a' to Char.code 'z' do
          Body.Sink.write sink (String.make 1 (Char.chr c))
        done;
        Body.Sink.write sink "\n") ]
```

<pre><code><b>$ cd proffer/example/8-stream</b>
<b>$ dune exec ./stream.exe</b>
Running at http://localhost:8765
</code></pre>

<br>

Ask curl not to buffer, and the numbers arrive one second apart:

<pre><code><b>$ curl -i -N http://localhost:8765/countdown</b>
HTTP/1.1 200 OK
Date: Wed, 02 Sep 2026 09:31:57 GMT
Content-Type: text/plain
Transfer-Encoding: chunked
Connection: keep-alive

5
4
3
2
1
Liftoff!
</code></pre>

The function you give `Resp.stream` receives a *sink*, and everything written
to it is sent to the client. The server sends the response headers first and
then runs your function, so by the time it runs the status and headers are
already final. Only use the sink inside that function.

When the total size is not known in advance, as with the countdown, the
backend sends the body in chunks and the client reads until the end marker.
When you do know the size, pass `length` and the response carries a
`Content-Length` instead. The count must be exact:

<pre><code><b>$ curl -i http://localhost:8765/alphabet</b>
HTTP/1.1 200 OK
Date: Wed, 02 Sep 2026 09:31:57 GMT
Content-Type: text/plain
Content-Length: 27
Connection: keep-alive

abcdefghijklmnopqrstuvwxyz
</code></pre>

A streamed body is not sent for a `HEAD` request, and the function is not
called at all in that case.

<br>

**Next steps:**

- [**`9-auth`**](../9-auth#readme) protects part of the site with a password.
- [**`a-negotiate`**](../a-negotiate#readme) serves HTML, JSON or text depending
  on what the client accepts.

<br>

[Up to the tutorial index](../#readme)

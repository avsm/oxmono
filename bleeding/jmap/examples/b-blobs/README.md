# `b-blobs`

<br>

Binary data does not travel through the JSON API. A blob is POSTed to one URL
the session names and fetched back from another, and the method calls only ever
refer to it by the id the upload returned. This step sends a small string up,
brings it back, checks the bytes, and then does the same thing again without
ever holding the blob in memory. The frame is the one from
[**`1-session`**](../1-session#readme). No method call is made at all, so
nothing here goes through `Client.call_exn` and the `using` array never comes
into it.

```ocaml
  let expansion = function
    | Ok url -> url
    | Error error ->
        Fmt.failwith "URI template: %a" Httpz_uri.Template.pp_error error
  in
  Fmt.pr "upload    %s@." (Client.upload_url client);
  Fmt.pr "          %s@."
    (expansion
       (Proto.Template.expand
          ~vars:[ ("accountId", account) ]
          (Client.upload_url client)));
  let up =
    Client.upload_exn client ~account_id ~content_type:"text/plain" ~data
  in
  let blob_id = up.blob_id in
  Fmt.pr "          blobId=%a type=%s size=%Ld@." Proto.Id.pp blob_id
    (Cli.terminal_text up.type_) up.size;
```

<br>

An upload is an ordinary HTTP POST of the bytes with their `Content-Type` to the
session's `uploadUrl`
([RFC 8620 §6.1](https://www.rfc-editor.org/rfc/rfc8620#section-6.1)). The
answer is a small JSON object, decoded here as a
[`Proto.Blob.upload_response`](../../lib/proto/proto_blob.mli), carrying the
`blobId` every later method call will use, the media type the server has decided
to serve the blob as, and the size it actually stored. That size against what
you sent is the cheapest check that nothing was truncated on the way.

```ocaml
  let name = "a blob.txt" in
  Fmt.pr "download  %s@." (Client.download_url client);
  Fmt.pr "          %s@."
    (expansion
       (Proto.Blob.expand_download_url
          ~template:(Client.download_url client)
          { account_id; blob_id; type_ = "text/plain"; name }));
  let back =
    Client.download_exn client ~account_id ~blob_id ~name ~accept:"text/plain"
      ()
  in
  if not (String.equal back data) then
    Fmt.failwith "the %d bytes downloaded differ from the %d uploaded"
      (String.length back) (String.length data);
  Fmt.pr "          %d bytes, identical to what went up@." (String.length back);
```

Neither URL is a fixed path. Both are URI templates in the sense of
[RFC 6570](https://www.rfc-editor.org/rfc/rfc6570), at its level 1, and RFC 8620
§6.1 and §6.2 name the variables, `{accountId}` for an upload and
`{accountId}`, `{blobId}`, `{type}` and `{name}` for a download.
[`Proto.Template.expand`](../../lib/proto/proto_template.mli) fills them in and
percent encodes each value, so `text/plain` travels as `text%2Fplain` and the
space in `a blob.txt` as `%20`. The result keeps a malformed
server-advertised template in the command's error path. The step prints each
template beside its expansion.
[`Proto.Blob.expand_download_url`](../../lib/proto/proto_blob.mli) is the same
checked expansion with the four download variables named.

[`Client.upload`](../../eio/client.mli) and
[`Client.download`](../../eio/client.mli) do the expansion themselves and
present the blob as a `string`, which is right until the blob is a video. Each
has an `_exn` twin, used here, that raises `Jmap_client_error` rather than
returning a result, and the frame prints that and exits 1.

```ocaml
  let length = Int64.of_int (String.length data) in
  let streamed =
    Client.upload_flow_exn client ~account_id ~content_type:"text/plain" ~length
      (Eio.Flow.string_source data)
  in
  let buffer = Buffer.create (String.length data) in
  let served =
    Client.download_to_exn client ~account_id ~blob_id:streamed.blob_id
      ~name:"streamed.txt"
      (Eio.Flow.buffer_sink buffer)
  in
  if not (String.equal (Buffer.contents buffer) data) then
    Fmt.failwith "the streamed round trip changed the bytes";
  Fmt.pr "stream    blobId=%a served as %s, identical@." Proto.Id.pp
    streamed.blob_id (Cli.terminal_text served)
```

The streaming pair never builds the blob at all. `Client.upload_flow` reads its
bytes from an `Eio.Flow.source` as the request body goes out, and
`Client.download_to` writes the response body to an `Eio.Flow.sink` as it
arrives. The source here is an
[`Eio.Flow.string_source`](https://ocaml-multicore.github.io/eio/eio/Eio/Flow/index.html)
and the sink an `Eio.Flow.buffer_sink`. An attachment on disk would use
`Eio.Path.with_open_in` and the file's own size as `~length`, which lets the
body go out with a `Content-Length` rather than chunked.

`download_to` returns the media type the server actually served. RFC 8620 §6.2
has the `{type}` variable be the type the server sets in `Content-Type`, since a
blob is bytes with no type of its own, and it is `application/octet-stream` here
because that is `Client.download_to`'s default `accept`. A client that
reinterprets the bytes should look at the type that came back.

The oracle answers an upload with a 201 rather than a 200. RFC 8620 §6.1 fixes
no status code, and neither status changes anything for a client. Nothing needs
cleaning up either.
RFC 8620 §6.1 lets a server expire a blob no object refers to, and neither of
these is referred to, so the step is safe to run as often as you like.

<pre><code><b>$ dune exec -- examples/b-blobs/blobs.exe --allow-insecure</b>
upload    http://localhost:18080/jmap/upload/{accountId}/
          http://localhost:18080/jmap/upload/user1/
          blobId=G254e6e029d113f5b7a2cfb063dffeea48ab8da87 type=text/plain size=33
download  http://localhost:18080/jmap/download/{accountId}/{blobId}/{name}?accept={type}
          http://localhost:18080/jmap/download/user1/G254e6e029d113f5b7a2cfb063dffeea48ab8da87/a%20blob.txt?accept=text%2Fplain
          33 bytes, identical to what went up
stream    blobId=G254e6e029d113f5b7a2cfb063dffeea48ab8da87 served as application/octet-stream, identical
</code></pre>

Both uploads produced the same `blobId`, because the bytes were the same and
this server names a blob after their digest. That is a server's business and not
something to rely on.

<br>

**Next steps:**

- The next example, [**`c-import`**](../c-import#readme), takes a blob holding
  an RFC 5322 message and turns it into real mail.
- [**`f-push`**](../f-push#readme) reaches the third endpoint the session names,
  the event source, which is neither the API URL nor a blob URL.

<br>

**See also:**

- [**`p-stream`**](../p-stream#readme) does the same round trips against a file of
  your choosing and reports the media type at every step.

<br>

[Up to the tutorial index](../#readme)

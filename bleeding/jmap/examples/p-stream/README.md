# `p-stream`

<br>

An attachment is a file, and a file can be larger than the memory a client has
to spare. This step moves one to the server and back a megabyte at a time,
never holding the whole of it, and checks the size at each end of the trip.
[**`b-blobs`**](../b-blobs#readme) introduces the two blob endpoints, their URI
templates and the in-memory pair `Client.upload` and `Client.download`; this
one is the same round trip written the way a mail client writes it.

```ocaml
let mib = 1024 * 1024
let chunk = String.init mib (fun i -> Char.chr (32 + (i mod 95)))

let generate fs path ~size =
  Eio.Path.with_open_out ~create:(`Or_truncate 0o600) Eio.Path.(fs / path)
  @@ fun file ->
  let rec fill written =
    if written < size then begin
      let n = min mib (size - written) in
      Eio.Flow.copy_string (String.sub chunk 0 n) file;
      fill (written + n)
    end
  in
  fill 0

let size_of fs path =
  Eio.Path.with_open_in Eio.Path.(fs / path) @@ fun file ->
  Optint.Int63.to_int64 (Eio.File.size file)
```

<br>

With no arguments the step has no attachment to hand, so it writes one. Four
megabytes of printable bytes go a megabyte at a time into a temporary file it
removes at the end. `--file PATH` streams a file of your own instead, and
`--type MIME` sets the media type it goes up with. The file is only ever
reached through
[`Eio.Path`](https://ocaml-multicore.github.io/eio/eio/Eio/Path/index.html), so
its size comes from `Eio.File.size` rather than from the bytes, which is the
number the upload needs.

```ocaml
  let length = size_of fs source in
  let limit = max_size_upload client in
  Fmt.pr "file      %s@." source;
  Fmt.pr "          %Ld bytes, maxSizeUpload %s@." length
    (Option.fold ~none:"unstated" ~some:Int64.to_string limit);
  Option.iter
    (fun l ->
      if length > l then Fmt.failwith "the session accepts %Ld bytes at most" l)
    limit;
  let blob =
    Eio.Path.with_open_in Eio.Path.(fs / source) @@ fun file ->
    Client.upload_flow_exn client ~account_id ~content_type ~length file
  in
  let blob_id = blob.blob_id in
  Fmt.pr "upload    blobId=%a type=%s size=%Ld@." Proto.Id.pp blob_id
    (Cli.terminal_text blob.type_) blob.size;
  if blob.size <> length then
    Fmt.failwith "the server stored %Ld of the %Ld bytes sent" blob.size length;
```

<br>

`maxSizeUpload` of the core capability object
([RFC 8620 §2](https://www.rfc-editor.org/rfc/rfc8620#section-2)) is the largest
blob the server accepts. A client that streams checks it before spending the
bandwidth, since the alternative is a 413 after the whole file has gone out.
The Cyrus oracle answers 0, to which RFC 8620 gives no meaning, so
`max_size_upload` reads 0 as no stated limit.

[`Client.upload_flow`](../../eio/client.mli) is the POST of
[RFC 8620 §6.1](https://www.rfc-editor.org/rfc/rfc8620#section-6.1) with its
body read from an `Eio.Flow.source` as the request goes out, so the client's
memory does not grow with the file. `~length` is what the upload sends as
`Content-Length`; without it the body is chunked, which a JMAP server may
refuse to buffer, so pass it whenever the size is known. The body is read once
and cannot be replayed, so a redirect on this request fails rather than
repeating it.

The upload response carries the size the server stored. Comparing it with the
length sent is the first half of the end-to-end check, and the only one that
catches a body truncated on the way up.

```ocaml
  let served =
    Eio.Path.with_open_out ~create:(`Or_truncate 0o600) Eio.Path.(fs / target)
    @@ fun file ->
    Client.download_to_exn client ~account_id ~blob_id
      ~name:(Filename.basename source) ~accept:content_type file
  in
  let back = size_of fs target in
  Fmt.pr "download  %s@." target;
  Fmt.pr "          %Ld bytes, asked for %s, served as %s@." back
    (Cli.terminal_text content_type) (Cli.terminal_text served);
  if back <> length then
    Fmt.failwith "the download is %Ld bytes and the upload was %Ld" back length;
  let sent = Digest.file source and got = Digest.file target in
  if not (String.equal sent got) then
    Fmt.failwith "the streamed round trip changed the bytes";
  Fmt.pr "digest    %s, identical end to end@." (Digest.to_hex sent)
```

<br>

[`Client.download_to`](../../eio/client.mli) is the GET of
[RFC 8620 §6.2](https://www.rfc-editor.org/rfc/rfc8620#section-6.2) writing the
response body to an `Eio.Flow.sink` as it arrives, here an open file, so a blob
of any size lands on disk without a copy in memory. The client's body limit
does not apply to it, the bytes being the caller's to place. Its timeout covers
the response head and then each wait for more bytes rather than the total
transfer. A stalled body can therefore leave a prefix in the destination; use
a temporary file and rename after success when the target must be atomic.

A blob is bytes and has no type of its own. `~accept` is the media type the
server is asked to serve it as, and `download_to` is the type it actually
served, which RFC 8620 §6.2 leaves to the server. Ask for the type the blob was
uploaded with and reinterpret the bytes by what came back, not by what you
asked for. `~accept` defaults to `application/octet-stream`, which is the right
answer for a file being saved and the wrong one for a part being displayed.
`~name` is the filename the server offers in `Content-Disposition` and reaches
the URL percent encoded, as `b-blobs` shows.

Both files are then compared without either being loaded. The sizes come from
the filesystem and the digests from a single pass over each file. Nothing here
holds more than one megabyte of the blob at a time.

<br>

<pre><code><b>$ dune exec -- examples/p-stream/stream.exe --allow-insecure</b>
file      /tmp/jmap-stream-ad9ec1.bin
          4194304 bytes, maxSizeUpload unstated
upload    blobId=Gd33f68fe313e48ffc8c963322abdd7a4ba600d91 type=application/octet-stream size=4194304
download  /tmp/jmap-stream-back-94f06f.bin
          4194304 bytes, asked for application/octet-stream, served as application/octet-stream
digest    147705fd4662ddab495720ed21613233, identical end to end
</code></pre>

The generated file has the same contents on every run, and this server names a
blob after the digest of its bytes, so the `blobId` is the same on every run
too. Neither blob is referred to by any object, and RFC 8620 §6.1 lets the
server expire one that is not, so the step leaves nothing behind on the server
either.

<br>

[Up to the tutorial index](../#readme)

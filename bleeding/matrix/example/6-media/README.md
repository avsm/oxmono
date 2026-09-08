# `6-media`

<br>

`media` logs in to a homeserver with a password, uploads a file to the
media repository, sends it to a room, then downloads the same content back
and prints the byte count of each to show they match. It reads the
homeserver, the user and the password from the environment and takes a
room id and a file path as its two arguments.

<br>

Matrix keeps binary content such as images and files separate from the
timeline, in a homeserver's media repository, addressed by an `mxc://`
URI. An event that refers to a file carries this URI rather than the
file's bytes, so a client has to resolve the URI, through a download or a
thumbnail request, before it can show the content to a user.

<br>

The login, the argument parsing and a `content_type_of_filename` helper
that maps a file extension to a MIME type are left out below. The rest of
the file is:

```ocaml
let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
let data = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path) in
let mxc =
  M.Media.upload client ~content_type ~data ~filename:(Filename.basename path)
    ()
in
Printf.printf "Uploaded %s\n%!" (M.Media.Mxc.to_string mxc);
let send =
  if String.starts_with ~prefix:"image/" content_type then
    M.Messages.send_image
  else M.Messages.send_file
in
let event_id =
  send client ~room_id ~body:(Filename.basename path) ~url:mxc ()
in
Printf.printf "Sent %s\n%!" (Matrix_proto.Id.Event_id.to_string event_id);
let content =
  M.Media.download client
    ~server_name:(M.Media.Mxc.server_name mxc)
    ~media_id:(M.Media.Mxc.media_id mxc)
in
Printf.printf "Downloaded %d bytes (uploaded %d)\n%!"
  (String.length content.body) (String.length data)
```

<br>

[`Media.upload`](../../lib/matrix_eio/media.mli) sends the file's content
type and bytes to the homeserver and returns an
[`mxc://` URI](../../lib/matrix_client/media.mli), the only way the Matrix
API names stored content. The URI names a server and an identifier on it,
and says nothing about where the content is actually served from. A client
resolves it later, either through `Media.mxc_to_http` for a browser, or, as
here, through `Media.download`.

<br>

The event a room sees carries this URI and whatever the sender claims about
the file, not the file itself.
[`Messages.send_file`](../../lib/matrix_eio/messages.mli) and
[`Messages.send_image`](../../lib/matrix_eio/messages.mli) take the same
arguments, one sending an `m.file` and the other an `m.image`. The program
picks between them from the content type, since a client renders `m.image`
inline and everything else as a download link.

<br>

An [`Mxc.t`](../../lib/matrix_client/media.mli) splits into the
`server_name` that holds the content and the `media_id` it is filed under,
which is what [`Media.download`](../../lib/matrix_eio/media.mli) takes.
Downloads, thumbnails and the upload configuration all use the
authenticated media endpoints introduced in Matrix 1.11, so downloading
needs the same logged-in client that uploaded.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USER=alice-b-88f9a4 MATRIX_PASSWORD=pw12345</b>
<b>$ echo "Hello from the 6-media tutorial example." > hello.txt</b>
<b>$ dune exec -- example/6-media/media.exe '!hVdZpMdZPOSfFSarMh:localhost' hello.txt</b>
Uploaded mxc://localhost/jIVDHvDaWUfQLoiFUufzGEON
Sent $ahxuBp6cYj49ohwCQpHF1U9LiFugRBzHyrzxftpChBU
Downloaded 41 bytes (uploaded 41)
</code></pre>

<br>

**Next:** [`7-profile`](../7-profile#folders-and-files) keeps the session
on disk so the next run does not need a password.

<br>

[Up to the example index](../#readme)

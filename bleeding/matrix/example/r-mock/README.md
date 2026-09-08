# `r-mock`

<br>

`mock` builds a `Matrix_client.Client.t` over a mock HTTP backend instead of
a homeserver, logs in, sends a text message, and prints the method, URL and
body of every request the mock backend saw. It then scripts an
`M_FORBIDDEN` response and prints the `Error.t` that
`Matrix_client.Messages.send_text` returns for it. It takes no arguments and
needs no homeserver.

<br>

`matrix-chat.client` never raises for a failed request. Every endpoint function
returns `(_, Error.t) result`, and the HTTP backend a client talks through is
a plain `Fetch.t` value rather than something the library opens for itself.
Handing a client a backend that answers from a script, instead of one that
opens a socket, is what makes the client-server API testable without a
homeserver, and is how this library's own test suite works.

<br>

```ocaml
module M = Matrix_client
module Id = Matrix_proto.Id

let secure_random =
  object
    method secure_random =
      Eio.Flow.string_source (String.init 4096 (fun i -> Char.chr (i land 255)))
  end

let client_of fetch =
  let config =
    M.Client.config ~homeserver:(Uriz.of_string_exn "https://hs.example") ()
  in
  M.Client.create ~config ~fetch ~random:(M.Random.of_env secure_random)

let scripted log bodies =
  let remaining = ref bodies in
  Fetch_mock.client (fun req ->
      record log req;
      match !remaining with
      | [] -> Fetch_mock.respond "{}" req
      | body :: rest ->
          remaining := rest;
          Fetch_mock.respond body req)

let room_id = Id.Room_id.of_string_exn "!demo:hs.example"

let () =
  Eio_mock.Backend.run @@ fun () ->
  let log = ref [] in
  let fetch =
    scripted log
      [
        {|{"user_id":"@alice:hs.example","access_token":"tok","device_id":"DEV1"}|};
        {|{"event_id":"$1:hs.example"}|};
      ]
  in
  let client = client_of fetch in
  let session =
    match M.Auth.login_password client ~user:"alice" ~password:"pw" () with
    | Ok s -> s
    | Error e ->
        Printf.printf "login failed: %s\n" (M.Error.to_string e);
        exit 1
  in
  let logged_in = M.Client.with_session client session in
  (match M.Messages.send_text logged_in ~room_id ~body:"hello" () with
  | Ok event_id -> Printf.printf "sent %s\n" (Id.Event_id.to_string event_id)
  | Error e -> Printf.printf "send failed: %s\n" (M.Error.to_string e));
  print_log log;
  let log = ref [] in
  let fetch =
    Fetch_mock.client (fun req ->
        record log req;
        Fetch_mock.respond ~status:403
          {|{"errcode":"M_FORBIDDEN","error":"not a member of the room"}|} req)
  in
  let logged_in = M.Client.with_session (client_of fetch) session in
  match M.Messages.send_text logged_in ~room_id ~body:"hello" () with
  | Ok _ -> assert false
  | Error e -> Format.printf "error: %a\n" M.Error.pp e
```

`record`, which appends one entry describing a request to `log`, and
`print_log`, which renders that log, are the rest of the file.

<br>

[`Matrix_client.Client.config`](../../lib/matrix_client/client.mli) and
[`Client.create`](../../lib/matrix_client/client.mli) build a client from a
homeserver, a `fetch` and a
[`Matrix_client.Random.t`](../../lib/matrix_client/random.mli). Nothing here
opens a socket, so `Random.of_env` is given a fixed byte stream rather than
`env#secure_random`, which keeps the transaction identifier in the printed
`send` request the same on every run. `scripted` answers each request in
turn with the next body in the list, the same harness
`test/test_cs_api.ml` uses under the name `mock_seq`.

<br>

[`Matrix_client.Auth.login_password`](../../lib/matrix_client/auth.mli)
matches the first scripted body against the login endpoint and is `Ok`
carrying a `Client.session`.
[`Matrix_client.Client.with_session`](../../lib/matrix_client/client.mli)
attaches it, and
[`Matrix_client.Messages.send_text`](../../lib/matrix_client/messages.mli)
matches the second scripted body and is `Ok` carrying the event id it names.
`print_log` then prints both requests the mock recorded, in the order the
library made them.

<br>

The second mock answers every request with a 403 carrying an `M_FORBIDDEN`
error object. `send_text` decodes it into
[`Error.Matrix_error`](../../lib/matrix_client/error.mli), and `Error.pp`
renders it on one line. `r-errors` covers the constructor `matrix-chat.client`
uses for every other kind of failure a call can return.

<pre><code><b>$ dune exec -- example/r-mock/mock.exe</b>
sent $1:hs.example
POST https://hs.example/_matrix/client/v3/login {"type":"m.login.password","identifier":{"type":"m.id.user","user":"alice"},"password":"pw"}
PUT https://hs.example/_matrix/client/v3/rooms/!demo:hs.example/send/m.room.message/mAAECAwQFBgcICQoLDA0ODw {"msgtype":"m.text","body":"hello"}
error: Matrix error M_FORBIDDEN: not a member of the room
</code></pre>

<br>

**Next:** [`r-errors`](../r-errors#folders-and-files) provokes the same kind
of failure, and others, against a real homeserver.

**See also:** [`2-send`](../2-send#folders-and-files) sends a message the
same way through `matrix-chat.eio`, which wraps this layer and raises instead of
returning a result.

<br>

[Up to the example index](../#readme)

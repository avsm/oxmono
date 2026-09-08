# `r-errors`

<br>

`errors` logs in to a homeserver, then deliberately provokes three kinds of
failure through `Matrix_client` and prints which one each call returned. It
finishes with a retry loop that keeps failing until it gives up. It reads
`MATRIX_HOMESERVER`, `MATRIX_USER` and `MATRIX_PASSWORD` from the
environment and takes no arguments. A throwaway account is enough.

<br>

`matrix-chat.client` reports every failure as an `Error.t` rather than raising,
and `Error.t` has a case for each place a call can go wrong: the server
answered with a Matrix error object, the request never reached the server,
a client with no session was asked for something only a session gives, and
more besides. A caller that only checks `Ok` against `Error` cannot act on
that difference. This example matches on the cases that matter for a retry
policy, the ones worth trying again against the ones that need surfacing to
the user unchanged.

<br>

```ocaml
let ok = function
  | Ok v -> v
  | Error e ->
      Format.eprintf "unexpected error: %a\n%!" M.Error.pp e;
      exit 1

(* Only these two kinds of failure are worth trying again: the request never
   reached the server, or the server asked the caller to slow down. *)
let is_retryable = function
  | M.Error.Network_error _ -> true
  | M.Error.Matrix_error { errcode = M.Error.M_LIMIT_EXCEEDED; _ } -> true
  | _ -> false

let retry_after_ms = function
  | M.Error.Matrix_error { retry_after_ms; _ } -> retry_after_ms
  | _ -> None

let rec attempt client n =
  match M.Auth.whoami client with
  | Ok user_id -> Printf.printf "whoami: %s\n" (Id.User_id.to_string user_id)
  | Error e when (not (is_retryable e)) || n >= 3 ->
      Printf.printf "gave up after attempt %d: %s\n" n (M.Error.to_string e)
  | Error e ->
      (match retry_after_ms e with
      | Some ms -> Printf.printf "attempt %d failed, retrying after %dms\n" n ms
      | None -> Printf.printf "attempt %d failed, retrying\n" n);
      attempt client (n + 1)

let () =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  Eio_main.run @@ fun env ->
  let fetch = Fetch_httpz.std env in
  let random = M.Random.of_env env in
  let config = M.Client.config ~homeserver () in
  let client = M.Client.create ~config ~fetch ~random in
  let session = ok (M.Auth.login_password client ~user ~password ()) in
  let client = M.Client.with_session client session in

  let missing_alias = Id.Room_alias.of_string_exn "#does-not-exist:localhost" in
  (match
     M.Rooms.join client ~room_id_or_alias:(`Room_alias missing_alias) ()
   with
  | Error (M.Error.Matrix_error { errcode = M.Error.M_NOT_FOUND; _ }) ->
      Printf.printf "matrix error: room not found\n"
  | Error e -> Printf.printf "unexpected error: %s\n" (M.Error.to_string e)
  | Ok _ -> Printf.printf "unexpectedly joined\n");

  let anonymous = M.Client.create ~config ~fetch ~random in
  (match M.Presence.set_presence anonymous ~presence:M.Presence.Online () with
  | Error M.Error.No_session -> Printf.printf "no session, as expected\n"
  | Error e -> Printf.printf "unexpected error: %s\n" (M.Error.to_string e)
  | Ok () -> Printf.printf "unexpectedly answered\n");

  let closed_port =
    M.Client.config ~homeserver:(Uriz.of_string_exn "http://127.0.0.1:1") ()
  in
  let unreachable = M.Client.create ~config:closed_port ~fetch ~random in
  (match M.Auth.whoami unreachable with
  | Error (M.Error.Network_error msg) -> Printf.printf "network error: %s\n" msg
  | Error e -> Printf.printf "unexpected error: %s\n" (M.Error.to_string e)
  | Ok _ -> Printf.printf "unexpectedly answered\n");

  attempt unreachable 1
```

`getenv`, which reads a variable from the environment or exits with status 1
on a missing one, is as in `1-login` and is the rest of the file.

<br>

[`Fetch_httpz.std`](https://github.com/avsm/httpz) is a real `Fetch.t` over
TLS and plain sockets, and is the backend `matrix-chat.eio` uses by default.
[`Matrix_client.Client.create`](../../lib/matrix_client/client.mli) takes it
directly, so the program calls `Matrix_client.Auth`, `Rooms` and `Presence`
against the real homeserver without going through `matrix-chat.eio`'s raising
wrapper. `ok` unwraps a result the program expects to succeed, printing
[`Error.pp`](../../lib/matrix_client/error.mli) and exiting on the one call
that is not part of the demonstration, the login itself.

<br>

[`Matrix_client.Rooms.join`](../../lib/matrix_client/rooms.mli) on an alias
no room in the directory holds is
[`Error.Matrix_error`](../../lib/matrix_client/error.mli) carrying
`errcode = M_NOT_FOUND`, matched directly on the record rather than through
[`Error.errcode`](../../lib/matrix_client/error.mli) since the program
already knows which case it expects.

<br>

[`Matrix_client.Presence.set_presence`](../../lib/matrix_client/presence.mli)
acts as the logged-in user, so it needs the user id out of the client's
session before it can build the request. `anonymous` carries no session, and
the call fails with `Error.No_session` without a request ever leaving the
program.

<br>

`unreachable`'s homeserver names a port nothing listens on.
[`Matrix_client.Auth.whoami`](../../lib/matrix_client/auth.mli) against it
fails with `Error.Network_error`, carrying the text `Fetch`'s
`Connection_failure` raised. A Matrix error comes from the server;
a network error means the server was never reached at all, and `attempt`
treats the two differently.

<br>

`attempt` retries `whoami` against `unreachable`, which fails with
`Error.Network_error` on every call. `is_retryable` says a network error is
worth trying again, alongside `M_LIMIT_EXCEEDED`, whose `retry_after_ms`
`attempt` prints when the server sent one. Every other case, and a third
failure of a retryable one, ends the loop and reports the last error to the
user instead of trying again forever.

<pre><code><b>$ curl -s -X POST http://127.0.0.1:8008/_matrix/client/v3/register \
    -H 'Content-Type: application/json' \
    -d '{"username":"alice-e-77919d7e","password":"pw77919d7e","auth":{"type":"m.login.dummy"}}'</b>
{"user_id":"@alice-e-77919d7e:localhost","home_server":"localhost","access_token":"syt_...","device_id":"..."}
<b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USER=alice-e-77919d7e MATRIX_PASSWORD=pw77919d7e</b>
<b>$ dune exec -- example/r-errors/errors.exe</b>
matrix error: room not found
no session, as expected
network error: Eio.Io Http Connection_failure Refused Unix_error (Connection refused, "connect", ""),
  GET http://127.0.0.1:1/_matrix/client/v3/account/whoami (Accept: application/json)
attempt 1 failed, retrying
attempt 2 failed, retrying
gave up after attempt 3: Network error: Eio.Io Http Connection_failure Refused Unix_error (Connection refused, "connect", ""),
  GET http://127.0.0.1:1/_matrix/client/v3/account/whoami (Accept: application/json)
</code></pre>

<br>

**Next:** [`u-room-list`](../u-room-list#folders-and-files) moves up to
`matrix-chat.ui`, the reactive layer built on top of this one.

**See also:** [`r-mock`](../r-mock#folders-and-files) provokes the same
`M_FORBIDDEN` shape of failure against a mock backend, with no homeserver
needed.

<br>

[Up to the example index](../#readme)

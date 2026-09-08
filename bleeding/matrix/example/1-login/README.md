# `1-login`

<br>

`login` logs in to a homeserver with a password, prints the user id and the
device id of the new session, and logs out again. It reads the homeserver, the
user and the password from the environment and takes no arguments.

<br>

Matrix accounts live on homeservers, and a client talks only to its own
homeserver over HTTP. Every request after the first carries an access token
that identifies the account and the device making it, and obtaining that
token is what a login is. This example does nothing else, so that the shape
every later example shares is visible on its own.

```ocaml
module M = Matrix_eio

let getenv name =
  match Sys.getenv_opt name with
  | Some v -> v
  | None ->
      Printf.eprintf "missing environment variable %s\n" name;
      exit 1

let () =
  let homeserver = Uriz.of_string_exn (getenv "MATRIX_HOMESERVER") in
  let user = getenv "MATRIX_USER" in
  let password = getenv "MATRIX_PASSWORD" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.login_password ~sw ~env ~homeserver ~user ~password () in
  let user_id = M.Auth.whoami client in
  let device_id = M.Client.device_id client in
  Printf.printf "Logged in as %s on device %s\n%!"
    (Matrix_proto.Id.User_id.to_string user_id)
    (Matrix_proto.Id.Device_id.to_string device_id);
  M.Auth.logout client
```

<br>

`MATRIX_HOMESERVER` is the base URL of the homeserver. `MATRIX_USER` is the
localpart or the full user id of the account. `MATRIX_PASSWORD` is its
password. A missing variable is reported on standard error and the program
exits with status 1.

<br>

[`Matrix_eio.login_password`](../../lib/matrix_eio/matrix_eio.mli) performs
the `m.login.password` flow and returns a
[`Matrix_eio.Client.t`](../../lib/matrix_eio/client.mli) that holds the access
token the server issued. The switch owns the connection and every fiber the
library starts on it, and the environment is the one `Eio_main.run` passes to
its callback.

<br>

[`Matrix_eio.Auth.whoami`](../../lib/matrix_eio/auth.mli) asks the server which
user the access token belongs to. `Matrix_eio.Client.device_id` is the device
identifier the server assigned to this login, and it is read from the login
response without a further request.
[`Matrix_eio.Auth.logout`](../../lib/matrix_eio/auth.mli) invalidates the
access token, so the session cannot be used again.

<br>

A user id names an account and has the form `@localpart:server`. A device id
names one login session of that account, and the server assigns a new one at
each login. Later examples store encryption keys under the device id.

<br>

A request that fails raises `Eio.Io`. The program does not catch it, so a wrong
password prints the error and the program exits with a non-zero status.

<pre><code><b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USER=alice-a7f3 MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- example/1-login/login.exe</b>
Logged in as @alice-a7f3:localhost on device YELKMMAUHE
</code></pre>

<br>

**Next:** [`2-send`](../2-send#folders-and-files) sends a message to a room.

**See also:** [`r-mock`](../r-mock#folders-and-files) performs the same
login against a mock HTTP backend, with no server at all.

<br>

[Up to the example index](../#readme)

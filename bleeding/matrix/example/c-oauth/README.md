# `c-oauth`

<br>

`oauth` logs in with the OAuth 2.0 authorisation code grant instead of a
password, opening a browser for the one step that needs a human. It reads
the homeserver from `--homeserver` or `MATRIX_HOMESERVER` and takes no other
argument.

<br>

The password grant in `1-login` hands the client a secret the user typed,
which the client could mishandle or a phisher could ask for outside Matrix
entirely. OAuth 2.0 login moves the credential check to a page the
authorisation server itself renders, so the client never sees the password
and the account can grant a client only the access it agrees to in the
browser, revocable later without a password change. A homeserver that
delegates authentication this way expects every client to log in through a
browser rather than a `--password` flag.

```ocaml
module M = Matrix_eio
module Cli = Matrix_cli

let run () homeserver =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let client = M.connect ~sw ~env ~homeserver () in
  let open_url url =
    Logs.app (fun m -> m "Open this URL in a browser to authorise this client:");
    Logs.app (fun m -> m "");
    Logs.app (fun m -> m "  %s" url);
    Logs.app (fun m -> m "");
    Logs.app (fun m -> m "Waiting for the browser to come back...");
    M.Oauth.browser_opener ~sw ~env url
  in
  let session = M.Oauth.login_with_browser ~env client ~open_url () in
  let client = M.Client.with_session client session in
  Logs.app (fun m ->
      m "Logged in as %s on device %s"
        (Matrix_proto.Id.User_id.to_string (M.Auth.whoami client))
        (Matrix_proto.Id.Device_id.to_string session.device_id))
```

<br>

[`Matrix_eio.connect`](../../lib/matrix_eio/matrix_eio.mli) opens a client
with no session yet installed.
[`Matrix_eio.Oauth.login_with_browser`](../../lib/matrix_eio/oauth.mli) runs
the whole flow end to end. It discovers the authorisation server from
`/_matrix/client/v1/auth_metadata`, registers this client dynamically when no
`client_id` was given, binds a loopback listener on `127.0.0.1` and an
ephemeral port, and builds the authorisation URL. It hands that URL to
`open_url` rather than opening it, so the program prints it and asks
[`Oauth.browser_opener`](../../lib/matrix_eio/oauth.mli) to try the desktop's
URL handler as well.

Once the browser follows the redirect back to the loopback listener,
`login_with_browser` checks that the `state` it sent comes back unchanged,
exchanges the authorisation code for tokens and returns a session. The
session is not yet installed on `client`, which is why
[`Client.with_session`](../../lib/matrix_eio/client.mli) still has to run
before [`Auth.whoami`](../../lib/matrix_eio/auth.mli) can ask who it belongs
to. A failure anywhere in the flow raises `Eio.Io`, carrying `State_mismatch`,
`Denied` or `Not_registered` for a protocol failure and an `Error.err` for
anything lower.

There is no `--token` flag. A command line is readable by every other
process on the machine, the same reason `Cli.password_opt_term` refuses a
`--password` flag from `8-cli` on.

<br>

The throwaway Synapse this tree runs against has no OAuth 2.0 authorisation
server configured, so the transcript below fails at the discovery step.

<pre><code><b>$ dune exec -- example/c-oauth/oauth.exe --homeserver http://127.0.0.1:8008 -v</b>
oauth.exe: [WARNING] HTTP error: status=404 body={"errcode":"M_UNRECOGNIZED","error":"Unrecognized request"}
oauth: internal error, uncaught exception:
       Eio.Io HTTP 404: no OAuth 2.0 authorisation server: neither
       /auth_metadata nor m.authentication in the well-known
</code></pre>

Against a homeserver that delegates authentication, such as Matrix
Authentication Service, the same command instead prints an authorisation
URL, waits for the browser, and finishes with the same `Logged in as ... on
device ...` line every login example in this tutorial ends with.

<br>

**Next:** [`d-bot`](../d-bot#folders-and-files) turns `4-echo` into a
`matrix-chat.bot` plugin.

**See also:** the [`omatrix`](../../bin/omatrix) client runs the same flow
through `omatrix login --oauth`, and saves the session to a profile
afterwards.

<br>

[Up to the example index](../#readme)

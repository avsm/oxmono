# `a-verify`

<br>

`verify` runs SAS device verification, comparing seven emoji over an
authenticated channel and, once a person confirms they match on both
screens, marking the other device verified. Given a user id it sends the
request. Given `--listen` it waits for one instead. It reads `--homeserver`,
`--username`, `--password-file`, `--profile` and `-v` as in `8-cli`, plus
either a user id or `--listen`.

<br>

Encryption alone tells a device that a message came from some device holding
a particular identity key, not that the key belongs to the person the user
thinks they are talking to, since a compromised or malicious homeserver
could otherwise hand out a substitute key. Device verification closes that
gap by having the two people compare a value derived from both keys over a
channel the server cannot forge, seven emoji read aloud or typed in. Once a
device is verified, a user's other devices will share room keys with it
without asking again.

```ocaml
(* The SAS key agreement needs the peer's identity key, which only a
   [/keys/query] can supply, so both a request and an accept fetch it first. *)
let fetch_device enc client user_id =
  M.Encryption.track_users enc [ user_id ];
  M.Encryption.execute_requests enc client (M.Encryption.outgoing_requests enc)

(* after login and building [enc] as in 9-encrypt *)
(match target with
| Some target ->
    fetch_device enc client target;
    ignore (Vs.request ver client target)
| None -> Logs.app (fun m -> m "Waiting for a verification request..."));
let on_sync _ =
  if listen && not !accepted then
    List.iter
      (fun s ->
        if Flow.session_stage s = Flow.Requested then begin
          accepted := true;
          if ask "Accept? [y/N] " then begin
            fetch_device enc client (Flow.session_their_user_id s);
            Vs.accept ver client s
          end
          else Vs.cancel ver client s Cancel_code.User
        end)
      (Vs.sessions ver);
  match !result with
  | None -> M.Sync.Continue
  | Some r -> (* Encryption.save, print Verified or Cancelled *) M.Sync.Stop
in
M.run_sync ~sw ~env client ~on_sync ~encryption:enc ~verification:ver ()
```

`confirm`, which prints the emoji and asks whether they match, and `ask`,
which treats anything but an explicit yes as a no, are omitted.

<br>

[`Verification_service.create`](../../lib/matrix_eio/verification_service.mli)
drives the SAS state machine from the sync loop. It turns
`m.key.verification.*` to-device events into calls to
[`accept`](../../lib/matrix_eio/verification_service.mli),
[`cancel`](../../lib/matrix_eio/verification_service.mli) and the state
machine in between, and calls this program's `confirm` exactly once, with
the emoji to show, at the point a person has to look at the screen. It needs
an `encryption:` machine, built as in `9-encrypt`, because verification
traffic travels as to-device messages and the flow's result changes that
machine's record of which devices are verified.

<br>

`fetch_device` runs before both
[`request`](../../lib/matrix_eio/verification_service.mli) and
[`accept`](../../lib/matrix_eio/verification_service.mli). A side that skips
it has no record of the peer's identity key to check the exchange's MAC
against, and the flow cancels with `m.unknown_transaction` instead of
completing. `track_users` marks the target's device list outdated, and the
following `execute_requests` call fetches it with `/keys/query`, so neither
side depends on having synced with the other before.

<br>

[`run_sync`](../../lib/matrix_eio/matrix_eio.mli) with both `~encryption`
and `~verification` routes every `m.key.verification.*` event to the service
automatically. `on_sync` watches
[`Vs.sessions`](../../lib/matrix_eio/verification_service.mli) for an
incoming `Requested` flow to ask about, and checks the `result` ref
`on_result` filled in. That has to happen inside `on_sync`, since it runs on
the sync fiber and code placed after `run_sync` in `run` would execute
before the flow finishes.

<pre><code><b>$ export XDG_DATA_HOME=$(mktemp -d) MATRIX_PASSWORD=pw12345</b>
<b>$ dune exec -- omatrix login -s http://127.0.0.1:8008 -u vy5a-6370 --profile alice</b>
Session saved to profile 'alice'
<b>$ dune exec -- example/a-verify/verify.exe --homeserver http://127.0.0.1:8008 \
    --username vy5b-6370 --profile bob --listen -v</b>
Waiting for a verification request...
</code></pre>

From another terminal, the [`omatrix`](../../bin/omatrix) client sends the
request.

<pre><code><b>$ dune exec -- omatrix verify --profile alice '@vy5b-6370:localhost'</b>
Verification request sent to @vy5b-6370:localhost; waiting for them to accept...

Verifying @vy5b-6370:localhost on device UJIOSIMXHW

    ☂️  Umbrella
    🍎  Apple
    ✂️  Scissors
    ... (seven in all, the same seven the other side sees)

Do they match? [y/N] y
Verified @vy5b-6370:localhost (UJIOSIMXHW).
</code></pre>

Back in the first terminal, at the same moment.

<pre><code>@vy5a-6370:localhost wants to verify with you.
Accept? [y/N] y

Verifying @vy5a-6370:localhost
  ☂️  Umbrella
  🍎  Apple
  ✂️  Scissors
  ...
Do the emoji match on both screens? [y/N] y
Verified @vy5a-6370:localhost (WHMVAHRKJD)
</code></pre>

<br>

**Next:** [`b-backup`](../b-backup#folders-and-files) backs up room keys so
a new, unverified device can still read old messages.

**See also:** the [`omatrix`](../../bin/omatrix) client offers `verify` and
`verify --listen` as the same two sides of this example.

<br>

[Up to the example index](../#readme)

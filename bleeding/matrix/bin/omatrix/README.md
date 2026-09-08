# omatrix

<br>

`omatrix` is the installed command-line Matrix client, covering session
persistence, messages, sync, end-to-end encryption, device verification and
key backup. Where the [numbered tutorial](../../example/README.md) builds
one concept per program, `omatrix` puts those concepts behind one
`cmdliner` binary.

<pre><code><b>$ omatrix --help=plain</b>
NAME
       omatrix - Command-line Matrix client

COMMANDS
       login    Authenticate with a homeserver and store session (password,
                or --oauth for the browser flow)
       logout   Clear stored session and invalidate token
       whoami   Show current session information
       msg      Send a message to a room or user, encrypting when the room
                is encrypted
       sync     Follow the timeline, decrypting as it goes
       keys     Create or restore this device's encryption keys
       verify   Verify another device with emoji (SAS)
       backup   Manage the server-side room key backup
</code></pre>

<br>

Every subcommand takes `--profile NAME`, defaulting to `default`, and stores
the session, this device's Olm account and Megolm sessions, and the backup
state under `$XDG_DATA_HOME/matrix/profiles/NAME/`. It is the same directory
[`matrix-bot`](../matrix-bot) uses, so a human and a bot can share one
profile store on the same machine, in different profiles.
A password reaches `omatrix` through `--password-file FILE` or
`$MATRIX_PASSWORD`. There is no flag that takes the password itself, because
a command line is readable by every other process on the machine.

<br>

`login -s URL -u USER` authenticates with a password read from
`--password-file` or `$MATRIX_PASSWORD`, and saves the session to the
profile. `login --oauth -s URL` runs the OAuth 2.0 authorisation code flow
instead, and needs no username or password. `whoami` prints the stored
profile, user id, device id, homeserver and last-used time. `logout` clears
the stored session and revokes the access token, unless `--local` is given.
This corresponds to [`7-profile`](../7-profile).

<pre><code><b>$ export MATRIX_PASSWORD=omatrixpw123</b>
<b>$ omatrix login -s http://127.0.0.1:8008 -u omatrixdemo -v</b>
omatrix.exe: [INFO] Logging in as omatrixdemo
Session saved to profile 'default'
User ID: @omatrixdemo:localhost
Device ID: HLWGIEFEDE
<b>$ omatrix whoami</b>
Profile: default
User ID: @omatrixdemo:localhost
Device ID: HLWGIEFEDE
Homeserver: http://127.0.0.1:8008
<b>$ omatrix logout</b>
Session cleared for profile 'default'
</code></pre>

`msg -t USER "text"` finds or creates a direct-message room, and `msg -r
ROOM "text"` sends to an existing one. The room's own `m.room.encryption`
state decides whether the message is encrypted, not a flag, so the same
command works in either kind of room, and it prints the sent event id. This
corresponds to [`2-send`](../2-send).

<pre><code><b>$ omatrix msg -t @peer:localhost "Hello from the omatrix README!" -v</b>
omatrix.exe: [INFO] Created room !eXLEqGhFJhOMWFPXFG:localhost
Message sent (event ID: $RM_qdPKsuty7lPR6futp1-hPaX0h_dgXs35QVW8RTd0)
</code></pre>

`keys init` generates this device's Olm account if it has none, publishes
its device keys, and prints its Ed25519 fingerprint in groups of four for a
person to read aloud. Both `msg` in an encrypted room and `sync` need this
to have run once per profile. This corresponds to
[`9-encrypt`](../9-encrypt).

<pre><code><b>$ omatrix keys init</b>
Profile:     default
User ID:     @omatrixdemo:localhost
Device ID:   HLWGIEFEDE
Fingerprint: 5tkt ymQB eJmn UzRy zRYi l2pn /Itc SfTK r/lJ yS1f eJc
Identity:    S4VEBkzQ1treI+jsB9l07A/LtgjcxdqY9V4W3WI4u1o

Generated a new Olm account and published its device keys.
</code></pre>

`sync` follows the timeline and prints each new event as `[room] sender:
body`, one response at a time with `--count N` or until interrupted without
it. `--no-encryption` skips loading the encryption keys and prints an
encrypted event as ciphertext. A decrypted event is marked `*`, and one
whose Megolm session has not yet arrived is marked `!`. This corresponds to
[`3-sync`](../3-sync).

<pre><code><b>$ omatrix sync --count 1</b>
  [Empty Room] @omatrixdemo:localhost: &lt;m.room.create&gt;
  [Empty Room] @omatrixdemo:localhost: &lt;m.room.member&gt;
  [Empty Room] @omatrixdemo:localhost: Hello from the omatrix README!
</code></pre>

`verify USER [DEVICE]` sends a verification request and prints seven emoji
for a person to compare against the other screen, then asks whether they
match. `verify --listen` waits for a request instead of sending one. A
verified device is the only one this client will forward room keys to. This
corresponds to [`a-verify`](../a-verify).

`backup enable` creates a server-side backup, uploads this device's room
keys, and prints the recovery key exactly once. `backup status` reports the
server's version and count, whether this device is backing up, and how many
sessions are pending. `backup restore RECOVERY_KEY` reads an existing backup
into the current profile. This corresponds to
[`b-backup`](../b-backup).

<pre><code><b>$ omatrix backup enable</b>
Backup version 1 created.
0 room keys uploaded.

Recovery key:

    EsTR jwph iqy6 uV3V 5NCo DVj4 W3g2 kHmF fi3x v8XQ tTJy H1fu

Write this down. It is the only thing that can read the backup, it is not
stored on the server, and this is the only time omatrix will show it.
<b>$ omatrix backup status</b>
Server:  version 1, 0 keys
         algorithm m.megolm_backup.v1.curve25519-aes-sha2, etag 0
Local:   backing up to version 1
Pending: 0 sessions not yet uploaded
</code></pre>

<br>

**See also:** [`matrix-bot`](../matrix-bot) runs unattended over the same
kind of profile.

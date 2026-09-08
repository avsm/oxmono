# jmap-mosaic

`jmap-mosaic` reads, labels, files and replies to mail over JMAP in the
terminal. It is a demonstration of the `jmap` library at the size of a program
rather than of a tutorial step, not a mail client to live in: it holds no
local store, refetches rather than syncing, and shows the plain text body
only.

Lists show at most the newest 100 messages and do not yet paginate. Each plain
text body value is limited to 64 KiB. There is no HTML rendering, attachment
viewer, new-message composer, or offline sync. These are limits of the demo;
the underlying JMAP library has paging, body-part and attachment APIs.

The user interface is [Mosaic](https://github.com/tmattio/mosaic) in the Elm
Architecture. `Jmap_mosaic.Model` is the state, the messages that change it
and the JMAP work it wants done, and it mentions neither the terminal nor the
network. `Jmap_mosaic.View` draws a model and reads the keyboard.
`Jmap_mosaic.Io` makes the connection the profile or login screen asks for,
performs one unit of that work against it inside an Eio fiber and dispatches
the messages it produces. `Jmap_mosaic.Login` adapts the shared
`Jmap_eio.Profile` store to the login screen.

## Running

On its first run the client opens on a login screen. A successful login creates
a named profile; later runs open on the profile picker and connect without
asking for the credential again. It also takes a session URL and credential
from the command line or environment, and connects directly when both are
given.

```sh
export JMAP_SESSION_URL=https://api.fastmail.com/.well-known/jmap
export JMAP_API_KEY=<token>
export JMAP_AUTH=bearer
jmap-mosaic
```

Against the Cyrus test server of `test/oracle`, which speaks cleartext HTTP
and basic authentication:

```sh
export JMAP_SESSION_URL=http://localhost:18080/.well-known/jmap
export JMAP_AUTH=basic
export JMAP_API_KEY=user1:x
jmap-mosaic --allow-insecure
```

`--profile NAME` (or `JMAP_PROFILE=NAME`) connects directly with a saved
profile. `--account` names the account to act on and defaults to the primary
mail account of the session. A mail-only shared account can be browsed and
filed normally; replying needs the JMAP submission capability on that account.
`jmap-mosaic --help` describes the rest.

## Login

With no saved profiles, session URL or key, the client opens on the login
screen instead of the mailboxes.

```
        ┌─Sign in──────────────────────────────────────────────────┐
        │ Profile      personal                                    │
        │ Session URL  http://localhost:18080/.well-known/jmap     │
        │ Auth         basic                                       │
        │ Username     user2                                       │
        │ Password     •••••                                       │
        │                                                          │
        └──────────────────────────────────────────────────────────┘
```

The Username field is shown for the basic scheme only, a bearer token having
no user. The password or token is never echoed; the box draws one bullet for
each character. While the connection is being made the box says `connecting`
and takes no keys. A failed connection leaves the form as it was with the
reason under it and clears the secret.

| Key | Effect |
| --- | --- |
| `Tab` | Move to the next field, as does the down arrow |
| `Shift-Tab` | Move to the previous field, as does the up arrow |
| `Ctrl-A` | Switch between basic and bearer |
| `Space` | Switch between basic and bearer, on the Auth field |
| `Backspace` | Rub out the last character of the field |
| `Enter` | Connect |
| paste | Insert the pasted text into the field in hand, less any line ending |
| `Esc` | Return to the profile picker, or quit when there are no profiles |

A login that succeeds is saved in
`$XDG_CONFIG_HOME/jmap/profiles/NAME`, under `~/.config/jmap/profiles` when
`XDG_CONFIG_HOME` is unset. This is the shared `Jmap_eio.Profile` store, so the
same login can be selected by other JMAP programs. The file holds the URL,
authentication scheme, username and bearer token or password in plain text.
The directory is created with mode 0700 and every profile with mode 0600. An
existing directory with group or other permissions is ignored without changing
its mode, so the credential is readable only by the user. Profile names may contain letters,
digits, `.`, `-` and `_`.

The startup picker uses `j`/`k` or the arrows to choose a profile, `Enter` to
connect, `n` to create one and `e` to edit one. `q` or `Esc` quits.

Setting `JMAP_SESSION_URL` and `JMAP_API_KEY`, or passing `--url` and
`--api-key`, skips the screen and connects at once. `JMAP_API_KEY_FILE` and
`--api-key-file` name a file whose first line holds the secret instead, and
`JMAP_AUTH` or `--auth` chooses the scheme. A basic key is `user:password`,
whose halves fill the Username and Password fields. Anything given this way
wins over the chosen profile, and a setting that is missing is asked for on
the screen.

## Smart searches

The first two rows in the navigation pane are server-side searches across the
account:

- **Unread** matches mail without `$seen`.
- **Unanswered >30d** matches mail received more than 30 days ago without
  `$answered`, excluding drafts.

Press `1` or `2` from either pane to open them immediately. They otherwise act
like folders for navigation and reading. Filing is disabled from a smart search
because a result may belong to more than one mailbox; switch to a mailbox first.

## Keys

| Key | Effect |
| --- | --- |
| `Tab` | Move the focus between the mailboxes and the messages |
| `j` `k` | Move down and up in the focused pane, as do the arrows |
| `Enter` | Open the selected mailbox or message |
| `1` / `2` | Open Unread / Unanswered >30d |
| `Esc` | Go back one screen |
| `R` | Reload the mailboxes and the current list |
| `u` | Toggle `$seen` on the current message |
| `f` | Toggle `$flagged` on the current message |
| `m` | File the current message into another mailbox |
| `r` | Reply to the open message |
| `Ctrl-S` | Send the reply |
| `Esc` | Abandon the reply |
| `q` | Quit, from the mailbox screen |

Opening a message marks it `$seen`. A reply is prefilled with the quoted text
body, is addressed from `Reply-To` or `From`, carries `In-Reply-To` and
`References` built from the message it answers, and sets `$answered` on it.

## Testing

`dune test mosaic` runs the model under Alcotest with scripted messages and no
terminal or server.

`mosaic/test/headless.ml` is the other half. It logs in with a credential the
server refuses and then with the right one, performs each action of the client
once against a live account with a scripted dispatch, prints the messages that
came back, and undoes what it did. It writes to the account, so it is pointed
at a test server. It needs seeded mail, another mailbox for filing, and a
usable sending identity. The current Cyrus fixture returns an empty identity
address and does not support `Identity/set`, so its headless reply step cannot
run without server-side identity configuration.

```sh
dune exec mosaic/test/headless.exe
```

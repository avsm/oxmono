# `m-organise`

<br>

This step is the filing a mail client does when the user drags a selection out
of the Inbox into a project folder and flags it. It moves the three newest
messages into a mailbox of its own, flags all three, lists the mailbox, then
puts them back and destroys the mailbox.

The frame is the one from [**`1-session`**](../1-session#readme). Every write
goes through one function:

```ocaml
let file client ~account_id ?if_in_state what update =
  let set =
    Client.call_exn client (Chain.email_set ~account_id ?if_in_state ~update ())
  in
  (match Proto.Method.set_failures set with
  | f :: _ ->
      Fmt.failwith "%s: Email/set refused %a" what Proto.Method.pp_set_failure f
  | [] -> ());
  Fmt.pr "%s %d message(s), state %s -> %s@." what
    (List.length (Option.value set.updated ~default:[]))
    (Option.value set.old_state ~default:"?")
    set.new_state;
  set
```

<br>

The `update` argument of a `/set` is a map from record id to PatchObject, so one
call changes as many messages as the user selected and the server applies the
patches in an order of its own choosing
([RFC 8620 §5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3)).
[**`7-keywords`**](../7-keywords#readme) covers the PatchObject and
[`Proto.Patch`](../../lib/proto/proto_patch.mli). A record the server refuses is
reported under `notUpdated` rather than failing the request, so `file` stops on
the first entry of
[`Proto.Method.set_failures`](../../lib/proto/proto_method.mli).

A `/set` response reports the state of the type before and after it ran, in
`oldState` and `newState`. A client stores the `newState` to ask
`Email/changes` what moved, which is [**`n-resync`**](../n-resync#readme), and
hands it to the next `/set` as `ifInState` to be told rather than to overwrite
if someone else wrote in between. The Cyrus oracle moves an account's state on
once per record touched.

<br>

Filing is two bulk updates, the second carrying the state the first returned:

```ocaml
  let move id =
    ( id,
      Proto.Patch.v
        [ Patch.add_to_mailbox folder; Patch.remove_from_mailbox inbox ] )
  in
  let moved = file client ~account_id "filed" (List.map move inbox_ids) in
  let flag id = (id, Proto.Patch.v [ Patch.set_keyword `Flagged ]) in
  ignore
    (file client ~account_id ~if_in_state:moved.new_state "flagged"
       (List.map flag inbox_ids));
```

An Email holds its mailboxes in `mailboxIds`, a map from Mailbox id to `true`
([RFC 8621 §4.1.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.1)), so
moving a message is one patch at `mailboxIds/<destination>` and another at
`mailboxIds/<source>`, the second set to `null` for a removal.
[`Proto.Email.Patch`](../../lib/mail/mail_email.mli) writes both as
`add_to_mailbox` and `remove_from_mailbox`.
[`Proto.Email.mailbox_list`](../../lib/mail/mail_email.mli) reads the map the
other way, as the ids it sends to `true`, beside the `keyword_list` of
[**`7-keywords`**](../7-keywords#readme).

The two travel in the same patch because an Email "MUST belong to at least one
Mailbox" for as long as it exists (RFC 8621 §4.1.1), and the oracle answers a
patch that empties `mailboxIds` by removing the message. A patch names only the
paths it changes, so the move says nothing about keywords and the flagging
nothing about mailboxes, and an id survives a move, so the ids read from the
Inbox are the ids to file, list and restore. The mailbox they move into is
created first, by the creation id of
[**`8-mailboxes-set`**](../8-mailboxes-set#readme), named with the process id so
that two runs do not collide.

<br>

Reading the mailbox back is the query and get pair of
[**`3-inbox`**](../3-inbox#readme), filtered on `inMailbox`:

```ocaml
  let Results.[ query; filed ] =
    Client.run_exn client
      Chain.(
        let* q =
          email_query ~account_id
            ~filter:(Proto.Email.filter ~in_mailbox:folder ())
            ~calculate_total:true ()
        in
        let+ g =
          email_get ~account_id ~ids:(from_query q)
            ~properties:[ `Id; `Subject; `Keywords; `Mailbox_ids ]
            ()
        in
        Handles.[ q; g ])
  in
  Fmt.pr "%S holds %a message(s)@.%a@." folder_name
    Fmt.(option ~none:(any "?") int64)
```

[`Proto.Email.filter`](../../lib/mail/mail_email.mli) builds the FilterCondition
of [RFC 8621 §4.4.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.1) from
the fields given, `inMailbox` here holding a single Mailbox id, and
`~calculate_total:true` asks for the size of the whole result rather than of the
page returned. The program prints that count and the messages behind it, then
checks that each carries `$flagged` and is in the new mailbox alone, and exits
non-zero if not.

<br>

Unfiling and destroying the mailbox are the finaliser of a `Fun.protect` around
the filing, so an account is left as it was found however the program ends:

```ocaml
  let unfile () =
    let restore (e : Proto.Email.t) =
      ( Option.get e.id,
        Proto.Patch.v
          [
            Patch.set_mailboxes (Proto.Email.mailbox_list e);
            Patch.set_keywords (Proto.Email.keyword_list e);
          ] )
    in
```

The finaliser then prints how many mailboxes were destroyed and any refusal the
server reported. `set_mailboxes` and `set_keywords` replace a whole map rather
than patching one key of it, so the restore puts each message back into exactly
the mailboxes and keywords it was read with, whatever the run did in between.

The messages leave the mailbox before it is destroyed, so `Mailbox/set` needs no
`onDestroyRemoveEmails`, and a mailbox that still held mail would be refused
with `mailboxHasEmail`
([RFC 8621 §2.5](https://www.rfc-editor.org/rfc/rfc8621#section-2.5)).

<br>

<pre><code><b>$ dune exec -- examples/m-organise/organise.exe --allow-insecure</b>
Inbox
Mcb99bd4700327fe55f3fe592  run-examples oracle seed 2/3 [] in [A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E]
M8056379505da921de34a8509  run-examples oracle seed 1/3 [] in [A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E]
M4486c0260ac1fda7cdf2a917  run-examples oracle seed 3/3 [] in [A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E]
created "tutorial-3548704-project" (66317e4e-d5ae-41f2-b3b0-f37acdef4411)
filed 3 message(s), state 2106 -> 2112
flagged 3 message(s), state 2112 -> 2115
"tutorial-3548704-project" holds 3 message(s)
Mcb99bd4700327fe55f3fe592  run-examples oracle seed 2/3 [$flagged] in [66317e4e-d5ae-41f2-b3b0-f37acdef4411]
M8056379505da921de34a8509  run-examples oracle seed 1/3 [$flagged] in [66317e4e-d5ae-41f2-b3b0-f37acdef4411]
M4486c0260ac1fda7cdf2a917  run-examples oracle seed 3/3 [$flagged] in [66317e4e-d5ae-41f2-b3b0-f37acdef4411]
restored 3 message(s), state 2115 -> 2121
destroyed 1 mailbox(es)
</code></pre>

The three messages keep their ids across the move, and the account is left as
it was found, so the program may be run over and over.

<br>

**Next steps:**

- The next example, [**`n-resync`**](../n-resync#readme), takes the states these
  writes returned and brings a local cache up to date from them.
- [**`o-watch`**](../o-watch#readme) is told about a change like this one as it
  happens.

<br>

[Up to the tutorial index](../#readme)

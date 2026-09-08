# `7-keywords`

<br>

This step is the first one that changes the account. It marks the newest message
in the Inbox as read, prints the keywords before and after, and puts them back
exactly as they were.

The frame is the one from [**`1-session`**](../1-session#readme). The two
`Email/set` calls are what is new:

```ocaml
let update client ~account_id ?if_in_state id patch =
  Fmt.pr "  patch %a@."
    Fmt.(list ~sep:sp string)
    (List.map fst (Proto.Patch.to_list patch));
  let set =
    Client.call_exn client
      (Chain.email_set ~account_id ?if_in_state ~update:[ (id, patch) ] ())
  in
  (match Proto.Method.set_failures set with
  | (id, e) :: _ ->
      Fmt.failwith "Email/set left %a alone, %a" Proto.Id.pp id
        Proto.Error.Set_error.pp e
  | [] -> ());
  Fmt.pr "  new state %s@." set.new_state
```

```ocaml
      let _, before = keywords client ~account_id email_id in
      Fmt.pr "%a@.before    %a@." Proto.Id.pp email_id pp_keywords before;
      update client ~account_id email_id
        (Proto.Patch.v [ Proto.Email.Patch.set_keyword `Seen ]);
      let state, after = keywords client ~account_id email_id in
      Fmt.pr "after     %a@." pp_keywords after;
      update client ~account_id ~if_in_state:state email_id
        (Proto.Patch.v [ Proto.Email.Patch.set_keywords before ]);
      Fmt.pr "restored  %a@." pp_keywords
        (snd (keywords client ~account_id email_id))
```

<br>

Every write in JMAP goes through one method. `Foo/set` takes a `create` map, an
`update` map and a `destroy` list, and applies all three in that order
([RFC 8620 §5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3)). The value
of an `update` entry is a *PatchObject* rather than a whole record, so a client
changes one property without resending the rest and without overwriting a change
someone else made in the meantime.

A PatchObject is an unordered set of patches whose keys are JSON Pointers with
the leading `/` implied, and whose values are what to set, with `null` meaning
remove. [`Proto.Patch`](../../lib/proto/proto_patch.mli) holds a validated one
and checks the two restrictions the specification places on it, that no pointer
may reach inside an array and that no pointer may be a prefix of another.
[`Proto.Email.Patch`](../../lib/mail/mail_email.mli) builds the entries an Email
takes, so the pointer `keywords/$seen` is written ``set_keyword `Seen`` and
never spelled out.

A keyword is a case insensitive string, and
[RFC 8621 §4.1.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.1.1)
registers eight of them, `$seen` among them.
[`Proto.Keyword`](../../lib/mail/mail_keyword.mli) gives each a constructor and
leaves anything else as `` `Custom ``, validating the octets a custom keyword
may hold before the server has to reject it. On the wire the property is a map
to `true`, a JSON object being the only unordered set the format has, so
[`Proto.Email.keyword_list`](../../lib/mail/mail_email.mli) is what reads it as
the set it stands for.

<br>

The response of a `/set` reports each record separately. A record that was
changed appears under `updated` and one that was refused appears under
`not_updated` with a SetError saying why, so a partial failure is a value to
read rather than an error for the whole request
([RFC 8620 §5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3), which
[RFC 8621 §4.6](https://www.rfc-editor.org/rfc/rfc8621#section-4.6) adopts
unchanged for `Email/set`). A caller that only wants to know whether anything
was refused reads
[`Proto.Method.set_failures`](../../lib/proto/proto_method.mli), which is
`not_created`, `not_updated` and `not_destroyed` run together. Each entry is a
record id and the SetError the `/set` reported for it.
[`Proto.Method.pp_set_failure`](../../lib/proto/proto_method.mli) prints the
pair. The message here names the id itself, so it prints the SetError alone with
[`Proto.Error.Set_error.pp`](../../lib/proto/proto_error.mli).

The second `/set` carries `~if_in_state`, the argument of RFC 8620 §5.3 that
names the state the client believes the type is in. The state comes from the
`Email/get` just before it, so the restore is refused with a `stateMismatch`
error, changing nothing, if anyone else touched an Email in the account in
between. Without it the restore would silently overwrite whatever they did. A
client that is told `stateMismatch` re-reads the records, works out what
changed, and decides again.

The restore uses `set_keywords before`, which replaces the whole `keywords` map
with the one the message started with. ``remove_keyword `Seen`` would undo this
particular run, but replacing the map leaves the message identical however many
times the program is run, including on a message that was already read.

The Inbox is found by
[`Sync.mailbox_id_exn`](../../eio/sync.mli), the role lookup of
[**`3-inbox`**](../3-inbox#readme), which sends a `Mailbox/query` and a
`Mailbox/get` in a request of its own. A role belongs to at most one Mailbox of
an account ([RFC 8621 §2](https://www.rfc-editor.org/rfc/rfc8621#section-2)), so
the answer is a single id and an account without one is an error.

<br>

<pre><code><b>$ dune exec -- examples/7-keywords/keywords.exe --allow-insecure</b>
M7a3904e7e0bbf143bc120da7
before    (none)
  patch keywords/$seen
  new state 1812
after     $seen
  patch keywords
  new state 1813
restored  (none)
</code></pre>

Cyrus numbers its states, and each `/set` moves the account on by one, so the
two calls leave the account two states further along than it started.

<br>

**Next steps:**

- The next example, [**`8-mailboxes-set`**](../8-mailboxes-set#readme), creates
  records rather than changing them.
- [**`e-changes`**](../e-changes#readme) takes the state strings this step
  printed and asks the server what moved between them.

<br>

**See also:**

- [**`m-organise`**](../m-organise#readme) files a message with one patch that sets a
  keyword and moves it between mailboxes at the same time.

<br>

[Up to the tutorial index](../#readme)

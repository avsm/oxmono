# `8-mailboxes-set`

<br>

This step creates two mailboxes in one call, the second of them inside the
first, and then destroys both. Neither mailbox has an id when the request is
written, so the child names its parent by the *creation id* the client chose for
it.

The frame is the one from [**`1-session`**](../1-session#readme). The creating
call, the reading of what it created, and the destroying call are what is new:

```ocaml
  let name what = Printf.sprintf "tutorial-%d-%s" (Unix.getpid ()) what in
  let parent = Proto.Mailbox.creation "parent"
  and child = Proto.Mailbox.creation "child" in
  let created =
    Client.call_exn client
      (Chain.mailbox_set ~account_id
         ~create:
           [
             (parent, Proto.Mailbox.create_exn ~name:(name "parent") ());
             ( child,
               Proto.Mailbox.create_exn ~name:(name "child")
                 ~parent_id:(Proto.Id.creation_ref parent)
                 () );
           ]
         ())
  in
```

```ocaml
  let created_id cid =
    Option.bind (Proto.Method.created created cid) Proto.Mailbox.id
  in
  let ids = List.filter_map created_id [ child; parent ] in
  let destroy () =
    let gone =
      Client.call_exn client
        (Chain.mailbox_set ~account_id ~destroy:(Chain.ids ids)
           ~on_destroy_remove_emails:true ())
    in
```

<br>

The `create` argument of a `/set` is a map from a creation id the client invents
to the object to create
([RFC 8620 §5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3)).
[`Proto.Mailbox.creation`](../../lib/mail/mail_mailbox.mli) makes one such id as
a token typed by the record it names, so a token minted for a mailbox cannot key
an `Email/set` and a misspelling in a later reference to it does not compile.
Every record type has one, `Proto.Email.creation`, `Proto.Identity.creation` and
`Proto.Submission.creation` among them, and the untyped
[`Proto.Id.creation`](../../lib/proto/proto_id.mli) takes its type from the first
`/set` that uses it.

That token appears on the wire in two forms. As the key of the `create` map it
is the bare creation id, and everywhere the id is *used*, in another property of
the same call or in a later call of the same request, it is the same string
prefixed with `#`, which is `Proto.Id.creation_ref`. A `#` is outside the
alphabet [RFC 8620 §1.2](https://www.rfc-editor.org/rfc/rfc8620#section-1.2)
gives an Id, so decoding a `#` out of a server response is refused.

The object itself comes from
[`Proto.Mailbox.create`](../../lib/mail/mail_mailbox.mli), which takes only the
properties a client is allowed to set and checks them against
[RFC 8621 §2](https://www.rfc-editor.org/rfc/rfc8621#section-2), so an empty
name is caught before the request is sent. The counts and rights a server
maintains cannot be given at all. It returns a `Proto.Mailbox.t`, which is what
[`Chain.mailbox_set`](../../lib/core/chain.mli) takes, `create` being a list of
records rather than of JSON. `Proto.Mailbox.create_exn` makes the same checks
and raises `Invalid_argument` rather than returning a result, for the names a
program builds itself and knows to be good, as these two are.

The server resolves every creation reference before it builds the response, so
what comes back in `created` is keyed by creation id and carries real ids.
[`Proto.Method.created`](../../lib/proto/proto_method.mli) reads one record out
of that map by its token. It holds the properties the server set and nothing the
client already knew, so this program reads the names and parents back with a
separate `Mailbox/get`.

<br>

Destroying is ordered. A mailbox that still has a child is refused with
`mailboxHasChild`
([RFC 8621 §2.5](https://www.rfc-editor.org/rfc/rfc8621#section-2.5)), so the
`destroy` list names the child first. `~on_destroy_remove_emails:true` is the
extra argument the same section gives `Mailbox/set`, destroying the messages a
mailbox held rather than failing with `mailboxHasEmail`. It runs from a
`Fun.protect` finaliser and the names carry the process id, so the program
leaves nothing behind and may be run twice over.

<br>

<pre><code><b>$ dune exec -- examples/8-mailboxes-set/mailboxes_set.exe --allow-insecure</b>
created, new state 1817
  #parent -> d78e443d-383e-460b-8d5a-80b7963a961c
  #child -> 4d40b56f-2276-4afc-8464-dd8e66f43322
  tutorial-560389-parent     parentId=(top level)
  tutorial-560389-child      parentId=d78e443d-383e-460b-8d5a-80b7963a961c
destroyed 2 mailbox(es)
</code></pre>

There is no result reference to the records a `/set` creates. `created` is an
object keyed by creation id, and
[RFC 8620 §3.7](https://www.rfc-editor.org/rfc/rfc8620#section-3.7) lets `*` map
through an array only. A later call in the same request names a new record by
its creation reference instead.

<br>

**Next steps:**

- The next example, [**`9-errors`**](../9-errors#readme), asks the server for
  things it will refuse, at all three levels it can refuse them.
- [**`a-send`**](../a-send#readme) uses a creation reference across two method
  calls to submit a draft it creates in the same request.

<br>

**See also:**

- [**`m-organise`**](../m-organise#readme) creates a mailbox, files a message into it and
  destroys both again.

<br>

[Up to the tutorial index](../#readme)

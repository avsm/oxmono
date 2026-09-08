# `9-errors`

<br>

A JMAP failure arrives at one of three levels. The whole request can be
refused, one method call inside an otherwise good request can be refused, or
one record inside an otherwise good method call can be refused. This step asks
the server for four things it will not do and prints what comes back at each
level. The frame is the one from [**`1-session`**](../1-session#readme), and
this is the one step that steps out of it. Elsewhere a failure is raised and
[`Cli.main`](../../eio/cli.mli) prints it and exits 1. Here each demonstration
takes the result form, or wraps the call in `Chain.attempt`, and prints the
failure itself.

```ocaml
  let client = ctx.client and account_id = ctx.account_id in
  let urn = "urn:example:jmap:nonexistent" in
  (match
     Client.call client ~capabilities:[ urn ]
       (Chain.mailbox_get ~account_id ~properties:[ `Id ] ())
   with
  | Ok _ -> Fmt.pr "1. request   the server accepted %s@." urn
  | Error (Client.Jmap_error e) ->
      Fmt.pr "1. request   %a@." Proto.Error.Request_error.pp e;
      Fmt.pr "   status    %a@." Fmt.(option ~none:(any "none") int) e.status
  | Error e -> Fmt.pr "1. request   %a@." Client.pp_error e);

  let unsupported =
    Chain.email_query ~account_id
      ~sort:[ Proto.Filter.comparator "unsupportedProperty" ]
      ~limit:1L ()
  in
  (match Client.call client unsupported with
  | Ok _ -> Fmt.pr "2. method    the server sorted by unsupportedProperty@."
  | Error (Client.Method_error e) ->
      Fmt.pr "2. method    %a@." Proto.Error.Method_error.pp e
  | Error e -> Fmt.pr "2. method    %a@." Client.pp_error e);
  let Results.[ sorted; boxes ] =
    Client.run_exn client
      Chain.(
        let* q = unsupported in
        let+ m = mailbox_get ~account_id ~properties:[ `Id ] () in
        Handles.[ attempt q; m ])
  in
  Fmt.pr
    "   attempt   %a, and the Mailbox/get beside it still read %d mailboxes@."
    Fmt.(result ~ok:(any "no error") ~error:Proto.Error.Method_error.pp)
```

The rest of the program is elided here and shown below.

<br>

The first demonstration names a capability in `using` that no server has. Every
other step lets [`Client.call`](../../eio/client.mli) fill that array in from
the session, which [**`2-mailboxes`**](../2-mailboxes#readme) explains. A
request the server will not accept at all is answered with an HTTP error status
and a problem details body
([RFC 8620 §3.6.1](https://www.rfc-editor.org/rfc/rfc8620#section-3.6.1), in
the format of [RFC 7807](https://www.rfc-editor.org/rfc/rfc7807)).
`Client.call` decodes that body into `Error (Jmap_error e)`, where `e` is a
[`Proto.Error.Request_error.t`](../../lib/proto/proto_error.mli) carrying the
type URN, the status and the human readable `detail`. The other constructors of
[`Client.error`](../../eio/client.mli) cover the failures the server never got
to answer, such as a socket that never opened.

The second names a sort the server does not implement. The request as a whole
is well formed, so the server answers 200 and replaces the response of that one
call with an object named `error`
([RFC 8620 §3.6.2](https://www.rfc-editor.org/rfc/rfc8620#section-3.6.2)).
`Client.call` reports that as `Error (Method_error e)`, the constructor of
`Client.error` that means the exchange succeeded and the method did not, and
`Client.call_exn` raises it. That is what a program which only reports failures
wants.

A program that carries on wants the failure as a value in the middle of a read
of several calls. [`Chain.attempt`](../../lib/core/chain.mli) turns a handle
into one whose response is a `result`, so the `Email/query` that fails no longer
fails the `Mailbox/get` sent beside it and
[`Client.run_exn`](../../eio/client.mli) still returns both. `unsupportedSort`
and `unsupportedFilter` from a `/query`
([RFC 8620 §5.5](https://www.rfc-editor.org/rfc/rfc8620#section-5.5)) mean retry
with something simpler, and `cannotCalculateChanges` from a `/changes` means
resynchronise, which is why they are worth reading rather than raising on.

The third is a record the server will not create.

```ocaml
  (match Proto.Mailbox.create ~name:"" () with
  | Ok _ -> Fmt.pr "3. client    an empty Mailbox name was accepted@."
  | Error msg -> Fmt.pr "3. client    %s@." msg);
  let clash = Proto.Mailbox.creation "clash" in
  let second =
    Client.call_exn client
      (Chain.mailbox_set ~account_id
         ~create:[ (clash, Proto.Mailbox.create_exn ~name:"Inbox" ()) ]
         ())
  in
  (match Proto.Method.not_created second clash with
  | Some e ->
      Fmt.pr "   set       %a on %a@." Proto.Error.Set_error.pp e
        Fmt.(option ~none:(any "no property") (list ~sep:comma string))
        e.properties
  | None ->
      Fmt.pr "   set       a second Inbox was created, destroying it again@.";
      let born =
        Option.to_list
          (Option.bind (Proto.Method.created second clash) Proto.Mailbox.id)
      in
      let gone =
        Client.call_exn client
          (Chain.mailbox_set ~account_id ~destroy:(Chain.ids born) ())
      in
      if not (List.is_empty (Proto.Method.set_failures gone)) then
        Fmt.failwith "the second Inbox is left behind");

  let bad =
    { ctx.config with api_key = "not-a-credential"; api_key_file = None }
```

A `/set` succeeds as a method call and reports per record, so the failure is a
SetError in `notCreated` rather than an error response
([RFC 8620 §5.3](https://www.rfc-editor.org/rfc/rfc8620#section-5.3)). A
[`Proto.Error.Set_error.t`](../../lib/proto/proto_error.mli) carries the type
and, for `invalidProperties`, the names of the properties at fault.
[`Proto.Method.not_created`](../../lib/proto/proto_method.mli) reads the one
that belongs to a creation id, and
[`Proto.Method.set_failures`](../../lib/proto/proto_method.mli) is `notCreated`,
`notUpdated` and `notDestroyed` together, so one match over it is the whole
question of whether a `/set` did what it was asked.

The obvious way to provoke one is an empty mailbox name, but
[`Proto.Mailbox.create`](../../lib/mail/mail_mailbox.mli) checks the rules of
[RFC 8621 §2](https://www.rfc-editor.org/rfc/rfc8621#section-2) before anything
is sent and returns an `Error`, which the step prints as well. The name
`"Inbox"` breaks no rule a client can check, so `Proto.Mailbox.create_exn`, the
form of the same check that raises rather than returning a result, cannot raise
there, and what the server sees is a request for a second mailbox called
`Inbox`. The same section requires there be "no two sibling Mailboxes with both
the same parent and the same name". A server that accepts it leaves a mailbox
behind, so the branch that reports one destroys it again by the id
[`Proto.Method.created`](../../lib/proto/proto_method.mli) reads back under the
same creation token.

The fourth never gets as far as JMAP.

```ocaml
  match Cli.connect ~sw:ctx.sw ctx.env bad with
  | Ok _ -> Fmt.pr "4. http      the server answered an unauthenticated fetch@."
  | Error (Client.Http_error (status, _)) ->
      Fmt.pr "4. http      HTTP error %d on the session resource@." status
  | Error e -> Fmt.pr "4. http      %a@." Client.pp_error e
```

[RFC 8620 §8.2](https://www.rfc-editor.org/rfc/rfc8620#section-8.2) leaves
authentication to the HTTP layer, so a credential the server will not take is
an ordinary 401. [`Cli.connect`](../../eio/cli.mli) returns that failure rather
than printing it and exiting, and the `env`, `sw` and `config` it needs are the
other three fields of the [`Cli.context`](../../eio/cli.mli) the frame handed
over. The API key here is not `user:password`, so
[`Auth`](../../eio/auth.mli) has nothing valid to send and the fetch goes out
unauthenticated. The oracle accepts any password for its five users and answers
503, not 401, for a user it has never heard of, which is why the step drops the
credential rather than getting it wrong.

<pre><code><b>$ dune exec -- examples/9-errors/errors.exe --allow-insecure</b>
1. request   urn:ietf:params:jmap:error:unknownCapability (The Request object used capability 'urn:example:jmap:nonexistent', which is not supported by this server.)
   status    400
2. method    unsupportedSort
   attempt   unsupportedSort, and the Mailbox/get beside it still read 10 mailboxes
3. client    a Mailbox name must be at least 1 character long
   set       invalidProperties on name
4. http      HTTP error 401 on the session resource
</code></pre>

Every one of those is a failure and the program still exits 0, because each was
asked for. Nothing is created, so the step may be run as often as you like.

<br>

**Next steps:**

- The next example, [**`a-send`**](../a-send#readme), sends a message, and reads
  the same `notCreated` when a server refuses an `EmailSubmission`.
- [**`e-changes`**](../e-changes#readme) acts on a method error rather than
  reporting it, when the server answers `cannotCalculateChanges`.

<br>

**See also:**

- [**`n-resync`**](../n-resync#readme) falls back to a full resynchronise
  when the server can no longer calculate changes from the state it holds.

<br>

[Up to the tutorial index](../#readme)

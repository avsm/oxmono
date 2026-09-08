# `2-mailboxes`

<br>

This step makes the first method call. It asks for every Mailbox in the
account with four of the properties a Mailbox has and prints them as a table,
with the JSON of the request and the response either side of the call, so the
shape of a JMAP exchange is visible once.

```ocaml
module Client = Jmap_eio.Client
module Chain = Jmap.Chain
module Proto = Jmap.Proto

let doc = "List the mailboxes of an account with a Mailbox/get call"
let count = Option.fold ~none:"-" ~some:Int64.to_string
let role = Option.fold ~none:"-" ~some:Proto.Mailbox.role_to_string

let () =
  Jmap_eio.Cli.main "mailboxes" ~doc @@ fun ctx ->
  let chain =
    Chain.mailbox_get ~account_id:ctx.account_id
      ~properties:[ `Name; `Role; `Unread_emails; `Total_emails ]
      ()
  in
  let capabilities = Client.default_capabilities ctx.client in
  Fmt.pr "%a@.@." Proto.Request.pp (Chain.build_request ~capabilities chain);
  let h, response = Client.chain_exn ctx.client chain in
  Fmt.pr "%a@.@." Proto.Response.pp response;
  let r = Chain.parse_exn h response in
  Fmt.pr "%-24s %-8s %6s %6s@." "NAME" "ROLE" "UNREAD" "TOTAL";
  List.iter
    (fun (m : Proto.Mailbox.t) ->
      Fmt.pr "%-24s %-8s %6s %6s@."
        (Option.value m.name ~default:"(unnamed)")
        (role m.role) (count m.unread_emails) (count m.total_emails))
    r.list;
  Fmt.pr "@.%d mailbox(es), state %s@." (List.length r.list) r.state
```

The frame is the one from [**`1-session`**](../1-session#readme).
`Cli.main` has connected the client and resolved the account before the body
runs, so `ctx.account_id` is the account every call from here on names.

<br>

## The invocation triple

[RFC 8620 §3.3](https://www.rfc-editor.org/rfc/rfc8620#section-3.3) makes a
request an object with a `using` array and a `methodCalls` array, and
[§3.2](https://www.rfc-editor.org/rfc/rfc8620#section-3.2) makes every entry
of `methodCalls` an *Invocation*, a three element array of the method name,
its arguments and a *method call id* that the response to it carries back. The
client picks those ids, and [`Chain`](../../lib/core/chain.mli) picks them for
you, `c0` here. [`Chain.build_request`](../../lib/core/chain.mli) builds the
request without sending it, and
[`Proto.Request.pp`](../../lib/proto/proto_request.mli) prints it.

`using` is the set of capability URIs "the client wishes to use",
which the server consults to decide which methods and arguments exist at all.
`Mailbox/get` is defined by RFC 8621, so `urn:ietf:params:jmap:mail` has to be
in it, and `urn:ietf:params:jmap:core` always is. A call with no
`~capabilities` sends [`Client.default_capabilities`](../../eio/client.mli),
those of core, mail, submission and vacation response that the session
advertises, so hardly any step names them; this one asks the client for the
list only because `build_request` has no client to ask. `~capabilities` is for
a call belonging to some other capability, and the steps that pass it are
[**`9-errors`**](../9-errors#readme) and [**`a-send`**](../a-send#readme).

## Sending and reading a call

[`Client.call_exn`](../../eio/client.mli) sends a chain and hands back the
decoded response of the call it ends in, and is how a request is made in the
steps that follow. This step wants the response document as well as the
records in it, so it uses [`Client.chain_exn`](../../eio/client.mli), which
sends the request and hands back the handles of the chain beside the response.
[`Proto.Response.pp`](../../lib/proto/proto_response.mli) prints the response
and [`Chain.parse_exn`](../../lib/core/chain.mli) decodes the part of it a
handle names.

[RFC 8620 §3.4](https://www.rfc-editor.org/rfc/rfc8620#section-3.4) gives the
response the shape of the request, a `methodResponses` array of Invocations
each carrying the method call id of the call it answers, which is how a client
tells apart responses to several calls sent together.

All three functions raise, and `Cli.main` turns the exception into a message
and exit status 1. For a program that must carry on, the result forms
`Client.call`, `Client.chain` and `Chain.parse` report the two ways one call
fails. A `Method_error` is the server answering that call with an `error`
object
([RFC 8620 §3.6.2](https://www.rfc-editor.org/rfc/rfc8620#section-3.6.2)), and
a `Json_error` means the server and this library disagree about the wire
format. [**`9-errors`**](../9-errors#readme) takes both apart.

## The method call

[`Chain.mailbox_get`](../../lib/core/chain.mli) adds a `Mailbox/get`, the
standard `/get` of
[RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1) over the
Mailbox type of
[RFC 8621 §2](https://www.rfc-editor.org/rfc/rfc8621#section-2). Omitting
`ids` asks for every Mailbox, which a server may refuse for a large type but
not for this one. The response also carries a `state` string, the state of the
Mailbox type in this account, which
[**`e-changes`**](../e-changes#readme) feeds back to ask what has changed
since.

`properties` names what to return. Its variants come from
[`Proto.Mailbox.property`](../../lib/mail/mail_mailbox.mli), so
`` `Unread_emails `` is spelled `unreadEmails` by the library and cannot be
mistyped. It is also why the fields of
[`Proto.Mailbox.t`](../../lib/mail/mail_mailbox.mli) are options. `m.name` is
a `string option` because the record holds whatever the call asked for, and
`None` means the property was not requested. The exception is `id`, which RFC
8620 §5.1 has a `/get` return "even if not explicitly requested", so `m.id` is
set although this call did not ask for it.

<pre><code><b>$ dune exec -- examples/2-mailboxes/mailboxes.exe --allow-insecure</b>
{
  "using": ["urn:ietf:params:jmap:core", "urn:ietf:params:jmap:mail",
             "urn:ietf:params:jmap:submission"],
  "methodCalls": [
                   ["Mailbox/get",
                     {
                       "properties": ["name", "role", "unreadEmails",
                                       "totalEmails"],
                       "accountId": "user1"
                     }, "c0"]]
}

{
  "methodResponses": [
                       ["Mailbox/get",
                         {
                           "state": "2122",
                           "list": [
                                     {
                                       "id": "A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E",
                                       "name": "Inbox",
                                       "role": "inbox",
                                       "totalEmails": 440,
                                       "unreadEmails": 440
                                     },
                                     [nine more Mailbox objects omitted]],
                           "notFound": [],
                           "accountId": "user1"
                         }, "c0"]],
  "sessionState": "0"
}

NAME                     ROLE     UNREAD  TOTAL
Inbox                    inbox       440    440
Archive                  archive       0      0
Drafts                   drafts        0      0
Oracle Test              -             0      0
Sent                     sent          0      0
Spam                     junk          0      0
Trash                    trash         0      0
push-probe-1             -             0      0
push-probe-2             -             0      0
push-probe-3             -             0      0

10 mailbox(es), state 2122
</code></pre>

All but the first Mailbox object is left out of the response above, which is
otherwise the output as printed, under the environment of
[**`1-session`**](../1-session#readme). The default `using` names three
capabilities rather than four because Cyrus offers no vacation response one.
The mailboxes with no role are left over from earlier runs against this test
server, which creates six of its own.

A `role` is the IMAP special-use attribute of
[RFC 6154](https://www.rfc-editor.org/rfc/rfc6154) that RFC 8621 §2 gives a
Mailbox. It is how a client finds the Inbox without matching on a name the
user may have translated, which the next step does.

<br>

**Next steps:**

- [**`3-inbox`**](../3-inbox#readme) finds the Inbox by its role and lists the
  newest messages in it, with two method calls in one request.
- [**`8-mailboxes-set`**](../8-mailboxes-set#readme) is the other half of the
  Mailbox type, creating and destroying them with `Mailbox/set`.

<br>

**See also:**

- [**`m-organise`**](../m-organise#readme) files and labels messages across the
  mailboxes this step lists.

<br>

[Up to the tutorial index](../#readme)

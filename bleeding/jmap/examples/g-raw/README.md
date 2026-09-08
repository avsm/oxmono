# `g-raw`

<br>

Everything so far has gone through typed builders and typed responses. A real
deployment eventually offers something this library has no type for, so the
last step opens the four doors out of the typed layer and shows what comes back
through each. The frame is the one from
[**`1-session`**](../1-session#readme); `obj` and `string_list` build the two
shapes of `Jsont.json` the program needs.

```ocaml
module Chain = Jmap.Chain
module Proto = Jmap.Proto
module Client = Jmap_eio.Client
module Sync = Jmap_eio.Sync

let obj fields =
  Jsont.Json.object'
    (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) fields)

let string_list l = Jsont.Json.list (List.map Jsont.Json.string l)

let () =
  Jmap_eio.Cli.main "raw" ~doc:"Reach past the typed layer to raw JSON"
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in
  let sent = obj [ ("hello", Jsont.Json.string "world") ] in
  Fmt.pr "Core/echo sent %a and got back %a@." Jsont.Json.pp sent Jsont.Json.pp
    (Client.call_exn client (Chain.echo sent));

  let inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
  let arguments =
    obj
      [
        ("accountId", Jsont.Json.string (Proto.Id.to_string account_id));
        ("ids", string_list [ Proto.Id.to_string inbox ]);
        ("properties", string_list [ "role" ]);
      ]
  in
  Fmt.pr "@.Mailbox/get, parsed as Jsont.json@.%a@." Jsont.Json.pp
    (Client.call_exn client
       (Chain.raw_invocation ~name:"Mailbox/get" ~arguments));

  (match
     Client.call client
       (Chain.email_get ~account_id ~ids:(Chain.ids []) ~properties:[ `Id ]
          ~properties_raw:[ "x-vendor-thing" ] ())
   with
  | Ok _ -> Fmt.pr "@.properties_raw x-vendor-thing: accepted@."
  | Error e -> Fmt.pr "@.properties_raw x-vendor-thing: %a@." Client.pp_error e);

  let session = Client.session client in
  Fmt.pr "@.Session extension members: %a@." Jsont.Json.pp session.unknown;
  Fmt.pr "unknown_member \"fmSessionId\": %a@."
    Fmt.(option ~none:(any "absent") Jsont.Json.pp)
    (Proto.Session.unknown_member session "fmSessionId")
```

<br>

## `Core/echo`

[RFC 8620 §4](https://www.rfc-editor.org/rfc/rfc8620#section-4) defines one
method that does nothing. `Core/echo` returns its arguments unchanged, and
[`Chain.echo`](../../lib/core/chain.mli) sends any `Jsont.json` and hands the
answer back as `Jsont.json`, which is how you check that a request reaches a
server before anything else is in the way. `Jsont.Json.pp` prints one.

## A method with no builder

[`Chain.raw_invocation`](../../lib/core/chain.mli) adds a call by name with an
arguments object you built yourself, and its response parses as `Jsont.json`
rather than into a record. It is the door for a method an extension capability
adds, or for one added since this library was written, and the handle it
returns takes part in a chain like any other.
[`Chain.invocation`](../../lib/core/chain.mli) is the same call given a codec,
for a response you would rather decode into a type of your own.

Hand-built arguments are the one place where
[§3.7](https://www.rfc-editor.org/rfc/rfc8620#section-3.7)'s rule against
holding both `foo` and `#foo` can be broken, which a server answers with
`invalidArguments`. `raw_invocation` checks for that pair and raises rather
than send it, and [`Chain.check_arguments`](../../lib/core/chain.mli) is the
same check as a value.

## Properties with no variant

`~properties` takes typed variants, so a name this library does not know goes
in `~properties_raw` beside them; both lists are sent as the one `properties`
argument, the typed names first. Asking for a property the server does not
define is answered with `invalidArguments`
([§5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1)), which is what the
oracle does with `x-vendor-thing`, so the call goes through
[`Client.call`](../../eio/client.mli) and the failure arrives as
`Error (Method_error _)` rather than as an exception.

## Members with no field

Whatever a server sends back under a name no field of a record holds is kept in
that record's `unknown` map rather than dropped, so a decoded value re-encodes
with everything it arrived with.
[§2](https://www.rfc-editor.org/rfc/rfc8620#section-2) has a client ignore the
Session properties it is not expecting; this library keeps them instead, for
every object it decodes. The map is [`Proto.Unknown`](../../lib/proto/proto_unknown.mli), a
`Jsont.json` object, and
[`Proto.Session.unknown_member`](../../lib/proto/proto_session.mli) reads one
member by name. Cyrus sends none, so the map prints as `{}`; on a hosted
service such as Fastmail it is where the vendor's own fields arrive.

<pre><code><b>$ export JMAP_SESSION_URL=http://localhost:18080/.well-known/jmap</b>
<b>$ export JMAP_API_KEY=user1:x JMAP_AUTH=basic</b>
<b>$ dune exec -- examples/g-raw/raw.exe --allow-insecure</b>
Core/echo sent {"hello": "world"} and got back {"hello": "world"}

Mailbox/get, parsed as Jsont.json
{
  "state": "1881",
  "list": [{"id": "A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E", "role": "inbox"}],
  "notFound": [],
  "accountId": "user1"
}

properties_raw x-vendor-thing: invalidArguments

Session extension members: {}
unknown_member "fmSessionId": absent
</code></pre>

That is the end of the tutorial. The programs in the section below are whole
mail-client tasks built out of these sixteen pieces.

<br>

**Next steps:**

- [**The remaining programs**](../#readme) are longer, each one a task a mail client
  actually performs, and together they are the coverage check for this library
  against a real server.

<br>

**See also:**

- [**`l-parse`**](../l-parse#readme) mixes typed and raw access, printing
  a Mailbox as JSON beside the typed record.

<br>

[Up to the tutorial index](../#readme)

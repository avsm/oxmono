# `d-paging`

<br>

A server answers a `/query` with as many ids as it chooses and refuses a `/get`
that names more objects than it will handle at once, so walking a whole message
list is a loop rather than a call. This step runs both loops over the account's
messages, newest first, which is the rest of
[`Jmap_eio.Sync`](../../eio/sync.mli) that
[**`3-inbox`**](../3-inbox#readme) named. The frame is the one from
[**`1-session`**](../1-session#readme).

```ocaml
module Chain = Jmap.Chain
module Proto = Jmap.Proto
module Client = Jmap_eio.Client
module Sync = Jmap_eio.Sync

let pp_ids = Fmt.(list ~sep:(any " ") Proto.Id.pp)

let pp_email ppf (e : Proto.Email.t) =
  Fmt.pf ppf "  %a  %s"
    Fmt.(option ~none:(any "?") Proto.Id.pp)
    e.id
    (Option.value e.subject ~default:"(no subject)")

let () =
  Jmap_eio.Cli.main "paging"
    ~doc:"Page an Email/query and batch the Email/get that follows it"
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in
  let newest ~position ~limit =
    Chain.email_query ~account_id
      ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
      ~position ~limit ()
  in
  let q = Client.call_exn client (newest ~position:0L ~limit:3L) in
  Fmt.pr "Email/query  position %Ld  limit %a  ids %a@." q.position
    Fmt.(option ~none:(any "as asked") int64)
    q.limit pp_ids q.ids;

  Fmt.pr "@.Sync.pages ~page_size:3L, first four pages@.";
  Seq.iteri
    (fun i -> function
      | Ok ids -> Fmt.pr "  page %d  %a@." i pp_ids ids
      | Error e -> Fmt.failwith "page %d: %a" i Sync.pp_error e)
    (Seq.take 4 (Sync.pages client ~page_size:3L newest));

  let ids =
    match Sync.all_ids client ~page_size:3L ~max:7 newest with
    | Ok ids -> ids
    | Error e -> Fmt.failwith "all_ids: %a" Sync.pp_error e
  in
  if List.is_empty ids then Fmt.failwith "the account has no messages";
  Fmt.pr "@.Sync.all_ids ~max:7  %d ids@." (List.length ids);

  Fmt.pr "@.maxObjectsInGet %a, batching Email/get by 2@."
    Fmt.(option ~none:(any "unset") int64)
    (Sync.max_objects_in_get client);
  match
    Sync.get_all client ~batch:2L ids (fun ~ids ->
        Chain.email_get ~account_id ~ids:(Chain.ids ids)
          ~properties:[ `Id; `Subject ] ())
  with
  | Error e -> Fmt.failwith "get_all: %a" Sync.pp_error e
  | Ok (emails, not_found) ->
      Fmt.pr "%a@." Fmt.(vbox (list ~sep:cut pp_email)) emails;
      if not (List.is_empty not_found) then
        Fmt.pr "  notFound %a@." pp_ids not_found
```

<br>

## The limit the server chose

[RFC 8620 §5.5](https://www.rfc-editor.org/rfc/rfc8620#section-5.5) gives a
`/query` a zero-based `position` and a `limit`, and has the response repeat the
`position` it used and, "if the server set a limit or used a different limit
than that given in the request", the `limit` it applied. So
[`query_response.limit`](../../lib/proto/proto_method.mli) is an `int64 option`.
`None` means the server used the number asked for, and a smaller `Some` is the
number to page by. A client that counts in its own limit skips results the
moment the server is stricter.

## Paging

[`Sync.pages`](../../eio/sync.mli) is that loop. It takes a function rather
than a request, calls it once per page with that page's `position` and `limit`,
and is a lazy `Seq` of id lists, one request sent each time the sequence is
advanced. The filter and the sort must not change between pages, which is why
`newest` closes over both. The walk stops when a page comes back shorter than
the limit the server reported for it, or empty, and an `Error` is always its
last element.

A nonempty page must begin at the position requested. A forward jump would
skip messages and is rejected along with a repeated/backward page. Changing
`queryState` also ends the walk with an error. `~fuel` bounds its request count,
and exhaustion is an error rather than a silently truncated result.

A `Seq` composes, so `Seq.take 4` ends the walk after four pages and no fifth
request is sent. [`Sync.all_ids`](../../eio/sync.mli) is the same walk
concatenated, with `~max` capping how many ids are collected, so `~max:7` over
pages of three sends three requests and drops the two ids beyond the seventh.

## Batching the `/get`

[RFC 8620 §5.1](https://www.rfc-editor.org/rfc/rfc8620#section-5.1) has a `/get`
answer `requestTooLarge` when it is asked for more ids than the server will
process in one call, and [§2](https://www.rfc-editor.org/rfc/rfc8620#section-2)
publishes that number as `maxObjectsInGet` in the session's core capability.
[`Sync.max_objects_in_get`](../../eio/sync.mli) reads it, and
[`Sync.get_all`](../../eio/sync.mli) splits an id list into calls of that size,
or of `~batch` when you want to watch the splitting happen. The batches run as
concurrent fibers, no more at once than `maxConcurrentRequests` allows, and the
result pairs the objects with the ids the server did not know. Within one batch
the order is the server's, since §5.1 does not promise a `/get` returns objects
in the order they were asked for.

The same capability carries `maxCallsInRequest`, which bounds the request
around that call rather than the call itself, and
[`Sync.chain_fits`](../../eio/sync.mli) answers whether a chain stays inside
it. Nothing splits an oversized chain, because a back reference cannot cross a
request boundary.

<pre><code><b>$ export JMAP_SESSION_URL=http://localhost:18080/.well-known/jmap</b>
<b>$ export JMAP_API_KEY=user1:x JMAP_AUTH=basic</b>
<b>$ dune exec -- examples/d-paging/paging.exe --allow-insecure</b>
Email/query  position 0  limit as asked  ids M9e4c057e1a700e4faa77779e M79907f72d98c7558d36c0df3 M7a3904e7e0bbf143bc120da7

Sync.pages ~page_size:3L, first four pages
  page 0  M9e4c057e1a700e4faa77779e M79907f72d98c7558d36c0df3 M7a3904e7e0bbf143bc120da7
  page 1  M6f7f30573e7597edac27f072 M4712885ac8f0083dd8a7466b Mb132cd5e028224b744fc5f42
  page 2  M9d027a77f5a14f6c3550e997 M0ffee90db2ba1a2ee9d51789 Mcf7d3435871e6248976ca37b
  page 3  M7287d600382b8e575204f0fd M24ba697a0959f2dddae1d321 Ma38b256986a7beec9a2bdf49

Sync.all_ids ~max:7  7 ids

maxObjectsInGet 4096, batching Email/get by 2
  M9e4c057e1a700e4faa77779e  a message delivered while f-push waits
  M79907f72d98c7558d36c0df3  a message delivered while f-push waits
  M7a3904e7e0bbf143bc120da7  run-examples oracle seed 1/3
  M6f7f30573e7597edac27f072  run-examples oracle seed 3/3
  M4712885ac8f0083dd8a7466b  run-examples oracle seed 2/3
  Mb132cd5e028224b744fc5f42  oracle-3894810-1788373650-25
  M9d027a77f5a14f6c3550e997  oracle-3894810-1788373650-27
</code></pre>

The oracle honoured the limit of three, so the printed limit is `as asked`, and
the account held more than twelve messages, so `Seq.take 4` rather than the
server ended the walk. Seed an account of your own with
`scripts/run-examples.sh --seed 8`.

<br>

**Next steps:**

- [**`e-changes`**](../e-changes#readme) is the second loop the protocol
  forces, the one that brings a stale cache up to date instead of refetching
  the list.
- [**`f-push`**](../f-push#readme) is the third, and says when to run the
  second.

<br>

**See also:**

- [**`h-search`**](../h-search#readme) pages a filtered query and asks for the
  matching text of each result.

<br>

[Up to the tutorial index](../#readme)

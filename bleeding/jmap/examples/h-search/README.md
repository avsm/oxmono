# `h-search`

<br>

A search box asks one question made of many parts: a word, a mailbox, a date
range, a keyword to exclude, and a page of the answer to show. This step builds
that filter, pages the result, sorts it two ways, collapses threads, and asks
the server for the matching text of each hit.

```ocaml
let doc = "Search the Inbox the way a mail client's search box does"
let page_size = 5L
```

The frame is the one from [**`1-session`**](../1-session#readme), in the
[`Cli.main'`](../../eio/cli.mli) form of [**`4-filter`**](../4-filter#readme)
because the search terms are the program's own options. `--text WORD` is the
word the user typed and `--days N` the date range the search box offers.

<br>

## The filter

```ocaml
let () =
  Jmap_eio.Cli.main' "search" ~doc ~args @@ fun ctx (text, days) ->
  let client = ctx.client and account_id = ctx.account_id in
  let inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
  let cutoff =
    let now = Eio.Time.now (Eio.Stdenv.clock ctx.env) in
    match Ptime.of_float_s (now -. (float_of_int days *. 86_400.)) with
    | Some t -> Ptime.truncate ~frac_s:0 t
    | None -> Fmt.failwith "cannot represent a cutoff %d days ago" days
  in
  let filter =
    Proto.Filter.and_
      [
        Proto.Email.filter ~in_mailbox:inbox ~after:cutoff ();
        Proto.Filter.or_
          [ Proto.Email.filter ~text (); Proto.Email.filter ~from:text () ];
        Proto.Filter.not_ [ Proto.Email.filter ~has_keyword:`Draft () ];
      ]
  in
```

The Inbox is found by role with [`Sync.mailbox_id_exn`](../../eio/sync.mli), as
in [**`3-inbox`**](../3-inbox#readme).
[RFC 8620 §5.5](https://www.rfc-editor.org/rfc/rfc8620#section-5.5) makes a
filter a tree of `Condition` leaves and `Operator` nodes, which
[**`4-filter`**](../4-filter#readme) introduces. `Proto.Filter.and_`, `or_` and
`not_` build the nodes and
[`Proto.Email.filter`](../../lib/mail/mail_email.mli) builds a leaf from named
arguments. A leaf keeps the Emails satisfying every argument it was given, so
the mailbox and the date are one condition rather than two, while the two ways
a word may match need an `or_` of their own and the `not_` excluding drafts is
a NOR over its whole list.

The arguments are the conditions of
[RFC 8621 §4.4.1](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.1).
`after` keeps the Emails received at or after the given time, so the cutoff is
a `Ptime.t` truncated to whole seconds and printed with
[`Proto.Date.to_utc_string`](../../lib/proto/proto_date.mli). `text` searches
the message and its headers, `from` matches the From header alone, and
`has_keyword` takes a [`Proto.Keyword.t`](../../lib/mail/mail_keyword.mli).

<br>

## One page of the answer

```ocaml
  let recent = [ Proto.Email.sort ~ascending:false `Received_at ] in
  let query ?(sort = recent) ?collapse_threads () =
    Chain.email_query ~account_id ~filter ~sort ~limit:page_size
      ~calculate_total:true ?collapse_threads ()
  in
  let page = Client.call_exn client (query ()) in
  Fmt.pr "@.Page at position %Ld: %d ids, limit %a, total %a@." page.position
    (List.length page.ids)
    Fmt.(option ~none:(any "as asked") int64)
    page.limit
    Fmt.(option ~none:(any "not calculated") int64)
    page.total;
  Sync.pages client ~page_size (fun ~position ~limit ->
      Chain.email_query ~account_id ~filter ~sort:recent ~position ~limit ())
  |> Seq.take 3
  |> Seq.iteri (fun i -> function
    | Ok ids -> Fmt.pr "  page %d: %d ids@." i (List.length ids)
    | Error e -> Fmt.failwith "page %d: %a" i Sync.pp_error e);
```

The sort is a list of comparators applied in order, which
[`Proto.Email.sort`](../../lib/mail/mail_email.mli) builds from the properties
§4.4.2 allows. `~calculate_total:true` asks for the size of the whole result
set beside the page, which RFC 8620 §5.5 has a server omit otherwise.

A page is a `position` and a `limit`, and the response repeats the position it
used and the limit it applied when that is not the one asked for.
[`Sync.pages`](../../eio/sync.mli) walks a result set by the server's own
limit, one request per page, lazily, and [**`d-paging`**](../d-paging#readme)
covers the rest of it. The filter and the sort must not change between pages,
so the paging function repeats both.

<br>

## Sorting and collapsing

```ocaml
  let unread_first =
    [
      Proto.Email.sort (`Has_keyword `Seen);
      Proto.Email.sort ~ascending:false `Received_at;
    ]
  in
  (match Client.call client (query ~sort:unread_first ()) with
  | Ok q -> Fmt.pr "@.Sorted unread first: %d ids@." (List.length q.ids)
  | Error e -> Fmt.pr "@.Sorted unread first: %a@." Client.pp_error e);
  let ids_of collapse_threads =
    (Client.call_exn client (query ~collapse_threads ())).ids
  in
  Fmt.pr "collapseThreads false: %d ids, true: %d ids@."
    (List.length (ids_of false))
    (List.length (ids_of true));
```

[RFC 8621 §4.4.2](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.2) adds
`hasKeyword`, `allInThreadHaveKeyword` and `someInThreadHaveKeyword` to the
properties an `Email/query` sorts on, and requires the Comparator to carry a
`keyword` property naming the keyword. `` `Has_keyword `` carries it, ascending
puts the unread messages first, and the second comparator orders each group by
arrival.

RFC 8620 §5.5 lets a server refuse a sort it does not support with
`unsupportedSort`. That is an error a client acts on rather than reports, so
the call goes through [`Client.call`](../../eio/client.mli), whose
`Error (Method_error _)` [**`9-errors`**](../9-errors#readme) describes.

`~collapse_threads:true` is the argument of
[RFC 8621 §4.4.3](https://www.rfc-editor.org/rfc/rfc8621#section-4.4.3) that
returns one Email per conversation rather than one per message, which
[**`6-threads`**](../6-threads#readme) uses to list conversations.

<br>

## The hits and their snippets

```ocaml
  let hits = page.ids in
  if List.is_empty hits then
    Fmt.failwith "nothing matched; widen the search with --text or --days";
  (match
     Sync.get_all client hits (fun ~ids ->
         Chain.email_get ~account_id ~ids:(Chain.ids ids)
           ~properties:[ `Id; `Subject; `From; `Received_at ]
           ())
   with
  | Error e -> Fmt.failwith "Email/get: %a" Sync.pp_error e
  | Ok (emails, _not_found) ->
      Fmt.pr "@.Hits:@.";
      Proto.Method.in_ids_order ~id:Proto.Email.id hits emails
      |> List.iter (fun (e : Proto.Email.t) ->
          Fmt.pr "  %s  %-28s %s@."
            (Option.fold ~none:"?" ~some:Proto.Date.to_utc_string e.received_at)
            (match e.from with
            | Some (a :: _) -> a.Proto.Email_address.email
            | _ -> "-")
            (Option.value e.subject ~default:"(no subject)")));
  match
    Client.call client
      (Chain.search_snippet_get ~account_id ~filter ~email_ids:(Chain.ids hits)
         ())
  with
  | Error e -> Fmt.pr "@.SearchSnippet/get: %a@." Client.pp_error e
  | Ok snippets ->
      Fmt.pr "@.Snippets:@.";
      List.iter
        (fun (s : Proto.Search_snippet.t) ->
          Fmt.pr "  %a@." Proto.Id.pp s.email_id;
          Option.iter (Fmt.pr "    subject: %s@.") s.subject;
          Option.iter (Fmt.pr "    preview: %s@.") s.preview)
        snippets.list
```

[`Sync.get_all`](../../eio/sync.mli) fetches the page's Emails in batches no
larger than `maxObjectsInGet`, and
[`Proto.Method.in_ids_order`](../../lib/proto/proto_method.mli) puts the answer
back into the order the query gave, since a `/get` may return its objects in
any order, reading each record's id through `~id`, which for an Email is
[`Proto.Email.id`](../../lib/mail/mail_email.mli).

[RFC 8621 §5](https://www.rfc-editor.org/rfc/rfc8621#section-5) defines the
SearchSnippet, the part of an Email that matched a query with the matching
words wrapped in `<mark>` elements, ready to display in a result list.
[`Chain.search_snippet_get`](../../lib/core/chain.mli) must be given the same
filter as the `Email/query` that produced the ids, since the snippet is of the
match rather than of the message. Its response is not a standard `/get`. A
[`Proto.Search_snippet.t`](../../lib/mail/mail_snippet.mli) has no `id`, the
response carries no `state` string, and `notFound` is `Id[]|null`. A `subject`
or `preview` of `None` means that part of the Email did not match.

<br>

<pre><code><b>$ dune exec -- examples/h-search/search.exe --allow-insecure</b>
inMailbox A0BFFEDA-A6A4-11F1-8E77-BE70CA53046E AND after 2026-08-03T23:13:27Z AND (text "oracle" OR from "oracle") AND NOT $draft

Page at position 0: 5 ids, limit as asked, total 408
  page 0: 5 ids
  page 1: 5 ids
  page 2: 5 ids

Sorted unread first: 5 ids
collapseThreads false: 5 ids, true: 5 ids

Hits:
  2026-09-02T19:42:46Z  alice@example.org            oracle-1613390-1788378166-1
  2026-09-02T19:42:46Z  alice@example.org            oracle-1613390-1788378166-3
  2026-09-02T19:31:06Z  alice@example.org            oracle-1159494-1788377466-27
  2026-09-02T19:31:05Z  alice@example.org            oracle-1159494-1788377465-23
  2026-09-02T19:31:05Z  alice@example.org            oracle-1159494-1788377465-25

Snippets:
  Me88f79f3046cd0a5a87b0387
    subject: &lt;mark&gt;oracle&lt;/mark&gt;-1613390-1788378166-1
    preview: Hello from the &lt;mark&gt;oracle&lt;/mark&gt; harness.
  M1594a5ebacfb354e9f9c2b68
    subject: &lt;mark&gt;oracle&lt;/mark&gt;-1613390-1788378166-3
    preview: Hello from the &lt;mark&gt;oracle&lt;/mark&gt; harness.
...
</code></pre>

The last three snippets are elided. Each repeats those three lines under a
different id. The account behind this run holds several hundred seeded messages, all of them
unrelated to one another, so collapsing threads removes nothing and the two
sorts return the same five ids. The Cyrus oracle indexes the text of a message
but not its subject alone, so a `subject` condition matches nothing there while
`text` matches.

<br>

**Next steps:**

- [**`i-reading`**](../i-reading#readme) takes one of these hits and displays
  it as a reading pane does.
- [**`d-paging`**](../d-paging#readme) walks a result set to its end rather
  than stopping at the third page.

<br>

[Up to the tutorial index](../#readme)

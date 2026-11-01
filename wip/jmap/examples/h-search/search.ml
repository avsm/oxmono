(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Sync = Jmap_eio.Sync

let doc = "Search the Inbox the way a mail client's search box does"
let page_size = 5L

let args =
  let open Cmdliner in
  let text =
    let doc = "The word to search for, in a message or in its From address." in
    Arg.(value & opt string "oracle" & info [ "text" ] ~docv:"WORD" ~doc)
  in
  let days =
    let doc = "Search only mail received in the last $(docv) days." in
    Arg.(value & opt int 30 & info [ "days" ] ~docv:"N" ~doc)
  in
  Term.(const (fun text days -> (text, days)) $ text $ days)

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
  Fmt.pr "inMailbox %a AND after %s AND (text %S OR from %S) AND NOT %s@."
    Proto.Id.pp inbox
    (Proto.Date.to_utc_string cutoff)
    text text
    (Proto.Keyword.to_string `Draft);
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

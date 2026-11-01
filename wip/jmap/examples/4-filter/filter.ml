(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Client = Jmap_eio.Client
module Sync = Jmap_eio.Sync
module Chain = Jmap.Chain
module Results = Jmap.Chain.Results
module Proto = Jmap.Proto

let doc = "Count the Inbox three ways with three Email/query filters"

let text_term =
  let open Cmdliner in
  let doc = "The word an Email must hold for the third filter to match it." in
  Arg.(value & opt string "oracle" & info [ "text" ] ~docv:"WORD" ~doc)

let () =
  Jmap_eio.Cli.main' "filter" ~doc ~args:text_term @@ fun ctx text ->
  let account_id = ctx.account_id in
  let inbox = Sync.mailbox_id_exn ctx.client ~account_id `Inbox in
  let in_inbox = Proto.Email.filter ~in_mailbox:inbox () in
  let both other = Proto.Filter.and_ [ in_inbox; other ] in
  let unseen = both (Proto.Email.filter ~not_keyword:`Seen ()) in
  let mentioning = both (Proto.Email.filter ~text ()) in
  let sort = [ Proto.Email.sort ~ascending:false `Received_at ] in
  let Results.[ all; unread; got; matching ] =
    Client.run_exn ctx.client
      Chain.(
        let* all =
          email_query ~account_id ~filter:in_inbox ~calculate_total:true
            ~limit:1L ()
        in
        let* unread =
          email_query ~account_id ~filter:unseen ~sort ~calculate_total:true
            ~limit:5L ()
        in
        let* g =
          email_get ~account_id ~ids:(from_query unread)
            ~properties:[ `Id; `Subject ] ()
        in
        let+ matching =
          email_query ~account_id ~filter:mentioning ~calculate_total:true
            ~limit:1L ()
        in
        Handles.[ all; unread; g; matching ])
  in
  let row label (r : Proto.Method.query_response) =
    Fmt.pr "%-40s %a@." label Fmt.(option ~none:(any "-") int64) r.total
  in
  row "inMailbox" all;
  row ("inMailbox AND notKeyword " ^ Proto.Keyword.to_string `Seen) unread;
  row (Fmt.str "inMailbox AND text %S" text) matching;
  Fmt.pr "@.The newest unread messages:@.";
  List.iter
    (fun (e : Proto.Email.t) ->
      Fmt.pr "  %s@." (Option.value e.subject ~default:"(no subject)"))
    got.list

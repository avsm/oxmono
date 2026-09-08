(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Client = Jmap_eio.Client
module Sync = Jmap_eio.Sync
module Chain = Jmap.Chain
module Results = Jmap.Chain.Results
module Proto = Jmap.Proto

let doc = "Print the newest messages in the Inbox, in one request"
let date = Option.fold ~none:"?" ~some:Proto.Date.to_utc_string

let sender (e : Proto.Email.t) =
  match e.from with
  | Some (a :: _) -> Option.value a.Proto.Email_address.name ~default:a.email
  | _ -> "-"

let () =
  Jmap_eio.Cli.main "inbox" ~doc @@ fun ctx ->
  let account_id = ctx.account_id in
  let inbox = Sync.mailbox_id_exn ctx.client ~account_id `Inbox in
  let Results.[ query; got ] =
    Client.run_exn ctx.client
      Chain.(
        let* q =
          email_query ~account_id
            ~filter:(Proto.Email.filter ~in_mailbox:inbox ())
            ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
            ~limit:10L ()
        in
        let+ g =
          email_get ~account_id ~ids:(from_query q)
            ~properties:[ `Id; `Received_at; `From; `Subject ]
            ()
        in
        Handles.[ q; g ])
  in
  if List.is_empty query.ids then Fmt.failwith "the Inbox is empty";
  List.iter
    (fun (e : Proto.Email.t) ->
      Fmt.pr "%-20s  %-24s  %s@." (date e.received_at) (sender e)
        (Option.value e.subject ~default:"(no subject)"))
    (Proto.Method.in_ids_order ~id:Proto.Email.id query.ids got.list);
  Fmt.pr "@.%d of the newest messages in %a@." (List.length query.ids)
    Proto.Id.pp inbox

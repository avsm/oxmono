(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

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

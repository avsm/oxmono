(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

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

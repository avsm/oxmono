(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Client = Jmap_eio.Client
module Proto = Jmap.Proto

let doc = "Fetch the JMAP session resource and print what it says"

let () =
  Jmap_eio.Cli.main "session" ~doc @@ fun ctx ->
  let s = Client.session ctx.client in
  Fmt.pr "username               %s@." s.username;
  Fmt.pr "apiUrl                 %s@." (Client.api_url ctx.client);
  Fmt.pr "state                  %s@." s.state;
  let account_id =
    match Proto.Session.primary_account_for Proto.Capability.mail s with
    | Some id -> id
    | None -> Fmt.failwith "no primary account for %s" Proto.Capability.mail
  in
  let account =
    match Proto.Session.find_account account_id s with
    | Some a -> a
    | None -> Fmt.failwith "no account %a in the session" Proto.Id.pp account_id
  in
  Fmt.pr "primary mail account   %a (%s)%s@." Proto.Id.pp account_id
    account.name
    (if account.is_read_only then ", read only" else "");
  match Proto.Session.core_capability s with
  | None -> Fmt.failwith "the server does not offer %s" Proto.Capability.core
  | Some c ->
      Fmt.pr "maxObjectsInGet        %Ld@." c.max_objects_in_get;
      Fmt.pr "maxConcurrentRequests  %Ld@." c.max_concurrent_requests

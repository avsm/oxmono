(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Cli = Jmap_eio.Cli
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results

let () =
  Cli.main "errors"
    ~doc:"Read a JMAP failure at each of the levels it can happen"
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in
  let urn = "urn:example:jmap:nonexistent" in
  (match
     Client.call client ~capabilities:[ urn ]
       (Chain.mailbox_get ~account_id ~properties:[ `Id ] ())
   with
  | Ok _ -> Fmt.pr "1. request   the server accepted %s@." urn
  | Error (Client.Jmap_error e) ->
      Fmt.pr "1. request   %a@." Proto.Error.Request_error.pp e;
      Fmt.pr "   status    %a@." Fmt.(option ~none:(any "none") int) e.status
  | Error e -> Fmt.pr "1. request   %a@." Client.pp_error e);

  let unsupported =
    Chain.email_query ~account_id
      ~sort:[ Proto.Filter.comparator "unsupportedProperty" ]
      ~limit:1L ()
  in
  (match Client.call client unsupported with
  | Ok _ -> Fmt.pr "2. method    the server sorted by unsupportedProperty@."
  | Error (Client.Method_error e) ->
      Fmt.pr "2. method    %a@." Proto.Error.Method_error.pp e
  | Error e -> Fmt.pr "2. method    %a@." Client.pp_error e);
  let Results.[ sorted; boxes ] =
    Client.run_exn client
      Chain.(
        let* q = unsupported in
        let+ m = mailbox_get ~account_id ~properties:[ `Id ] () in
        Handles.[ attempt q; m ])
  in
  Fmt.pr
    "   attempt   %a, and the Mailbox/get beside it still read %d mailboxes@."
    Fmt.(result ~ok:(any "no error") ~error:Proto.Error.Method_error.pp)
    sorted (List.length boxes.list);

  (match Proto.Mailbox.create ~name:"" () with
  | Ok _ -> Fmt.pr "3. client    an empty Mailbox name was accepted@."
  | Error msg -> Fmt.pr "3. client    %s@." msg);
  let clash = Proto.Mailbox.creation "clash" in
  let second =
    Client.call_exn client
      (Chain.mailbox_set ~account_id
         ~create:[ (clash, Proto.Mailbox.create_exn ~name:"Inbox" ()) ]
         ())
  in
  (match Proto.Method.not_created second clash with
  | Some e ->
      Fmt.pr "   set       %a on %a@." Proto.Error.Set_error.pp e
        Fmt.(option ~none:(any "no property") (list ~sep:comma string))
        e.properties
  | None ->
      Fmt.pr "   set       a second Inbox was created, destroying it again@.";
      let born =
        Option.to_list
          (Option.bind (Proto.Method.created second clash) Proto.Mailbox.id)
      in
      let gone =
        Client.call_exn client
          (Chain.mailbox_set ~account_id ~destroy:(Chain.ids born) ())
      in
      if not (List.is_empty (Proto.Method.set_failures gone)) then
        Fmt.failwith "the second Inbox is left behind");

  let bad =
    { ctx.config with api_key = "not-a-credential"; api_key_file = None }
  in
  match Cli.connect ~sw:ctx.sw ctx.env bad with
  | Ok _ -> Fmt.pr "4. http      the server answered an unauthenticated fetch@."
  | Error (Client.Http_error (status, _)) ->
      Fmt.pr "4. http      HTTP error %d on the session resource@." status
  | Error e -> Fmt.pr "4. http      %a@." Client.pp_error e

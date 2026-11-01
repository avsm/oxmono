(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Patch = Jmap.Proto.Email.Patch
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results
module Sync = Jmap_eio.Sync

let doc = "File the newest Inbox messages into a mailbox and flag them"

let pp_email ppf (e : Proto.Email.t) =
  Fmt.pf ppf "%a  %-24s [%a] in [%a]"
    Fmt.(option ~none:(any "?") Proto.Id.pp)
    e.id
    (Option.value e.subject ~default:"(no subject)")
    Fmt.(list ~sep:sp Proto.Keyword.pp)
    (Proto.Email.keyword_list e)
    Fmt.(list ~sep:sp Proto.Id.pp)
    (Proto.Email.mailbox_list e)

let file client ~account_id ?if_in_state what update =
  let set =
    Client.call_exn client (Chain.email_set ~account_id ?if_in_state ~update ())
  in
  (match Proto.Method.set_failures set with
  | f :: _ ->
      Fmt.failwith "%s: Email/set refused %a" what Proto.Method.pp_set_failure f
  | [] -> ());
  Fmt.pr "%s %d message(s), state %s -> %s@." what
    (List.length (Option.value set.updated ~default:[]))
    (Option.value set.old_state ~default:"?")
    set.new_state;
  set

let () =
  Jmap_eio.Cli.main "organise" ~doc @@ fun ctx ->
  let account_id = ctx.account_id and client = ctx.client in
  let inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
  let Results.[ newest; got ] =
    Client.run_exn client
      Chain.(
        let* q =
          email_query ~account_id
            ~filter:(Proto.Email.filter ~in_mailbox:inbox ())
            ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
            ~limit:3L ()
        in
        let+ g =
          email_get ~account_id ~ids:(from_query q)
            ~properties:[ `Id; `Subject; `Keywords; `Mailbox_ids ]
            ()
        in
        Handles.[ q; g ])
  in
  let inbox_ids = newest.ids in
  if List.is_empty inbox_ids then
    Fmt.failwith "the Inbox is empty; deliver a message first";
  let before =
    Proto.Method.in_ids_order ~id:Proto.Email.id inbox_ids got.list
  in
  Fmt.pr "Inbox@.%a@." Fmt.(list ~sep:cut pp_email) before;

  let folder_name = Printf.sprintf "tutorial-%d-project" (Unix.getpid ()) in
  let cid = Proto.Mailbox.creation "folder" in
  let created =
    Client.call_exn client
      (Chain.mailbox_set ~account_id
         ~create:[ (cid, Proto.Mailbox.create_exn ~name:folder_name ()) ]
         ())
  in
  let folder =
    match Option.bind (Proto.Method.created created cid) Proto.Mailbox.id with
    | Some id -> id
    | None -> Fmt.failwith "Mailbox/set did not create %S" folder_name
  in
  Fmt.pr "created %S (%a)@." folder_name Proto.Id.pp folder;

  let unfile () =
    let restore (e : Proto.Email.t) =
      ( Option.get e.id,
        Proto.Patch.v
          [
            Patch.set_mailboxes (Proto.Email.mailbox_list e);
            Patch.set_keywords (Proto.Email.keyword_list e);
          ] )
    in
    ignore (file client ~account_id "restored" (List.map restore before));
    let destroyed =
      Client.call_exn client
        (Chain.mailbox_set ~account_id ~destroy:(Chain.ids [ folder ]) ())
    in
    Fmt.pr "destroyed %d mailbox(es)%a@."
      (List.length (Option.value destroyed.destroyed ~default:[]))
      Fmt.(list ~sep:nop (any ", " ++ Proto.Method.pp_set_failure))
      (Proto.Method.set_failures destroyed)
  in
  Fun.protect ~finally:unfile @@ fun () ->
  let move id =
    ( id,
      Proto.Patch.v
        [ Patch.add_to_mailbox folder; Patch.remove_from_mailbox inbox ] )
  in
  let moved = file client ~account_id "filed" (List.map move inbox_ids) in
  let flag id = (id, Proto.Patch.v [ Patch.set_keyword `Flagged ]) in
  ignore
    (file client ~account_id ~if_in_state:moved.new_state "flagged"
       (List.map flag inbox_ids));

  let Results.[ query; filed ] =
    Client.run_exn client
      Chain.(
        let* q =
          email_query ~account_id
            ~filter:(Proto.Email.filter ~in_mailbox:folder ())
            ~calculate_total:true ()
        in
        let+ g =
          email_get ~account_id ~ids:(from_query q)
            ~properties:[ `Id; `Subject; `Keywords; `Mailbox_ids ]
            ()
        in
        Handles.[ q; g ])
  in
  Fmt.pr "%S holds %a message(s)@.%a@." folder_name
    Fmt.(option ~none:(any "?") int64)
    query.total
    Fmt.(list ~sep:cut pp_email)
    filed.list;
  List.iter
    (fun (e : Proto.Email.t) ->
      if not (Proto.Email.has_keyword `Flagged e) then
        Fmt.failwith "a filed message is not flagged";
      if Proto.Email.mailbox_list e <> [ folder ] then
        Fmt.failwith "a filed message is not in the project mailbox alone")
    filed.list

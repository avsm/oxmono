(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results
module Client = Jmap_eio.Client
module Sync = Jmap_eio.Sync

let pp_ids ppf = function
  | [] -> Format.pp_print_string ppf "-"
  | l -> Fmt.(list ~sep:(any " ") Proto.Id.pp) ppf l

let drain client ~account_id ~since label =
  match Sync.email_changes client ~account_id ~since () with
  | Ok (`Changes c) ->
      Fmt.pr "@.%s@.  created %a  updated %a  destroyed %a@." label pp_ids
        c.Sync.created pp_ids c.Sync.updated pp_ids c.Sync.destroyed;
      Fmt.pr "  newState %s hasMore %b@." c.Sync.new_state c.Sync.has_more
  | Ok `Cannot_calculate_changes -> Fmt.pr "@.%s cannotCalculateChanges@." label
  | Error e -> Fmt.failwith "Email/changes: %a" Sync.pp_error e

let flip client ~account_id ~email entry =
  let update = [ (email, Proto.Patch.v [ entry ]) ] in
  let set = Client.call_exn client (Chain.email_set ~account_id ~update ()) in
  match Proto.Method.set_failures set with
  | f :: _ -> Fmt.failwith "%a" Proto.Method.pp_set_failure f
  | [] -> ()

let () =
  Jmap_eio.Cli.main "changes" ~doc:"Follow /changes from a state string"
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in
  let Results.[ since_email; since_mailbox; unread ] =
    Client.run_exn client
      Chain.(
        let* e = email_state ~account_id in
        let* m = mailbox_state ~account_id in
        let+ q =
          email_query ~account_id
            ~filter:(Proto.Email.filter ~not_keyword:`Seen ())
            ~limit:1L ()
        in
        Handles.[ e; m; q ])
  in
  let email =
    match unread.ids with
    | id :: _ -> id
    | [] -> Fmt.failwith "no unread message to change; seed the account first"
  in
  Fmt.pr "Email state %s, Mailbox state %s@." since_email since_mailbox;
  Fmt.pr "$seen on and off %a@." Proto.Id.pp email;
  flip client ~account_id ~email (Proto.Email.Patch.set_keyword `Seen);
  flip client ~account_id ~email (Proto.Email.Patch.remove_keyword `Seen);
  drain client ~account_id ~since:since_email "Email/changes since that state";
  drain client ~account_id ~since:"stale" "Email/changes since a stale state";
  let Results.[ mc; got ] =
    Client.run_exn client
      Chain.(
        let* c = mailbox_changes ~account_id ~since_state:since_mailbox () in
        let+ g =
          mailbox_get ~account_id ~ids:(from_changes_updated c)
            ~properties_ref:(from_changes_updated_properties c)
            ()
        in
        Handles.[ c; attempt g ])
  in
  let c = mc.changes in
  Fmt.pr "@.Mailbox/changes since that state@.  created %a  updated %a@." pp_ids
    c.created pp_ids c.updated;
  Fmt.pr "  newState %s  updatedProperties %a@." c.new_state
    Fmt.(option ~none:(any "null") (list ~sep:(any ", ") string))
    mc.updated_properties;
  match got with
  | Error e -> Fmt.pr "  Mailbox/get %a@." Proto.Error.Method_error.pp e
  | Ok g ->
      Fmt.pr "  Mailbox/get returned %d mailbox(es) with just those@."
        (List.length g.list)

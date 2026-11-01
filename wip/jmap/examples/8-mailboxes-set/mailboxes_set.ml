(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto

let doc = "Create a mailbox and a child with creation ids, then destroy both"

let () =
  Jmap_eio.Cli.main "mailboxes_set" ~doc @@ fun ctx ->
  let account_id = ctx.account_id and client = ctx.client in
  let name what = Printf.sprintf "tutorial-%d-%s" (Unix.getpid ()) what in
  let parent = Proto.Mailbox.creation "parent"
  and child = Proto.Mailbox.creation "child" in
  let created =
    Client.call_exn client
      (Chain.mailbox_set ~account_id
         ~create:
           [
             (parent, Proto.Mailbox.create_exn ~name:(name "parent") ());
             ( child,
               Proto.Mailbox.create_exn ~name:(name "child")
                 ~parent_id:(Proto.Id.creation_ref parent)
                 () );
           ]
         ())
  in
  let created_id cid =
    Option.bind (Proto.Method.created created cid) Proto.Mailbox.id
  in
  let ids = List.filter_map created_id [ child; parent ] in
  let destroy () =
    let gone =
      Client.call_exn client
        (Chain.mailbox_set ~account_id ~destroy:(Chain.ids ids)
           ~on_destroy_remove_emails:true ())
    in
    Fmt.pr "destroyed %d mailbox(es)@."
      (List.length (Option.value gone.destroyed ~default:[]))
  in
  Fun.protect ~finally:destroy @@ fun () ->
  (match Proto.Method.set_failures created with
  | (cid, e) :: _ ->
      Fmt.failwith "Mailbox/set did not create #%a, %a" Proto.Id.pp cid
        Proto.Error.Set_error.pp e
  | [] -> ());
  Fmt.pr "created, new state %s@." created.new_state;
  List.iter
    (fun cid ->
      Fmt.pr "  #%a -> %a@." Proto.Id.pp_creation cid
        Fmt.(option ~none:(any "?") Proto.Id.pp)
        (created_id cid))
    [ parent; child ];
  let got =
    Client.call_exn client
      (Chain.mailbox_get ~account_id ~ids:(Chain.ids ids)
         ~properties:[ `Id; `Name; `Parent_id ] ())
  in
  List.iter
    (fun (m : Proto.Mailbox.t) ->
      Fmt.pr "  %-26s parentId=%a@."
        (Option.value m.name ~default:"?")
        Fmt.(option ~none:(any "(top level)") Proto.Id.pp)
        m.parent_id)
    got.list

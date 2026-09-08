(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Sync = Jmap_eio.Sync

let doc = "Set and unset the $seen keyword of the newest Inbox message"

let pp_keywords ppf = function
  | [] -> Format.pp_print_string ppf "(none)"
  | ks -> Fmt.(list ~sep:sp Proto.Keyword.pp) ppf ks

let keywords client ~account_id id =
  let got =
    Client.call_exn client
      (Chain.email_get ~account_id ~ids:(Chain.ids [ id ])
         ~properties:[ `Keywords ] ())
  in
  match got.list with
  | [ e ] -> (got.state, Proto.Email.keyword_list e)
  | _ -> Fmt.failwith "Email/get returned no message for %a" Proto.Id.pp id

let update client ~account_id ?if_in_state id patch =
  Fmt.pr "  patch %a@."
    Fmt.(list ~sep:sp string)
    (List.map fst (Proto.Patch.to_list patch));
  let set =
    Client.call_exn client
      (Chain.email_set ~account_id ?if_in_state ~update:[ (id, patch) ] ())
  in
  (match Proto.Method.set_failures set with
  | (id, e) :: _ ->
      Fmt.failwith "Email/set left %a alone, %a" Proto.Id.pp id
        Proto.Error.Set_error.pp e
  | [] -> ());
  Fmt.pr "  new state %s@." set.new_state

let () =
  Jmap_eio.Cli.main "keywords" ~doc @@ fun ctx ->
  let account_id = ctx.account_id and client = ctx.client in
  let inbox = Sync.mailbox_id_exn client ~account_id `Inbox in
  let newest =
    Client.call_exn client
      (Chain.email_query ~account_id
         ~filter:(Proto.Email.filter ~in_mailbox:inbox ())
         ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
         ~limit:1L ())
  in
  match newest.ids with
  | [] -> Fmt.failwith "the Inbox is empty; deliver a message first"
  | email_id :: _ ->
      let _, before = keywords client ~account_id email_id in
      Fmt.pr "%a@.before    %a@." Proto.Id.pp email_id pp_keywords before;
      update client ~account_id email_id
        (Proto.Patch.v [ Proto.Email.Patch.set_keyword `Seen ]);
      let state, after = keywords client ~account_id email_id in
      Fmt.pr "after     %a@." pp_keywords after;
      update client ~account_id ~if_in_state:state email_id
        (Proto.Patch.v [ Proto.Email.Patch.set_keywords before ]);
      Fmt.pr "restored  %a@." pp_keywords
        (snd (keywords client ~account_id email_id))

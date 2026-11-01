(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Proto = Jmap.Proto
module Client = Jmap_eio.Client
module Sync = Jmap_eio.Sync

let pp_ids = Fmt.(list ~sep:(any " ") Proto.Id.pp)

let pp_email ppf (e : Proto.Email.t) =
  Fmt.pf ppf "  %a  %s"
    Fmt.(option ~none:(any "?") Proto.Id.pp)
    e.id
    (Option.value e.subject ~default:"(no subject)")

let () =
  Jmap_eio.Cli.main "paging"
    ~doc:"Page an Email/query and batch the Email/get that follows it"
  @@ fun ctx ->
  let client = ctx.client and account_id = ctx.account_id in
  let newest ~position ~limit =
    Chain.email_query ~account_id
      ~sort:[ Proto.Email.sort ~ascending:false `Received_at ]
      ~position ~limit ()
  in
  let q = Client.call_exn client (newest ~position:0L ~limit:3L) in
  Fmt.pr "Email/query  position %Ld  limit %a  ids %a@." q.position
    Fmt.(option ~none:(any "as asked") int64)
    q.limit pp_ids q.ids;

  Fmt.pr "@.Sync.pages ~page_size:3L, first four pages@.";
  Seq.iteri
    (fun i -> function
      | Ok ids -> Fmt.pr "  page %d  %a@." i pp_ids ids
      | Error e -> Fmt.failwith "page %d: %a" i Sync.pp_error e)
    (Seq.take 4 (Sync.pages client ~page_size:3L newest));

  let ids =
    match Sync.all_ids client ~page_size:3L ~max:7 newest with
    | Ok ids -> ids
    | Error e -> Fmt.failwith "all_ids: %a" Sync.pp_error e
  in
  if List.is_empty ids then Fmt.failwith "the account has no messages";
  Fmt.pr "@.Sync.all_ids ~max:7  %d ids@." (List.length ids);

  Fmt.pr "@.maxObjectsInGet %a, batching Email/get by 2@."
    Fmt.(option ~none:(any "unset") int64)
    (Sync.max_objects_in_get client);
  match
    Sync.get_all client ~batch:2L ids (fun ~ids ->
        Chain.email_get ~account_id ~ids:(Chain.ids ids)
          ~properties:[ `Id; `Subject ] ())
  with
  | Error e -> Fmt.failwith "get_all: %a" Sync.pp_error e
  | Ok (emails, not_found) ->
      Fmt.pr "%a@." Fmt.(vbox (list ~sep:cut pp_email)) emails;
      if not (List.is_empty not_found) then
        Fmt.pr "  notFound %a@." pp_ids not_found

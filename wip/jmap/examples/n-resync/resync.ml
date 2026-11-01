(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Chain = Jmap.Chain
module Client = Jmap_eio.Client
module Proto = Jmap.Proto
module Results = Jmap.Chain.Results
module Sync = Jmap_eio.Sync

let doc = "Bring a local cache back into step with /changes and /queryChanges"
let list_size = 10
let newest = [ Proto.Email.sort ~ascending:false `Received_at ]
let email_properties = [ `Id; `Subject; `Received_at; `Keywords ]
let mailbox_properties = [ `Id; `Name; `Total_emails; `Unread_emails ]

let since_term =
  let open Cmdliner in
  let doc =
    "Resume the Email delta from the state string $(docv), as printed by an \
     earlier run. The default, 0, is the state of a cache holding nothing."
  in
  Arg.(value & opt string "0" & info [ "since" ] ~docv:"STATE" ~doc)

type cache = {
  emails : (Proto.Id.t, Proto.Email.t) Hashtbl.t;
  mailboxes : (Proto.Id.t, Proto.Mailbox.t) Hashtbl.t;
  mutable ids : Proto.Id.t list;
  mutable email_state : string;
  mutable mailbox_state : string;
  mutable query_state : string;
}

let pp_list pp ppf = function
  | [] -> Format.pp_print_string ppf "-"
  | l -> Fmt.(list ~sep:(any " ") pp) ppf l

let pp_ids = pp_list Proto.Id.pp

let pp_added =
  pp_list (fun ppf (a : Proto.Filter.added_item) ->
      Fmt.pf ppf "%a@@%Ld" Proto.Id.pp a.id a.index)

let put tbl id v = Option.iter (fun id -> Hashtbl.replace tbl id v) id
let without id l = List.filter (fun x -> not (Proto.Id.equal x id)) l

let insert_at index id l =
  let rec go n = function
    | l when n <= 0 -> id :: l
    | [] -> [ id ]
    | x :: r -> x :: go (n - 1) r
  in
  go index (without id l)

let merge_mailbox cache (m : Proto.Mailbox.t) =
  let or_else v old = if Option.is_some v then v else old in
  let m =
    match Option.bind m.id (Hashtbl.find_opt cache.mailboxes) with
    | None -> m
    | Some (o : Proto.Mailbox.t) ->
        {
          m with
          name = or_else m.name o.name;
          total_emails = or_else m.total_emails o.total_emails;
          unread_emails = or_else m.unread_emails o.unread_emails;
        }
  in
  put cache.mailboxes m.id m

let fetch_emails client ~account_id cache ids =
  match
    Sync.get_all client ids (fun ~ids ->
        Chain.email_get ~account_id ~ids:(Chain.ids ids)
          ~properties:email_properties ())
  with
  | Error e -> Fmt.failwith "Email/get: %a" Sync.pp_error e
  | Ok (emails, not_found) ->
      List.iter (fun (e : Proto.Email.t) -> put cache.emails e.id e) emails;
      List.iter (Hashtbl.remove cache.emails) not_found;
      Fmt.pr "  cached %d record(s), %d id(s) gone before the Email/get@."
        (List.length emails) (List.length not_found)

let fetch_mailboxes client ~account_id cache ids =
  match
    Sync.get_all client ids (fun ~ids ->
        Chain.mailbox_get ~account_id ~ids:(Chain.ids ids)
          ~properties:mailbox_properties ())
  with
  | Error e -> Fmt.failwith "Mailbox/get: %a" Sync.pp_error e
  | Ok (mailboxes, _) ->
      List.iter
        (fun (m : Proto.Mailbox.t) -> put cache.mailboxes m.id m)
        mailboxes

let list_query ~account_id =
  Chain.email_query ~account_id ~sort:newest ~limit:(Int64.of_int list_size) ()

let set_list cache (q : Proto.Method.query_response) =
  cache.ids <- q.ids;
  cache.query_state <- q.query_state;
  Fmt.pr "  %d id(s) at queryState %s@." (List.length cache.ids)
    cache.query_state

let rebuild client ~account_id cache =
  let Results.[ query; state ] =
    Client.run_exn client
      Chain.(
        let* q = list_query ~account_id in
        let+ s = email_state ~account_id in
        Handles.[ q; s ])
  in
  Hashtbl.reset cache.emails;
  cache.email_state <- state;
  Fmt.pr "  refetching the cache at state %s@." state;
  set_list cache query;
  fetch_emails client ~account_id cache cache.ids

let rec resync_emails ?(fuel = 100) client ~account_id cache =
  Fmt.pr "Email/changes since %s@." cache.email_state;
  match
    Sync.email_changes client ~account_id ~since:cache.email_state
      ~max_changes:8L ~fuel:1 ()
  with
  | Error e -> Fmt.failwith "Email/changes: %a" Sync.pp_error e
  | Ok `Cannot_calculate_changes ->
      Fmt.pr "  cannotCalculateChanges: the cache is too old to patch@.";
      rebuild client ~account_id cache
  | Ok (`Changes c) ->
      Fmt.pr "  created %a  updated %a  destroyed %a@." pp_ids c.created pp_ids
        c.updated pp_ids c.destroyed;
      Fmt.pr "  newState %s hasMore %b@." c.new_state c.has_more;
      List.iter (Hashtbl.remove cache.emails) c.destroyed;
      cache.ids <- List.fold_left (Fun.flip without) cache.ids c.destroyed;
      fetch_emails client ~account_id cache (c.created @ c.updated);
      cache.email_state <- c.new_state;
      if c.has_more then
        if fuel <= 1 then
          Fmt.failwith "request budget exhausted; resume Email/changes from %s"
            cache.email_state
        else resync_emails ~fuel:(fuel - 1) client ~account_id cache

let rec resync_mailboxes client ~account_id cache =
  Fmt.pr "@.Mailbox/changes since %s@." cache.mailbox_state;
  let Results.[ delta; created; updated ] =
    Client.run_exn client
      Chain.(
        let* ch =
          mailbox_changes ~account_id ~since_state:cache.mailbox_state
            ~max_changes:3L ()
        in
        let* n =
          mailbox_get ~account_id ~ids:(from_changes_created ch)
            ~properties:mailbox_properties ()
        in
        let+ u =
          mailbox_get ~account_id ~ids:(from_changes_updated ch)
            ~properties_ref:(from_changes_updated_properties ch)
            ()
        in
        Handles.[ ch; n; attempt u ])
  in
  let c = delta.changes in
  Fmt.pr "  created %a  updated %a  destroyed %a@." pp_ids c.created pp_ids
    c.updated pp_ids c.destroyed;
  Fmt.pr "  newState %s hasMoreChanges %b updatedProperties %a@." c.new_state
    c.has_more_changes
    Fmt.(option ~none:(any "null") (list ~sep:(any ",") string))
    delta.updated_properties;
  List.iter (Hashtbl.remove cache.mailboxes) c.destroyed;
  List.iter
    (fun (m : Proto.Mailbox.t) -> put cache.mailboxes m.id m)
    created.list;
  (match updated with
  | Error e ->
      Fmt.pr "  the #properties reference was refused (%s)@."
        (Proto.Error.Method_error.to_string e);
      fetch_mailboxes client ~account_id cache c.updated
  | Ok got ->
      let unknown =
        List.filter (fun id -> not (Hashtbl.mem cache.mailboxes id)) c.updated
      in
      List.iter (merge_mailbox cache) got.list;
      fetch_mailboxes client ~account_id cache unknown);
  let previous = cache.mailbox_state in
  cache.mailbox_state <- c.new_state;
  if c.has_more_changes && cache.mailbox_state <> previous then
    resync_mailboxes client ~account_id cache

let resync_list client ~account_id cache =
  Fmt.pr "@.Email/queryChanges since %s@." cache.query_state;
  match
    Client.call_exn client
      (Chain.attempt_call
         (Chain.email_query_changes ~account_id
            ~since_query_state:cache.query_state ~sort:newest ~max_changes:64L
            ()))
  with
  | Error e ->
      Fmt.pr "  %s: querying the list again@."
        (Proto.Error.Method_error.to_string e);
      set_list cache (Client.call_exn client (list_query ~account_id))
  | Ok qc ->
      Fmt.pr "  removed %a  added %a@." pp_ids qc.removed pp_added qc.added;
      let kept = List.fold_left (Fun.flip without) cache.ids qc.removed in
      let list =
        List.fold_left
          (fun l (a : Proto.Filter.added_item) ->
            insert_at (Int64.to_int a.index) a.id l)
          kept qc.added
      in
      cache.ids <- List.filteri (fun i _ -> i < list_size) list;
      cache.query_state <- qc.new_query_state

let pp_mailbox ppf (m : Proto.Mailbox.t) =
  Fmt.pf ppf "  %-18s total %a unread %a"
    (Option.value m.name ~default:"?")
    Fmt.(option ~none:(any "-") int64)
    m.total_emails
    Fmt.(option ~none:(any "-") int64)
    m.unread_emails

let pp_message cache ppf id =
  match Hashtbl.find_opt cache.emails id with
  | None -> Fmt.pf ppf "  %a is in the list but not in the cache" Proto.Id.pp id
  | Some (e : Proto.Email.t) ->
      Fmt.pf ppf "  %s %s %s"
        (Option.fold ~none:"?" ~some:Proto.Date.to_utc_string e.received_at)
        (if Proto.Email.has_keyword `Seen e then " " else "*")
        (Option.value e.subject ~default:"(no subject)")

let print_cache cache =
  Fmt.pr "@.Cache: %d mailbox(es), %d record(s), a list of %d@."
    (Hashtbl.length cache.mailboxes)
    (Hashtbl.length cache.emails)
    (List.length cache.ids);
  Hashtbl.fold (fun _ m acc -> m :: acc) cache.mailboxes []
  |> List.sort (fun (a : Proto.Mailbox.t) (b : Proto.Mailbox.t) ->
      compare a.name b.name)
  |> Fmt.pr "%a@." Fmt.(vbox (list ~sep:cut pp_mailbox));
  Fmt.pr "%a@." Fmt.(vbox (list ~sep:cut (pp_message cache))) cache.ids

let () =
  Jmap_eio.Cli.main' "resync" ~doc ~args:since_term @@ fun ctx since ->
  let client = ctx.client and account_id = ctx.account_id in
  let cache =
    {
      emails = Hashtbl.create 64;
      mailboxes = Hashtbl.create 16;
      ids = [];
      email_state = since;
      mailbox_state = "0";
      query_state = "0";
    }
  in
  resync_emails client ~account_id cache;
  resync_mailboxes client ~account_id cache;
  resync_list client ~account_id cache;
  let missing =
    List.filter (fun id -> not (Hashtbl.mem cache.emails id)) cache.ids
  in
  if not (List.is_empty missing) then (
    Fmt.pr "@.%d message(s) of the list are not cached@." (List.length missing);
    fetch_emails client ~account_id cache missing);
  print_cache cache;
  Fmt.pr "@.Resume with: --since %s@." cache.email_state

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type get_args = {
  account_id : Proto_id.t;
  ids : Proto_id.t list option;
  properties : string list option;
}

let get_args ~account_id ?ids ?properties () = { account_id; ids; properties }
let get_args_make account_id ids properties = { account_id; ids; properties }

let get_args_jsont =
  let kind = "GetArgs" in
  Jsont.Object.map ~kind get_args_make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
  (* RFC 8620 Section 5.1: an explicit null [ids] means "all the records",
     which an absent member does not, so [None] is written out as null.  An
     absent [properties] already means "all the properties", so [None] omits
     the member. *)
  |> Proto_json_map.nullable_mem_null "ids" (Jsont.list Proto_id.jsont)
       ~enc:(fun a -> a.ids)
  |> Proto_json_map.nullable_mem "properties" (Jsont.list Jsont.string)
       ~enc:(fun a -> a.properties)
  |> Jsont.Object.finish

type 'a get_response = {
  account_id : Proto_id.t;
  state : string;
  list : 'a list;
  not_found : Proto_id.t list;
}

let get_response_jsont (type a) (obj_jsont : a Jsont.t) : a get_response Jsont.t
    =
  let kind = "GetResponse" in
  let make account_id state list not_found =
    { account_id; state; list; not_found }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
  |> Jsont.Object.mem "state" Jsont.string ~enc:(fun r -> r.state)
  |> Jsont.Object.mem "list" (Jsont.list obj_jsont) ~enc:(fun r -> r.list)
  |> Jsont.Object.mem "notFound" (Jsont.list Proto_id.jsont) ~enc:(fun r ->
      r.not_found)
  |> Jsont.Object.finish

let in_ids_order ~id ids l =
  let by_id = Hashtbl.create (List.length l) in
  List.iter
    (fun r ->
      match id r with
      | None -> ()
      | Some i ->
          let k = Proto_id.to_string i in
          if not (Hashtbl.mem by_id k) then Hashtbl.add by_id k r)
    l;
  List.filter_map (fun i -> Hashtbl.find_opt by_id (Proto_id.to_string i)) ids

type changes_args = {
  account_id : Proto_id.t;
  since_state : string;
  max_changes : int64 option;
}

let changes_args ~account_id ~since_state ?max_changes () =
  Option.iter
    (fun n ->
      if n <= 0L then
        invalid_arg
          "Jmap.Proto.Method.changes_args: max_changes must be positive")
    max_changes;
  { account_id; since_state; max_changes }

let changes_args_make account_id since_state max_changes =
  { account_id; since_state; max_changes }

let positive_uint53_jsont =
  let kind = "Positive UnsignedInt53" in
  let validate n =
    if n > 0L then n
    else Jsont.Error.msgf Jsont.Meta.none "%s: expected > 0" kind
  in
  Jsont.map ~kind ~dec:validate ~enc:validate Proto_int53.Unsigned.jsont

let changes_args_jsont =
  let kind = "ChangesArgs" in
  Jsont.Object.map ~kind changes_args_make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
  |> Jsont.Object.mem "sinceState" Jsont.string ~enc:(fun a -> a.since_state)
  |> Proto_json_map.nullable_mem "maxChanges" positive_uint53_jsont
       ~enc:(fun a -> a.max_changes)
  |> Jsont.Object.finish

type changes_response = {
  account_id : Proto_id.t;
  old_state : string;
  new_state : string;
  has_more_changes : bool;
  created : Proto_id.t list;
  updated : Proto_id.t list;
  destroyed : Proto_id.t list;
}

let changes_response_make account_id old_state new_state has_more_changes
    created updated destroyed =
  {
    account_id;
    old_state;
    new_state;
    has_more_changes;
    created;
    updated;
    destroyed;
  }

let changes_response_mems ~changes map =
  map
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r ->
      (changes r).account_id)
  |> Jsont.Object.mem "oldState" Jsont.string ~enc:(fun r ->
      (changes r).old_state)
  |> Jsont.Object.mem "newState" Jsont.string ~enc:(fun r ->
      (changes r).new_state)
  |> Jsont.Object.mem "hasMoreChanges" Jsont.bool ~enc:(fun r ->
      (changes r).has_more_changes)
  |> Jsont.Object.mem "created" (Jsont.list Proto_id.jsont) ~enc:(fun r ->
      (changes r).created)
  |> Jsont.Object.mem "updated" (Jsont.list Proto_id.jsont) ~enc:(fun r ->
      (changes r).updated)
  |> Jsont.Object.mem "destroyed" (Jsont.list Proto_id.jsont) ~enc:(fun r ->
      (changes r).destroyed)

let changes_response_jsont =
  let kind = "ChangesResponse" in
  Jsont.Object.map ~kind changes_response_make
  |> changes_response_mems ~changes:Fun.id
  |> Jsont.Object.finish

type 'a set_args = {
  account_id : Proto_id.t;
  if_in_state : string option;
  create : ('a Proto_id.creation * 'a) list option;
  update : (Proto_id.t * Proto_patch.t) list option;
  destroy : Proto_id.t list option;
}

let set_args ~account_id ?if_in_state ?create ?update ?destroy () =
  { account_id; if_in_state; create; update; destroy }

let set_args_jsont (type a) (obj_jsont : a Jsont.t) : a set_args Jsont.t =
  let kind = "SetArgs" in
  let make account_id if_in_state create update destroy =
    { account_id; if_in_state; create; update; destroy }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
  |> Proto_json_map.nullable_mem "ifInState" Jsont.string ~enc:(fun a ->
      a.if_in_state)
  |> Proto_json_map.nullable_mem "create" (Proto_json_map.of_creation obj_jsont)
       ~enc:(fun a -> a.create)
  |> Proto_json_map.nullable_mem "update"
       (Proto_json_map.of_id_or_creation Proto_patch.jsont) ~enc:(fun a ->
         a.update)
  (* RFC 8620 Section 5.3: an entry of [destroy] is a client argument and so
     may be a creation reference to a record created in the same /set. *)
  |> Proto_json_map.nullable_mem "destroy"
       (Jsont.list Proto_id.jsont_or_creation) ~enc:(fun a -> a.destroy)
  |> Jsont.Object.finish

type 'a set_response = {
  account_id : Proto_id.t;
  old_state : string option;
  new_state : string;
  created : (Proto_id.t * 'a) list option;
  updated : (Proto_id.t * 'a option) list option;
  destroyed : Proto_id.t list option;
  not_created : (Proto_id.t * Proto_error.Set_error.t) list option;
  not_updated : (Proto_id.t * Proto_error.Set_error.t) list option;
  not_destroyed : (Proto_id.t * Proto_error.Set_error.t) list option;
}

let set_response_jsont (type a) (obj_jsont : a Jsont.t) : a set_response Jsont.t
    =
  let kind = "SetResponse" in
  let make account_id old_state new_state created updated destroyed not_created
      not_updated not_destroyed =
    {
      account_id;
      old_state;
      new_state;
      created;
      updated;
      destroyed;
      not_created;
      not_updated;
      not_destroyed;
    }
  in
  let errors = Proto_json_map.of_id Proto_error.Set_error.jsont in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
  |> Proto_json_map.nullable_mem "oldState" Jsont.string ~enc:(fun r ->
      r.old_state)
  |> Jsont.Object.mem "newState" Jsont.string ~enc:(fun r -> r.new_state)
  |> Proto_json_map.nullable_mem "created" (Proto_json_map.of_id obj_jsont)
       ~enc:(fun r -> r.created)
  |> Proto_json_map.nullable_mem "updated"
       (Proto_json_map.of_id (Jsont.option obj_jsont))
       ~enc:(fun r -> r.updated)
  |> Proto_json_map.nullable_mem "destroyed" (Jsont.list Proto_id.jsont)
       ~enc:(fun r -> r.destroyed)
  |> Proto_json_map.nullable_mem "notCreated" errors ~enc:(fun r ->
      r.not_created)
  |> Proto_json_map.nullable_mem "notUpdated" errors ~enc:(fun r ->
      r.not_updated)
  |> Proto_json_map.nullable_mem "notDestroyed" errors ~enc:(fun r ->
      r.not_destroyed)
  |> Jsont.Object.finish

let find_creation map c =
  match map with
  | None -> None
  | Some l ->
      let cid = Proto_id.creation_id c in
      Option.map snd (List.find_opt (fun (k, _) -> Proto_id.equal k cid) l)

let created r c = find_creation r.created c
let not_created r c = find_creation r.not_created c

let set_failures r =
  let l = Option.value ~default:[] in
  l r.not_created @ l r.not_updated @ l r.not_destroyed

let pp_set_failure ppf (id, e) =
  Format.fprintf ppf "%a: %a" Proto_id.pp id Proto_error.Set_error.pp e

type 'a copy_args = {
  from_account_id : Proto_id.t;
  if_from_in_state : string option;
  account_id : Proto_id.t;
  if_in_state : string option;
  create : ('a Proto_id.creation * 'a) list;
  on_success_destroy_original : bool;
  destroy_from_if_in_state : string option;
}

let copy_args_check ~from_account_id ~account_id ~create =
  if Proto_id.equal from_account_id account_id then
    Some "source and destination accounts must differ"
  else if List.is_empty create then Some "create must not be empty"
  else None

let copy_args ~from_account_id ?if_from_in_state ~account_id ?if_in_state
    ~create ?(on_success_destroy_original = false) ?destroy_from_if_in_state ()
    =
  (match copy_args_check ~from_account_id ~account_id ~create with
  | Some msg -> invalid_arg ("Jmap.Proto.Method.copy_args: " ^ msg)
  | None -> ());
  {
    from_account_id;
    if_from_in_state;
    account_id;
    if_in_state;
    create;
    on_success_destroy_original;
    destroy_from_if_in_state;
  }

let copy_args_jsont (type a) (obj_jsont : a Jsont.t) : a copy_args Jsont.t =
  let kind = "CopyArgs" in
  let make from_account_id if_from_in_state account_id if_in_state create
      on_success_destroy_original destroy_from_if_in_state =
    {
      from_account_id;
      if_from_in_state;
      account_id;
      if_in_state;
      create;
      on_success_destroy_original;
      destroy_from_if_in_state;
    }
  in
  let validate args =
    match
      copy_args_check ~from_account_id:args.from_account_id
        ~account_id:args.account_id ~create:args.create
    with
    | Some msg -> Jsont.Error.msgf Jsont.Meta.none "%s: %s" kind msg
    | None -> args
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "fromAccountId" Proto_id.jsont ~enc:(fun a ->
      a.from_account_id)
  |> Proto_json_map.nullable_mem "ifFromInState" Jsont.string ~enc:(fun a ->
      a.if_from_in_state)
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
  |> Proto_json_map.nullable_mem "ifInState" Jsont.string ~enc:(fun a ->
      a.if_in_state)
  |> Jsont.Object.mem "create" (Proto_json_map.of_creation obj_jsont)
       ~enc:(fun a -> a.create)
  |> Jsont.Object.mem "onSuccessDestroyOriginal" Jsont.bool ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.on_success_destroy_original)
       ~enc_omit:(fun b -> not b)
  |> Proto_json_map.nullable_mem "destroyFromIfInState" Jsont.string
       ~enc:(fun a -> a.destroy_from_if_in_state)
  |> Jsont.Object.finish
  |> Jsont.map ~kind ~dec:validate ~enc:validate

type 'a copy_response = {
  from_account_id : Proto_id.t;
  account_id : Proto_id.t;
  old_state : string option;
  new_state : string;
  created : (Proto_id.t * 'a) list option;
  not_created : (Proto_id.t * Proto_error.Set_error.t) list option;
}

let copy_response_jsont (type a) (obj_jsont : a Jsont.t) :
    a copy_response Jsont.t =
  let kind = "CopyResponse" in
  let make from_account_id account_id old_state new_state created not_created =
    { from_account_id; account_id; old_state; new_state; created; not_created }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "fromAccountId" Proto_id.jsont ~enc:(fun r ->
      r.from_account_id)
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
  |> Proto_json_map.nullable_mem "oldState" Jsont.string ~enc:(fun r ->
      r.old_state)
  |> Jsont.Object.mem "newState" Jsont.string ~enc:(fun r -> r.new_state)
  |> Proto_json_map.nullable_mem "created" (Proto_json_map.of_id obj_jsont)
       ~enc:(fun r -> r.created)
  |> Proto_json_map.nullable_mem "notCreated"
       (Proto_json_map.of_id Proto_error.Set_error.jsont) ~enc:(fun r ->
         r.not_created)
  |> Jsont.Object.finish

type 'filter query_args = {
  account_id : Proto_id.t;
  filter : 'filter Proto_filter.filter option;
  sort : Proto_filter.comparator list option;
  position : int64;
  anchor : Proto_id.t option;
  anchor_offset : int64;
  limit : int64 option;
  calculate_total : bool;
}

let query_args ~account_id ?filter ?sort ?(position = 0L) ?anchor
    ?(anchor_offset = 0L) ?limit ?(calculate_total = false) () =
  {
    account_id;
    filter;
    sort;
    position;
    anchor;
    anchor_offset;
    limit;
    calculate_total;
  }

let query_args_jsont (type f) (filter_cond_jsont : f Jsont.t) :
    f query_args Jsont.t =
  let kind = "QueryArgs" in
  let make account_id filter sort position anchor anchor_offset limit
      calculate_total =
    {
      account_id;
      filter;
      sort;
      position;
      anchor;
      anchor_offset;
      limit;
      calculate_total;
    }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
  |> Proto_json_map.nullable_mem "filter"
       (Proto_filter.filter_jsont filter_cond_jsont) ~enc:(fun a -> a.filter)
  |> Proto_json_map.nullable_mem "sort"
       (Jsont.list Proto_filter.comparator_jsont) ~enc:(fun a -> a.sort)
  |> Jsont.Object.mem "position" Proto_int53.Signed.jsont ~dec_absent:(fun () -> 0L)
       ~enc:(fun a -> a.position)
       ~enc_omit:(fun n -> Int64.equal 0L n)
  |> Proto_json_map.nullable_mem "anchor" Proto_id.jsont ~enc:(fun a ->
      a.anchor)
  |> Jsont.Object.mem "anchorOffset" Proto_int53.Signed.jsont ~dec_absent:(fun () -> 0L)
       ~enc:(fun a -> a.anchor_offset)
       ~enc_omit:(fun n -> Int64.equal 0L n)
  |> Proto_json_map.nullable_mem "limit" Proto_int53.Unsigned.jsont
       ~enc:(fun a -> a.limit)
  |> Jsont.Object.mem "calculateTotal" Jsont.bool ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.calculate_total)
       ~enc_omit:(fun b -> not b)
  |> Jsont.Object.finish

type query_response = {
  account_id : Proto_id.t;
  query_state : string;
  can_calculate_changes : bool;
  position : int64;
  ids : Proto_id.t list;
  total : int64 option;
  limit : int64 option;
}

let query_response_make account_id query_state can_calculate_changes position
    ids total limit =
  {
    account_id;
    query_state;
    can_calculate_changes;
    position;
    ids;
    total;
    limit;
  }

let query_response_jsont =
  let kind = "QueryResponse" in
  Jsont.Object.map ~kind query_response_make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
  |> Jsont.Object.mem "queryState" Jsont.string ~enc:(fun r -> r.query_state)
  |> Jsont.Object.mem "canCalculateChanges" Jsont.bool ~enc:(fun r ->
      r.can_calculate_changes)
  |> Jsont.Object.mem "position" Proto_int53.Unsigned.jsont ~enc:(fun r ->
      r.position)
  |> Jsont.Object.mem "ids" (Jsont.list Proto_id.jsont) ~enc:(fun r -> r.ids)
  |> Jsont.Object.opt_mem "total" Proto_int53.Unsigned.jsont ~enc:(fun r ->
      r.total)
  |> Jsont.Object.opt_mem "limit" Proto_int53.Unsigned.jsont ~enc:(fun r ->
      r.limit)
  |> Jsont.Object.finish

type 'filter query_changes_args = {
  account_id : Proto_id.t;
  filter : 'filter Proto_filter.filter option;
  sort : Proto_filter.comparator list option;
  since_query_state : string;
  max_changes : int64 option;
  up_to_id : Proto_id.t option;
  calculate_total : bool;
}

let query_changes_args ~account_id ?filter ?sort ~since_query_state ?max_changes
    ?up_to_id ?(calculate_total = false) () =
  {
    account_id;
    filter;
    sort;
    since_query_state;
    max_changes;
    up_to_id;
    calculate_total;
  }

let query_changes_args_jsont (type f) (filter_cond_jsont : f Jsont.t) :
    f query_changes_args Jsont.t =
  let kind = "QueryChangesArgs" in
  let make account_id filter sort since_query_state max_changes up_to_id
      calculate_total =
    {
      account_id;
      filter;
      sort;
      since_query_state;
      max_changes;
      up_to_id;
      calculate_total;
    }
  in
  Jsont.Object.map ~kind make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun a -> a.account_id)
  |> Proto_json_map.nullable_mem "filter"
       (Proto_filter.filter_jsont filter_cond_jsont) ~enc:(fun a -> a.filter)
  |> Proto_json_map.nullable_mem "sort"
       (Jsont.list Proto_filter.comparator_jsont) ~enc:(fun a -> a.sort)
  |> Jsont.Object.mem "sinceQueryState" Jsont.string ~enc:(fun a ->
      a.since_query_state)
  |> Proto_json_map.nullable_mem "maxChanges" Proto_int53.Unsigned.jsont
       ~enc:(fun a -> a.max_changes)
  |> Proto_json_map.nullable_mem "upToId" Proto_id.jsont ~enc:(fun a ->
      a.up_to_id)
  |> Jsont.Object.mem "calculateTotal" Jsont.bool ~dec_absent:(fun () -> false)
       ~enc:(fun a -> a.calculate_total)
       ~enc_omit:(fun b -> not b)
  |> Jsont.Object.finish

type query_changes_response = {
  account_id : Proto_id.t;
  old_query_state : string;
  new_query_state : string;
  total : int64 option;
  removed : Proto_id.t list;
  added : Proto_filter.added_item list;
}

let query_changes_response_make account_id old_query_state new_query_state total
    removed added =
  { account_id; old_query_state; new_query_state; total; removed; added }

let query_changes_response_jsont =
  let kind = "QueryChangesResponse" in
  Jsont.Object.map ~kind query_changes_response_make
  |> Jsont.Object.mem "accountId" Proto_id.jsont ~enc:(fun r -> r.account_id)
  |> Jsont.Object.mem "oldQueryState" Jsont.string ~enc:(fun r ->
      r.old_query_state)
  |> Jsont.Object.mem "newQueryState" Jsont.string ~enc:(fun r ->
      r.new_query_state)
  |> Jsont.Object.opt_mem "total" Proto_int53.Unsigned.jsont ~enc:(fun r ->
      r.total)
  |> Jsont.Object.mem "removed" (Jsont.list Proto_id.jsont) ~enc:(fun r ->
      r.removed)
  |> Jsont.Object.mem "added" (Jsont.list Proto_filter.added_item_jsont)
       ~enc:(fun r -> r.added)
  |> Jsont.Object.finish

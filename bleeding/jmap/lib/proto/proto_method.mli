@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The standard JMAP methods.

    {{:https://datatracker.ietf.org/doc/html/rfc8620#section-5} RFC 8620 Section
     5} defines six methods that every data type may support, named [Foo/get],
    [Foo/changes], [Foo/set], [Foo/copy], [Foo/query] and [Foo/queryChanges].
    The arguments and responses below are the ones common to all of them,
    parameterised by the record type or filter condition of the data type.

    @canonical Jmap.Proto.Method *)

(** {1 Foo/get} *)

type get_args = {
  account_id : Proto_id.t;  (** The account to fetch from. *)
  ids : Proto_id.t list option;
      (** The ids to fetch. [None] means every record and is written out as an
          explicit ["ids": null], per the [Id[]|null] type RFC 8620 Section 5.1
          gives it. *)
  properties : string list option;
      (** The properties to return. [None] means every property. *)
}
(** The type for the arguments of a [/get] call. *)

val get_args :
  account_id:Proto_id.t ->
  ?ids:Proto_id.t list ->
  ?properties:string list ->
  unit ->
  get_args
(** [get_args ~account_id ()] is the arguments of a [/get] call on [account_id].
    [ids] and [properties] are [None] unless given. *)

val get_args_jsont : get_args Jsont.t
(** [get_args_jsont] is the codec for the arguments of a [/get] call. *)

type 'a get_response = {
  account_id : Proto_id.t;  (** The account fetched from. *)
  state : string;  (** The state of the data type in that account. *)
  list : 'a list;  (** The records fetched. *)
  not_found : Proto_id.t list;  (** The requested ids that do not exist. *)
}
(** The type for the response of a [/get] call over records of type ['a]. *)

val get_response_jsont : 'a Jsont.t -> 'a get_response Jsont.t
(** [get_response_jsont record] is the codec for the response of a [/get] call
    whose records are coded by [record]. *)

val in_ids_order :
  id:('a -> Proto_id.t option) -> Proto_id.t list -> 'a list -> 'a list
(** [in_ids_order ~id ids l] is the records of [l] in the order of [ids], where
    [id r] is the id of the record [r]. A record whose id is [None] or is absent
    from [ids] is dropped, and an id no record of [l] carries adds nothing.

    RFC 8620 Section 5.1 lets a [/get] return "the list of objects [...] in any
    order", so the order of the [/query] that produced [ids] is restored this
    way. Two records of [l] with the same id keep the first. *)

(** {1 Foo/changes} *)

type changes_args = {
  account_id : Proto_id.t;  (** The account to fetch changes in. *)
  since_state : string;  (** The state to fetch changes since. *)
  max_changes : int64 option;
      (** The maximum number of ids to return. A supplied value is positive. *)
}
(** The type for the arguments of a [/changes] call. *)

val changes_args :
  account_id:Proto_id.t ->
  since_state:string ->
  ?max_changes:int64 ->
  unit ->
  changes_args
(** [changes_args ~account_id ~since_state ()] is the arguments of a [/changes]
    call. [max_changes] is [None] unless given.

    @raise Stdlib.exception-Invalid_argument if [max_changes] is not positive.
*)

val changes_args_jsont : changes_args Jsont.t
(** [changes_args_jsont] is the codec for the arguments of a [/changes] call. *)

type changes_response = {
  account_id : Proto_id.t;  (** The account the changes are in. *)
  old_state : string;  (** The state the changes are since. *)
  new_state : string;  (** The state the changes bring the client to. *)
  has_more_changes : bool;
      (** Whether further changes remain between [new_state] and the current
          state. *)
  created : Proto_id.t list;  (** The ids of the records created. *)
  updated : Proto_id.t list;  (** The ids of the records updated. *)
  destroyed : Proto_id.t list;  (** The ids of the records destroyed. *)
}
(** The type for the response of a [/changes] call. *)

val changes_response_jsont : changes_response Jsont.t
(** [changes_response_jsont] is the codec for the response of a [/changes] call.
*)

val changes_response_mems :
  changes:('o -> changes_response) @ portable ->
  ( 'o,
    Proto_id.t ->
    string ->
    string ->
    bool ->
    Proto_id.t list ->
    Proto_id.t list ->
    Proto_id.t list ->
    'a )
  Jsont.Object.map ->
  ('o, 'a) Jsont.Object.map
(** [changes_response_mems ~changes map] is [map] with the seven members of a
    [/changes] response added, each read on encoding from [changes] of the value
    being encoded. It serves a data type whose [/changes] response carries
    members of its own, such as the [updatedProperties] of a [Mailbox/changes].
*)

(** {1 Foo/set} *)

type 'a set_args = {
  account_id : Proto_id.t;  (** The account to change. *)
  if_in_state : string option;
      (** The state the change requires. [None] applies the change whatever the
          current state. *)
  create : ('a Proto_id.creation * 'a) list option;
      (** The records to create, keyed by creation id. *)
  update : (Proto_id.t * Proto_patch.t) list option;
      (** The patches to apply, keyed by record id. A key may be a creation
          reference to a record created in the same call. *)
  destroy : Proto_id.t list option;
      (** The ids of the records to destroy. An entry may be a creation
          reference to a record created in the same call, per RFC 8620 Section
          5.3. *)
}
(** The type for the arguments of a [/set] call over records of type ['a]. *)

val set_args :
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  ?create:('a Proto_id.creation * 'a) list ->
  ?update:(Proto_id.t * Proto_patch.t) list ->
  ?destroy:Proto_id.t list ->
  unit ->
  'a set_args
(** [set_args ~account_id ()] is the arguments of a [/set] call on [account_id].
    Every other argument is [None] unless given. *)

val set_args_jsont : 'a Jsont.t -> 'a set_args Jsont.t
(** [set_args_jsont record] is the codec for the arguments of a [/set] call
    whose records are coded by [record]. *)

type 'a set_response = {
  account_id : Proto_id.t;  (** The account that was changed. *)
  old_state : string option;  (** The state before the change. *)
  new_state : string;  (** The state after the change. *)
  created : (Proto_id.t * 'a) list option;
      (** The records created, keyed by creation id, with the properties the
          server set. *)
  updated : (Proto_id.t * 'a option) list option;
      (** The records updated, keyed by record id. The value is [None] when the
          server set no property beyond those the client gave. *)
  destroyed : Proto_id.t list option;  (** The ids of the records destroyed. *)
  not_created : (Proto_id.t * Proto_error.Set_error.t) list option;
      (** Why each record that was not created failed. *)
  not_updated : (Proto_id.t * Proto_error.Set_error.t) list option;
      (** Why each record that was not updated failed. *)
  not_destroyed : (Proto_id.t * Proto_error.Set_error.t) list option;
      (** Why each record that was not destroyed failed. *)
}
(** The type for the response of a [/set] call over records of type ['a]. *)

val set_response_jsont : 'a Jsont.t -> 'a set_response Jsont.t
(** [set_response_jsont record] is the codec for the response of a [/set] call
    whose records are coded by [record]. Every map and list member of the
    response is typed [T|null] by RFC 8620 Section 5.3, so an absent member and
    an explicit [null] both decode to [None]. *)

val created : 'a set_response -> 'a Proto_id.creation -> 'a option
(** [created r c] is the record [r] reports under the creation id [c] in its
    [created] map, holding the properties the server set on it. It is [None]
    when the [/set] did not create a record under [c], either because the create
    failed or because it was never asked for. RFC 8620 Section 5.3 reports a
    create that failed under [notCreated] instead, which {!val-not_created} and
    {!set_failures} read. *)

val not_created :
  'a set_response -> 'a Proto_id.creation -> Proto_error.Set_error.t option
(** [not_created r c] is the error [r] reports under the creation id [c] in its
    [notCreated] map. It is [None] when the [/set] created a record under [c] or
    was never asked to. *)

val set_failures :
  'a set_response -> (Proto_id.t * Proto_error.Set_error.t) list
(** [set_failures r] is the [notCreated], [notUpdated] and [notDestroyed]
    entries of [r], in that order. It is [[]] when every record the [/set] named
    was created, updated or destroyed. *)

val pp_set_failure :
  Format.formatter -> Proto_id.t * Proto_error.Set_error.t -> unit
(** [pp_set_failure ppf (id, e)] prints [id] and the error [e] a [/set] reported
    for it, as one entry of {!set_failures}, on [ppf]. The id is followed by the
    error, so a message that names the id itself prints the error alone with
    {!Jmap.Proto.Error.Set_error.pp} instead. *)

(** {1 Foo/copy} *)

type 'a copy_args = {
  from_account_id : Proto_id.t;  (** The account to copy records from. *)
  if_from_in_state : string option;
      (** The state the source account must be in. *)
  account_id : Proto_id.t;  (** The account to copy records to. *)
  if_in_state : string option;
      (** The state the destination account must be in. *)
  create : ('a Proto_id.creation * 'a) list;
      (** The records to copy, keyed by creation id. Each carries the [id] of
          the record in the source account. *)
  on_success_destroy_original : bool;
      (** Whether to destroy the originals once the copy succeeds. *)
  destroy_from_if_in_state : string option;
      (** The state the implicit [/set] destroying the originals requires. *)
}
(** The type for the arguments of a [/copy] call over records of type ['a],
    defined by RFC 8620 Section 5.4. *)

val copy_args :
  from_account_id:Proto_id.t ->
  ?if_from_in_state:string ->
  account_id:Proto_id.t ->
  ?if_in_state:string ->
  create:('a Proto_id.creation * 'a) list ->
  ?on_success_destroy_original:bool ->
  ?destroy_from_if_in_state:string ->
  unit ->
  'a copy_args
(** [copy_args ~from_account_id ~account_id ~create ()] is the arguments of a
    [/copy] call. [on_success_destroy_original] defaults to [false]. The
    remaining optional arguments are [None] unless given.

    @raise Stdlib.exception-Invalid_argument
      if the accounts are equal or [create] is empty. *)

val copy_args_jsont : 'a Jsont.t -> 'a copy_args Jsont.t
(** [copy_args_jsont record] is the codec for the arguments of a [/copy] call
    whose records are coded by [record]. *)

type 'a copy_response = {
  from_account_id : Proto_id.t;
      (** The account the records were copied from. *)
  account_id : Proto_id.t;  (** The account the records were copied to. *)
  old_state : string option;
      (** The state of the destination account before the copy. *)
  new_state : string;
      (** The state of the destination account after the copy. *)
  created : (Proto_id.t * 'a) list option;
      (** The records created, keyed by creation id. *)
  not_created : (Proto_id.t * Proto_error.Set_error.t) list option;
      (** Why each record that was not copied failed. *)
}
(** The type for the response of a [/copy] call over records of type ['a]. *)

val copy_response_jsont : 'a Jsont.t -> 'a copy_response Jsont.t
(** [copy_response_jsont record] is the codec for the response of a [/copy] call
    whose records are coded by [record]. *)

(** {1 Foo/query} *)

type 'filter query_args = {
  account_id : Proto_id.t;  (** The account to query. *)
  filter : 'filter Proto_filter.filter option;
      (** The filter the records must match. [None] matches every record. *)
  sort : Proto_filter.comparator list option;
      (** The comparators to sort by, most significant first. [None] leaves the
          order to the server. *)
  position : int64;
      (** The index in the sorted results of the first id to return. A negative
          value counts back from the end. *)
  anchor : Proto_id.t option;
      (** The id to take the position from instead of [position]. *)
  anchor_offset : int64;
      (** The offset from [anchor] of the first id to return. *)
  limit : int64 option;
      (** The maximum number of ids to return. [None] leaves the limit to the
          server. *)
  calculate_total : bool;
      (** Whether to ask the server for the total number of results. *)
}
(** The type for the arguments of a [/query] call whose filter conditions have
    type ['filter]. *)

val query_args :
  account_id:Proto_id.t ->
  ?filter:'filter Proto_filter.filter ->
  ?sort:Proto_filter.comparator list ->
  ?position:int64 ->
  ?anchor:Proto_id.t ->
  ?anchor_offset:int64 ->
  ?limit:int64 ->
  ?calculate_total:bool ->
  unit ->
  'filter query_args
(** [query_args ~account_id ()] is the arguments of a [/query] call on
    [account_id]. [position] and [anchor_offset] default to [0L] and
    [calculate_total] to [false]. The remaining optional arguments are [None]
    unless given. *)

val query_args_jsont : 'filter Jsont.t -> 'filter query_args Jsont.t
(** [query_args_jsont condition] is the codec for the arguments of a [/query]
    call whose filter conditions are coded by [condition]. It builds a filter
    codec, so [condition] is under the restrictions of
    {!Jmap.Proto.Filter.filter_jsont}. *)

type query_response = {
  account_id : Proto_id.t;  (** The account that was queried. *)
  query_state : string;
      (** The state of the query, for a later [/queryChanges] call. *)
  can_calculate_changes : bool;
      (** Whether the server supports [/queryChanges] for this query. *)
  position : int64;
      (** The index in the sorted results of the first id returned. *)
  ids : Proto_id.t list;  (** The ids of the matching records. *)
  total : int64 option;
      (** The total number of results, when [calculate_total] was requested. *)
  limit : int64 option;
      (** The limit the server enforced. RFC 8620 Section 5.5 returns it "only
          if the server set a limit or used a different limit than that given in
          the request", and a client paging results must use it rather than the
          limit it asked for. *)
}
(** The type for the response of a [/query] call. *)

val query_response_jsont : query_response Jsont.t
(** [query_response_jsont] is the codec for the response of a [/query] call. *)

(** {1 Foo/queryChanges} *)

type 'filter query_changes_args = {
  account_id : Proto_id.t;  (** The account to query. *)
  filter : 'filter Proto_filter.filter option;
      (** The filter the query was made with. *)
  sort : Proto_filter.comparator list option;
      (** The sort the query was made with. *)
  since_query_state : string;
      (** The [queryState] of the previous [/query] response. *)
  max_changes : int64 option;
      (** The maximum number of changes to return. Zero requests no changes. *)
  up_to_id : Proto_id.t option;
      (** The last id to report changes up to, inclusive. *)
  calculate_total : bool;
      (** Whether to ask the server for the total number of results. *)
}
(** The type for the arguments of a [/queryChanges] call whose filter conditions
    have type ['filter], defined by RFC 8620 Section 5.6. *)

val query_changes_args :
  account_id:Proto_id.t ->
  ?filter:'filter Proto_filter.filter ->
  ?sort:Proto_filter.comparator list ->
  since_query_state:string ->
  ?max_changes:int64 ->
  ?up_to_id:Proto_id.t ->
  ?calculate_total:bool ->
  unit ->
  'filter query_changes_args
(** [query_changes_args ~account_id ~since_query_state ()] is the arguments of a
    [/queryChanges] call. [calculate_total] defaults to [false] and the
    remaining optional arguments are [None] unless given. *)

val query_changes_args_jsont :
  'filter Jsont.t -> 'filter query_changes_args Jsont.t
(** [query_changes_args_jsont condition] is the codec for the arguments of a
    [/queryChanges] call whose filter conditions are coded by [condition]. It
    builds a filter codec, so [condition] is under the restrictions of
    {!Jmap.Proto.Filter.filter_jsont}. *)

type query_changes_response = {
  account_id : Proto_id.t;  (** The account that was queried. *)
  old_query_state : string;  (** The query state the changes are since. *)
  new_query_state : string;
      (** The query state the changes bring the client to. *)
  total : int64 option;
      (** The total number of results, when [calculate_total] was requested. *)
  removed : Proto_id.t list;  (** The ids that are no longer in the results. *)
  added : Proto_filter.added_item list;
      (** The ids now in the results, each with the index it occupies. *)
}
(** The type for the response of a [/queryChanges] call. *)

val query_changes_response_jsont : query_changes_response Jsont.t
(** [query_changes_response_jsont] is the codec for the response of a
    [/queryChanges] call. *)

(** A mailbox lease with commands serialized across fibers, documented in
    [Imap_eio.Selected]. *)

type t

val create : Session.t -> int -> Imap.Response.select_metadata ->
  Imap.Response.t list -> t
(** [create session generation info updates] is a lease valid while the
    session generation is [generation]. *)

val invalidate : t -> unit
(** [invalidate t] expires [t] and closes the session if a command on [t]
    is still running. *)

val info : t -> (Imap.Response.select_metadata, Error.t) result
val select_updates : t -> (Imap.Response.t list, Error.t) result

type saved_search
(** An RFC 5182 saved result that the next ordinary UID SEARCH on the
    connection invalidates. *)

val saved_search_count : saved_search -> int64
val uid_search_save : t -> criterion:string -> (saved_search, Error.t) result
val uid_search_saved :
  saved_search -> criterion:string -> (int64 list, Error.t) result
val uid_fetch_saved : saved_search -> ?partial:(int64 * int64) ->
  items:string list -> unit -> (Imap.Response.fetch list, Error.t) result
val uid_search : t -> string -> (int64 list, Error.t) result
val uid_sort :
  t -> keys:(Imap.Command.sort_key * Imap.Command.sort_order) list ->
  charset:string -> criterion:string -> (int64 list, Error.t) result

type sort_result = {
  count : int64;
  first : int64 option;
  last : int64 option;
  uids : int64 list option;
  range : (int64 * int64) option;
}

val uid_sort_extended : t -> returns:Imap.Command.sort_return list ->
  keys:(Imap.Command.sort_key * Imap.Command.sort_order) list ->
  charset:string -> criterion:string -> (sort_result, Error.t) result
val uid_thread :
  t -> algorithm:Imap.Command.thread_algorithm -> charset:string ->
  criterion:string -> (Imap.Response.thread list, Error.t) result
val uid_search_partial : t -> range:(int64 * int64) -> criterion:string ->
  (Imap.Response.esearch, Error.t) result

type search_page = {
  uids : int64 list;
  complete : bool;
  limit : int64 option;
  resume_before : int64 option;
}

val uid_search_page : t -> ?before:int64 -> string ->
  (search_page, Error.t) result
val uid_search_range : t -> first:int64 -> last:int64 ->
  (int64 list, Error.t) result
val uid_fetch_partial : t -> set:string -> items:string list ->
  range:(int64 * int64) -> (Imap.Response.fetch list, Error.t) result

val fetch_binary_to : t -> ?max_bytes:int64 -> ?partial:(int64 * int64) ->
  uid:int64 -> section:int list -> _ Eio.Flow.sink ->
  (int64 option, Error.t) result
(** [fetch_binary_to t ~uid ~section sink] streams decoded BINARY.PEEK bytes
    into [sink], which stay provisional until the call returns [Ok]. *)

type binary_size_row = { uid : int64; size : int64 }

val uid_fetch_binary_sizes : t -> uids:int64 list -> section:int list ->
  unit -> (binary_size_row list, Error.t) result

val fetch_to : t -> ?max_bytes:int64 -> uid:int64 ->
  _ Eio.Flow.sink -> (unit, Error.t) result
(** [fetch_to t ~uid sink] streams the message body into [sink], which stays
    provisional until the call returns [Ok ()]. *)

val uid_fetch : t -> set:string -> items:string list ->
  (string list, Error.t) result
(** [uid_fetch t ~set ~items] is the raw text of every FETCH row in the
    response. *)

type envelope_row = {
  uid : int64;
  envelope : Imap.Response.envelope;
}

val uid_fetch_envelopes : t -> uids:int64 list -> unit ->
  (envelope_row list, Error.t) result

type bodystructure_row = {
  uid : int64;
  bodystructure : Imap.Response.bodystructure;
}

val uid_fetch_bodystructures : t -> uids:int64 list -> unit ->
  (bodystructure_row list, Error.t) result

type preview_row = { uid : int64; preview : string option }

val uid_fetch_previews : t -> ?lazy_:bool -> uids:int64 list -> unit ->
  (preview_row list, Error.t) result

type object_id_row = {
  uid : int64;
  email_id : string;
  thread_id : string option;
}

val uid_fetch_object_ids : t -> uids:int64 list -> unit ->
  (object_id_row list, Error.t) result

type object_id_plus_row = {
  uid : int64;
  ids : Imap.Response.compound_object_id;
}

val uid_fetch_object_ids_plus : t -> uids:int64 list -> unit ->
  (object_id_plus_row list, Error.t) result
val fetch_metadata_range : ?size:bool -> ?internal_date:bool ->
  t -> first:int64 -> last:int64 ->
  modseq:bool -> (Imap.Response.fetch list, Error.t) result

type store_receipt = {
  modified : Imap.Proto.Uid_set.t;
  updates : Imap.Response.fetch list;
}

val uid_store_saved : saved_search ->
  operation:[ `Add | `Remove | `Replace ] ->
  flags:Mail_flag.Imap_flag.t list -> ?unchangedsince:int64 -> unit ->
  (store_receipt, Error.t) result
val uid_store_flags : t -> set:Imap.Proto.Uid_set.t ->
  operation:[ `Add | `Remove | `Replace ] ->
  flags:Mail_flag.Imap_flag.t list -> ?unchangedsince:int64 ->
  unit -> (store_receipt, Error.t) result

type copy_mapping = {
  source_first : Imap.Proto.Uid.t;
  destination_first : Imap.Proto.Uid.t;
  length : int64;
}

type copy_receipt = {
  uidvalidity : Imap.Proto.Uidvalidity.t;
  source : Imap.Proto.Uid_set.t;
  destination : Imap.Proto.Uid_set.t;
  mapping : copy_mapping list;
}

val uid_copy_saved :
  saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
val uid_move_saved :
  saved_search -> mailbox:string -> (copy_receipt option, Error.t) result
val uid_expunge_saved : saved_search -> (unit, Error.t) result
val uid_copy : t -> set:Imap.Proto.Uid_set.t -> mailbox:string ->
  (copy_receipt option, Error.t) result
val uid_move : t -> set:Imap.Proto.Uid_set.t -> mailbox:string ->
  (copy_receipt option, Error.t) result
val uid_expunge : t -> set:Imap.Proto.Uid_set.t -> (unit, Error.t) result

val wait_for_change : t -> (Imap.Response.t list, Error.t) result
(** [wait_for_change t] runs one IDLE exchange and returns the unsolicited
    responses that ended it. *)

val fetch_changes : t -> set:Imap.Proto.Uid_set.t ->
  since:Imap.Proto.Modseq.t -> vanished:bool ->
  (Imap.Response.t list, Error.t) result
val fetch_changes_range : t -> first:int64 -> last:int64 ->
  since:Imap.Proto.Modseq.t -> (Imap.Response.fetch list, Error.t) result
val uid_batches : t -> ?range:(int64 * int64) -> size:int64 ->
  unit -> (Imap.Response.uidbatches, Error.t) result
val notify_set : t -> ?status:bool -> groups:Imap.Command.notify_group list ->
  unit -> (Imap.Response.mailbox_status list, Error.t) result
val notify_none : t -> (unit, Error.t) result
val noop : t -> (Imap.Response.t list, Error.t) result

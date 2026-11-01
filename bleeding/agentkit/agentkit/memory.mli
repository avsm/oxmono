(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What an agent knows between one context and the next.

    Memory is a set of entries. A version is a complete snapshot of that set,
    written once and never rewritten, in a file named [%06d.json]. Versions form
    a chain, each naming its parent, and [current] holds the version in force.

    A snapshot rather than a diff, because memory is kilobytes and a version has
    to be inspectable on its own. Replaying a chain to see what was believed
    last Tuesday is work a person doing an audit should not have to do.

    Nothing here prunes. A version is never removed and never rewritten. A store
    that grows over years is a problem for a person with a broom, and a store
    that quietly loses history is a problem nobody can fix.

    {1 The order a mutation writes in}

    {!write} and {!forget} write the snapshot and fsync it, call [journal], and
    move [current] last. The caller's [journal] appends the
    {!Journal.Memory_write} record, so the record is on disk before any version
    claims to be in force. That order is what makes the two stores agree after a
    crash: a snapshot the journal never named was never in force, and {!recover}
    removes it. Passing [journal] rather than returning between the two steps is
    what keeps a caller from getting the order wrong. *)

(** {1 Entries} *)

(** What an entry is for, which is what a brief sorts on. *)
type kind =
  | Fact  (** something durable that was learned *)
  | Open_item  (** work in progress, which is what makes a restart survivable *)
  | Reference  (** a pointer outward, a URL or an identifier *)
  | Procedure  (** how to do something that had to be worked out once *)
  | Episode  (** an observation or completed activity, compressed in briefs *)

val kind_name : kind -> string
(** [kind_name k] is [k] as it is written in JSON, such as ["open_item"]. *)

val kind_of_name : string -> kind option
(** [kind_of_name s] is the kind written [s], or [None]. *)

type entry = {
  id : string;  (** what the agent calls the entry *)
  kind : kind;
  title : string;  (** one line, which is what a listing shows *)
  body : string;  (** the text of it *)
  tags : string list;
  created : int;  (** the version the entry first appeared in *)
  updated : int;  (** the version it last changed in *)
}

type snapshot = {
  version : int;  (** the version this file is *)
  parent : int;  (** the version it succeeded, and 0 for the first *)
  time : string;  (** when it was written, in RFC 3339 UTC *)
  seq : int;  (** the journal record that caused it *)
  cause : string;  (** why it was written *)
  entries : entry list;  (** the whole of memory at this version *)
}
(** One version, which is the whole of memory at a point in time. *)

val jsont : snapshot Jsont.t
(** [jsont] is the codec, which is the schema. Nothing writes a snapshot by
    hand. *)

(** {1 The store} *)

type t
(** A memory store, being a directory of versions. *)

exception Version_exists of int
(** Raised by a mutation that would write over the version it names. A
    journalled version is never rewritten, so a store in this state is one
    {!recover} has not been run on. *)

exception No_version of int
(** Raised by {!read} for a version the store does not hold. *)

exception No_entry of string
(** Raised by {!forget} for an id no entry has. Minting a version that changed
    nothing would put a mutation in the trace that did not happen. *)

exception Corrupt of { path : string; msg : string }
(** Raised for a file under the store that does not decode, naming it and what
    was wrong with it. *)

val create : clock:_ Eio.Time.clock -> _ Eio.Path.t -> t
(** [create ~clock dir] is the store in [dir], which is created if it is absent.
    [clock] stamps each version it mints. *)

(** {1 Reading} *)

val version : t -> int
(** [version t] is the version in force, and 0 when nothing has been written. *)

val versions : t -> int list
(** [versions t] are the versions the store holds, ascending. It includes a
    snapshot [current] does not point at, which is what {!recover} is for. *)

val read : t -> int -> snapshot
(** [read t v] is version [v]. This is what [--at V] reads, and later versions
    do not change what it says. *)

val read_current : t -> snapshot option
(** [read_current t] is the version in force, and [None] when nothing has been
    written. *)

val entries : t -> entry list
(** [entries t] are the entries of the version in force, and the empty list when
    nothing has been written. *)

(** {1 Mutating} *)

val write :
  t ->
  seq:int ->
  cause:string ->
  journal:(Journal.memory_write -> unit) ->
  id:string ->
  kind:kind ->
  title:string ->
  body:string ->
  tags:string list ->
  int
(** [write t ~seq ~cause ~journal ~id ~kind ~title ~body ~tags] mints the next
    version, holding every entry of the one before with [id] added or replaced,
    and is that version's number.

    [seq] is the journal record the version points back at, which is
    {!val:Journal.next_seq} of the journal [journal] appends to. [cause] says
    why, and reaches both the snapshot and the journal record. [created] is kept
    from the entry that was there and is the new version for one that was not.
    [updated] is the new version.

    [journal] is called after the snapshot is fsynced and before [current]
    moves. It raises if the record cannot be written, which leaves a snapshot
    the journal never named. {!recover} removes such a snapshot at the next
    startup, so the run stops with the two stores still agreeing. *)

val forget :
  t ->
  seq:int ->
  cause:string ->
  journal:(Journal.memory_write -> unit) ->
  string ->
  int
(** [forget t ~seq ~cause ~journal id] mints the next version with [id] absent,
    and is that version's number. The version that held the entry is not
    touched, so the entry is still readable at every version it was in. It
    raises {!No_entry} if no entry has [id]. The arguments are as {!write}. *)

(** {1 Recovery} *)

type recovered = {
  adopted : int option;
      (** the version [current] was moved forward to, if it was behind *)
  removed : int list;  (** the snapshots removed, ascending *)
}
(** What {!recover} did. Both are for the caller to journal. *)

val recover : t -> journalled:int -> recovered
(** [recover t ~journalled] settles the store against the journal, where
    [journalled] is the highest version any {!Journal.Memory_write} record
    names, and 0 if none does.

    A snapshot above [journalled] was never named by the journal, so it was
    never in force. It is the leaving of a crash between the snapshot and the
    record, and it is removed. The caller journals an {!Journal.Error} saying
    so. Removing it is also what keeps the next mutation from meeting its own
    debris and refusing with {!Version_exists}.

    A snapshot at or below [journalled] and above [current] was named by the
    journal, so it is a version and [current] is moved forward to the highest
    such. That is the crash between the record and the move. *)

val episode_tree : entry list -> Memo.t
(** [episode_tree entries] is a derived tree of only the episodic entries, in
    their creation order. Current facts, procedures and tasks stay outside the
    tree so their age does not hide them. *)

val summaries : t -> (string * string) list
(** [summaries t] reads the derived cache and returns only keys valid for
    current episodes. Invalid files raise [Corrupt]. *)

val save_summary : t -> key:string -> text:string -> unit
(** [save_summary t ~key ~text] atomically caches at most 512 UTF-8 bytes for a
    current episode range. Obsolete keys raise [Invalid_argument]. Callers
    serialize mutations as they do for {!write}. Summaries are expendable and
    are not new memory versions. Historical snapshots remain authoritative. *)

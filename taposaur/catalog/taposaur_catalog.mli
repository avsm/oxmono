(** SQLite storage and queries. This library does not open archives,
    parse documents or choose bibliographic values. *)

type read
type write
type 'access t
val open_read : sw:Eio.Switch.t -> _ Eio.Path.t -> read t
(** [open_read ~sw path] opens an existing current-schema catalogue. *)
val open_write : sw:Eio.Switch.t -> _ Eio.Path.t -> write t
(** [open_write ~sw path] creates or migrates a catalogue. Read handles
    cannot be passed to publication operations. Connections serialize
    access within an Eio domain and may share a local WAL database. *)

(** {1 Search and raw records} *)

module Query = Query
type page = { papers : Taposaur.Summary.t list; next : string option }
val search : 'a t -> Query.t -> page
(** [search catalog query] returns summaries in UID order. Identity queries
    are exact. Other queries use literal FTS words, with metadata filters
    combined by AND. Query text is limited to 512 bytes and page size to
    1..100. SQL and FTS operators in user text have no special meaning. *)
val find : 'a t -> Taposaur.Uid.t -> Taposaur.Summary.t option
(** [find catalog uid] returns the same summary shape as search. *)
val find_index : 'a t -> Taposaur.Uid.t -> Taposaur.Index.t option
(** [find_index catalog uid] returns the original imported index row.
    Derived metadata is never returned as input to its own resolver. *)

(** {1 Atomic shard publication} *)

module Shard : sig
  type stamp = { size : int64; mtime : float }
  val unchanged : 'a t -> shard:string -> stamp -> bool
  (** [unchanged catalog ~shard stamp] compares the stored source stamp
      and requires no outstanding import error. *)
  val replace :
    write t -> shard:string -> stamp ->
    ((Taposaur.Index.t -> unit) -> unit) -> int
  (** [replace catalog ~shard stamp read] calls [read emit] to stage rows,
      then publishes them, search entries and counters atomically. It
      validates shard membership and UID uniqueness. The callback must
      verify source stability before returning. Exceptions leave the
      previous shard intact. The final transaction defers cancellation.
      Returns the number of published rows. Replacements invalidate
      metadata snapshots even when raw rows are byte-identical. [read]
      runs with the handle held exclusively and must not reenter it. *)
  val record_error : write t -> shard:string -> string -> unit
  (** [record_error catalog ~shard message] records an unsuccessful import. *)
  val set_discovered : write t -> int -> unit
  (** [set_discovered catalog count] records a complete shard inventory. *)
  val errors : 'a t -> (string * string) list
  (** [errors catalog] returns at most 100 recent shard import errors. *)

end

(** {1 Derived metadata snapshots} *)

module Metadata : sig
  type candidate
  val index : candidate -> Taposaur.Index.t
  (** [index candidate] is the original source row for this snapshot. *)
  val pending :
    ?force:bool -> ?uid:Taposaur.Uid.t -> ?after:string ->
    limit:int -> version:int -> 'a t -> candidate list
  (** [pending ~limit ~version catalog] selects at most [limit] snapshots
      in UID order. [limit] is 1..1000 and [version] is positive. Completed
      attempts at this version are skipped unless [force] is set. *)
  val publish :
    write t -> candidate -> version:int ->
    (Taposaur.Bibliography.t, string) result -> bool
  (** [publish catalog candidate ~version result] atomically updates
      bibliography and FTS, or records an error while retaining the last
      successful projection. Returns [false] when a shard replacement
      invalidated the snapshot. It never changes holdings or raw rows. *)
  val errors : 'a t -> (Taposaur.Uid.t * string) list
  (** [errors catalog] returns at most 100 recent metadata failures. *)

end

(** {1 Coverage and diagnostics} *)

type stats = {
  papers : int; pdf : int; xml : int; tei : int; s2orc : int;
  loaded_shards : int; discovered_shards : int option;
  errors : int; last_import : float option;
  titles : int; abstracts : int; publishers : int;
  enriched : int; enrichment_errors : int;
}
val stats : 'a t -> stats
(** [stats catalog] combines shard counters and metadata coverage.
    Metadata counts scan paper rows. [enriched] counts successful cached
    attempts across resolver versions. *)

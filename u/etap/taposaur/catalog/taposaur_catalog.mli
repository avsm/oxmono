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

module Snapshot : sig
  type catalog = read t
  type t
  type row = {
    summary : Taposaur.Summary.t; stamp : string;
    stale : bool; error : string option; diagnostics : string list;
  }
  val with_read : catalog -> (t -> 'a) -> 'a
  (** [with_read catalog f] calls [f] inside one SQLite read transaction.
      Use only this module's operations inside [f]. The token expires when
      [f] returns. Do not yield or perform I/O in [f]. Exceptions roll back.
      Captures should be bounded to avoid retaining the writer's WAL. *)
  val search : t -> Query.t -> page
  val find : t -> Taposaur.Uid.t -> row option
end

(** {1 Atomic shard publication} *)

module Shard : sig
  type stamp = { size : int64; mtime : float }
  val paths : ?after:string -> limit:int -> 'a t -> string list
  (** [paths ~limit catalog] lists imported shards in path order.
      [after] is an exclusive path cursor. [limit] is 1..1000. *)
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
  val upsert : write t -> shard:string -> stamp -> Taposaur.Index.t ->
    [ `Inserted | `Updated | `Unchanged ]
  (** [upsert catalog ~shard stamp row] publishes one row of an imported
      shard in a short transaction, for a writer that has just rewritten
      exactly that row in the shard file and supplies the file's new
      [stamp]. The row's search entry, counters and generation change as
      they would under {!replace}, invalidating only that paper's pending
      metadata snapshot; resolved metadata and cached details for the
      paper are discarded, as a reload would. [`Unchanged] means the stored
      row already equals [row], and only the stamp was updated. Raises
      [Invalid_argument] when the shard has never been imported or [row]
      belongs elsewhere. *)
  val retire : write t -> shard:string -> unit
  (** [retire catalog ~shard] removes a shard, its rows and diagnostics
      atomically. The caller must verify that retirement is intended. *)
  val record_error : write t -> shard:string -> string -> unit
  (** [record_error catalog ~shard message] records an unsuccessful import. *)
  val set_discovered : write t -> int -> unit
  (** [set_discovered catalog count] records a complete shard inventory. *)
  val errors : 'a t -> (string * string) list
  (** [errors catalog] returns at most 100 recent shard import errors. *)

end

(** {1 Derived metadata snapshots} *)

module Metadata : sig
val fill_abstract : write t -> uid:Taposaur.Uid.t -> abstract:string ->
  update_detail:(Taposaur.Index.t -> string -> string) -> bool
(** [fill_abstract t ~uid ~abstract ~update_detail] is {!fill_publisher}
    for a missing abstract: the row, its search index and its cached
    resolution are updated in one transaction, and nothing changes when
    the paper already has an abstract. *)
val fill_publisher : write t -> uid:Taposaur.Uid.t -> publisher:string ->
  update_detail:(Taposaur.Index.t -> string -> string) -> bool
(** [fill_publisher t ~uid ~publisher ~update_detail] atomically fills a
    missing publisher and fences concurrent resolver publications. Other fields,
    cache timestamps and freshness are preserved. [update_detail] runs inside
    the SQLite transaction and must be pure. Returns false for absent papers
    or existing publishers. The caller owns audit and evidence retention. *)

  type candidate
  val index : candidate -> Taposaur.Index.t
  (** [index candidate] is the original source row for this snapshot. *)
  val row_id : candidate -> int64
  (** [row_id candidate] is the cursor within a shard scan. *)
  val source_stamp : candidate -> string option
  (** [source_stamp candidate] is the last successfully read source stamp. *)
  val invalidate : write t -> candidate -> bool
  (** [invalidate catalog candidate] schedules a refresh while preserving
      cached values. Returns [false] if another publication intervened. *)
  type status = {
    source_stamp : string option; stale : bool; diagnostics : string list;
    error : string option;
  }
  val status : 'a t -> Taposaur.Uid.t -> status option
  (** [status catalog uid] returns source freshness and parse diagnostics. *)
  val pending :
    ?force:bool -> ?retry_errors:bool -> ?need_details:bool ->
    ?uid:Taposaur.Uid.t -> ?shard:string -> ?after_row:int64 -> ?after:string ->
    limit:int -> version:int -> 'a t -> candidate list
  (** [pending ~limit ~version catalog] selects at most [limit] snapshots
      in UID order. [limit] is 1..1000 and [version] is positive. Completed
      attempts at this version are skipped unless stale or [force] is set.
      [retry_errors] includes manifest and document failures. [need_details]
      also selects successful or partial rows without a detail cache.
      With [shard], selects only that shard in row-ID order using
      [after_row] instead of [after]. This uses the existing shard index.
      Cursors are for one scan, not durable checkpoints across imports. *)
  val publish :
    ?source_stamp:string -> ?detail_cache:string -> ?diagnostics:string list ->
    ?text:bool -> write t -> candidate -> version:int ->
    (Taposaur.Bibliography.t, string) result -> bool
  (** [publish catalog candidate ~version result] atomically updates
      bibliography and FTS, or records an error while retaining the last
      successful projection. Returns [false] when a shard replacement or
      metadata publication invalidated the snapshot. [diagnostics] records
      document failures alongside partial metadata. [source_stamp] identifies
      the source snapshot used for a successful publication. [text] records
      whether the resolved documents hold body prose; omitted, it is
      unknown. It never changes holdings or raw rows. *)
  val errors : 'a t -> (Taposaur.Uid.t * string) list
  (** [errors catalog] returns at most 100 recent metadata failures. *)

  type detail = {
    index : Taposaur.Index.t; json : string; saved_at : float;
    stale : bool; error : string option; diagnostics : string list;
  }
  val detail : 'a t -> Taposaur.Uid.t -> detail option
  (** [detail catalog uid] reads a cached detail payload and its original
      index and status in one snapshot. Failed refreshes retain old details.
      Shard replacement removes them. Successful publication sets the cache
      to [detail_cache], or clears it if that argument is omitted. *)
  val detail_count : 'a t -> int
  (** [detail_count catalog] returns the maintained count of saved details,
      including entries marked stale or whose latest refresh failed. *)

end

(** {1 Coverage and diagnostics} *)

type stats = {
  papers : int; pdf : int; xml : int; tei : int; s2orc : int;
  loaded_shards : int; discovered_shards : int option;
  errors : int; last_import : float option;
  titles : int; abstracts : int; publishers : int;
  enriched : int; enrichment_errors : int;
  partial : int; stale : int;
}
val stats : 'a t -> stats
(** [stats catalog] combines shard counters and metadata coverage.
    Metadata counts are maintained transactionally. [enriched] counts complete, non-stale cached
    attempts across resolver versions. [partial] counts document failures. *)

(** {1 Journal selection} *)

type journal = { name : string; papers : int }
val journals : ?limit:int -> 'a t -> text:string -> journal list
(** [journals catalog ~text] suggests journal names from the loaded catalogue.
    Matching uses literal substrings with ASCII case folding. Prefix matches
    precede other matches, then journals are ordered by paper count and name.
    An empty query returns the most represented journals. [limit] is 1..20
    (default 12), and [text] is at most 256 bytes. Counts cover the catalogue,
    independent of active search filters, and update with publication. *)

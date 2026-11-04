(** Acceptance of validated artifacts into the central paper archive.

    {1 NAME}
    Taposaur_accept - locked, recoverable publication into [papers/].

    {1 SYNOPSIS}
    {[
      let archive = Taposaur_accept.open_root ~audit ~sw (fs / root) in
      let receipt = Taposaur_accept.accept archive ~cmd:"taposaur-jobs"
        ~at uid [{ source = File staged; artifact }] in
      ignore (Taposaur_accept.refresh_index archive uid)
    ]}

    {1 DESCRIPTION}
    This library and the consolidation regeneration are the writers
    beneath an archive root. It complements {!Taposaur_store}, which
    writes a separate staging tree, and {!Taposaur_archive}, which only
    reads. A paper directory is
    [papers/] followed by {!Taposaur.Layout.paper}. It holds [paper.json],
    [events.jsonl], the artifacts and a [.lock] file.

    Every update holds the directory's {!Taposaur_lock} for its duration.
    Callers must not hold it across HTTP transfers or conversions, so
    inputs are complete files or byte strings before acceptance starts.

    {1 INVARIANTS}
    - Existing artifact files are never overwritten or truncated. A new
      artifact whose requested filename is taken receives a numbered name.
    - Content already held, identified by SHA-256 and format, is not
      duplicated. The receipt reports it as existing.
    - Only the fields in {!mutable_fields} may change in an existing
      manifest, and only from a missing value to a present one. The
      identity, the artifact list order and earlier artifact records are
      retained. The DOI must match the paper.
    - Every change appends events before the manifest is replaced. The
      event log is append-only and keeps its original bytes.
    - A durable intent precedes file publication. Reopening the paper
      completes an interrupted publication without duplicating files or
      events, then removes the intent. A publication interrupted before
      any file or event became visible is abandoned instead, and the
      caller's retry repeats it.
    - Artifact names never collide with [paper.json], [events.jsonl],
      temporary files or hidden files. Final names are placed with
      [link], which refuses an existing entry even from a writer outside
      this protocol.
    - New directories are created mode 0755 and files 0644. A replaced
      shard or manifest keeps its existing mode.
    - Lease expiry in a job queue is not a fence. A second worker with the
      same inputs observes the manifest and reports the content as
      existing instead of publishing it again.

    {1 ORDERING}
    Accept writes the intent record and syncs it, copies each source to a
    temporary file in the paper directory and syncs it, renames it to its
    final name, appends the event bytes and syncs the log, replaces
    [paper.json] atomically, syncs the directory and finally unlinks the
    intent. Recovery repeats whichever steps are incomplete, verifying
    digests and log prefixes before each one. *)

type t
(** A writable archive root confined with Eio. *)

exception Busy of Taposaur_lock.owner option
(** Another writer holds the paper directory or shard lock. *)

val open_root : ?audit:Taposaur_audit.session -> sw:Eio.Switch.t ->
  _ Eio.Path.t -> t
(** [open_root ~sw root] confines writes beneath [root], which must contain
    [papers/] and [index/]. [root] must be a Unix filesystem path. Lock
    files beside the papers work on local disks and on CephFS. Operations
    record audit events in [audit] when supplied. *)

val mutable_fields : string list
(** [mutable_fields] names the manifest fields acceptance may fill when
    missing: [title], [publisher], [year], [pmid] and [pmcid]. *)

type source =
  | File of Eio.Fs.dir_ty Eio.Path.t  (** A complete regular file. *)
  | Bytes of string  (** Complete content already in memory. *)
(** Where an artifact's bytes come from. Sources are read fully and
    hashed before any archive file is created. *)

type addition = {
  source : source;
  artifact : Taposaur.Manifest.Artifact.t;
      (** The record to publish. [file] is the requested name, a single
          filename component. [sha256] and [bytes], when supplied, must
          match the source. [at] is the record's timestamp. *)
}

type receipt = {
  uid : Taposaur.Uid.t;
  added : Taposaur.Manifest.Artifact.t list;
      (** Newly published records with their final filenames. *)
  existing : Taposaur.Manifest.Artifact.t list;
      (** Records already holding identical content, left unchanged. *)
  manifest_sha256 : string;  (** Digest of the published [paper.json]. *)
}
(** The durable outcome. Repeating an acceptance with the same inputs
    returns every artifact as existing and the same manifest digest. *)

val accept : ?audit:Taposaur_audit.session -> t ->
  ?identity:Taposaur.Bibliography.t ->
  ?clear_failures:Taposaur.Manifest.Route.t list -> cmd:string ->
  at:string -> Taposaur.Uid.t -> addition list -> receipt
(** [accept t ~cmd ~at uid additions] publishes [additions] into the paper
    directory of [uid], creating the directory, manifest and event log for
    a new paper. [cmd] names the writer in each event. [at] is the RFC 3339
    timestamp recorded on identity events. [identity] fills missing
    {!mutable_fields} and must name the same DOI. [clear_failures] removes
    recorded conversion failures for those routes, for use after a
    successful conversion. Each addition produces one outcome event, and a
    changed identity one more. Raises {!Busy} when another writer holds the
    directory, [Invalid_argument] for unsafe filenames or a mismatched
    digest, and [Failure] when the directory's manifest names a different
    paper or an interrupted publication cannot be reconciled. [audit]
    replaces the session given to {!open_root} for this call, so a
    caller's operation encloses the publication's records. *)

val record_failure : ?audit:Taposaur_audit.session -> t -> cmd:string ->
  at:string -> Taposaur.Uid.t -> Taposaur.Manifest.Failure.t -> unit
(** [record_failure t ~cmd ~at uid failure] appends an error event naming
    the route and replaces the manifest's failure record for that route,
    adding the previous attempt count. No artifact changes. The paper
    must already exist. *)

val record_refusal : ?audit:Taposaur_audit.session -> t -> cmd:string ->
  at:string -> Taposaur.Uid.t -> Taposaur.Manifest.Refusal.t -> unit
(** [record_refusal t ~cmd ~at uid refusal] records that an acquisition
    strategy could not supply the paper. It creates the paper directory and
    an artifact-free manifest when the paper is new, appends an error event
    naming the strategy, and replaces the manifest's refusal for that
    strategy, adding the previous attempt count. Later sweeps read the
    refusal instead of asking the publisher again. Accepting an artifact
    through that strategy removes it. *)

val manifest : t -> Taposaur.Uid.t -> Taposaur.Manifest.t option
(** [manifest t uid] reads the current manifest under the directory lock,
    completing any interrupted publication first. [None] means the paper
    has no manifest. *)

val index_row : t -> Taposaur.Uid.t -> Taposaur.Index.t option
(** [index_row t uid] derives the catalogue import row from the manifest
    and the files it lists. Holdings fields come from the artifacts, with
    PDF and TEI paths as native archive paths. Bibliographic fields are
    retained from the shard's existing row, with the manifest's title,
    publisher, year and corrections filling values the row lacks. [None]
    means the paper has no manifest. *)

type index_change = Added | Updated | Unchanged
(** Whether a shard rewrite inserted, replaced or kept the paper's row. *)

val refresh_index : ?audit:Taposaur_audit.session -> t -> Taposaur.Uid.t ->
  index_change
(** [refresh_index t uid] rewrites the paper's row in its
    {!Taposaur.Layout.index} shard under that shard's lock. The shard is
    replaced atomically with its other rows byte-for-byte unchanged. A
    row equal to the stored one leaves the file untouched. The catalogue
    does not observe the change until the shard is imported again. Raises
    {!Busy} when another writer holds the shard and [Failure] when the
    paper has no manifest. *)

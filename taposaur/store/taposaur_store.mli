(** Publication into a dedicated staging tree. No accepted archive or index
    is modified. Staging trees must be on a local Unix filesystem. *)

type t
type paper
exception Busy
exception Limit_exceeded
exception Empty_artifact
val open_root : ?audit:Taposaur_audit.session -> sw:Eio.Switch.t -> _ Eio.Path.t -> t
(** [open_root ~sw root] opens an existing staging directory. *)
val with_paper : t -> Taposaur.Uid.t -> (paper -> 'a) -> 'a
(** [with_paper store uid f] holds an advisory per-paper lock while [f]
    runs. [Busy] means another writer owns the paper. It recovers an
    interrupted publication before calling [f]. Existing papers not created
    by this writer are refused. Locks are released on cancellation. *)
val manifest : paper -> Taposaur.Manifest.t
(** [manifest paper] is the current projection of its committed events. *)
val identify : paper -> at:string -> Taposaur.Bibliography.t -> unit
(** [identify paper ~at bibliography] records missing identity fields.
    The DOI must match the staged paper. Repeated values add no event. *)
val add :
  ?max_bytes:int -> paper -> at:string ->
  source:Taposaur.Manifest.Source.t -> format:Taposaur.Manifest.Format.t ->
  strategy:string -> status:int -> url:string ->
  validate:(Eio.File.ro_ty Eio.Resource.t -> unit) ->
  _ Eio.Flow.source -> Taposaur.Manifest.Artifact.t
(** [add paper ~validate flow] streams, hashes and validates an artifact
    before publishing it. [max_bytes] defaults to 128 MiB. [validate] must
    raise for unacceptable content. Identical bytes from the same source
    and format reuse the existing artifact. New bytes get a new filename.
    A durable intent precedes the event append and atomic manifest update.
    Retrying recovers an interrupted append without duplicating events.
    Cancellation is deferred during final publication. *)
val record_error : paper -> at:string -> source:string -> string -> unit
(** [record_error paper ~at ~source message] appends an acquisition failure.
    Converter names are refused because this writer does not project
    conversion failures. *)

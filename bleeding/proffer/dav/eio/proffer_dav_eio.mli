(** Confined Eio storage for [Proffer_dav]. *)

val reader : sw:Eio.Switch.t -> ?max_entries:int ->
  _ Eio.Path.t -> Proffer_dav.Reader.t
(** [reader ~sw path] opens a fresh subtree and exports regular files and
    collections. Linux openat2, procfs descriptor reopening and a native Eio
    directory are required. Metadata ETags are weak. External changes can
    change a live enumeration or file, so use a snapshot for stable reads.
    Symlinks, mount traversal, multiply linked files and special files are
    rejected. The switch must outlive requests. *)

type quota = {
  max_file_bytes : int64;
  max_storage_bytes : int64;
  max_staging_bytes : int64;
  max_entries : int;
  max_metadata_bytes : int;
}
val writer : sw:Eio.Switch.t -> create:bool -> quota:quota ->
  clock:_ Eio.Time.clock -> mono_clock:_ Eio.Time.Mono.t ->
  random:_ Eio.Flow.source -> _ Eio.Path.t -> Proffer_dav.Writer.t
(** [writer ~create ~quota ... store] opens a private managed store. Its
    immutable content and atomic manifest implement the DAV namespace.
    This is not an in-place export of a normal directory. A new store must
    be empty, owned by the current OS user and have mode 0700. The caller
    must keep other local writers out. An exclusive process lease prevents
    concurrent cooperating writers.
    Manifest publication is the commit point. Recovery validates state and
    removes owned unreferenced staging objects before serving requests. *)

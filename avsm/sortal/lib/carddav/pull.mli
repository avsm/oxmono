(** Conservative remote-to-Sortal reconciliation. Full source/card versions and
    separate common baselines are retained in durable local journals. *)

val remote_record :
  Common.value -> string -> string -> string -> string -> Common.value

val merge_records : Common.value -> Common.value -> Common.value -> Common.value

val reconcile :
  Common.value ->
  Common.value ->
  string ->
  string ->
  string ->
  string ->
  Common.value
(** [reconcile base local old_card new_card uid store] merges supported remote
    changes. Concurrent changes to the same field or unsupported card edits
    raise [Common.Error]. Unrelated local changes stay local. *)

val seed_directory : ?seed:string -> string -> string

val prepare :
  ?previous:string ->
  ?seed:string ->
  bundle:string ->
  snapshot:string ->
  source:string ->
  output:string ->
  unit ->
  Common.value
(** Write a new journal, without modifying source files or existing journals.
    [previous] is the last applied journal for this account and collection. *)

val apply :
  dav:Remote.t -> dry_run:bool -> username:string -> string -> Common.value
(** Verify prepared hashes and current server/local versions before each atomic
    source write. Interrupted writes can be replayed. [dry_run] changes neither
    source files nor any journal file. This never writes the server. *)

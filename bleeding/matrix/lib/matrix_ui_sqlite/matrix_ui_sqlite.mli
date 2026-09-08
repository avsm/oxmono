(** matrix_ui_sqlite — a SQLite backend for the UI event store.

    {!create} opens a file and hands back a {!Matrix_ui.Event_store.t} over it,
    so a client keeps its timelines across restarts. The schema is private to
    this library and versioned. The immediately previous timeline schema is
    migrated additively to add detached-event storage; unknown older schemas are
    dropped and recreated because the cache is derived data. *)

val create :
  ?plaintext_policy:Matrix_ui.Event_store.plaintext_policy ->
  string ->
  (Matrix_ui.Event_store.t, Matrix_ui.Event_store.Error.t) result
(** [create path] opens or creates the store at [path]. [plaintext_policy]
    defaults to {!Matrix_ui.Event_store.Ciphertext_only}.
    {!Matrix_ui.Event_store.close} closes the file. *)

val create_media_store :
  ?retention:Matrix_client.Media_store.retention_policy ->
  string ->
  (Matrix_client.Media_store.t, Matrix_client.Error.t) result
(** [create_media_store path] opens a media cache in the SQLite database at
    [path]. Its tables and metadata are private and versioned; existing UI
    event-store tables in the same database are preserved. *)

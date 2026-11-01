(** Bounded rendering of scholarly TEI documents. *)
val render :
  ?max_bytes:int -> ?max_output:int ->
  ?resolve_doi:(string -> Taposaur.Bibliography.t option) ->
  ?journal_url:(string -> string option) ->
  ?source_url:(string -> string option) ->
  _ Eio.Flow.source -> (string, string) result
(** [render flow] renders document structure, citations, notes, tables,
    figures and mathematics. Resolvers supply local cached metadata only.
    Unknown markup retains its text. Unsafe URLs and attributes are omitted. *)

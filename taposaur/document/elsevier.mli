(** Elsevier retrieval XML, not a generic publisher XML reader. *)
val metadata :
  ?max_bytes:int -> _ Eio.Flow.source ->
  (Taposaur.Bibliography.t, string) result
(** [metadata flow] reads [coredata], stopping before the article body.
    Defaults to 1 MiB of input and 256 levels of nesting. Empty fields
    are absent. Unsupported formats, XML errors and limit violations
    return [Error]. I/O errors and cancellation propagate. External
    entities are not resolved. *)

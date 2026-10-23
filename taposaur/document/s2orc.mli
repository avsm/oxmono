(** Bibliographic fields in corpus S2ORC JSON. *)
val metadata :
  ?max_bytes:int -> _ Eio.Flow.source ->
  (Taposaur.Bibliography.t, string) result
(** [metadata flow] reads title, author names and an optional abstract.
    Unstructured names remain verbatim in [Author.family]. The default
    input bound is 16 MiB. Body annotations are not interpreted as
    bibliography. JSON and limit errors are returned. I/O errors and
    cancellation propagate. *)

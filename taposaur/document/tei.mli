(** TEI metadata extraction and constrained HTML rendering. *)

val metadata_string : string -> (Taposaur.Bibliography.t, string) result
(** [metadata_string xml] reads the header and front matter from [xml]. *)
val metadata :
  ?max_bytes:int -> _ Eio.Flow.source ->
  (Taposaur.Bibliography.t, string) result
(** [metadata flow] stops before the body. The default input bound is
    1 MiB and nesting is capped at 256 elements. XML and limit errors are
    returned. I/O errors and cancellation propagate. *)

val render_html :
  ?max_bytes:int -> ?max_output:int -> _ Eio.Flow.source ->
  (string, string) result
(** [render_html flow] renders TEI text using a fixed HTML vocabulary.
    Source attributes, URLs and executable markup are not copied.
    Defaults are 16 MiB of input and 32 MiB of output, with 256 levels of
    nesting. XML errors include line and column. External entities are
    not resolved. I/O errors and cancellation propagate. *)

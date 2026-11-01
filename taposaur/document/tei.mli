(** TEI metadata extraction and constrained HTML rendering. *)

val metadata_string : string -> (Taposaur.Bibliography.t, string) result
(** [metadata_string xml] reads the header and front matter from [xml]. *)
val metadata :
  ?max_bytes:int -> _ Eio.Flow.source ->
  (Taposaur.Bibliography.t, string) result
(** [metadata flow] stops before the body. The default input bound is
    1 MiB and nesting is capped at 256 elements. XML and limit errors are
    returned. I/O errors and cancellation propagate. *)

val validate_string : ?max_bytes:int -> string -> (unit, string) result
(** [validate_string xml] checks the entire document for well-formed XML
    with a TEI root. It does not validate against a TEI schema. Defaults
    to 16 MiB and 256 nesting levels. External entities are not resolved. *)

val render_html :
  ?max_bytes:int -> ?max_output:int ->
  ?resolve_doi:(string -> Taposaur.Bibliography.t option) ->
  ?journal_url:(string -> string option) ->
  ?source_url:(string -> string option) -> _ Eio.Flow.source ->
  (string, string) result
(** [render_html flow] renders TEI text using a fixed HTML vocabulary.
    Bibliographies have separate entries and labelled identifiers. Internal
    references link to namespaced anchors with citation backlinks. HTTP(S)
    links are checked. Resolvers supply cached bibliographies and journal
    links. MathML uses a fixed vocabulary and TeX is marked for browser
    typesetting. Unsupported elements retain their text. Remote graphics
    require user activation. Executable markup is never copied.
    Defaults are 16 MiB of input and 32 MiB of output, with 256 levels of
    nesting. XML errors include line and column. External entities are
    not resolved. I/O errors and cancellation propagate. *)

(** TEI metadata extraction and constrained HTML rendering. *)

type inspection = {
  bibliography : Taposaur.Bibliography.t;
  text : bool;  (** The body holds prose outside figures, notes, tables
                    and formulae. *)
  derived_abstract : bool;
      (** The abstract came from the body, not the header or front
          matter. *)
}

val inspect_string : string -> (inspection, string) result
(** [inspect_string xml] reads the header, the front matter and the start
    of the body from [xml]. *)
val inspect :
  ?max_bytes:int -> _ Eio.Flow.source -> (inspection, string) result
(** [inspect flow] reads the header and front matter, then scans the body
    only as far as needed: when the header carries no abstract, the first
    four top-level sections are examined for one headed "Abstract" or
    "Summary", or failing that the unheaded block before the first headed
    section, as GROBID emits when its header model misses the abstract:
    the whole block when it is 200 to 3000 characters, otherwise its first
    paragraph when that is 200 to 6000 characters, since GROBID sometimes
    runs the abstract and the introduction together. Sentence and
    paragraph breaks become spaces. The scan stops once the abstract is settled and body text
    has been seen, so references are never read. The default input bound
    is 1 MiB and nesting is capped at 256 elements; a body beyond the
    byte bound still yields the header. XML and limit errors are
    returned. I/O errors and cancellation propagate. *)
val metadata_string : string -> (Taposaur.Bibliography.t, string) result
(** [metadata_string xml] is the bibliography of {!inspect_string}. *)
val metadata :
  ?max_bytes:int -> _ Eio.Flow.source ->
  (Taposaur.Bibliography.t, string) result
(** [metadata flow] is the bibliography of {!inspect}. *)

val body_text :
  ?max_bytes:int -> ?max_output:int -> _ Eio.Flow.source ->
  (string option, string) result
(** [body_text flow] is the prose of the TEI body: section headings,
    paragraphs and list items in document order, one block per line with a
    blank line between blocks. Whitespace is collapsed and sentence breaks
    become spaces. The text of figures, notes, tables and formulae is left
    out, as it is for {!inspect}, and the back matter is never read. The
    result is [None] when the document has no body or the body holds no
    prose. Defaults are 16 MiB of input and 8 MiB of output, with 256
    levels of nesting; exceeding either is an error, as is malformed XML.
    I/O errors and cancellation propagate. *)
val body_text_string : ?max_output:int -> string -> (string option, string) result
(** [body_text_string xml] is {!body_text} over an in-memory document. *)

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

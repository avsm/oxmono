(** A {{:https://github.com/kermitt2/Pub2TEI}Pub2TEI} conversion
    service, which turns publisher XML (Elsevier, Springer, JATS, PMC,
    bioRxiv and the rest) into TEI.

    The service is self-hosted and no credential is involved, so
    {!client} mints its capability from a [~server] base URL rather
    than from a key. A trailing ["/"] on it is ignored. Conversions
    answer [application/xml].

    The refinement options, which are on by default, run the extracted
    content back through GROBID and, for {!type-consolidation}, through
    a bibliographical service such as {!Glutton}. A whole article's
    references take seconds to tens of seconds that way, so keep one
    request in flight ([Fetch.with_limits ~max_concurrent:1]) and expect
    to raise the surrounding [Eio.Time.with_timeout]. *)

type consolidation =
  | Off  (** Keep the references as the publisher wrote them. *)
  | Full  (** Replace a matched reference with the record found. *)
  | Doi_only  (** Add the DOI of a matched reference and nothing else. *)
(** What to do with a reference that a bibliographical service
    recognises. These are GROBID's [consolidateReferences] levels 0, 1
    and 2. *)

type t
(** The authority to call one Pub2TEI server, minted by {!client} with
    the server address inside it. It is abstract, so {!client} is the
    only way to make one and another service's capability cannot stand
    in for it. *)

val client : server:string -> _ Fetch.t -> t
(** [client ~server t] is [t] bound to the Pub2TEI server rooted at
    [server], whose trailing ["/"] is ignored. The operations only ever
    build URLs under [server], but [t] itself is not restricted to that
    origin, so narrow it with {!Fetch.restrict} before minting if that
    matters. Every wrapper goes on [t], as the result is not a client.

    The conversions POST, so a [t] narrowed with {!Fetch.read_only}
    mints a capability whose conversions are denied at runtime.

    @raise Invalid_argument if [server] is not an http or https URL, or
      carries a query. *)

val server : t -> string
(** [server t] is the base URL [t] was minted for, without its
    trailing ["/"]. *)

val isalive : sw:Eio.Switch.t -> t -> Fetch.response
(** [isalive ~sw t] asks [t]'s server whether it is up. The body is the
    plain text ["true"]. *)

val process_xml :
  sw:Eio.Switch.t ->
  ?segment_sentences:bool ->
  ?grobid_refine:bool ->
  ?generate_ids:bool ->
  ?consolidate_references:consolidation ->
  ?filename:string ->
  ?boundary:string ->
  t ->
  string ->
  Fetch.response
(** [process_xml ~sw t doc] converts the publisher XML document [doc] to
    TEI. [segment_sentences] marks sentence boundaries in the text,
    [grobid_refine] re-runs the extracted header through GROBID,
    [generate_ids] gives the TEI elements identifiers (default [true]
    for the three of them), and [consolidate_references] resolves the
    references (default {!Full}). The server defaults every option off,
    so each is supplied here instead. A caller who wants less has to say
    so, with [~grobid_refine:false] or [~consolidate_references:Off].

    The endpoint takes [multipart/form-data], built with {!Fetch.Form}.
    [doc] travels as the [input] part under [filename] (default
    ["input.xml"]) and the options as fields beside it. [boundary] fixes
    the multipart boundary, which is otherwise derived from the parts,
    and is only worth giving to make a request byte-for-byte
    reproducible.

    @raise Invalid_argument if [filename] holds a quote or a line
      break, or if [boundary] is not an RFC 9110 token, is longer than
      70 characters, or occurs in a part. Any of those would put a body
      on the wire that the server would misparse. *)

val process_text :
  sw:Eio.Switch.t ->
  ?segment_sentences:bool ->
  ?grobid_refine:bool ->
  ?generate_ids:bool ->
  ?consolidate_references:consolidation ->
  t ->
  string ->
  Fetch.response
(** [process_text ~sw t text] converts the fragment [text] to TEI, with
    the options of {!process_xml}. The fields travel as an
    [application/x-www-form-urlencoded] body, which is what the
    endpoint's [@FormParam] annotations ask for. Despite the name it is
    the same XML pipeline as {!process_xml}, so [text] must be a
    well-formed publisher XML fragment rather than prose.

    Pub2TEI 0.3 leaves that endpoint unreachable, answering 415 to the
    urlencoded body and 500 to a multipart one. Check the status before
    trusting a conversion, and use {!process_xml} in the meantime. *)

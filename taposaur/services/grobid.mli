(** A {{:https://github.com/kermitt2/grobid}GROBID} service, which
    extracts structured TEI from PDF documents with machine-learning
    models.

    The service is self-hosted and no credential is involved, so
    {!client} mints its capability from a [~server] base URL rather
    than from a key. A trailing ["/"] on it is ignored. Extractions
    answer [application/xml].

    Full-text processing runs the whole document through the models
    and, for consolidation, through a bibliographical service such as
    {!Glutton}. An article takes seconds, so keep few requests in
    flight. The service answers 503 when all its threads are busy and
    204 when nothing could be extracted from the PDF. *)

type consolidation =
  | Off  (** Keep the metadata as extracted. *)
  | Full  (** Replace a matched record with the one found. *)
  | Doi_only  (** Add the DOI of a matched record and nothing else. *)
(** What to do with a header or reference that a bibliographical
    service recognises. These are GROBID's consolidation levels 0, 1
    and 2. *)

type t
(** The authority to call one GROBID server, minted by {!client} with
    the server address inside it. It is abstract, so {!client} is the
    only way to make one and another service's capability cannot stand
    in for it. *)

val client : server:string -> _ Fetch.t -> t
(** [client ~server t] is [t] bound to the GROBID server rooted at
    [server], whose trailing ["/"] is ignored. The operations only ever
    build URLs under [server], but [t] itself is not restricted to that
    origin, so narrow it with {!Fetch.restrict} before minting if that
    matters. Every wrapper goes on [t], as the result is not a client.

    {!process_fulltext} POSTs, so a [t] narrowed with {!Fetch.read_only}
    mints a capability whose extractions are denied at runtime.

    @raise Invalid_argument if [server] is not an http or https URL, or
      carries a query. *)

val server : t -> string
(** [server t] is the base URL [t] was minted for, without its
    trailing ["/"]. *)

val isalive : sw:Eio.Switch.t -> t -> Fetch.response
(** [isalive ~sw t] asks [t]'s server whether it is up. The body is the
    plain text ["true"]. *)

val default_coordinates : string list
(** [default_coordinates] is the eleven TEI elements the models can
    place on the page, namely ["ref"], ["biblStruct"], ["persName"],
    ["figure"], ["formula"], ["head"], ["s"], ["p"], ["note"], ["title"]
    and ["affiliation"], which is what {!process_fulltext} asks for when
    it is not told otherwise. Filter it rather than restating it, so a
    narrower request stays in step with the list as it grows. *)

val process_fulltext :
  sw:Eio.Switch.t ->
  ?segment_sentences:bool ->
  ?generate_ids:bool ->
  ?consolidate_header:consolidation ->
  ?consolidate_citations:consolidation ->
  ?consolidate_funders:consolidation ->
  ?include_raw_citations:bool ->
  ?include_raw_affiliations:bool ->
  ?include_raw_copyrights:bool ->
  ?include_discarded_text:bool ->
  ?tei_coordinates:string list ->
  ?filename:string ->
  ?boundary:string ->
  t ->
  string ->
  Fetch.response
(** [process_fulltext ~sw t pdf] extracts the PDF document [pdf] as TEI.
    The server defaults every option off, so each is supplied here at
    its most informative value instead. A caller who wants less has to
    say so, with [~segment_sentences:false] or
    [~consolidate_header:Off].

    [segment_sentences] marks sentence boundaries in the text and
    [generate_ids] gives the TEI elements identifiers (default
    [true]). [consolidate_header] and [consolidate_citations] resolve
    the header and the references against a bibliographical service
    (default {!Full}). [include_raw_citations] and
    [include_raw_affiliations] keep the raw strings beside the parsed
    forms (default [true]). Consolidation replaces what it matches
    with the record the service returned, so the raw strings are the
    only remaining trace of what the document itself said.
    [include_raw_copyrights] does the same for the copyright and
    licence statement, and [include_discarded_text] emits the text the
    models drop, such as running headers and footers (default [true]).

    [consolidate_funders] resolves the funders named in the header, and
    is the one option defaulting to {!Off}. A biblio-glutton
    consolidation service answers 400 to every funder query, as it
    mirrors Crossref's works rather than the separate Funder Registry,
    so turn this on only against a Crossref-backed service, where it
    costs one lookup per funder.

    [tei_coordinates] names the TEI elements that are to carry a
    [coords] attribute placing them back on the page they were read
    from. A box in it reads ["page,x,y,width,height"], the page
    counted from one and the rest in PDF units from the upper-left
    corner of the page with the y-axis running downwards. An element
    the layout breaks across lines or columns carries one box per
    fragment, joined by [";"]. A name the models never emit is
    ignored, the default is {!default_coordinates}, and [[]] asks for
    no coordinates at all.

    The endpoint takes [multipart/form-data], built with {!Fetch.Form}.
    [pdf] travels as the [input] part under [filename] (default
    ["input.pdf"]) and the options as fields beside it. [boundary] fixes
    the multipart boundary, which is otherwise derived from the parts.

    @raise Invalid_argument if [filename] holds a quote or a line
      break, or if [boundary] is not an RFC 9110 token, is longer than
      70 characters, or occurs in a part. Any of those would put a body
      on the wire that the server would misparse. *)

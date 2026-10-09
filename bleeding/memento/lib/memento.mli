(** Memento datetime negotiation and TimeMaps.

    {!Link} reuses Fetch's RFC 8288 codec. {!Timemap} describes the JSON
    TimeMap guide. Network and server helpers live in [Memento_fetch] and
    [Memento_proffer]. No archive service or transport is selected here. *)

module Datetime : sig
  type t = Ptime.t
  (** A [t] is an archival timestamp in UTC. *)
  val of_http : string @ local -> (t, string) result @@ portable
  (** [of_http s] parses an HTTP date, including the obsolete HTTP forms.
      Two-digit years use Httpz's fixed 1970 to 2069 interpretation. *)
  val to_http : t -> string @@ portable
  (** [to_http t] emits IMF-fixdate in GMT, discarding fractional seconds. *)
  val of_json : string -> (t, string) result @@ portable
  (** [of_json s] parses an RFC 3339 timestamp with an explicit time zone. *)
  val to_json : t -> string @@ portable
  (** [to_json t] emits RFC 3339 in UTC, retaining nonzero fractions. *)
end

type capture = { uri : string; datetime : Datetime.t }
(** A [capture] identifies a prior representation and its archival timestamp.
    JSON capture URIs are absolute. Link targets may be relative until resolved. *)

module Timemap : sig
  type reference = {
    uri : string;
    from : Datetime.t option;
    until : Datetime.t option;
    memento_compliant : bool option;
    archive_id : string option;
  }
  (** A [reference] identifies another TimeMap. Bounds are inclusive when
      present. [memento_compliant] encodes as ["yes"] or ["no"]. *)
  type formats = { json_format : string option; link_format : string option }
  (** A [formats] gives available serializations of the same TimeMap. *)
  type mementos = {
    list : capture list;
    first : capture option;
    last : capture option;
    closest : capture option;
  }
  (** A [mementos] lists captures on this page. [first] and [last] refer to
      the server's entire history. [closest] is available on paged maps. *)
  type pages = { prev : reference option; next : reference option }
  (** A [pages] gives adjacent pages. Missing or JSON null links mean no page. *)
  type t = {
    original_uri : string;
    timegate_uri : string option;
    timemap_uri : formats option;
    mementos : mementos option;
    pages : pages option;
    timemap_index : reference list option;
  }
  (** A [t] is a basic, paged or indexed JSON TimeMap. Basic and paged maps
      have [mementos]. Indexed maps have [timemap_index] and no [pages]. *)
  val jsont : t Jsont.t
  (** [jsont] validates the three forms in both directions. Additional
      members are ignored. Index children may omit [pages].
      URIs must be absolute and timestamps must be RFC 3339. The guide's
      example names [list] and [timemap_index] are used, rather than the
      conflicting names [all] and [indexes] in its table and prose. *)
  val captures : t -> capture list @@ portable
  (** [captures t] is this page's list, or empty for an index. *)
  val references : t -> reference list @@ portable
  (** [references t] gives index entries or adjacent pages. It does not fetch. *)
end

module Link : sig
  type t = Fetch.Header.link
  (** A [t] is a Link value, retaining unknown parameters such as [license]. *)
  val memento : ?rels:string list -> capture -> t @@ portable
  (** [memento c] points to [c] with its mandatory HTTP datetime attribute.
      [rels] adds navigation relations such as [first], [prev] or [last]. *)
  val timemap :
    ?from:Datetime.t -> ?until:Datetime.t -> media_type:string -> string -> t @@ portable
  (** [timemap ~media_type uri] points to a TimeMap with optional coverage. *)
  val captures : t list -> (capture list, string) result @@ portable
  (** [captures links] extracts mementos. Missing, duplicate or malformed
      datetime attributes are errors. Targets are returned as received. *)
  val timemap_media : t list Fetch.Media.t
  (** [timemap_media] is the shared Fetch/Proffer codec for
      application/link-format TimeMaps. Encoding and decoding require exactly
      one original link and valid datetime attributes on every memento link.
      Invalid encoding raises Invalid_argument. *)
end

val nearest : Datetime.t -> capture list -> capture option @@ portable
(** [nearest datetime captures] chooses the nearest capture, preferring the
    earlier capture on ties. Empty input returns [None]. This is a local
    selection policy, not a requirement imposed on remote TimeGates. *)

module Headers : sig
  val accept_datetime : Datetime.t Fetch.Header.t
  (** [accept_datetime] is the request datetime codec. *)
  type metadata = {
    links : Link.t list;
    datetime : Datetime.t option;
    is_timegate : bool;
    is_memento : bool;
    do_not_negotiate : bool;
  }
  (** A [metadata] reports advertised capabilities. A resource can be both
      a TimeGate and a Memento. An unclassified resource is not necessarily
      an original. [Last-Modified] is never treated as an archival timestamp. *)
  val read : Http.Header.t -> (metadata, string) result
  (** [read headers] parses repeated Link fields and Vary tokens. Malformed
      Link or Memento-Datetime fields are errors. *)
  val memento : original:string -> capture -> (string * string) list @@ portable
  (** [memento ~original c] gives required Memento response fields. *)
  val timegate : original:string -> capture -> (string * string) list @@ portable
  (** [timegate ~original c] gives fields for a 302 TimeGate response.
      Include [accept-datetime] when merging with an existing Vary field. *)
end

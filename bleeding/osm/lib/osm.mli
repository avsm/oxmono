(** OpenStreetMap services through a caller-supplied Fetch client. Transport
    errors and Eio cancellation propagate as exceptions. Bound calls with
    [Eio.Time.with_timeout_exn] when a deadline is required. *)

(** {1 Values and codecs} *)

module Point = Point
(** Validated geographic coordinates in degrees. *)

module Bounds = Bounds
(** Named geographic bounds and wire-format codecs. *)

module Tags = Tags
(** Extensible string-valued OSM tags. *)

module Reference = Reference
(** Object identity combining kind and numeric ID. *)

module Address = Address
(** Common Nominatim address components and additional fields. *)

module Place = Place
(** Decoded Nominatim places and their source references. *)

module Element = Element
(** Decoded nodes, ways, relations, areas and counts. *)

module Dataset = Dataset
(** Indexed Overpass responses with pure reference traversal. *)

module Query = Query
(** Typed builders for a subset of Overpass QL. *)

(** {1 HTTP client} *)

type error =
  | Http_status of { status : int; body : string }
      (** Non-2xx HTTP status with its bounded response body. *)
  | Invalid_response of string
      (** Invalid JSON or a malformed known response shape. *)
  | Api_error of string  (** A service error or a nonempty Overpass remark. *)
  | Response_too_large of int  (** The configured byte limit was exceeded. *)

type t
(** A client with operator-selected endpoints. Construction performs no I/O. *)

val v :
  ?overpass_url:string ->
  ?nominatim_url:string ->
  ?max_response:int ->
  _ Fetch.t ->
  t
(** [v http] is a client using [http]. Base URLs default to
    [http://marpe:8888/overpass/api/] and [http://marpe:8888/nominatim/]. A
    trailing slash is optional. URLs must be HTTP(S) without credentials, query
    or fragment. [max_response] defaults to 16 MiB and must be positive and
    below [max_int]. Invalid configuration raises [Invalid_argument]. Configure
    credentials, retries, rate limits and User-Agent on [http] before passing it
    here. Each operation owns and closes its HTTP response. There is no overall
    timeout or automatic retry policy in this library. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf error] formats [error], including server diagnostics. *)

(** {1 Geocoding} *)

module Nominatim : sig
  type structured = {
    amenity : string option;
    street : string option;
    city : string option;
    county : string option;
    state : string option;
    country : string option;
    postalcode : string option;
  }

  type search_query = Free_form of string | Structured of structured

  val structured :
    ?amenity:string ->
    ?street:string ->
    ?city:string ->
    ?county:string ->
    ?state:string ->
    ?country:string ->
    ?postalcode:string ->
    unit ->
    search_query
  (** [structured ()] constructs address search terms. At least one nonempty
      field is required when the query is submitted. *)

  val search_query :
    t ->
    ?limit:int ->
    ?addressdetails:bool ->
    ?language:string ->
    ?countrycodes:string list ->
    ?viewbox:Bounds.t ->
    ?bounded:bool ->
    search_query ->
    (Place.t list, error) result
  (** [search_query t query] supports free-form or structured terms, without
      mixing them. [viewbox] biases results. [bounded=true] filters to the
      viewbox and requires one. Other options match [search]. *)

  val search :
    t ->
    ?limit:int ->
    ?addressdetails:bool ->
    ?language:string ->
    ?countrycodes:string list ->
    ?viewbox:Bounds.t ->
    ?bounded:bool ->
    q:string ->
    unit ->
    (Place.t list, error) result
  (** [search t ~q ()] geocodes free-form [q] using JSONv2. An unmatched query
      returns [Ok []]. [limit] defaults to 10 and must be in 1..40.
      [addressdetails] defaults to true. [language] is an Accept-Language
      preference and [countrycodes] filters by ISO 3166-1 alpha-2 codes. Empty
      [q], an invalid limit or [bounded=true] without [viewbox] raises
      [Invalid_argument]. [bounded] defaults to false. *)

  val reverse :
    t ->
    ?zoom:int ->
    ?addressdetails:bool ->
    ?language:string ->
    lat:float ->
    lon:float ->
    unit ->
    (Place.t, error) result
  (** [reverse t ~lat ~lon ()] finds the nearest suitable indexed OSM object.
      [zoom] defaults to 18 and must be in 0..18. [addressdetails] defaults to
      true. Coordinates must be finite, latitude in -90..90 and longitude in
      -180..180. Invalid arguments raise [Invalid_argument]. A server response
      with no match is an error, not an empty place. *)

  val reverse_at :
    t ->
    ?zoom:int ->
    ?addressdetails:bool ->
    ?language:string ->
    Point.t ->
    (Place.t, error) result
  (** [reverse_at t point] is [reverse] using a validated point. *)
end

(** {1 Overpass} *)

module Overpass : sig
  val decode : string -> (Dataset.t, error) result
  (** [decode body] parses a response without I/O, rejecting API errors and
      nonempty remarks before decoding elements. *)

  val run : t -> 'a Query.t -> ('a, error) result
  (** [run t query] executes a typed query. Element queries return [Dataset.t]
      and count queries return [Element.count]. *)

  val query_raw : t -> string -> (string, error) result
  (** [query_raw t query] posts [query] in a form-encoded [data] field and
      returns the response bytes. It supports QL and XML queries and any output
      format. An empty query raises [Invalid_argument]. HTTP success can still
      contain an Overpass runtime error. *)

  val query : t -> string -> (Dataset.t, error) result
  (** [query t query] runs a complete query requesting JSON output, typically
      starting with a JSON output directive ([out:json]). It does not rewrite
      the query. A nonempty Overpass [remark] becomes [Api_error], so partial
      results from a runtime failure are never silently returned as complete. *)

  val status : t -> (string, error) result
  (** [status t] retrieves the status endpoint as text, including available
      slots and load information. HTTP success alone does not guarantee that a
      subsequent query will be admitted. *)
end

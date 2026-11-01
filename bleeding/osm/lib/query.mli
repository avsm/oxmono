(** Composable Overpass queries. Use [Osm.Overpass.query] for arbitrary QL. All
    tag-filter constructors reject empty keys and control characters. Regex
    syntax is validated by the server, not by this library. *)

type selector =
  | Nodes  (** Nodes only. *)
  | Ways  (** Ways only. *)
  | Relations  (** Relations only. *)
  | Nwr  (** Nodes, ways and relations. *)

type filter

val has : string -> filter
(** [has key] matches objects carrying [key], regardless of its value. *)

val missing : string -> filter
(** [missing key] matches objects without [key]. *)

val eq : string -> string -> filter
(** [eq key value] matches an exact tag value. *)

val matches : ?case_sensitive:bool -> string -> string -> filter
(** [matches key pattern] uses the server's regex syntax. Filters quote keys and
    values, escaping backslashes and quotes. Empty keys and control characters
    raise [Invalid_argument]. Multiple filters are conjunctive. *)

type scope

val global : scope
(** [global] imposes no geographic restriction. Queries can cover the entire
    database. *)

val bbox : Bounds.t -> scope
(** [bbox bounds] selects within [bounds], including antimeridian-crossing
    boxes. *)

val around : radius:float -> Point.t -> scope
(** [around ~radius point] selects within [radius] metres. Negative or nonfinite
    radii raise [Invalid_argument]. *)

val area : Reference.t -> scope
(** [area reference] uses [map_to_area] on a way or relation. Nodes raise
    [Invalid_argument]. A source object without a generated Overpass area
    matches nothing. No arithmetic assumption about area IDs is needed. *)

type selection

val select : ?filters:filter list -> scope:scope -> selector -> selection
(** [select ~scope selector] selects a kind of object in [scope]. [filters]
    defaults to none and combines with logical AND. *)

type detail =
  | Ids  (** Type and ID only. *)
  | Tags  (** Type, ID and tags. *)
  | Body  (** Tags, node coordinates and way/relation references. *)
  | Meta  (** Body plus edit metadata, where available. *)

type geometry =
  | No_geometry  (** No extra geometry. *)
  | Center  (** Bounding-box centers of ways and relations. *)
  | Bounds  (** Bounding boxes of ways and relations. *)
  | Full  (** Coordinates of referenced nodes and members. *)

type 'a t
(** ['a t] couples a generated query with its result codec. *)

val elements :
  ?timeout:int ->
  ?detail:detail ->
  ?geometry:geometry ->
  ?limit:int ->
  ?recurse:bool ->
  selection list ->
  Dataset.t t
(** [elements selections] unions selections and returns objects. Defaults are 25
    server seconds, body detail and no geometry. [recurse] includes all
    descendant members. A result [limit] can omit referenced objects even with
    recursion. An empty selection list or nonpositive limit/timeout raises
    [Invalid_argument]. The server timeout is not an HTTP deadline. *)

val count : ?timeout:int -> selection list -> Element.count t
(** [count selections] counts the union and decodes exactly one count object.
    [timeout] defaults to 25 seconds. Empty selections or nonpositive timeouts
    raise [Invalid_argument]. Encoding through [jsont] emits a minimal response
    envelope containing the count, without original server metadata. *)

val to_ql : 'a t -> string
(** [to_ql query] is a complete JSON-output Overpass QL program. *)

val jsont : 'a t -> 'a Jsont.t
(** [jsont query] is the codec for [query]'s expected response. *)

val references :
  ?timeout:int -> ?recurse:bool -> Reference.t list -> Dataset.t t
(** [references refs] retrieves objects by kind and ID. [recurse=true] includes
    descendant members. This is an explicit follow-up for missing references. An
    empty list or nonpositive timeout raises [Invalid_argument]. Defaults are 25
    server seconds and no recursion. Output detail is body. *)

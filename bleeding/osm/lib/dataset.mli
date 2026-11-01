@@ portable

(** An immutable indexed Overpass response. Traversal performs no I/O. *)

type t

val jsont : t Jsont.t
(** [jsont] maps Overpass JSON. It preserves unknown fields and does not
    interpret server remarks. The HTTP client rejects error remarks. *)

val elements : t -> Element.t list
(** [elements result] returns all entries in server order, including duplicates
    and unknown types. *)

val raw : t -> Jsont.json
(** [raw result] is the complete response object, including unknown fields. *)

val generator : t -> string option
(** [generator result] is the server software identification, when supplied. *)

val timestamp : t -> string option
(** [timestamp result] is the unparsed [osm3s.timestamp_osm_base] string. It
    dates the OSM database snapshot, not the request. *)

val copyright : t -> string option
(** [copyright result] is the server-provided attribution text, when supplied. *)

val find : Reference.t -> t -> Element.t option
(** [find reference result] looks up an object by kind and ID. If repeated in
    the response, the last occurrence wins. [elements] preserves all entries. *)

val nodes : t -> Element.node list
(** [nodes result] selects nodes, retaining response order and duplicates. *)

val ways : t -> Element.way list
(** [ways result] selects ways, retaining response order and duplicates. *)

val relations : t -> Element.relation list
(** [relations result] selects relations, retaining response order and
    duplicates. *)

val counts : t -> Element.count list
(** [counts result] selects count entries in response order. Counts are
    server-reported, not computed from the returned objects. *)

type incomplete = Not_requested | Missing of Reference.t list

val way_nodes : t -> Element.way -> (Element.node list, incomplete) result
(** [way_nodes result way] resolves nodes in way order, retaining duplicates.
    [Not_requested] means the node list was omitted. [Missing refs] lists
    references not present in [result]. *)

val relation_members :
  t ->
  Element.relation ->
  ((Element.member * Element.t) list, incomplete) result
(** [relation_members result relation] resolves immediate members in order,
    retaining roles. It does not recurse through relation cycles. *)

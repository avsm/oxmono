@@ portable

type kind = Node | Way | Relation

val kind_jsont : kind Jsont.t
(** [kind_jsont] maps the strings [node], [way] and [relation]. Other strings
    fail. *)

val kind_name : kind -> string
(** [kind_name kind] is its lowercase OSM type name. *)

type t = private { kind : kind; id : int64 }

val v : kind -> int64 -> t
(** [v kind id] identifies an OSM object. Nonpositive [id] raises
    [Invalid_argument]. IDs are scoped by kind. *)

val jsont : t Jsont.t
(** [jsont] maps the [type] and [ref] members of a relation member. *)

val to_string : t -> string
(** [to_string reference] is a Nominatim reference such as [R123]. *)

val compare : t -> t -> int
(** [compare a b] orders references by kind (node, way, relation), then numeric
    ID. *)

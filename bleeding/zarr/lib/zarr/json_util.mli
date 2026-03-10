(** {1 JSON Utilities}

    Internal helpers for constructing and destructuring {!Jsont.json}
    values. Also provides string I/O via {!Jsont_bytesrw}.

    These are convenience wrappers around the jsont generic JSON type.
    Users who need to build attribute objects for Zarr arrays and groups
    can use these helpers. *)

(** {2 Constructors}

    Build {!Jsont.json} values with no metadata. *)

val str : string -> Jsont.json
(** [str s] is a JSON string. *)

val num : float -> Jsont.json
(** [num f] is a JSON number. *)

val int : int -> Jsont.json
(** [int i] is a JSON number from an integer. *)

val bool : bool -> Jsont.json
(** [bool b] is a JSON boolean. *)

val null : Jsont.json
(** [null] is the JSON null value. *)

val obj : Jsont.mem list -> Jsont.json
(** [obj mems] is a JSON object with the given members. *)

val arr : Jsont.json list -> Jsont.json
(** [arr elts] is a JSON array with the given elements. *)

val mem : string -> Jsont.json -> Jsont.mem
(** [mem name value] creates a JSON object member. *)

(** {2 Extractors}

    Access values from {!Jsont.json}. *)

val find_member : string -> Jsont.json -> Jsont.json option
(** [find_member name json] finds a member in a JSON object.
    Returns [None] for non-objects or missing members. *)

val member : string -> Jsont.json -> Jsont.json
(** [member name json] finds a member, returning {!null} if absent. *)

val to_string_exn : Jsont.json -> string
(** [to_string_exn json] extracts a string or raises [Failure]. *)

val to_string_opt : Jsont.json -> string option
(** [to_string_opt json] extracts a string or returns [None]. *)

val to_int_exn : Jsont.json -> int
(** [to_int_exn json] extracts an integer from a number.
    Raises [Failure] if not a number or not an integer. *)

val to_int_opt : Jsont.json -> int option
(** [to_int_opt json] extracts an integer or returns [None]. *)

val to_float_exn : Jsont.json -> float
(** [to_float_exn json] extracts a float or raises [Failure]. *)

val to_float_opt : Jsont.json -> float option

val to_bool_exn : Jsont.json -> bool
(** [to_bool_exn json] extracts a boolean or raises [Failure]. *)

val to_bool_opt : Jsont.json -> bool option

val to_list_exn : Jsont.json -> Jsont.json list
(** [to_list_exn json] extracts an array or raises [Failure]. *)

val to_list_opt : Jsont.json -> Jsont.json list option

val to_object_exn : Jsont.json -> Jsont.object'
(** [to_object_exn json] extracts object members or raises [Failure]. *)

val is_null : Jsont.json -> bool
(** [is_null json] is [true] iff [json] is a null value. *)

(** {2 String I/O} *)

val of_string : string -> Jsont.json
(** [of_string s] parses a JSON string into a generic JSON value.
    @raise Failure if the string is not valid JSON. *)

val to_string : ?format:Jsont.format -> Jsont.json -> string
(** [to_string json] serialises a JSON value to a string.
    Defaults to {!Jsont.Minify}. *)

val to_string_pretty : Jsont.json -> string
(** [to_string_pretty json] serialises with indentation. *)

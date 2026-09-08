(** json_codec — client-only JSON construction helpers.

    Protocol-wide checked primitives, maps and timestamps live in
    {!Matrix_proto.Json.Codec}. This compatibility module retains the helpers
    used by existing client modules plus URI and object-construction functions
    which belong at the client boundary. *)

val obj : (string * Jsont.json) list -> Jsont.json
(** [obj members] is the JSON object with [members], in the order given. *)

val merge_extra_content :
  Jsont.json -> ?extra_content:Jsont.json -> unit -> Jsont.json
(** [merge_extra_content base ?extra_content ()] appends the members of the
    extra JSON object that are not already present in [base]. Thus typed or
    generated fields always win collisions, while vendor fields retain their
    input order. Both values must be JSON objects. *)

val string_map : 'a Jsont.t -> (string * 'a) list Jsont.t
(** [string_map codec] reads and writes a JSON object whose every value is read
    by [codec], as an association list. Decoding sorts by key. *)

val uri : Uriz.t Jsont.t
(** Reads and writes a URI reference as a JSON string. Invalid URI syntax
    produces a decoding error. *)

val keyed_map :
  ?skip_invalid:bool ->
  what:string ->
  of_string:(string -> ('k, [ `Msg of string ]) result) @ portable ->
  to_string:('k -> string) @ portable ->
  'a Jsont.t ->
  ('k * 'a) list Jsont.t
(** [keyed_map ~what ~of_string ~to_string codec] reads and writes a JSON object
    whose member names are read by [of_string] and whose values are read by
    [codec], as an association list. Decoding sorts by member name and fails
    when [of_string] rejects one, naming [what] in the error. [skip_invalid]
    drops a member whose name [of_string] rejects instead of failing. It
    defaults to [false]. *)

val ptime : Ptime.t Jsont.t
(** Reads and writes an instant as an RFC 3339 string in UTC. *)

val persisted_timestamp : Matrix_proto.Event.Timestamp.t Jsont.t
(** The migration codec for a millisecond timestamp in an existing on-disk
    schema. It retains Jsont's historical number-or-string input; Matrix wire
    timestamps must use {!Matrix_proto.Event.Timestamp.jsont}. *)

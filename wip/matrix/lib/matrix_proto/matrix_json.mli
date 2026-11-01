(** Checked Matrix JSON codecs and projections for untyped JSON.

    [Jsont.json] is a public variant, so a value whose shape is not known until
    it is read, such as an event content or a piece of account data, is
    inspected by pattern matching rather than by running a codec over it. These
    are the projections that inspection needs. Each is total. A value of the
    wrong shape gives [None] rather than raising. *)

(** {1 Checked codecs} *)

module Codec : sig
  (** The only primitive Jsont codecs Matrix wire descriptions should use.

      Jsont's stock integer codecs accept JSON strings and truncate fractional
      numbers. Its stock number codec maps non-finite values to JSON null, and
      its string encoder assumes valid UTF-8. These codecs reject those cases
      instead. *)

  type 'a t = 'a Jsont.t

  val bool : bool t @@ portable

  val string : string t @@ portable
  (** JSON strings containing valid UTF-8. Invalid caller-built strings also
      fail on encoding. *)

  val number : float t @@ portable
  (** Finite JSON numbers. JSON null, NaN and infinities are rejected on both
      sides of the codec. *)

  val int : int t @@ portable

  val int64 : int64 t @@ portable
  (** Integral JSON numbers in Matrix's interoperable [[-(2^53)+1; (2^53)-1]]
      range. Numeric strings and fractions are rejected before conversion. [int]
      additionally checks the platform [int] range. *)

  val uint : int t @@ portable

  val uint64 : int64 t @@ portable
  (** Non-negative variants of {!int} and {!int64}. *)

  val int8 : int t @@ portable

  val int16 : int t @@ portable

  val int32 : int32 t @@ portable

  val uint8 : int t @@ portable

  val uint16 : int t @@ portable
  (** Checked codecs for the corresponding fixed ranges. *)

  val int_range : min:int -> max:int -> int t @@ portable

  val int64_range : min:int64 -> max:int64 -> int64 t @@ portable
  (** Checked inclusive subranges. The bounds themselves must be ordered and lie
      in Matrix's interoperable integer range. *)

  val nullable : 'a t -> 'a option t @@ portable
  (** [nullable codec] accepts an explicit JSON null as [None]. *)

  val json : Jsont.json t @@ portable
  (** Generic JSON with recursively checked finite numbers, UTF-8 strings and
      member names, and unique object member names. The same checks run when a
      caller-built value is encoded. *)

  val validate : Jsont.json -> (unit, string) result @@ portable
  (** [validate json] applies the checks owned by {!json} without recoding the
      value. *)

  val validate_text : string -> (unit, string) result
  (** [validate_text text] verifies UTF-8 JSON syntax and rejects duplicate
      object members before a typed Jsont decoder can apply its last-member-wins
      behaviour. Resource limits such as nesting depth remain the responsibility
      of the I/O boundary. *)

  val string_map : 'a t -> (string * 'a) list t @@ portable
  (** A JSON object with checked UTF-8 member names and uniformly typed values,
      represented as an association list sorted by name. *)

  val string_map_mems :
    'a t ->
    ( 'a Stdlib.Map.MakePortable(String).t,
      'a,
      'a Stdlib.Map.MakePortable(String).t )
    Jsont.Object.Mems.map @@ portable
  (** A checked replacement for [Jsont.Object.Mems.string_map], for typed
      objects which retain unknown members. *)

  val as_string_map : 'a t -> 'a Stdlib.Map.MakePortable(String).t t @@ portable
  (** A checked replacement for [Jsont.Object.as_string_map]. *)

  val keyed_map :
    ?skip_invalid:bool ->
    what:string ->
    of_string:(string -> ('key, [ `Msg of string ]) result) @ portable ->
    to_string:('key -> string) @ portable ->
    'a t ->
    ('key * 'a) list t @@ portable
  (** [keyed_map] is {!string_map} with validated typed keys. Invalid decoded
      keys fail unless [skip_invalid] is [true]. *)

  val ptime : Ptime.t t @@ portable
  (** An RFC 3339 instant, encoded in UTC. *)

  module Legacy : sig
    val int : int t @@ portable

    val int64 : int64 t @@ portable
  (** Jsont's permissive migration codecs. They accept number-or-string
        encodings, including the historical unsafe-number boundary accepted by
        Jsont before 0.2. These are only for already-persisted formats; Matrix
        wire codecs must not use them. *)
  end
end

(** {1 Projections} *)

val as_string : Jsont.json -> string option @@ portable
  (** [as_string j] is the valid UTF-8 string [j] holds. *)

val as_bool : Jsont.json -> bool option @@ portable
  (** [as_bool j] is the boolean [j] holds. *)

val as_int : Jsont.json -> int option @@ portable
  (** [as_int j] is the integer [j] holds. It is [None] for a number with a
    fractional part, a non-finite value, or a value outside the Matrix-safe or
    platform integer ranges. *)

val as_int64 : Jsont.json -> int64 option @@ portable
  (** [as_int64 j] is the integer [j] holds. It is [None] for a number with a
    fractional part, a non-finite value, or a value outside the Matrix-safe
    range. *)

val as_float : Jsont.json -> float option @@ portable
  (** [as_float j] is the finite number [j] holds. *)

val as_array : Jsont.json -> Jsont.json list option @@ portable
  (** [as_array j] is the elements of the array [j] holds. *)

val as_object : Jsont.json -> Jsont.object' option @@ portable
  (** [as_object j] is the members of the object [j] holds. *)

(** {1 Member lookup} *)

val find_mem : string -> Jsont.json -> Jsont.json option @@ portable
  (** [find_mem name j] is the value of [j]'s [name] member. It is [None] if [j]
    is not an object or has no such member. *)

val find_string : string -> Jsont.json -> string option @@ portable
  (** [find_string name j] is {!find_mem} followed by {!as_string}. *)

val find_bool : string -> Jsont.json -> bool option @@ portable
  (** [find_bool name j] is {!find_mem} followed by {!as_bool}. *)

val find_int : string -> Jsont.json -> int option @@ portable
  (** [find_int name j] is {!find_mem} followed by {!as_int}. *)

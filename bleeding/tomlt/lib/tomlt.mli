(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative {{:https://toml.io/en/v1.1.0}TOML 1.1} codecs.

    Tomlt provides a bidirectional codec system for TOML files, inspired by
    {{:https://erratique.ch/software/jsont}Jsont}'s approach to JSON codecs.

    {2 Quick Start}

    Define a codec for your OCaml types:
    {v
    type config = { host : string; port : int; debug : bool }

    let config_codec =
      Tomlt.(Table.(
        obj (fun host port debug -> { host; port; debug })
        |> mem "host" string ~enc:(fun c -> c.host)
        |> mem "port" int ~enc:(fun c -> c.port)
        |> mem "debug" bool ~enc:(fun c -> c.debug) ~dec_absent:false
        |> finish
      ))
    v}

    For I/O operations (parsing strings, reading files), use {!Tomlt_bytesrw}:
    {v
    let () =
      match Tomlt_bytesrw.decode_string config_codec {|
        host = "localhost"
        port = 8080
      |} with
      | Ok config -> Printf.printf "Host: %s\n" config.host
      | Error e -> prerr_endline (Toml.Error.to_string e)
    v}

    {2 Codec Pattern}

    Each codec ['a t] defines:
    - A decoder: [Toml.t -> ('a, error) result]
    - An encoder: ['a -> Toml.t]

    Codecs compose through combinators to build complex types from
    simple primitives.

    {2 Cookbook}

    See the {{!page-cookbook}cookbook} for patterns and recipes:

    - {{!page-cookbook.config_files}Parsing configuration files}
    - {{!page-cookbook.optional_values}Optional and absent values}
    - {{!page-cookbook.datetimes}Working with datetimes}
    - {{!page-cookbook.arrays}Working with arrays}
    - {{!page-cookbook.tables}Nested tables and objects}
    - {{!page-cookbook.unknown_members}Unknown member handling}
    - {{!page-cookbook.validation}Validation and constraints}

    {2 Module Overview}

    - {!section:datetime} - Structured datetime types (for advanced use)
    - {!section:codec} - Core codec type and combinators
    - {!section:base} - Primitive type codecs
    - {!section:ptime_codecs} - Ptime-based datetime codecs
    - {!section:combinators} - Codec transformers
    - {!section:arrays} - Array codec builders
    - {!section:tables} - Table/object codec builders
    - {!section:codec_ops} - Encoding and decoding operations

    {2 Related Libraries}

    {ul
    {- [Tomlt_bytesrw] - Byte-level I/O for string and channel operations}
    {- [Tomlt_eio] - Eio integration with system timezone support}
    {- [Toml] - Low-level TOML value type and error handling}} *)

(** {1:preliminaries Preliminaries} *)

type 'a fmt = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

(** Sorts of TOML values.

    TOML values are classified into sorts (types). This module provides
    utilities for working with these sorts programmatically. *)
module Sort : sig
  type t =
    | String  (** Strings *)
    | Int     (** Integers *)
    | Float   (** Floating-point numbers *)
    | Bool    (** Booleans *)
    | Datetime (** Offset datetimes *)
    | Datetime_local (** Local datetimes *)
    | Date    (** Local dates *)
    | Time    (** Local times *)
    | Array   (** Arrays *)
    | Table   (** Tables (objects) *)
  (** The type for sorts of TOML values. *)

  val to_string : t -> string
  (** [to_string sort] is a human-readable string for [sort]. *)

  val pp : t fmt
  (** [pp] formats sorts. *)

  val of_toml : Toml.t -> t
  (** [of_toml v] returns the sort of TOML value [v]. *)

  val or_kind : kind:string -> t -> string
  (** [or_kind ~kind sort] is [to_string sort] if [kind] is [""] and
      [kind] otherwise. *)

  val kinded : kind:string -> t -> string
  (** [kinded ~kind sort] is [to_string sort] if [kind] is [""]
      and [String.concat " " \[kind; to_string sort\]] otherwise. *)
end

(** {1:datetime Structured Datetime Types}

    TOML 1.1 supports four datetime formats:

    - {b {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetime}}:
      [1979-05-27T07:32:00Z] or [1979-05-27T07:32:00-07:00]
    - {b {{:https://toml.io/en/v1.1.0#local-date-time}Local datetime}}:
      [1979-05-27T07:32:00] (no timezone)
    - {b {{:https://toml.io/en/v1.1.0#local-date}Local date}}:
      [1979-05-27]
    - {b {{:https://toml.io/en/v1.1.0#local-time}Local time}}:
      [07:32:00] or [07:32:00.999999]

    These modules provide structured representations for parsing and
    formatting. For most use cases, prefer the {!section:ptime_codecs}
    which provide a unified Ptime-based interface. *)

(** Timezone offsets for {{:https://toml.io/en/v1.1.0#offset-date-time}TOML
    offset datetimes}.

    Per RFC 3339, timezones are expressed as [Z] (UTC) or as
    [+HH:MM] / [-HH:MM] offsets from UTC. *)
module Tz : sig
  (** Timezone offset representation. *)
  type t =
    | UTC                                    (** UTC timezone, written as [Z] *)
    | Offset of { hours : int; minutes : int } (** Fixed offset from UTC *)

  val utc : t
  (** [utc] is the UTC timezone. *)

  val offset : hours:int -> minutes:int -> t
  (** [offset ~hours ~minutes] creates a fixed UTC offset.
      Hours may be negative for western timezones. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val compare : t -> t -> int
  (** [compare a b] is a total ordering. *)

  val to_string : t -> string
  (** [to_string tz] formats as ["Z"] or ["+HH:MM"]/["-HH:MM"]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt tz] pretty-prints the timezone. *)

  val of_string : string -> (t, string) result
  (** [of_string s] parses ["Z"], ["+HH:MM"], or ["-HH:MM"]. *)
end

(** {{:https://toml.io/en/v1.1.0#local-date}Local dates} (no timezone information).

    Represents a calendar date like [1979-05-27]. *)
module Date : sig
  type t = { year : int; month : int; day : int }
  (** A calendar date with year (4 digits), month (1-12), and day (1-31). *)

  val make : year:int -> month:int -> day:int -> t
  (** [make ~year ~month ~day] creates a date value. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  (** [to_string d] formats as ["YYYY-MM-DD"]. *)

  val pp : Format.formatter -> t -> unit
  val of_string : string -> (t, string) result
  (** [of_string s] parses ["YYYY-MM-DD"] format. *)
end

(** {{:https://toml.io/en/v1.1.0#local-time}Local times} (no date or timezone).

    Represents a time of day like [07:32:00] or [07:32:00.999999]. *)
module Time : sig
  type t = {
    hour : int;     (** Hour (0-23) *)
    minute : int;   (** Minute (0-59) *)
    second : int;   (** Second (0-59, 60 for leap seconds) *)
    frac : float;   (** Fractional seconds in range \[0.0, 1.0) *)
  }

  val make : hour:int -> minute:int -> second:int -> ?frac:float -> unit -> t
  (** [make ~hour ~minute ~second ?frac ()] creates a time value.
      [frac] defaults to [0.0]. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  (** [to_string t] formats as ["HH:MM:SS"] or ["HH:MM:SS.fff"]. *)

  val pp : Format.formatter -> t -> unit
  val of_string : string -> (t, string) result
end

(** {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetimes}
    (date + time + timezone).

    The complete datetime format per RFC 3339, like
    [1979-05-27T07:32:00Z] or [1979-05-27T07:32:00-07:00]. *)
module Datetime : sig
  type t = { date : Date.t; time : Time.t; tz : Tz.t }

  val make : date:Date.t -> time:Time.t -> tz:Tz.t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val of_string : string -> (t, string) result
end

(** {{:https://toml.io/en/v1.1.0#local-date-time}Local datetimes}
    (date + time, no timezone).

    Like [1979-05-27T07:32:00] - a datetime with no timezone
    information, representing "wall clock" time. *)
module Datetime_local : sig
  type t = { date : Date.t; time : Time.t }

  val make : date:Date.t -> time:Time.t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val of_string : string -> (t, string) result
end

(** {1:codec Codec Types} *)

(** Errors that can occur during codec operations. *)
type codec_error =
  | Type_mismatch of { expected : string; got : string }
      (** TOML value was not the expected type *)
  | Missing_member of string
      (** Required table member was not present *)
  | Unknown_member of string
      (** Unknown member found (when using [error_unknown]) *)
  | Value_error of string
      (** Value failed validation or parsing *)
  | Int_overflow of int64
      (** Integer value exceeds OCaml [int] range *)
  | Parse_error of string
      (** Parsing failed *)

val codec_error_to_string : codec_error -> string
(** [codec_error_to_string e] returns a human-readable error message. *)

(** The type of TOML codecs.

    A value of type ['a t] can decode TOML values to type ['a]
    and encode values of type ['a] to TOML. *)
type 'a t

val kind : 'a t -> string
(** [kind c] returns the kind description of codec [c]. *)

val doc : 'a t -> string
(** [doc c] returns the documentation string of codec [c]. *)

val with_doc : ?kind:string -> ?doc:string -> 'a t -> 'a t
(** [with_doc ?kind ?doc c] is [c] with its {!kind} or {!doc} updated
    to the corresponding values if specified. Unlike {!map}, this does
    not change the codec's decoding or encoding behavior.

    {4 Example}
    {[
      let person_id = with_doc ~kind:"person ID" int
    ]} *)

(** {1:base Base Type Codecs}

    Primitive codecs for {{:https://toml.io/en/v1.1.0}TOML 1.1}'s basic
    value types. *)

val bool : bool t
(** Codec for {{:https://toml.io/en/v1.1.0#boolean}TOML booleans}. *)

val int : int t
(** Codec for {{:https://toml.io/en/v1.1.0#integer}TOML integers} to OCaml [int].
    Supports decimal, hex ([0x]), octal ([0o]), and binary ([0b]) formats.
    @raise Int_overflow if the value exceeds platform [int] range. *)

val int32 : int32 t
(** Codec for {{:https://toml.io/en/v1.1.0#integer}TOML integers} to [int32]. *)

val int64 : int64 t
(** Codec for {{:https://toml.io/en/v1.1.0#integer}TOML integers} to [int64]. *)

val float : float t
(** Codec for {{:https://toml.io/en/v1.1.0#float}TOML floats}.
    Handles [inf], [-inf], and [nan] per the spec. *)

val number : float t
(** Codec that accepts both {{:https://toml.io/en/v1.1.0#integer}integers}
    and {{:https://toml.io/en/v1.1.0#float}floats} as [float].
    Integers are converted to floats during decoding. *)

val string : string t
(** Codec for {{:https://toml.io/en/v1.1.0#string}TOML strings} (UTF-8 encoded).
    Supports basic strings, literal strings, and their multiline variants. *)

val int_as_string : int t
(** Codec for integers stored as TOML strings.

    On decode, uses [int_of_string_opt] which accepts decimal, hex ([0x]),
    octal ([0o]), and binary ([0b]) formats.
    On encode, uses [Int.to_string] (decimal).

    Useful when integers must be stored as strings for compatibility,
    or when you need to preserve leading zeros or specific formats. *)

val int64_as_string : int64 t
(** Codec for 64-bit integers stored as TOML strings.

    Like {!int_as_string} but for [int64] values. Uses [Int64.of_string_opt]
    for decoding and [Int64.to_string] for encoding. *)

(** {1:ptime_codecs Ptime Datetime Codecs}

    Tomlt provides unified datetime handling using
    {{:https://erratique.ch/software/ptime}Ptime}. All TOML datetime formats
    can be decoded to [Ptime.t] timestamps.

    See the {{!page-cookbook.datetimes}cookbook} for detailed patterns
    and examples.

    {2 Choosing a Codec}

    - {!val:ptime} - Accepts any datetime format, normalizes to [Ptime.t]
    - {!val:ptime_opt} - Strict: only accepts offset datetimes with timezone
    - {!val:ptime_date} - For date-only fields
    - {!val:ptime_span} - For time-only fields (as duration from midnight)
    - {!val:ptime_full} - Preserves exact variant for roundtripping *)

val ptime :
  ?tz_offset_s:int ->
  ?get_tz:(unit -> int option) ->
  ?now:(unit -> Ptime.t) ->
  ?frac_s:int ->
  unit -> Ptime.t t
(** Datetime codec that converts any TOML datetime to {!Ptime.t}.

    Handles all TOML datetime variants by filling in sensible defaults.
    Encoding produces RFC 3339 offset datetime strings.

    See {{!page-cookbook.datetimes}Working with datetimes} for examples.

    @param tz_offset_s Timezone offset in seconds for local datetimes.
      Common: [0] (UTC), [3600] (+01:00), [-18000] (-05:00).
    @param get_tz Function to get timezone offset when [tz_offset_s]
      not provided. Use [Tomlt_unix.current_tz_offset_s] for system timezone.
    @param now Function for current time, used for time-only values.
      Use [Tomlt_unix.now] for system time.
    @param frac_s Fractional second digits (0-12) for encoding. *)

val ptime_opt : ?tz_offset_s:int -> ?frac_s:int -> unit -> Ptime.t t
(** Strict datetime codec that only accepts offset datetimes.

    Requires explicit timezone; rejects local datetimes, dates, and times.
    Use when you need unambiguous timestamps.

    See {{!page-cookbook.datetimes}Working with datetimes} for examples.

    @param tz_offset_s Timezone offset for encoding. Default: 0 (UTC).
    @param frac_s Fractional second digits for encoding. Default: 0. *)

val ptime_span : Ptime.Span.t t
(** Codec for TOML local times as [Ptime.Span.t] (duration from midnight).

    Decodes [07:32:00] to a span representing time since midnight.
    Values are clamped to [00:00:00] to [23:59:59.999999999].

    See {{!page-cookbook.datetimes}Working with datetimes} for examples. *)

val ptime_date : Ptime.date t
(** Codec for TOML local dates as [Ptime.date] ([(year, month, day)] tuple).

    Decodes [1979-05-27] to [(1979, 5, 27)]. Only accepts local dates.
    To work with dates as [Ptime.t] (at midnight), use {!ptime} instead.

    See {{!page-cookbook.datetimes}Working with datetimes} for examples. *)

val ptime_full :
  ?tz_offset_s:int ->
  ?get_tz:(unit -> int option) ->
  unit -> Toml.ptime_datetime t
(** Codec that preserves full datetime variant information.

    Returns a {!Toml.ptime_datetime} variant indicating exactly what was
    present in the TOML source. Essential for roundtripping TOML files
    while preserving the original format.

    See {{!page-cookbook.datetimes}Working with datetimes} and
    {{!page-cookbook.roundtripping}Roundtripping TOML} for examples.

    @param tz_offset_s Timezone offset for converting [`Datetime_local].
    @param get_tz Function for timezone when [tz_offset_s] not provided. *)

(** {1:combinators Codec Combinators} *)

val map :
  ?kind:string -> ?doc:string ->
  ?dec:('a -> 'b) -> ?enc:('b -> 'a) ->
  'a t -> 'b t
(** [map ?dec ?enc c] transforms codec [c] through functions.
    [dec] transforms decoded values; [enc] transforms values before encoding. *)

val const : ?kind:string -> ?doc:string -> 'a -> 'a t
(** [const v] is a codec that always decodes to [v] and encodes as empty. *)

val enum : ?cmp:('a -> 'a -> int) -> ?kind:string -> ?doc:string ->
  (string * 'a) list -> 'a t
(** [enum assoc] creates a codec for string enumerations.
    @param cmp Comparison function for finding values during encoding.
    @param assoc List of [(string, value)] pairs. *)

val option : ?kind:string -> ?doc:string -> 'a t -> 'a option t
(** [option c] wraps codec [c] to decode [Some v] or encode [None] as omitted. *)

val result : ok:'a t -> error:'b t -> ('a, 'b) result t
(** [result ~ok ~error] tries [ok] first, then [error]. *)

val rec' : 'a t Lazy.t -> 'a t
(** [rec' lazy_c] creates a recursive codec.
    Use for self-referential types:
    {v
    let rec tree = lazy Tomlt.(
      Table.(obj (fun v children -> Node (v, children))
        |> mem "value" int ~enc:(function Node (v, _) -> v)
        |> mem "children" (list (rec' tree)) ~enc:(function Node (_, cs) -> cs)
        |> finish))
    v} *)

val iter :
  ?kind:string -> ?doc:string ->
  ?dec:('a -> unit) -> ?enc:('a -> unit) ->
  'a t -> 'a t
(** [iter ?dec ?enc c] applies [dec] on decoding and [enc] on encoding
    but otherwise behaves like [c]. Useful for:
    - Asserting additional constraints on decoded values
    - Tracing/debugging codec behavior
    - Side effects during encoding/decoding

    {4 Example}
    {[
      (* Trace all decoded integers *)
      let traced_int = iter int
        ~dec:(fun i -> Printf.printf "Decoded: %d\n" i)

      (* Validate port range *)
      let port = iter int
        ~dec:(fun p ->
          if p < 0 || p > 65535 then
            failwith "port out of range")
    ]} *)

val recode : dec:'a t -> ('a -> 'b) -> enc:'b t -> 'b t
(** [recode ~dec f ~enc] decodes like [dec] followed by [f], and
    encodes using [enc]. This allows changing the TOML representation
    while maintaining bidirectionality.

    {4 Example}
    {[
      (* Store URI as string, decode to Uri.t *)
      let uri_codec =
        recode ~dec:string Uri.of_string ~enc:string_of_uri

      (* Convert between string list and comma-separated string *)
      let tags_codec =
        recode
          ~dec:string
          (String.split_on_char ',')
          ~enc:(list string)
    ]} *)

(** {2:query Query Combinators}

    Extract single values from arrays or tables without processing
    the entire structure. *)

val nth : ?absent:'a -> int -> 'a t -> 'a t
(** [nth n t] decodes the [n]th element of a TOML array with [t].
    Other elements are skipped.

    @param absent Value to use if the index is out of bounds.
      If not provided, an error is raised for out-of-bounds access.
    @raise Value_error if [n] is out of bounds and [absent] is not provided.

    {4 Example}
    {[
      (* Get first element of array *)
      let first = nth 0 string

      (* Get second element with default *)
      let second = nth ~absent:"default" 1 string
    ]} *)

val mem : ?absent:'a -> string -> 'a t -> 'a t
(** [mem name t] decodes the member named [name] from a TOML table with [t].
    Other members are skipped. This is simpler than {!Table} when you only
    need a single value.

    @param absent Value to use if the member doesn't exist.
      If not provided, an error is raised for missing members.

    {4 Example}
    {[
      (* Extract just the "version" field *)
      let version = mem "version" string

      (* With default value *)
      let debug = mem ~absent:false "debug" bool
    ]} *)

(** {2:folding Folding Combinators}

    Process all elements of an array or table. These are decode-only;
    encoding produces empty containers. *)

val fold_array : 'a t -> (int -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_array t f init] folds [f] over all elements of a TOML array,
    decoding each element with [t]. The fold function receives the index,
    decoded value, and accumulator.

    Encodes to an empty array (folding is decode-only).

    {4 Example}
    {[
      (* Sum all integers in array *)
      let sum = fold_array int (fun _i x acc -> x + acc) 0

      (* Collect values into a Set *)
      let string_set = fold_array string
        (fun _i s acc -> StringSet.add s acc) StringSet.empty
    ]} *)

val fold_table : 'a t -> (string -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_table t f init] folds [f] over all members of a TOML table,
    decoding each value with [t]. The fold function receives the key,
    decoded value, and accumulator.

    Encodes to an empty table (folding is decode-only).

    {4 Example}
    {[
      (* Build a map from table *)
      let string_map = fold_table string
        (fun k v acc -> StringMap.add k v acc) StringMap.empty

      (* Count members *)
      let count = fold_table any (fun _k _v n -> n + 1) 0
    ]} *)

(** {2:ignoring Ignoring and Placeholders} *)

val ignore : unit t
(** [ignore] maps any TOML value to [()] on decoding and errors on encoding.
    Use for values you want to skip during decoding. *)

val zero : unit t
(** [zero] maps any TOML value to [()] on decoding and encodes as an
    empty table. Useful for placeholder values. *)

val todo : ?kind:string -> ?doc:string -> ?dec_stub:'a -> unit -> 'a t
(** [todo ?dec_stub ()] is a placeholder codec for work in progress.
    - On decode: returns [dec_stub] if provided, errors otherwise
    - On encode: always errors

    Useful during development to mark incomplete parts of a codec.

    {4 Example}
    {[
      type config = { name : string; advanced : unit (* TODO *) }

      let config_codec = Tomlt.(Table.(
        obj (fun name advanced -> { name; advanced })
        |> mem "name" string ~enc:(fun c -> c.name)
        |> mem "advanced" (todo ~dec_stub:() ()) ~enc:(fun _ -> ())
        |> finish
      ))
    ]} *)

(** {1:arrays Array Codecs}

    Build codecs for {{:https://toml.io/en/v1.1.0#array}TOML arrays}.

    See {{!page-cookbook.arrays}Working with arrays} for patterns. *)

module Array : sig
  type 'a codec = 'a t

  (** Encoder specification for arrays. *)
  type ('array, 'elt) enc = {
    fold : 'acc. ('acc -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc
  }

  (** Array codec builder. *)
  type ('array, 'elt, 'builder) map

  val map :
    ?kind:string -> ?doc:string ->
    ?dec_empty:(unit -> 'builder) ->
    ?dec_add:('elt -> 'builder -> 'builder) ->
    ?dec_finish:('builder -> 'array) ->
    ?enc:('array, 'elt) enc ->
    'elt codec -> ('array, 'elt, 'builder) map
  (** [map elt] creates an array codec builder for elements of type ['elt]. *)

  val list : ?kind:string -> ?doc:string -> 'a codec -> ('a list, 'a, 'a list) map
  (** [list c] builds lists from arrays of elements decoded by [c]. *)

  val array : ?kind:string -> ?doc:string -> 'a codec -> ('a array, 'a, 'a list) map
  (** [array c] builds arrays from arrays of elements decoded by [c]. *)

  val finish : ('array, 'elt, 'builder) map -> 'array codec
  (** [finish m] completes the array codec. *)
end

val list : ?kind:string -> ?doc:string -> 'a t -> 'a list t
(** [list c] is a codec for {{:https://toml.io/en/v1.1.0#array}TOML arrays}
    as OCaml lists. *)

val array : ?kind:string -> ?doc:string -> 'a t -> 'a array t
(** [array c] is a codec for {{:https://toml.io/en/v1.1.0#array}TOML arrays}
    as OCaml arrays. *)

(** {1:tables Table Codecs}

    Build codecs for {{:https://toml.io/en/v1.1.0#table}TOML tables}
    using an applicative-style builder pattern.

    See the {{!page-cookbook.config_files}cookbook} for configuration patterns,
    {{!page-cookbook.optional_values}optional values}, and
    {{!page-cookbook.unknown_members}unknown member handling}. *)

module Table : sig
  type 'a codec = 'a t

  (** {2 Member Specifications} *)

  module Mem : sig
    type 'a codec = 'a t
    type ('o, 'a) t
    (** A member specification for type ['a] within object type ['o]. *)

    val v :
      ?doc:string ->
      ?dec_absent:'a ->
      ?enc:('o -> 'a) ->
      ?enc_omit:('a -> bool) ->
      string -> 'a codec -> ('o, 'a) t
    (** [v name codec] creates a member specification.
        @param doc Documentation for this member.
        @param dec_absent Default value if member is absent (makes it optional).
        @param enc Encoder function from object to member value.
        @param enc_omit Predicate to omit member during encoding. *)

    val opt :
      ?doc:string ->
      ?enc:('o -> 'a option) ->
      string -> 'a codec -> ('o, 'a option) t
    (** [opt name codec] creates an optional member that decodes to [None]
        when absent and is omitted when encoding [None]. *)
  end

  (** {2 Table Builder} *)

  type ('o, 'dec) map
  (** Builder state for a table codec producing ['o], currently decoding ['dec]. *)

  val obj : ?kind:string -> ?doc:string -> 'dec -> ('o, 'dec) map
  (** [obj f] starts building a table codec with decoder function [f].

      The function [f] receives each member's decoded value as arguments
      and returns the final decoded object. Build incrementally with [mem]:
      {v
      obj (fun a b c -> { a; b; c })
      |> mem "a" codec_a ~enc:...
      |> mem "b" codec_b ~enc:...
      |> mem "c" codec_c ~enc:...
      |> finish
      v} *)

  val obj' : ?kind:string -> ?doc:string -> (unit -> 'dec) -> ('o, 'dec) map
  (** [obj' f] is like [obj] but [f] is a thunk for side-effecting decoders. *)

  val mem :
    ?doc:string ->
    ?dec_absent:'a ->
    ?enc:('o -> 'a) ->
    ?enc_omit:('a -> bool) ->
    string -> 'a codec -> ('o, 'a -> 'dec) map -> ('o, 'dec) map
  (** [mem name codec m] adds a member to the table builder.

      @param name The TOML key name.
      @param codec The codec for the member's value.
      @param doc Documentation string.
      @param dec_absent Default value if absent (makes member optional).
      @param enc Extractor function for encoding.
      @param enc_omit Predicate; if [true], omit member during encoding. *)

  val opt_mem :
    ?doc:string ->
    ?enc:('o -> 'a option) ->
    string -> 'a codec -> ('o, 'a option -> 'dec) map -> ('o, 'dec) map
  (** [opt_mem name codec m] adds an optional member.
      Absent members decode as [None]; [None] values are omitted on encode. *)

  (** {2 Unknown Member Handling} *)

  val skip_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [skip_unknown m] ignores unknown members (the default). *)

  val error_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [error_unknown m] raises an error on unknown members. *)

  (** Collection of unknown members. *)
  module Mems : sig
    type 'a codec = 'a t

    type ('mems, 'a) enc = {
      fold : 'acc. ('acc -> string -> 'a -> 'acc) -> 'acc -> 'mems -> 'acc
    }

    type ('mems, 'a, 'builder) map

    val map :
      ?kind:string -> ?doc:string ->
      ?dec_empty:(unit -> 'builder) ->
      ?dec_add:(string -> 'a -> 'builder -> 'builder) ->
      ?dec_finish:('builder -> 'mems) ->
      ?enc:('mems, 'a) enc ->
      'a codec -> ('mems, 'a, 'builder) map

    val string_map : ?kind:string -> ?doc:string ->
      'a codec -> ('a Map.Make(String).t, 'a, (string * 'a) list) map
    (** [string_map codec] collects unknown members into a [StringMap]. *)

    val assoc : ?kind:string -> ?doc:string ->
      'a codec -> ((string * 'a) list, 'a, (string * 'a) list) map
    (** [assoc codec] collects unknown members into an association list. *)
  end

  val keep_unknown :
    ?enc:('o -> 'mems) ->
    ('mems, 'a, 'builder) Mems.map ->
    ('o, 'mems -> 'dec) map -> ('o, 'dec) map
  (** [keep_unknown mems m] collects unknown members.

      Unknown members are decoded using [mems] and passed to the decoder.
      If [enc] is provided, those members are included during encoding. *)

  val finish : ('o, 'o) map -> 'o codec
  (** [finish m] completes the table codec.
      @raise Invalid_argument if member names are duplicated. *)

  val inline : ('o, 'o) map -> 'o codec
  (** [inline m] is like [finish] but marks the table for inline encoding. *)
end

val array_of_tables : ?kind:string -> ?doc:string -> 'a t -> 'a list t
(** [array_of_tables c] decodes a
    {{:https://toml.io/en/v1.1.0#array-of-tables}TOML array of tables}.
    This corresponds to TOML's [[\[\[name\]\]]] syntax for defining
    arrays of table elements. *)

(** {1 Generic Value Codecs} *)

val value : Toml.t t
(** [value] passes TOML values through unchanged.
    Useful for preserving parts of a document without interpretation. *)

val value_mems : (string * Toml.t) list t
(** [value_mems] decodes a {{:https://toml.io/en/v1.1.0#table}table}
    as raw key-value pairs. *)

val any :
  ?kind:string -> ?doc:string ->
  ?dec_string:'a t -> ?dec_int:'a t -> ?dec_float:'a t -> ?dec_bool:'a t ->
  ?dec_datetime:'a t -> ?dec_array:'a t -> ?dec_table:'a t ->
  ?enc:('a -> 'a t) ->
  unit -> 'a t
(** [any ()] creates a codec that handles any TOML type.
    Provide decoders for each type you want to support.
    The [enc] function should return the appropriate codec for encoding. *)

(** {1:codec_ops Encoding and Decoding}

    Functions for converting between OCaml values and TOML values.
    For I/O operations (parsing strings, writing to files), see
    {!Tomlt_bytesrw}. *)

val decode : 'a t -> Toml.t -> ('a, Toml.Error.t) result
(** [decode c v] decodes TOML value [v] using codec [c]. *)

val decode_exn : 'a t -> Toml.t -> 'a
(** [decode_exn c v] is like [decode] but raises on error.
    @raise Toml.Error.Error on decode failure. *)

val encode : 'a t -> 'a -> Toml.t
(** [encode c v] encodes OCaml value [v] to TOML using codec [c]. *)

(** {1 Re-exported Modules} *)

module Toml = Toml
(** The raw TOML value module. Use for low-level TOML manipulation. *)

module Error = Toml.Error
(** Error types from the TOML parser. *)

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative {{:https://toml.io/en/v1.1.0}TOML 1.1} codecs.

    Toml provides a bidirectional codec system for TOML files, based on the
    {e finally tagged} approach from
    {{:https://github.com/dbuenzli/jsont/blob/main/paper/soup.tex}An alphabet
     for your data soups} by Daniel Bünzli.

    {2:quickstart Quick start}

    Define a codec for your OCaml types:
    {v
    type config = { host : string; port : int; debug : bool }

    let config_codec =
      Toml.Codec.(Table.(
        obj (fun host port debug -> { host; port; debug })
        |> mem "host" string ~enc:(fun c -> c.host)
        |> mem "port" int ~enc:(fun c -> c.port)
        |> mem "debug" bool ~enc:(fun c -> c.debug) ~dec_absent:false
        |> finish
      ))
    v}

    Decode from a string:
    {v
    let () =
      match Toml.of_string config_codec {|
        host = "localhost"
        port = 8080
      |} with
      | Ok config -> Printf.printf "Host: %s\n" config.host
      | Error e -> prerr_endline (Toml.Error.to_string e)
    v}

    {2:pattern Codec pattern}

    Each codec ['a codec] (= ['a Codec.t]) defines a bidirectional mapping
    between TOML and OCaml values. Codecs compose through combinators in
    {!Codec} to build complex types from simple primitives. *)

(** {1:preliminaries Preliminaries} *)

type 'a fmt = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

(** {1:errors Errors}

    Structural decoding errors with source context. *)

module Loc = Loc
module Meta = Loc.Meta
module Path = Loc.Path

module Error = Error
(** TOML error facade.

    Extends {!Loc.Error.type-kind} with TOML-specific typed kinds
    ({!Error.Lexer}, {!Error.Number}, {!Error.Datetime}, {!Error.Semantic},
    {!Error.Syntax}, {!Error.Encode}) and exports the shared {!Loc.module-Error}
    verbs. *)

exception Error of Error.t
(** Raised by decoders on structured errors. Alias of {!Loc.exception-Error}:
    [match exn with Toml.Error _ | Loc.Error _ -> ...] are equivalent. *)

exception Invalid_utf8_encode of int
(** Raised by the encoder when asked to write an OCaml string that contains
    malformed UTF-8. Carries the byte offset within the offending string. *)

(** {1:datetime Structured datetime types}

    TOML 1.1 supports four datetime formats:

    - {b {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetime}}:
      [1979-05-27T07:32:00Z] or [1979-05-27T07:32:00-07:00]
    - {b {{:https://toml.io/en/v1.1.0#local-date-time}Local datetime}}:
      [1979-05-27T07:32:00] (no timezone)
    - {b {{:https://toml.io/en/v1.1.0#local-date}Local date}}: [1979-05-27]
    - {b {{:https://toml.io/en/v1.1.0#local-time}Local time}}: [07:32:00] or
      [07:32:00.999999] *)

(** Timezone offsets for
    {{:https://toml.io/en/v1.1.0#offset-date-time}TOML offset datetimes}. *)
module Tz : sig
  (** The type for timezone offsets. *)
  type t =
    | UTC  (** UTC, written [Z]. *)
    | Offset of { hours : int; minutes : int }  (** Fixed offset from UTC. *)

  val utc : t
  (** [utc] is {!constructor-UTC}. *)

  val offset : hours:int -> minutes:int -> t
  (** [offset ~hours ~minutes] is [Offset { hours; minutes }]. A negative offset
      carries its sign on [hours]. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val compare : t -> t -> int
  (** [compare a b] is a total order, with {!constructor-UTC} before every
      offset. *)

  val to_string : t -> string
  (** [to_string t] is [t] in TOML syntax: ["Z"] for UTC, and ["+HH:MM"] or
      ["-HH:MM"] for an offset. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] prints {!to_string}. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the timezone [s] denotes in TOML syntax, accepting ["Z"]
      and ["z"] for UTC. This is [Error msg] if [s] is empty, too short for the
      form it starts with (six characters for ["+HH:MM"], five for ["HH:MM"]),
      or does not parse as an offset. *)
end

(** {{:https://toml.io/en/v1.1.0#local-date}Local dates}. *)
module Date : sig
  type t = { year : int; month : int; day : int }
  (** The type for local dates. *)

  val v : year:int -> month:int -> day:int -> t
  (** [v ~year ~month ~day] is [{ year; month; day }]. The fields are not
      checked against the calendar. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val compare : t -> t -> int
  (** [compare a b] is a total order, by year, then month, then day. *)

  val to_string : t -> string
  (** [to_string t] is [t] in TOML syntax, ["YYYY-MM-DD"]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] prints {!to_string}. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the date the first ten characters of [s] denote in TOML
      syntax. This is [Error msg] if [s] is shorter than that or its fields are
      not integers. *)
end

(** {{:https://toml.io/en/v1.1.0#local-time}Local times}. *)
module Time : sig
  type t = { hour : int; minute : int; second : int; frac : float }
  (** The type for local times. {!field-frac} is the fractional second, at least
      [0.] and less than [1.]. *)

  val v : hour:int -> minute:int -> second:int -> ?frac:float -> unit -> t
  (** [v ~hour ~minute ~second ~frac ()] is [{ hour; minute; second; frac }],
      with [frac] defaulting to [0.]. The fields are not checked against the
      clock. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val compare : t -> t -> int
  (** [compare a b] is a total order, by hour, then minute, then second, then
      fractional second. *)

  val to_string : t -> string
  (** [to_string t] is [t] in TOML syntax: ["HH:MM:SS"], or ["HH:MM:SS.fff"]
      with trailing zeros stripped when the fractional second is not [0.]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] prints {!to_string}. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the time [s] denotes in TOML syntax, its optional
      fractional part included. This is [Error msg] if [s] is shorter than eight
      characters or its fields are not numbers. *)
end

(** {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetimes}. *)
module Datetime : sig
  type t = { date : Date.t; time : Time.t; tz : Tz.t }
  (** The type for offset datetimes. *)

  val v : date:Date.t -> time:Time.t -> tz:Tz.t -> t
  (** [v ~date ~time ~tz] is [{ date; time; tz }]. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val compare : t -> t -> int
  (** [compare a b] is a total order, by date, then time, then timezone. *)

  val to_string : t -> string
  (** [to_string t] is [t] in TOML syntax: the date, ["T"], the time, then the
      timezone. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] prints {!to_string}. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the offset datetime [s] denotes in TOML syntax. This is
      [Error msg] if [s] has no date/time separator, or if its date, time or
      timezone does not parse. *)
end

(** {{:https://toml.io/en/v1.1.0#local-date-time}Local datetimes}. *)
module Datetime_local : sig
  type t = { date : Date.t; time : Time.t }
  (** The type for local datetimes, which carry no timezone. *)

  val v : date:Date.t -> time:Time.t -> t
  (** [v ~date ~time] is [{ date; time }]. *)

  val equal : t -> t -> bool
  (** [equal a b] is structural equality. *)

  val compare : t -> t -> int
  (** [compare a b] is a total order, by date, then time. *)

  val to_string : t -> string
  (** [to_string t] is [t] in TOML syntax: the date, ["T"], then the time. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] prints {!to_string}. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is the local datetime [s] denotes in TOML syntax. This is
      [Error msg] if [s] has no date/time separator, or if its date or time does
      not parse. *)
end

module Sort = Sort
(** Sorts of TOML values ({!Sort.String}, {!Sort.Int}, {!Sort.Float},
    {!Sort.Bool}, {!Sort.Datetime}, {!Sort.Datetime_local}, {!Sort.Date},
    {!Sort.Time}, {!Sort.Array}, {!Sort.Table}). Labels used in structured error
    contexts and {!Loc.Path} frames. *)

(** {1:codec Codec} *)

module Codec = Codec
(** Codec combinators. See {!module:Codec} for the full combinator API
    ([Toml.Codec.bool], [Toml.Codec.string], [Toml.Codec.Table.obj], etc.). *)

type 'a codec = 'a Codec.t
(** The type for TOML codecs. See {!Codec.t}. *)

(** {1:value Value module (with identity codec)}

    Re-exports {!module:Value} and adds the identity codec {!Value.codec}. *)
module Value : sig
  include module type of Value

  val codec : t Codec.t
  (** [codec] is the identity codec: decodes any TOML value unchanged and
      encodes it unchanged. *)
end

type t = Value.t
(** The type for TOML values. See {!Value.t}. *)

val pp : Format.formatter -> t -> unit
(** [pp] is {!Value.val-pp}. *)

module Cursor = Cursor
(** Zipper over {!Value.t} with dotted-key pointers. See {!module:Cursor}. *)

(** {1:codec_ops Decoding and Encoding}

    Decoding accepts two limit parameters that bound resource usage on untrusted
    input:

    - [max_depth] caps the nesting depth of TOML arrays and tables (default:
      [100]). Each segment of a dotted key nests a table, so [a.b.c = 1] counts
      as two.
    - [max_nodes] caps the total number of decoded atoms, arrays, and tables
      (default: [10_000_000]). *)

val decode :
  ?max_depth:int ->
  ?max_nodes:int ->
  'a codec ->
  Value.t ->
  ('a, Error.t) result
(** [decode ~max_depth ~max_nodes c v] is the OCaml value [c] decodes from the
    TOML value [v], within the limits above. *)

val decode_exn : ?max_depth:int -> ?max_nodes:int -> 'a codec -> Value.t -> 'a
(** [decode_exn ~max_depth ~max_nodes c v] is [decode ~max_depth ~max_nodes c v]
    but raises on failure.

    @raise Toml.exception-Error on decode failure. *)

val encode : 'a codec -> 'a -> Value.t
(** [encode c v] is the TOML value [c] encodes the OCaml value [v] into. *)

val of_reader :
  ?max_depth:int ->
  ?max_nodes:int ->
  'a codec ->
  Bytesrw.Bytes.Reader.t ->
  ('a, Error.t) result
(** [of_reader ~max_depth ~max_nodes c r] is the OCaml value [c] decodes from
    the TOML text read on [r], within the limits above. A caller wanting a
    string error can write [Result.map_error Error.to_string]. *)

val of_reader_exn :
  ?max_depth:int -> ?max_nodes:int -> 'a codec -> Bytesrw.Bytes.Reader.t -> 'a
(** [of_reader_exn ~max_depth ~max_nodes c r] is
    [of_reader ~max_depth ~max_nodes c r] but raises on failure.

    @raise Toml.exception-Error on decode failure. *)

val of_string :
  ?max_depth:int -> ?max_nodes:int -> 'a codec -> string -> ('a, Error.t) result
(** [of_string ~max_depth ~max_nodes c s] is the OCaml value [c] decodes from
    the TOML text [s], within the limits above. *)

val of_string_exn : ?max_depth:int -> ?max_nodes:int -> 'a codec -> string -> 'a
(** [of_string_exn ~max_depth ~max_nodes c s] is
    [of_string ~max_depth ~max_nodes c s] but raises on failure.

    @raise Toml.exception-Error on decode failure. *)

val to_writer :
  ?indent:int ->
  ?preserve:bool ->
  'a codec ->
  'a ->
  Bytesrw.Bytes.Writer.t ->
  unit
(** [to_writer ~indent ~preserve c v w] encodes [v] with [c] and writes the TOML
    text to [w].
    - [indent] lays the value out from scratch, and never copies. Defaults to
      [None] (compact: inline tables and arrays, no trailing newlines; the
      shortest valid TOML for the given value). [Some _] selects the sectioned
      pretty-printed form with [[section]] headers for nested tables and
      [[[array-of-tables]]] headers for arrays of tables. The integer would
      control per-level indentation of section contents; TOML sections are
      conventionally flush-left, so the encoder currently ignores [n] and emits
      the canonical sectioned form for any [Some _].
    - [preserve] defaults to [false]. When [true], a parsed document is written
      back as the bytes it was read from. Every node records the byte range it
      came from and the document those bytes belong to ({!Meta.text}), so
      writing is copying: what the caller has not touched is the source text
      under its own range, and none of it is re-derived from the value it
      denotes.

    That is what carries a TOML file's layout across, and a TOML file's layout
    is more than its whitespace. Comments survive, and so do the column an
    author aligned [=] to and the blank line between two tables. So does the
    choice among the three ways of writing one table: a [[section]] header, a
    dotted key and an inline table all parse to the same {!Value.t}, and each
    comes back as itself. So does the spelling of every scalar, which is the
    part no encoder could otherwise guess: [0x1F], [31] and [1_000] are one
    integer, ['C:\dir'] and ["C:\\dir"] are one string, and
    [1979-05-27 07:32:00z] and [1979-05-27T07:32:00Z] are one datetime, with
    nothing in the value to say which of them the file wrote.

    What the caller put there is written afresh. A leaf replaced through
    {!Cursor.set} takes the place of the bytes it stands over, and its
    surroundings -- the key, the spacing, the comment ending its line -- are
    still copied, so an edit changes the one thing it edited. Anything that
    parts the value from the document has nowhere to be spliced in: a member
    added or removed, a key renamed, a subtree built in OCaml. The whole value
    is then written in the [?indent] sectioned form, which is also what a value
    that was never parsed gets.

    A member removed is the one of those the value keeps no trace of: what is
    left still carries its own bytes and the table still carries the range they
    came from, so copying that range would write the removed member back out of
    the document. Copying is therefore allowed only where the members still
    account for the container's range, each of them where the document put it
    and with nothing but layout between them. A table reopened after another
    table fails that with nothing edited at all: written [a], then [b], then
    [a.c], the members of [a] are not one stretch of the file and the range of
    [a] holds the whole of [b]. Such a document is written afresh too.

    A node built with a parsed node's [?meta] claims that node's bytes, layout
    and all, since that is what the metadata says. Give it {!Meta.clear_text} of
    that metadata to keep where it was and not what was there.

    @raise Invalid_utf8_encode
      if a string or table key in the encoded value is not well-formed UTF-8,
      carrying the byte offset of the first malformed sequence. *)

val to_string : ?indent:int -> ?preserve:bool -> 'a codec -> 'a -> string
(** [to_string ~indent ~preserve c v] is the TOML text {!to_writer} would write
    for [v], with the same meaning for [indent] and [preserve].

    @raise Invalid_utf8_encode as {!to_writer} does. *)

(** {1:parser Low-level parser}

    Low-level TOML parser and encoder working on {!Value.t}. Exposed for sibling
    libraries that need file / flow I/O adapters; end users should prefer
    {!of_string}, {!of_reader}, {!to_string}, {!to_writer}. *)

module Parser = Parser

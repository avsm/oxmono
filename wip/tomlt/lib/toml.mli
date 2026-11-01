(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** {{:https://toml.io/en/v1.1.0}TOML 1.1} value types.

    This module provides the core TOML value type and operations for
    constructing, accessing, and manipulating TOML data. For parsing and
    encoding, see {!Tomlt_bytesrw}. For codec-based bidirectional encoding,
    see {!Tomlt}.

    {2 Quick Start}

    Create TOML values programmatically:
    {[
      let config = Toml.(table [
        "title", string "My App";
        "database", table [
          "host", string "localhost";
          "ports", array [int 5432L; int 5433L]
        ]
      ])
    ]}

    Access values:
    {[
      let host = Toml.to_string (Toml.find "host" (Toml.find "database" config))
      let ports = Toml.to_array (Toml.find "ports" (Toml.find "database" config))
      let port = Toml.to_int (List.hd ports)
    ]}

    See the {{!page-cookbook}cookbook} for common patterns and recipes.

    {2 Module Overview}

    - {!section:types} - TOML value representation
    - {!section:construct} - Value constructors
    - {!section:access} - Value accessors and type conversion
    - {!section:navigate} - Table navigation
    - {!section:ptime} - Ptime datetime conversions
    - {!section:pp} - Pretty printing
    - {!module:Error} - Structured error types *)

(** {1:types TOML Value Types} *)

(** The type of TOML values.

    TOML supports the following value types:
    - {{:https://toml.io/en/v1.1.0#string}Strings} (UTF-8 encoded)
    - {{:https://toml.io/en/v1.1.0#integer}Integers} (64-bit signed)
    - {{:https://toml.io/en/v1.1.0#float}Floats} (IEEE 754 double precision)
    - {{:https://toml.io/en/v1.1.0#boolean}Booleans}
    - {{:https://toml.io/en/v1.1.0#offset-date-time}Offset date-times} (RFC 3339 with timezone)
    - {{:https://toml.io/en/v1.1.0#local-date-time}Local date-times} (no timezone)
    - {{:https://toml.io/en/v1.1.0#local-date}Local dates}
    - {{:https://toml.io/en/v1.1.0#local-time}Local times}
    - {{:https://toml.io/en/v1.1.0#array}Arrays} (heterogeneous in TOML 1.1)
    - {{:https://toml.io/en/v1.1.0#table}Tables} (string-keyed maps) *)
type t =
  | String of string
      (** {{:https://toml.io/en/v1.1.0#string}TOML string}. *)
  | Int of int64
      (** {{:https://toml.io/en/v1.1.0#integer}TOML integer}. *)
  | Float of float
      (** {{:https://toml.io/en/v1.1.0#float}TOML float}. *)
  | Bool of bool
      (** {{:https://toml.io/en/v1.1.0#boolean}TOML boolean}. *)
  | Datetime of string
      (** {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetime},
          e.g. [1979-05-27T07:32:00Z]. *)
  | Datetime_local of string
      (** {{:https://toml.io/en/v1.1.0#local-date-time}Local datetime},
          e.g. [1979-05-27T07:32:00]. *)
  | Date_local of string
      (** {{:https://toml.io/en/v1.1.0#local-date}Local date},
          e.g. [1979-05-27]. *)
  | Time_local of string
      (** {{:https://toml.io/en/v1.1.0#local-time}Local time},
          e.g. [07:32:00]. *)
  | Array of t list
      (** {{:https://toml.io/en/v1.1.0#array}TOML array}. *)
  | Table of (string * t) list
      (** {{:https://toml.io/en/v1.1.0#table}TOML table}. *)
(** A TOML value. Tables preserve key insertion order. *)

(** {1:construct Value Constructors}

    These functions create TOML values. Use them to build TOML documents
    programmatically. *)

val string : string -> t
(** [string s] creates a {{:https://toml.io/en/v1.1.0#string}TOML string} value. *)

val int : int64 -> t
(** [int i] creates a {{:https://toml.io/en/v1.1.0#integer}TOML integer} value. *)

val int_of_int : int -> t
(** [int_of_int i] creates a {{:https://toml.io/en/v1.1.0#integer}TOML integer}
    value from an [int]. *)

val float : float -> t
(** [float f] creates a {{:https://toml.io/en/v1.1.0#float}TOML float} value. *)

val bool : bool -> t
(** [bool b] creates a {{:https://toml.io/en/v1.1.0#boolean}TOML boolean} value. *)

val array : t list -> t
(** [array vs] creates a {{:https://toml.io/en/v1.1.0#array}TOML array} value
    from a list of values. TOML 1.1 allows heterogeneous arrays. *)

val table : (string * t) list -> t
(** [table pairs] creates a {{:https://toml.io/en/v1.1.0#table}TOML table} value
    from key-value pairs. Keys should be unique; later bindings shadow earlier
    ones during lookup. *)

val datetime : string -> t
(** [datetime s] creates an {{:https://toml.io/en/v1.1.0#offset-date-time}offset
    datetime} value. The string should be in RFC 3339 format with timezone,
    e.g. ["1979-05-27T07:32:00Z"] or ["1979-05-27T07:32:00-07:00"]. *)

val datetime_local : string -> t
(** [datetime_local s] creates a {{:https://toml.io/en/v1.1.0#local-date-time}local
    datetime} value (no timezone). E.g. ["1979-05-27T07:32:00"]. *)

val date_local : string -> t
(** [date_local s] creates a {{:https://toml.io/en/v1.1.0#local-date}local date}
    value. E.g. ["1979-05-27"]. *)

val time_local : string -> t
(** [time_local s] creates a {{:https://toml.io/en/v1.1.0#local-time}local time}
    value. E.g. ["07:32:00"] or ["07:32:00.999"]. *)

(** {1:access Value Accessors}

    These functions extract OCaml values from TOML values.
    They raise [Invalid_argument] if the value is not of the expected type. *)

val to_string : t -> string
(** [to_string t] returns the string if [t] is a [String].
    @raise Invalid_argument if [t] is not a string. *)

val to_string_opt : t -> string option
(** [to_string_opt t] returns [Some s] if [t] is [String s], [None] otherwise. *)

val to_int : t -> int64
(** [to_int t] returns the integer if [t] is an [Int].
    @raise Invalid_argument if [t] is not an integer. *)

val to_int_opt : t -> int64 option
(** [to_int_opt t] returns [Some i] if [t] is [Int i], [None] otherwise. *)

val to_float : t -> float
(** [to_float t] returns the float if [t] is a [Float].
    @raise Invalid_argument if [t] is not a float. *)

val to_float_opt : t -> float option
(** [to_float_opt t] returns [Some f] if [t] is [Float f], [None] otherwise. *)

val to_bool : t -> bool
(** [to_bool t] returns the boolean if [t] is a [Bool].
    @raise Invalid_argument if [t] is not a boolean. *)

val to_bool_opt : t -> bool option
(** [to_bool_opt t] returns [Some b] if [t] is [Bool b], [None] otherwise. *)

val to_array : t -> t list
(** [to_array t] returns the list if [t] is a {{:https://toml.io/en/v1.1.0#array}TOML array}.
    @raise Invalid_argument if [t] is not an array. *)

val to_array_opt : t -> t list option
(** [to_array_opt t] returns [Some vs] if [t] is [Array vs], [None] otherwise. *)

val to_table : t -> (string * t) list
(** [to_table t] returns the association list if [t] is a {{:https://toml.io/en/v1.1.0#table}TOML table}.
    @raise Invalid_argument if [t] is not a table. *)

val to_table_opt : t -> (string * t) list option
(** [to_table_opt t] returns [Some pairs] if [t] is [Table pairs], [None] otherwise. *)

val to_datetime : t -> string
(** [to_datetime t] returns the datetime string for any datetime type.
    @raise Invalid_argument if [t] is not a datetime variant. *)

val to_datetime_opt : t -> string option
(** [to_datetime_opt t] returns [Some s] if [t] is any datetime variant. *)

(** {2 Type Predicates} *)

val is_string : t -> bool
(** [is_string t] is [true] iff [t] is a [String]. *)

val is_int : t -> bool
(** [is_int t] is [true] iff [t] is an [Int]. *)

val is_float : t -> bool
(** [is_float t] is [true] iff [t] is a [Float]. *)

val is_bool : t -> bool
(** [is_bool t] is [true] iff [t] is a [Bool]. *)

val is_array : t -> bool
(** [is_array t] is [true] iff [t] is an [Array]. *)

val is_table : t -> bool
(** [is_table t] is [true] iff [t] is a [Table]. *)

val is_datetime : t -> bool
(** [is_datetime t] is [true] iff [t] is any datetime variant. *)

(** {1:navigate Table Navigation}

    Functions for navigating and querying {{:https://toml.io/en/v1.1.0#table}TOML tables}.
    See also {{:https://toml.io/en/v1.1.0#keys}dotted keys} for path-based access. *)

val find : string -> t -> t
(** [find key t] returns the value associated with [key] in table [t].
    @raise Invalid_argument if [t] is not a table.
    @raise Not_found if [key] is not in the table. *)

val find_opt : string -> t -> t option
(** [find_opt key t] returns [Some v] if [key] maps to [v] in table [t],
    or [None] if [key] is not bound or [t] is not a table. *)

val mem : string -> t -> bool
(** [mem key t] is [true] if [key] is bound in table [t], [false] otherwise.
    Returns [false] if [t] is not a table. *)

val keys : t -> string list
(** [keys t] returns all keys in table [t].
    @raise Invalid_argument if [t] is not a table. *)

val get : string list -> t -> t
(** [get path t] navigates through nested tables following [path].
    For example, [get ["server"; "port"] t] returns [t.server.port].
    @raise Invalid_argument if any intermediate value is not a table.
    @raise Not_found if any key in [path] is not found. *)

val get_opt : string list -> t -> t option
(** [get_opt path t] is like [get] but returns [None] on any error. *)

val ( .%{} ) : t -> string list -> t
(** [t.%{path}] is [get path t].

    Example: [config.%{["database"; "port"]}]

    @raise Invalid_argument if any intermediate value is not a table.
    @raise Not_found if any key in the path is not found. *)

val ( .%{}<- ) : t -> string list -> t -> t
(** [t.%{path} <- v] returns a new table with value [v] at [path].
    Creates intermediate tables as needed.

    Example: [config.%{["server"; "host"]} <- string "localhost"]

    @raise Invalid_argument if [t] is not a table or if an intermediate
    value exists but is not a table. *)

(** {1:ptime Ptime Conversions}

    Convert between {{:https://toml.io/en/v1.1.0#offset-date-time}TOML datetime}
    values and {{:https://erratique.ch/software/ptime}Ptime} timestamps. Offset
    datetimes can be converted to/from [Ptime.t] since they represent specific
    instants on the UTC timeline. Local datetime types cannot be converted to
    [Ptime.t] without assuming a timezone. *)

val datetime_of_ptime : ?tz_offset_s:int -> ?frac_s:int -> Ptime.t -> t
(** [datetime_of_ptime ?tz_offset_s ?frac_s ptime] creates an
    {{:https://toml.io/en/v1.1.0#offset-date-time}offset datetime} from a ptime
    timestamp.
    @param tz_offset_s Timezone offset in seconds (default: 0 for UTC).
      Use positive values for east of UTC (e.g., 3600 for +01:00),
      negative for west (e.g., -18000 for -05:00).
    @param frac_s Number of fractional second digits to include (default: 0).
      Clipped to range \[0, 12\]. *)

val to_ptime : t -> Ptime.t
(** [to_ptime t] converts an {{:https://toml.io/en/v1.1.0#offset-date-time}offset
    datetime} to a ptime timestamp.
    @raise Invalid_argument if [t] is not a [Datetime] or if the datetime
    string cannot be parsed. Local datetime types cannot be converted. *)

val to_ptime_opt : t -> Ptime.t option
(** [to_ptime_opt t] returns [Some ptime] if [t] is a [Datetime] that can be
    parsed, [None] otherwise. *)

val to_ptime_tz : t -> (Ptime.t * Ptime.tz_offset_s option) option
(** [to_ptime_tz t] returns the ptime timestamp and timezone offset for an
    offset datetime. The timezone is [Some 0] for [Z], [Some offset_s] for
    explicit offsets like [+05:30], or [None] for the unknown local offset
    convention ([-00:00]). Returns [None] if [t] is not a [Datetime]. *)

val date_of_ptime : ?tz_offset_s:int -> Ptime.t -> t
(** [date_of_ptime ?tz_offset_s ptime] creates a {{:https://toml.io/en/v1.1.0#local-date}local
    date} from a ptime timestamp. The date is extracted in the given timezone
    (default: UTC). *)

val to_date : t -> Ptime.date
(** [to_date t] converts a {{:https://toml.io/en/v1.1.0#local-date}local date}
    to a ptime date tuple [(year, month, day)].
    @raise Invalid_argument if [t] is not a [Date_local] or cannot be parsed. *)

val to_date_opt : t -> Ptime.date option
(** [to_date_opt t] returns [Some date] if [t] is a [Date_local], [None] otherwise. *)

(** {2:ptime_unified Unified Ptime Datetime}

    Unifies all {{:https://toml.io/en/v1.1.0#offset-date-time}TOML datetime}
    formats using {!Ptime} types, while preserving information about what was
    originally specified in the TOML source.

    For {{:https://toml.io/en/v1.1.0#local-date-time}local datetimes} without
    timezone, pass [~tz_offset_s] to specify the timezone to use for
    conversion. If not provided, UTC (0) is used as the default. *)

type ptime_datetime = [
  | `Datetime of Ptime.t * Ptime.tz_offset_s option
      (** {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetime} with
          full timezone info. The offset is [Some 0] for [Z], [Some n] for
          explicit offsets, or [None] for the unknown local offset convention
          ([-00:00]). *)
  | `Datetime_local of Ptime.t
      (** {{:https://toml.io/en/v1.1.0#local-date-time}Local datetime} converted
          to [Ptime.t] using current system timezone. Preserves that the source
          had no explicit timezone. *)
  | `Date of Ptime.date
      (** {{:https://toml.io/en/v1.1.0#local-date}Local date} as
          [(year, month, day)]. *)
  | `Time of int * int * int * int
      (** {{:https://toml.io/en/v1.1.0#local-time}Local time} as
          [(hour, minute, second, nanoseconds)]. Nanoseconds range from 0 to
          999_999_999. *)
]
(** Datetime representation using {!Ptime}.

    This variant indicates both the ptime value and the precision level
    of datetime information present in the original TOML source. *)

val to_ptime_datetime : ?tz_offset_s:int -> t -> ptime_datetime option
(** [to_ptime_datetime ?tz_offset_s t] converts any TOML datetime value to
    a unified ptime representation.

    @param tz_offset_s Timezone offset for local datetimes. This is the offset
      to assume when the TOML value is a local datetime without explicit
      timezone. Defaults to 0 (UTC) if not provided.
    @return [None] if [t] is not a datetime type, [Some pdt] otherwise.

    Examples:
    - [Datetime "1979-05-27T07:32:00Z"] →
      [Some (`Datetime (ptime, Some 0))]
    - [Datetime_local "1979-05-27T07:32:00"] →
      [Some (`Datetime_local ptime)] (converted using current tz)
    - [Date_local "1979-05-27"] →
      [Some (`Date (1979, 5, 27))]
    - [Time_local "07:32:00.123"] →
      [Some (`Time (7, 32, 0, 123_000_000))] *)

val ptime_datetime_to_toml : ptime_datetime -> t
(** [ptime_datetime_to_toml pdt] converts a unified ptime datetime back to
    a TOML value, preserving the appropriate datetime variant:
    - [`Datetime (t, tz)] → [Datetime s] with timezone
    - [`Datetime_local t] → [Datetime_local s]
    - [`Date d] → [Date_local s]
    - [`Time (h, m, s, ns)] → [Time_local s] *)

val pp_ptime_datetime : Format.formatter -> ptime_datetime -> unit
(** [pp_ptime_datetime fmt pdt] pretty-prints the unified datetime. *)

(** {1:pp Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints [t] in TOML inline format.
    Tables are printed as inline tables. *)

val pp_value : Format.formatter -> t -> unit
(** [pp_value fmt t] pretty-prints a single TOML value.
    Same as {!val:pp}. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality on TOML values.
    NaN floats are considered equal to each other. *)

val compare : t -> t -> int
(** [compare a b] is a total ordering on TOML values. *)

(** {1:errors Error Handling} *)

module Error = Toml_error
(** Structured error types for TOML parsing and encoding. *)

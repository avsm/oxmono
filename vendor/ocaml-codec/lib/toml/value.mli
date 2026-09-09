(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {{:https://toml.io/en/v1.1.0}TOML 1.1} value types.

    This module provides the core TOML value type and operations for
    constructing, accessing, and manipulating TOML data. Every node carries a
    {!Loc.Meta.t}: parsers fill it with source locations, programmatic
    constructors default it to {!Loc.Meta.none}.

    {2 Quick Start}

    Create TOML values programmatically:
    {[
    let config =
      Toml.Value.(
        table
          [
            ("title", string "My App");
            ( "database",
              table
                [
                  ("host", string "localhost");
                  ("ports", array [ int 5432L; int 5433L ]);
                ] );
          ])
    ]}

    Access values:
    {[
    let host = Toml.Value.(to_string (get "host" (get "database" config)))
    let ports = Toml.Value.(to_array (get "ports" (get "database" config)))
    let port = Toml.Value.to_int (List.hd ports)
    ]}

    See the {{!page-cookbook}cookbook} for common patterns and recipes.

    {2 Module Overview}

    - {!section:types} - TOML value representation
    - {!section:construct} - Value constructors
    - {!section:access} - Value accessors and type conversion
    - {!section:navigate} - Table navigation
    - {!section:ptime} - Ptime datetime conversions
    - {!section:pp} - Pretty printing *)

(** {1:types TOML Value Types} *)

module Meta = Loc.Meta
(** Node metadata (source location + surrounding whitespace). *)

type 'a node = 'a Loc.node
(** An AST node: data plus its metadata. *)

type name = string node
(** The type for TOML table keys. A key's {!Loc.Meta.t} points at its source
    position in the document (for bare, quoted, or dotted keys). *)

(** The type of TOML values.

    Every constructor wraps its payload in an {!node}: the parser fills the
    metadata with source positions; programmatic constructors default it to
    {!Loc.Meta.none}.

    TOML supports the following value types:
    - {{:https://toml.io/en/v1.1.0#string}Strings} (UTF-8 encoded)
    - {{:https://toml.io/en/v1.1.0#integer}Integers} (64-bit signed)
    - {{:https://toml.io/en/v1.1.0#float}Floats} (IEEE 754 double precision)
    - {{:https://toml.io/en/v1.1.0#boolean}Booleans}
    - {{:https://toml.io/en/v1.1.0#offset-date-time}Offset date-times} (RFC 3339
      with timezone)
    - {{:https://toml.io/en/v1.1.0#local-date-time}Local date-times} (no
      timezone)
    - {{:https://toml.io/en/v1.1.0#local-date}Local dates}
    - {{:https://toml.io/en/v1.1.0#local-time}Local times}
    - {{:https://toml.io/en/v1.1.0#array}Arrays} (heterogeneous in TOML 1.1)
    - {{:https://toml.io/en/v1.1.0#table}Tables} (string-keyed maps) *)
type t =
  | String of string node
      (** {{:https://toml.io/en/v1.1.0#string}TOML string}. *)
  | Int of int64 node
      (** {{:https://toml.io/en/v1.1.0#integer}TOML integer}. *)
  | Float of float node  (** {{:https://toml.io/en/v1.1.0#float}TOML float}. *)
  | Bool of bool node
      (** {{:https://toml.io/en/v1.1.0#boolean}TOML boolean}. *)
  | Datetime of string node
      (** {{:https://toml.io/en/v1.1.0#offset-date-time}Offset datetime}, e.g.
          [1979-05-27T07:32:00Z]. *)
  | Datetime_local of string node
      (** {{:https://toml.io/en/v1.1.0#local-date-time}Local datetime}, e.g.
          [1979-05-27T07:32:00]. *)
  | Date_local of string node
      (** {{:https://toml.io/en/v1.1.0#local-date}Local date}, e.g.
          [1979-05-27]. *)
  | Time_local of string node
      (** {{:https://toml.io/en/v1.1.0#local-time}Local time}, e.g. [07:32:00].
      *)
  | Array of t list node  (** {{:https://toml.io/en/v1.1.0#array}TOML array}. *)
  | Table of (name * t) list node
      (** {{:https://toml.io/en/v1.1.0#table}TOML table}. Preserves key
          insertion order. Each key's metadata points at its source position. *)

val meta : t -> Meta.t
(** [meta v] is the source metadata of [v]'s top-level constructor. *)

val sort : t -> Sort.t
(** [sort v] is the TOML sort of [v]. *)

(** {1:construct Value Constructors}

    These functions create TOML values. Each accepts an optional [?meta]
    (defaults to {!Loc.Meta.none}). *)

val string : ?meta:Meta.t -> string -> t
(** [string s] creates a {{:https://toml.io/en/v1.1.0#string}TOML string} value.
*)

val int : ?meta:Meta.t -> int64 -> t
(** [int i] creates a {{:https://toml.io/en/v1.1.0#integer}TOML integer} value.
*)

val int_of_int : ?meta:Meta.t -> int -> t
(** [int_of_int i] creates a {{:https://toml.io/en/v1.1.0#integer}TOML integer}
    value from an {!val-int}. *)

val float : ?meta:Meta.t -> float -> t
(** [float f] creates a {{:https://toml.io/en/v1.1.0#float}TOML float} value. *)

val bool : ?meta:Meta.t -> bool -> t
(** [bool b] creates a {{:https://toml.io/en/v1.1.0#boolean}TOML boolean} value.
*)

val array : ?meta:Meta.t -> t list -> t
(** [array vs] creates a {{:https://toml.io/en/v1.1.0#array}TOML array} value
    from a list of values. TOML 1.1 allows heterogeneous arrays. *)

val table : ?meta:Meta.t -> (string * t) list -> t
(** [table pairs] creates a {{:https://toml.io/en/v1.1.0#table}TOML table} value
    from key-value pairs. Keys are wrapped with {!Loc.Meta.none}; use the
    underlying {!constructor-Table} constructor directly to preserve key
    metadata. *)

val datetime : ?meta:Meta.t -> string -> t
(** [datetime s] creates an
    {{:https://toml.io/en/v1.1.0#offset-date-time}offset datetime} value. *)

val datetime_local : ?meta:Meta.t -> string -> t
(** [datetime_local s] creates a
    {{:https://toml.io/en/v1.1.0#local-date-time}local datetime} value. *)

val date_local : ?meta:Meta.t -> string -> t
(** [date_local s] creates a {{:https://toml.io/en/v1.1.0#local-date}local date}
    value. *)

val time_local : ?meta:Meta.t -> string -> t
(** [time_local s] creates a {{:https://toml.io/en/v1.1.0#local-time}local time}
    value. *)

(** {1:access Value Accessors}

    These functions extract OCaml values from TOML values, discarding the
    surrounding metadata. They raise [Invalid_argument] if the value is not of
    the expected type. *)

val to_string : t -> string
(** [to_string t] returns the string if [t] is a {!constructor-String}.
    @raise Invalid_argument if [t] is not a string. *)

val to_string_opt : t -> string option
(** [to_string_opt t] returns [Some s] if [t] is [String s], [None] otherwise.
*)

val to_int : t -> int64
(** [to_int t] returns the integer if [t] is an {!constructor-Int}.
    @raise Invalid_argument if [t] is not an integer. *)

val to_int_opt : t -> int64 option
(** [to_int_opt t] returns [Some i] if [t] is [Int i], [None] otherwise. *)

val to_float : t -> float
(** [to_float t] returns the float if [t] is a {!constructor-Float}.
    @raise Invalid_argument if [t] is not a float. *)

val to_float_opt : t -> float option
(** [to_float_opt t] returns [Some f] if [t] is [Float f], [None] otherwise. *)

val to_bool : t -> bool
(** [to_bool t] returns the boolean if [t] is a {!constructor-Bool}.
    @raise Invalid_argument if [t] is not a boolean. *)

val to_bool_opt : t -> bool option
(** [to_bool_opt t] returns [Some b] if [t] is [Bool b], [None] otherwise. *)

val to_array : t -> t list
(** [to_array t] returns the list if [t] is a
    {{:https://toml.io/en/v1.1.0#array}TOML array}.
    @raise Invalid_argument if [t] is not an array. *)

val to_array_opt : t -> t list option
(** [to_array_opt t] returns [Some vs] if [t] is [Array vs], [None] otherwise.
*)

val to_table : t -> (string * t) list
(** [to_table t] returns the table's key-value pairs as a plain association list
    (key metadata is discarded).
    @raise Invalid_argument if [t] is not a table. *)

val to_table_opt : t -> (string * t) list option
(** [to_table_opt t] returns [Some pairs] if [t] is a {!constructor-Table},
    [None] otherwise. *)

val to_datetime : t -> string
(** [to_datetime t] returns the datetime string for any datetime type.
    @raise Invalid_argument if [t] is not a datetime variant. *)

val to_datetime_opt : t -> string option
(** [to_datetime_opt t] returns [Some s] if [t] is any datetime variant. *)

(** {2 Type Predicates} *)

val is_string : t -> bool
(** [is_string t] is [true] iff [t] is a {!constructor-String}. *)

val is_int : t -> bool
(** [is_int t] is [true] iff [t] is an {!constructor-Int}. *)

val is_float : t -> bool
(** [is_float t] is [true] iff [t] is a {!constructor-Float}. *)

val is_bool : t -> bool
(** [is_bool t] is [true] iff [t] is a {!constructor-Bool}. *)

val is_array : t -> bool
(** [is_array t] is [true] iff [t] is an {!constructor-Array}. *)

val is_table : t -> bool
(** [is_table t] is [true] iff [t] is a {!constructor-Table}. *)

val is_datetime : t -> bool
(** [is_datetime t] is [true] iff [t] is any datetime variant. *)

(** {1:navigate Table Navigation}

    Functions for navigating and querying
    {{:https://toml.io/en/v1.1.0#table}TOML tables}. See also
    {{:https://toml.io/en/v1.1.0#keys}dotted keys} for path-based access. *)

val get : string -> t -> t
(** [get key t] returns the value associated with [key] in table [t].
    @raise Invalid_argument if [t] is not a table.
    @raise Not_found if [key] is not in the table. *)

val opt : string -> t -> t option
(** [opt key t] returns [Some v] if [key] maps to [v] in table [t], or [None] if
    [key] is not bound or [t] is not a table. *)

val mem : string -> t -> bool
(** [mem key t] is [true] if [key] is bound in table [t], [false] otherwise.
    Returns [false] if [t] is not a table. *)

val keys : t -> string list
(** [keys t] returns all keys in table [t] as plain strings.
    @raise Invalid_argument if [t] is not a table. *)

val ( .%{} ) : t -> string list -> t
(** [t .%{path}] navigates nested tables following [path].

    Example: [config.%{["database"; "port"]}]

    @raise Invalid_argument if any intermediate value is not a table.
    @raise Not_found if any key in the path is not found. *)

val path_opt : string list -> t -> t option
(** [path_opt path t] is the value at [path] in [t], or [None] if any key is
    missing or an intermediate value is not a table. Total counterpart to
    [( .%{} )]. *)

val ( .%{}<- ) : t -> string list -> t -> t
(** [t .%{path} <- v] sets the value [v] at [path], returning a new table.
    Creates intermediate tables as needed.

    @raise Invalid_argument
      if {!type-t} is not a table or if an intermediate value exists but is not
      a table. *)

(** {1:ptime Ptime Conversions}

    Convert between {{:https://toml.io/en/v1.1.0#offset-date-time}TOML datetime}
    values and {{:https://erratique.ch/software/ptime}Ptime} timestamps. *)

val datetime_of_ptime : ?tz_offset_s:int -> ?frac_s:int -> Ptime.t -> t
(** [datetime_of_ptime ?tz_offset_s ?frac_s ptime] creates an
    {{:https://toml.io/en/v1.1.0#offset-date-time}offset datetime} from a ptime
    timestamp. *)

val to_ptime : t -> Ptime.t
(** [to_ptime t] converts an
    {{:https://toml.io/en/v1.1.0#offset-date-time}offset datetime} to a ptime
    timestamp.
    @raise Invalid_argument
      if [t] is not a {!constructor-Datetime} or if the datetime string cannot
      be parsed. *)

val to_ptime_opt : t -> Ptime.t option
(** [to_ptime_opt t] returns [Some ptime] if [t] is a {!constructor-Datetime}
    that can be parsed, [None] otherwise. *)

val to_ptime_tz : t -> (Ptime.t * Ptime.tz_offset_s option) option
(** [to_ptime_tz t] returns the ptime timestamp and timezone offset for an
    offset datetime. *)

val date_of_ptime : ?tz_offset_s:int -> Ptime.t -> t
(** [date_of_ptime ?tz_offset_s ptime] creates a
    {{:https://toml.io/en/v1.1.0#local-date}local date} from a ptime timestamp.
*)

val to_date : t -> Ptime.date
(** [to_date t] converts a {{:https://toml.io/en/v1.1.0#local-date}local date}
    to a ptime date tuple [(year, month, day)].
    @raise Invalid_argument
      if [t] is not a {!constructor-Date_local} or cannot be parsed. *)

val to_date_opt : t -> Ptime.date option
(** [to_date_opt t] returns [Some date] if [t] is a {!constructor-Date_local},
    [None] otherwise. *)

(** {2:ptime_unified Unified Ptime Datetime}

    Unifies all {{:https://toml.io/en/v1.1.0#offset-date-time}TOML datetime}
    formats using {!Ptime} types. *)

type ptime_datetime =
  [ `Datetime of Ptime.t * Ptime.tz_offset_s option
  | `Datetime_local of Ptime.t
  | `Date of Ptime.date
  | `Time of int * int * int * int ]
(** Datetime representation using {!Ptime}. *)

val to_ptime_datetime : ?tz_offset_s:int -> t -> ptime_datetime option
(** [to_ptime_datetime ?tz_offset_s t] converts any TOML datetime value to a
    unified ptime representation. *)

val toml_of_ptime_datetime : ptime_datetime -> t
(** [toml_of_ptime_datetime pdt] converts a unified ptime datetime back to a
    TOML value. *)

val pp_ptime_datetime : Format.formatter -> ptime_datetime -> unit
(** [pp_ptime_datetime fmt pdt] pretty-prints the unified datetime. *)

(** {1:pp Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints [t] in TOML inline format. Tables are printed as
    inline tables. Metadata is ignored. *)

val pp_value : Format.formatter -> t -> unit
(** [pp_value fmt t] pretty-prints a single TOML value. Same as {!val:pp}. *)

val equal : t -> t -> bool
(** [equal a b] is structural equality on TOML values. NaN floats are considered
    equal to each other. Ignores metadata. *)

val compare : t -> t -> int
(** [compare a b] is a total ordering on TOML values. Ignores metadata. *)

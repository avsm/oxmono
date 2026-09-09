(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Codec combinators for TOML values.

    Each codec ['a t] defines a bidirectional mapping between TOML and OCaml
    values. Codecs compose through combinators to build complex types from
    simple primitives. *)

(** {1:codec Codec Types} *)

type 'a t
(** The type of TOML codecs. *)

val kind : 'a t -> string
(** [kind c] is the kind label of [c]. *)

val doc : 'a t -> string
(** [doc c] is the documentation string of [c]. *)

val pp : Format.formatter -> 'a t -> unit
(** [pp ppf c] prints [c]'s kind. *)

val with_doc : ?kind:string -> ?doc:string -> 'a t -> 'a t
(** [with_doc ?kind ?doc c] overrides kind/doc metadata on [c]. *)

(** {1:base Base Type Codecs} *)

val bool : bool t
(** [bool] is the codec for TOML booleans. *)

val int : int t
(** [int] is the codec for TOML integers as OCaml [int]. *)

val int32 : int32 t
(** [int32] is the codec for TOML integers as OCaml [int32]. *)

val int64 : int64 t
(** [int64] is the codec for TOML integers as OCaml [int64]. *)

val float : float t
(** [float] is the codec for TOML floats. *)

val number : float t
(** [number] accepts both TOML integers and floats as float. *)

val string : string t
(** [string] is the codec for TOML strings. *)

val int_as_string : int t
(** [int_as_string] decodes an integer from a TOML string. *)

val int64_as_string : int64 t
(** [int64_as_string] decodes an int64 from a TOML string. *)

(** {1:ptime_codecs Ptime Datetime Codecs} *)

val ptime :
  ?tz_offset_s:int ->
  ?get_tz:(unit -> int option) ->
  ?now:(unit -> Ptime.t) ->
  ?frac_s:int ->
  unit ->
  Ptime.t t
(** [ptime ()] is a codec for all TOML datetime flavours as [Ptime.t]. *)

val ptime_opt : ?tz_offset_s:int -> ?frac_s:int -> unit -> Ptime.t t
(** [ptime_opt ()] is like {!ptime} but local datetimes use the provided offset
    only. *)

val ptime_span : Ptime.Span.t t
(** [ptime_span] is a codec for TOML times as [Ptime.Span.t] durations. *)

val ptime_date : Ptime.date t
(** [ptime_date] is a codec for TOML local dates as [Ptime.date]. *)

val ptime_full :
  ?tz_offset_s:int ->
  ?get_tz:(unit -> int option) ->
  unit ->
  Value.ptime_datetime t
(** [ptime_full ()] preserves TOML datetime variants exactly. *)

(** {1:combinators Codec Combinators} *)

val map :
  ?kind:string ->
  ?doc:string ->
  ?dec:('a -> 'b) ->
  ?enc:('b -> 'a) ->
  'a t ->
  'b t
(** [map ?dec ?enc c] transforms [c] through [dec] on decode and [enc] on
    encode. *)

val const : ?kind:string -> ?doc:string -> 'a -> 'a t
(** [const v] always decodes to [v] and encodes as the empty table. *)

val enum :
  ?cmp:('a -> 'a -> int) ->
  ?kind:string ->
  ?doc:string ->
  (string * 'a) list ->
  'a t
(** [enum cases] is a codec for the [(label, value)] cases. *)

val option : ?kind:string -> ?doc:string -> 'a t -> 'a option t
(** [option c] wraps [c] as [Some v]; missing members decode as [None]. *)

val result : ok:'a t -> error:'b t -> ('a, 'b) result t
(** [result ~ok ~error] tries [ok] first, then [error]. *)

val fix : 'a t Lazy.t -> 'a t
(** [fix c] is a recursive codec for self-referential types. *)

val iter :
  ?kind:string ->
  ?doc:string ->
  ?dec:('a -> unit) ->
  ?enc:('a -> unit) ->
  'a t ->
  'a t
(** [iter ?dec ?enc c] runs side effects on decode/encode without changing [c].
*)

val recode : dec:'a t -> ('a -> 'b) -> enc:'b t -> 'b t
(** [recode ~dec f ~enc] decodes with [dec], maps through [f], encodes with
    [enc]. *)

(** {2:query Query Combinators} *)

val key : string -> 'a t -> 'a t
(** [key name c] queries a table member by key [name], decoding it with [c]. On
    encoding, produces a single-member table. *)

(** {2:updates Updates} *)

val update_key : string -> 'a t -> Value.t t
(** [update_key name c] updates a table member. *)

val delete_key : string -> Value.t t
(** [delete_key name] deletes a table member. *)

val nth : ?absent:'a -> int -> 'a t -> 'a t
(** [nth n t] decodes the [n]th element of a TOML array. *)

val mem : ?absent:'a -> string -> 'a t -> 'a t
(** [mem name t] decodes the member named [name] from a TOML table. *)

(** {2:folding Folding Combinators} *)

val fold_array : 'a t -> (int -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_array c f init] folds [f] over each array element. *)

val fold_table : 'a t -> (string -> 'a -> 'b -> 'b) -> 'b -> 'b t
(** [fold_table c f init] folds [f] over each table member. *)

(** {2:ignoring Ignoring and Placeholders} *)

val ignore : unit t
(** [ignore] decodes anything to [()] and errors on encode. *)

val zero : unit t
(** [zero] decodes anything to [()] and encodes as the empty table. *)

(** {1:arrays Array Codecs} *)

module Array : sig
  type 'a codec = 'a t

  type ('array, 'elt) enc = {
    fold : 'acc. ('acc -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc;
  }

  type ('array, 'elt, 'builder) map

  val map :
    ?kind:string ->
    ?doc:string ->
    ?dec_empty:(unit -> 'builder) ->
    ?dec_add:('elt -> 'builder -> 'builder) ->
    ?dec_finish:('builder -> 'array) ->
    ?enc:('array, 'elt) enc ->
    'elt codec ->
    ('array, 'elt, 'builder) map
  (** [map elt] builds an array codec over elements [elt]. *)

  val list :
    ?kind:string -> ?doc:string -> 'a codec -> ('a list, 'a, 'a list) map
  (** [list c] is a codec producing OCaml lists. *)

  val array :
    ?kind:string -> ?doc:string -> 'a codec -> ('a array, 'a, 'a list) map
  (** [array c] is a codec producing OCaml arrays. *)

  val finish : ('array, 'elt, 'builder) map -> 'array codec
  (** [finish m] seals the builder into a codec. *)
end

val list : ?kind:string -> ?doc:string -> 'a t -> 'a list t
(** [list c] is the shorthand for [Array.(list c |> finish)]. *)

val array : ?kind:string -> ?doc:string -> 'a t -> 'a array t
(** [array c] is the shorthand for [Array.(array c |> finish)]. *)

(** {1:tables Table Codecs} *)

module Table : sig
  type 'a codec = 'a t

  module Mem : sig
    type 'a codec = 'a t
    type ('o, 'a) t

    val v :
      ?doc:string ->
      ?dec_absent:'a ->
      ?enc:('o -> 'a) ->
      ?enc_omit:('a -> bool) ->
      string ->
      'a codec ->
      ('o, 'a) t
    (** [v name codec] creates a member specification for [name]. *)

    val opt :
      ?doc:string ->
      ?enc:('o -> 'a option) ->
      string ->
      'a codec ->
      ('o, 'a option) t
    (** [opt name codec] creates an optional member specification. *)

    val name : ('o, 'a) t -> string
    (** [name m] is the member's name. *)

    val doc : ('o, 'a) t -> string
    (** [doc m] is the [?doc] passed at construction time, or [""]. *)

    val codec : ('o, 'a) t -> 'a codec
    (** [codec m] is the codec for decoding/encoding the member's value. *)

    val dec_absent : ('o, 'a) t -> 'a option
    (** [dec_absent m] is the default value the decoder should supply when the
        member is absent from the input, if any. *)
  end

  type ('o, 'dec) map
  (** Builder state for a table codec. *)

  val obj : ?kind:string -> ?doc:string -> 'dec -> ('o, 'dec) map
  (** [obj dec] starts building a table codec with constructor [dec]. *)

  val obj' : ?kind:string -> ?doc:string -> (unit -> 'dec) -> ('o, 'dec) map
  (** [obj' mk] is like {!obj} but [mk] is a thunk called lazily. *)

  val mem :
    ?doc:string ->
    ?dec_absent:'a ->
    ?enc:('o -> 'a) ->
    ?enc_omit:('a -> bool) ->
    string ->
    'a codec ->
    ('o, 'a -> 'dec) map ->
    ('o, 'dec) map
  (** [mem name codec m] adds a member to the table builder. *)

  val opt_mem :
    ?doc:string ->
    ?enc:('o -> 'a option) ->
    string ->
    'a codec ->
    ('o, 'a option -> 'dec) map ->
    ('o, 'dec) map
  (** [opt_mem name codec m] adds an optional member. *)

  val skip_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [skip_unknown m] silently ignores unknown members (the default). *)

  val error_unknown : ('o, 'dec) map -> ('o, 'dec) map
  (** [error_unknown m] raises an error on unknown members. *)

  module Mems : sig
    type 'a codec = 'a t

    type ('mems, 'a) enc = {
      fold : 'acc. ('acc -> string -> 'a -> 'acc) -> 'acc -> 'mems -> 'acc;
    }

    type ('mems, 'a, 'builder) map

    val kind : ('mems, 'a, 'builder) map -> string
    (** [kind m] is the [kind] label passed at {!val-map} construction time, or
        a derived default (["members of <elt.kind>"] /
        ["string map of <elt.kind>"] / ["assoc of <elt.kind>"]). *)

    val doc : ('mems, 'a, 'builder) map -> string
    (** [doc m] is the [doc] string passed at {!val-map} construction time, or
        [""]. *)

    val map :
      ?kind:string ->
      ?doc:string ->
      ?dec_empty:(unit -> 'builder) ->
      ?dec_add:(string -> 'a -> 'builder -> 'builder) ->
      ?dec_finish:('builder -> 'mems) ->
      ?enc:('mems, 'a) enc ->
      'a codec ->
      ('mems, 'a, 'builder) map
    (** [map c] builds a members codec over values of codec [c]. *)

    val string_map :
      ?kind:string ->
      ?doc:string ->
      'a codec ->
      ('a Map.Make(String).t, 'a, (string * 'a) list) map
    (** [string_map c] produces a [String]-keyed [Map]. *)

    val assoc :
      ?kind:string ->
      ?doc:string ->
      'a codec ->
      ((string * 'a) list, 'a, (string * 'a) list) map
    (** [assoc c] produces an association list. *)
  end

  val keep_unknown :
    ?enc:('o -> 'mems) ->
    ('mems, 'a, 'builder) Mems.map ->
    ('o, 'mems -> 'dec) map ->
    ('o, 'dec) map
  (** [keep_unknown m b] collects unknown members into the record via [m]. What
      a decode collects belongs to that decode: the codec may be decoded any
      number of times, from any number of domains, and each decode sees the
      unknown members of the table it was handed and no others. An unknown
      member that [m] fails to decode fails the decode, under that member's own
      error frame, rather than being dropped. *)

  val finish : ('o, 'o) map -> 'o codec
  (** [finish m] seals the builder into a standalone table codec. *)

  val inline : ('o, 'o) map -> 'o codec
  (** [inline m] is like {!finish} but encodes as a TOML inline table. *)
end

val array_of_tables : ?kind:string -> ?doc:string -> 'a t -> 'a list t
(** [array_of_tables c] is a codec for TOML's [[[array-of-tables]]]. *)

val any :
  ?kind:string ->
  ?doc:string ->
  ?dec_string:'a t ->
  ?dec_int:'a t ->
  ?dec_float:'a t ->
  ?dec_bool:'a t ->
  ?dec_datetime:'a t ->
  ?dec_array:'a t ->
  ?dec_table:'a t ->
  ?enc:('a -> 'a t) ->
  unit ->
  'a t
(** [any ?dec_string ?dec_int ?dec_float ?dec_bool ?dec_datetime ?dec_array
     ?dec_table ?enc ()] dispatches decoding by TOML value kind and chooses an
    encoder from [enc]. *)

(** {1:codec_ops Decoding and Encoding at the Value layer} *)

val of_toml :
  ?max_depth:int -> ?max_nodes:int -> 'a t -> Value.t -> ('a, Error.t) result
(** [of_toml c v] decodes TOML value [v] using codec [c]. *)

val of_toml_exn : ?max_depth:int -> ?max_nodes:int -> 'a t -> Value.t -> 'a
(** [of_toml_exn c v] is like {!val-of_toml} but raises [Loc.Error] on error. *)

val to_toml : 'a t -> 'a -> Value.t
(** [to_toml c v] encodes OCaml value [v] to TOML using codec [c]. *)

(** {1:value_codecs AST-preserving Codecs} *)

(** Codecs that preserve the TOML AST ({!Value.t}) instead of mapping to OCaml
    types. Used when you want to manipulate TOML generically. *)
module Value : sig
  type value := Value.t

  val t : value t
  (** [t] is the identity codec: decodes any TOML value unchanged and encodes it
      unchanged. Exposed here so {!Toml.Value.codec} can alias it without
      introducing a circular module dependency. *)

  val mems : (string * value) list t
  (** [mems] is a codec for table members as a raw assoc list. *)
end

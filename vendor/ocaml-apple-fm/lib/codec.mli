(** Typed structured values and tool arguments.

    An ['a value] combines the schema shown to Apple with the jsont codec used
    to encode and decode ['a]. {!Object} builds response values and {!Invoke}
    builds a named tool argument object.

    {[
      let rating = Codec.int_range ~minimum:1 ~maximum:10 ()
    ]} *)

@@ portable

type 'a value = 'a Apple_fm_base.Codec.value
(** A generation schema and codec for an OCaml value of type ['a]. *)

val null : unit value
(** JSON null, decoded as [()]. This requires macOS 26.4 or later. *)

val string : string value
(** Any JSON string. *)

val string_guided :
  ?constant:string ->
  ?choices:string list ->
  ?pattern:string ->
  unit ->
  string value
(** A constrained string. [constant] and [choices] are checked locally;
    [pattern] uses Apple's Swift regular-expression syntax. *)

val bool : bool value
(** Any JSON Boolean. *)

val int : int value
(** Any JSON integer. *)

val int_range : ?minimum:int -> ?maximum:int -> unit -> int value
(** An integer within the supplied inclusive bounds. *)

val float : float value
(** Any finite JSON number. *)

val float_range : ?minimum:float -> ?maximum:float -> unit -> float value
(** A finite number within the supplied inclusive bounds. *)

val array : ?minimum:int -> ?maximum:int -> 'a value -> 'a list value
(** [array item] is a list of [item] values with optional length bounds. *)

val enum :
  ('a : value mod contended).
  name:string ->
  ?description:string ->
  (string * 'a) list @ portable ->
  'a value
(** [enum ~name choices] maps each permitted JSON string to an OCaml value. *)

type 'a case = 'a Apple_fm_base.Codec.case
(** One typed alternative in an {!any_of} codec. *)

val case :
  inject:('a -> 'b) @ portable ->
  project:('b -> 'a option) @ portable ->
  'a value ->
  'b case
(** [case ~inject ~project value] adds one alternative of an OCaml sum type. *)

val any_of :
  name:string -> ?description:string -> 'a case list @ portable -> 'a value
(** [any_of ~name cases] accepts exactly one of [cases]. *)

val recursive :
  name:string -> ('a value -> 'a value) @ portable -> 'a value
(** [recursive ~name define] builds a named recursive value. *)

val map_value :
  dec:('a -> 'b) @ portable ->
  enc:('b -> 'a) @ portable ->
  'a value ->
  'b value
(** [map_value ~dec ~enc value] changes the OCaml representation. *)

module Object : sig
  type ('o, 'dec) map = ('o, 'dec) Apple_fm_base.Codec.Object.map
  (** An object codec under construction. *)

  val map :
    ('dec : value mod contended) 'o.
    string -> 'dec @ portable -> ('o, 'dec) map
  (** [map name constructor] starts a named object. *)

  val param :
    enc:('o -> 'a) @ portable ->
    ?description:string ->
    ?default:'a ->
    string ->
    'a value ->
    ('o, 'a -> 'b) map ->
    ('o, 'b) map
  (** [param ~enc name value map] adds a member. [default] permits omission. *)

  val optional :
    enc:('o -> 'a option) @ portable ->
    ?description:string ->
    string ->
    'a value ->
    ('o, 'a option -> 'b) map ->
    ('o, 'b) map
  (** [optional ~enc name value map] adds a member that may be absent. *)

  val seal : ('o, 'o) map -> 'o value
  (** [seal map] finishes the object. *)
end

type 'a t = 'a Apple_fm_base.Codec.t
(** A named argument object passed to a tool handler. *)

module Invoke : sig
  type ('o, 'dec) map = ('o, 'dec) Object.map
  (** Tool arguments under construction. *)

  val map :
    ('dec : value mod contended) 'o.
    string -> 'dec @ portable -> ('o, 'dec) map
  (** [map name constructor] starts the arguments for tool [name]. *)

  val param :
    enc:('o -> 'a) @ portable ->
    ?description:string ->
    ?default:'a ->
    string ->
    'a value ->
    ('o, 'a -> 'b) map ->
    ('o, 'b) map
  (** [param ~enc name value map] adds a parameter. [default] permits omission. *)

  val optional :
    enc:('o -> 'a option) @ portable ->
    ?description:string ->
    string ->
    'a value ->
    ('o, 'a option -> 'b) map ->
    ('o, 'b) map
  (** [optional ~enc name value map] adds a parameter that may be absent. *)

  val seal : ('o, 'o) map -> 'o t
  (** [seal map] finishes the arguments. *)
end

val map :
  dec:('a -> 'b) @ portable ->
  enc:('b -> 'a) @ portable ->
  'a t ->
  'b t
(** [map ~dec ~enc codec] changes the OCaml argument type. *)

val name : 'a t -> string
(** [name codec] is the tool name. *)

val schema : 'a t -> Schema.t
(** [schema codec] is the schema shown to Apple. *)

val decode_arguments : 'a t -> string -> ('a, string) result
(** [decode_arguments codec json] decodes a JSON argument object. *)

val encode_arguments : 'a t -> 'a -> (string, string) result
(** [encode_arguments codec value] encodes a JSON argument object. *)

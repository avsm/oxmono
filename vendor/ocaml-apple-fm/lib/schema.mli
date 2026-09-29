(** Apple generation schemas.

    A schema represents Apple's
    {{:https://developer.apple.com/documentation/foundationmodels/dynamicgenerationschema}
      [DynamicGenerationSchema]}.
    Use {!Codec} when generated JSON should also be converted to an OCaml value. *)

type t = Apple_fm_base.Schema.t
(** JSON that Apple may generate. *)

type property = Apple_fm_base.Schema.property = {
  name : string;
  description : string option;
  schema : t;
  optional : bool;
}
(** A named property in an object schema. *)

val string : t
(** Any JSON string. *)

val null : t
(** JSON null. This requires macOS 26.4 or later. *)

val string_guided :
  ?constant:string -> ?choices:string list -> ?pattern:string -> unit -> t
(** A constrained string. [constant] fixes the value, [choices] supplies the
    permitted values, and [pattern] is a Swift regular expression. *)

val integer : t
(** Any JSON integer. *)

val integer_range : ?minimum:int -> ?maximum:int -> unit -> t
(** An integer within the supplied inclusive bounds. *)

val number : t
(** Any JSON number. *)

val number_range : ?minimum:float -> ?maximum:float -> unit -> t
(** A finite number within the supplied inclusive bounds. *)

val boolean : t
(** Any JSON Boolean. *)

val array : ?minimum:int -> ?maximum:int -> t -> t
(** [array item] is an array of [item] values with optional length bounds. *)

val object_ :
  name:string ->
  ?description:string ->
  ?explicit_null:bool ->
  property list ->
  (t, string) result
(** [object_ ~name properties] is a named object. Names contain ASCII letters,
    digits, and underscores and cannot start with a digit. Property names must
    be unique. [explicit_null] requires macOS 26.4 or later. *)

val one_of :
  name:string -> ?description:string -> string list -> (t, string) result
(** [one_of ~name choices] is one string from [choices]. *)

val any_of : name:string -> ?description:string -> t list -> (t, string) result
(** [any_of ~name choices] is a value matching one of [choices]. *)

val reference : string -> t
(** [reference name] refers to a named schema supplied to {!with_dependencies}. *)

val with_dependencies : t -> t list -> t
(** [with_dependencies root definitions] resolves references in [root]. *)

val property : ?description:string -> ?optional:bool -> string -> t -> property
(** [property name schema] is an object member. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter schema] prints [schema] as compact JSON. *)

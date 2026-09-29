(** Typed tools callable by the model. *)

type t = Apple_fm_base.Tool.t
(** A named tool with typed arguments and a handler. *)

val v :
  ?includes_schema_in_instructions:bool ->
  description:string ->
  'a Codec.t ->
  ('a -> string) ->
  t
(** [v ~description codec handler] creates a text-result tool. Invalid calls are
    returned to the model. Eio cancellation and fatal runtime exceptions escape
    the handler. *)

val v_json :
  ?includes_schema_in_instructions:bool ->
  description:string ->
  output:'b Jsont.t ->
  'a Codec.t ->
  ('a -> 'b) ->
  t
(** [v_json ~output ~description codec handler] creates a structured-result
    tool. *)

val name : t -> string
(** [name tool] is the name used by the model. *)

val description : t -> string
(** [description tool] is the description shown to the model. *)

val schema : t -> Schema.t
(** [schema tool] describes its arguments. *)

val invoke : t -> string -> string
(** [invoke tool arguments] runs [tool] with a JSON argument object. *)

val invoke_json : t -> string -> string
(** [invoke_json tool arguments] returns the JSON observation sent to Apple. *)

val includes_schema_in_instructions : t -> bool
(** Whether Apple repeats the argument schema in its instructions. *)

val pp : Format.formatter -> t -> unit
(** [pp formatter tool] prints [tool]. *)

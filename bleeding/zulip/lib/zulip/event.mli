@@ portable

(** Raw events received from a Zulip event queue.

    An event envelope contains an identifier, a family name, and all remaining
    payload members. Typed payload validation is provided by {!Event_payload}.
*)

type t = private { id : Id.Event.t; type_ : Event_type.t; data : Jsont.json }
(** The type for raw event envelopes. [data] is a JSON object without the
    envelope members [id] and [type]. *)

val id : t -> Id.Event.t
(** [id event] is the queue-local identifier of [event]. *)

val type_ : t -> Event_type.t
(** [type_ event] is the payload family of [event]. *)

val data : t -> Jsont.json
(** [data event] is the payload object of [event] without the [id] and [type]
    members. *)

val jsont : t Jsont.t
(** [jsont] is a codec for event queue objects. The [id] and [type] members are
    required. All other members form {!val-data}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf event] writes the event identifier and family name to [ppf]. *)

val create :
  id:Id.Event.t ->
  type_:Event_type.t ->
  data:Jsont.json ->
  (t, Jsont.Error.t) result
(** [create ~id ~type_ ~data] is an event envelope with payload [data]. The
    result is [Error error] if [data] is not an object or contains an [id] or
    [type] member. Payload-family validation is deferred to
    {!Event_payload.of_event}. *)

@@ portable

(** Event pieces shared across the event families.

    Timestamps, the [m.relates_to] property and image metadata appear in several
    unrelated event types, so they are defined once here and re-exported from
    {!Matrix_event}. *)

module Timestamp : sig
  (** A point in time in milliseconds since the Unix epoch, as claimed by the
      server that created the event.

      It is not a reliable clock. Servers are not synchronised, a timestamp is
      not covered by the event's signature, and an event's position in the
      timeline is the authority on ordering. *)

  type t
  (** The type for millisecond timestamps. *)

  val of_ms : int64 -> t
  (** [of_ms ms] is the instant [ms] milliseconds after the Unix epoch. *)

  val to_ms : t -> int64
  (** [to_ms t] is [t] in milliseconds since the Unix epoch. *)

  val of_ptime : Ptime.t -> t
  (** [of_ptime pt] is [pt] truncated to milliseconds. *)

  val to_ptime_opt : t -> Ptime.t option
  (** [to_ptime_opt t] is [t] as a POSIX time. It is [None] when [t] falls
      outside the range [Ptime] represents. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same instant. *)

  val compare : t -> t -> int
  (** [compare a b] orders [a] before [b] when [a] is the earlier instant. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the millisecond count, not a calendar date. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. It reads and writes an integer. *)
end

module Rel_type : sig
  (** The kind of relationship an [m.relates_to] property declares.

      @see <https://spec.matrix.org/v1.11/client-server-api/#forming-relationships-between-events>
        Forming relationships between events *)

  type t =
    | Annotation  (** A reaction, which also carries a key. *)
    | Reference  (** A pointer to the event a flow or a poll started from. *)
    | Replace  (** An edit of the target. *)
    | Thread  (** A message in the thread the target roots. *)
    | Custom of string  (** A relationship this type does not name. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> t
  (** [of_string s] is the relationship [s] names, or [Custom s]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same relationship. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Relates_to : sig
  (** The [m.relates_to] property, which points one event at another.

      Reactions, edits, threads, poll responses, live-location updates and
      in-room key verification all use it. *)

  type t = {
    rel_type : Rel_type.t;
    event_id : Matrix_id.Event_id.t;  (** The event related to. *)
    key : string option;
        (** The annotation, normally an emoji. Present only under
            {!Rel_type.Annotation}. *)
  }

  val v : ?key:string -> rel_type:Rel_type.t -> Matrix_id.Event_id.t -> t
  (** [v ~rel_type event_id] is a relation of kind [rel_type] to [event_id].
      [key] defaults to absent. *)

  val reference : Matrix_id.Event_id.t -> t
  (** [reference event_id] is [v ~rel_type:Reference event_id]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] have the same kind, target and key.
  *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the kind and the target event. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Image_info : sig
  (** What the sender claims about an image.

      All of it is the sender's word. A client must not size a layout or
      allocate a buffer from it without checking what actually arrives. *)

  type t = {
    mimetype : string option;
    size : int option;  (** Bytes. *)
    h : int option;  (** Height in pixels. *)
    w : int option;  (** Width in pixels. *)
  }

  val v : ?mimetype:string -> ?size:int -> ?h:int -> ?w:int -> unit -> t
  (** [v ()] is image metadata. Every argument defaults to absent. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the members that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

@@ portable

(** The content of the space events.

    A space is a room whose [m.room.create] type is ["m.space"]. Its membership
    graph is built from [m.space.child] and [m.space.parent] state events.

    @see <https://spec.matrix.org/v1.11/client-server-api/#spaces> Spaces *)

module Space_child_content : sig
  (** The content of [m.space.child], a room this space contains, keyed by the
      child's room id as the event's state key.

      An event with no {!field-via} servers means the child was removed.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mspacechild>
        m.space.child *)

  type t = {
    via : string list option;
        (** Servers to try when joining the child. Required for the relationship
            to count. *)
    order : string option;
        (** Sort key among siblings, ASCII only. Children without one sort last,
            by [origin_server_ts]. *)
    suggested : bool option;  (** The space recommends this child. *)
  }

  val make : ?via:string list -> ?order:string -> ?suggested:bool -> unit -> t
  (** [make ()] is an [m.space.child] content. Every argument defaults to
      absent. *)

  val via : t -> string list option
  (** [via t] is the servers to try when joining the child. *)

  val order : t -> string option
  (** [order t] is the sort key among siblings. *)

  val suggested : t -> bool option
  (** [suggested t] is whether the space recommends the child. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the members that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Space_parent_content : sig
  (** The content of [m.space.parent], a space this room belongs to, keyed by
      the parent's room id as the event's state key. The claim is believed only
      if the sender can set state in the parent.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mspaceparent>
        m.space.parent *)

  type t = {
    via : string list option;
        (** Servers to try when joining the parent. Required for the
            relationship to count. *)
    canonical : bool option;
        (** This is the space to show the room under, where it has several. *)
  }

  val make : ?via:string list -> ?canonical:bool -> unit -> t
  (** [make ()] is an [m.space.parent] content. Both arguments default to
      absent. *)

  val via : t -> string list option
  (** [via t] is the servers to try when joining the parent. *)

  val canonical : t -> bool option
  (** [canonical t] is whether this is the space to show the room under. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the members that are present. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

(** A bounded description of a room, including rooms not currently joined. *)

type t = {
  room_id : Matrix_proto.Id.Room_id.t;
  canonical_alias : Matrix_proto.Id.Room_alias.t option;
  name : string option;
  topic : string option;
  avatar_url : Media.Mxc.t option;
  num_joined_members : int;
  num_active_members : int option;
  room_type : string option;
  create : Matrix_proto.Event.Room_create_content.t option;
      (** The persisted or remotely fetched create content. It carries the
          creator, room version, room type and predecessor reference. *)
  tombstone : Matrix_proto.Event.Room_tombstone_content.t option;
      (** The room's upgrade tombstone. Its replacement room is the successor.
      *)
  service_members : Matrix_proto.Id.User_id.t list option;
      (** Stable [m.room.member_hints], or the legacy functional-member list.
          [None] means this state was not available. *)
  join_rule : Matrix_proto.Event.Join_rule.t option;
  is_world_readable : bool option;
  membership : Store.membership option;
  is_direct : bool option;
  heroes : Store.hero list option;
}
(** The fields mirror the useful part of the Rust SDK's [RoomPreview].

    A remote summary does not provide active-member counts, create/tombstone
    state, service members, heroes or direct-chat state, so those fields are
    [None]. The state fallback and persisted projections do retain them. Event
    identifiers and timestamps are not part of a preview. *)

val of_room : Store.room_info -> t
(** [of_room room] projects persisted room state. *)

val get :
  Client.t ->
  store:Store.t ->
  room_id_or_alias:Directory.room_id_or_alias ->
  ?via:string list ->
  unit ->
  (t, Error.t) result
(** Gets a local preview for a joined room. Invited, knocked, left and unknown
    rooms first use MSC3266's room-summary endpoint, then fall back to current
    state plus joined members and finally to any persisted projection. When
    [via] is empty, a remote room's own server is supplied as a federation hint,
    matching matrix-rust-sdk. *)

(** Records binding a repository's CI to a Matrix room.

    A binding is an [org.recoil.tangled.matrix] record in the account of the
    person who made it, keyed by the repository's DID. It asks a
    [tangled-matrix] poster logged in as [bot] to report the repository's
    pipelines in [room]. A poster honours it only when its author is the
    repository's owner or a collaborator. The record is public, as all atproto
    records are, so it names the room but none of the room's messages. *)

val nsid : string
(** [nsid] is [org.recoil.tangled.matrix]. *)

type t = {
  repo : string;  (** The repository's DID, also the record's key. *)
  bot : string;  (** The poster's Matrix user ID, such as [@ci:example.org]. *)
  room : string;  (** The room's ID, such as [!abc:example.org]. *)
  alias : string option;
      (** The alias the room was named by, such as [#ci:example.org], kept for
          people to read. *)
  created_at : string;
}

val jsont : t Jsont.t
(** [jsont] is the record's codec, with its [$type]. *)

val valid_user_id : string -> bool
(** [valid_user_id s] holds for a Matrix user ID, [@localpart:server]. *)

val valid_room_id : string -> bool
(** [valid_room_id s] holds for a room ID, [!opaque:server] or, in room version
    12 and later, [!opaque]. *)

val valid_alias : string -> bool
(** [valid_alias s] holds for a room alias, [#name:server]. *)

val server_name : string -> string option
(** [server_name id] is the server part of a user ID or alias, after its first
    [:]. *)

val list : Tangled_api.t -> did:string -> t list
(** [list api ~did] is every binding [did] has made. Records that do not decode
    or whose key is not their repository are left out. *)

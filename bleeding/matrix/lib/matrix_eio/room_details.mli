(** Eio raising wrapper for {!Matrix_client.Room_details}. *)

type t = Matrix_client.Room_details.t

type member = Matrix_client.Room_details.member = {
  user_id : Matrix_proto.Id.User_id.t;
  display_name : string option;
  display_label : string;
  display_name_ambiguous : bool;
  avatar_url : Matrix_client.Media.Mxc.t option;
  membership : Matrix_proto.Event.Membership.t;
  role : Matrix_client.Room.role;
  is_service_member : bool;
  is_account_user : bool;
}

val create :
  client:Client.t ->
  state:Matrix_client.Base_client.state ->
  Matrix_proto.Id.Room_id.t ->
  t

val state : t -> Matrix_client.Base_client.state
val room_id : t -> Matrix_proto.Id.Room_id.t
val members_complete : t -> bool
val members : t -> member list
val member_count : t -> int
val service_member_count : t -> int
val human_member_count : t -> int
val ensure_members : t -> t

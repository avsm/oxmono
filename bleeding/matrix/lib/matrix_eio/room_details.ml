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

let create ~client ~state room_id =
  Matrix_client.Room_details.create ~client:(Client.base client) ~state room_id

let state = Matrix_client.Room_details.state
let room_id = Matrix_client.Room_details.room_id
let members_complete = Matrix_client.Room_details.members_complete
let members = Matrix_client.Room_details.members
let member_count = Matrix_client.Room_details.member_count
let service_member_count = Matrix_client.Room_details.service_member_count
let human_member_count = Matrix_client.Room_details.human_member_count

let ensure_members t =
  Error.unwrap ~context:"ensuring room members"
    (Matrix_client.Room_details.ensure_members t)

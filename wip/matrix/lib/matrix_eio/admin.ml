module A = Matrix_client.Admin

type connection = A.connection = {
  ip : string option;
  last_seen : Matrix_proto.Event.Timestamp.t option;
  user_agent : string option;
}

type connection_info = connection
type session = A.session = { connections : connection list }
type session_info = session
type device = A.device = { sessions : session list }
type device_info = device

type response = A.response = {
  user_id : Matrix_proto.Id.User_id.t option;
  devices : (string * device) list;
}

let whois client ~user_id =
  Error.unwrap ~context:"looking up admin user"
    (A.whois (Client.base client) ~user_id)

let get_user_info = whois

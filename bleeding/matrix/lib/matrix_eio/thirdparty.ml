type field_type = Matrix_client.Thirdparty.field_type = {
  regexp : string;
  placeholder : string;
}

type protocol_instance = Matrix_client.Thirdparty.protocol_instance = {
  network_id : string;
  desc : string;
  icon : string option;
  fields : (string * string) list;
  instance_id : string option;
}

type protocol = Matrix_client.Thirdparty.protocol = {
  user_fields : string list;
  location_fields : string list;
  icon : string;
  field_types : (string * field_type) list;
  instances : protocol_instance list;
}

type location = Matrix_client.Thirdparty.location = {
  alias : Matrix_proto.Id.Room_alias.t;
  protocol : string;
  fields : (string * string) list;
}

type user = Matrix_client.Thirdparty.user = {
  userid : Matrix_proto.Id.User_id.t;
  protocol : string;
  fields : (string * string) list;
}

let protocols client =
  Error.unwrap ~context:"listing third-party protocols"
    (Matrix_client.Thirdparty.protocols (Client.base client))

let get_protocol client ~name =
  Error.unwrap ~context:"getting third-party protocol"
    (Matrix_client.Thirdparty.get_protocol (Client.base client) ~name)

let locations_of_alias client ~alias =
  Error.unwrap ~context:"resolving third-party alias"
    (Matrix_client.Thirdparty.locations_of_alias (Client.base client) ~alias)

let locations client ~protocol ?fields () =
  Error.unwrap ~context:"listing third-party locations"
    (Matrix_client.Thirdparty.locations (Client.base client) ~protocol ?fields
       ())

let users_of_user_id client ~user_id =
  Error.unwrap ~context:"resolving third-party user"
    (Matrix_client.Thirdparty.users_of_user_id (Client.base client) ~user_id)

let users client ~protocol ?fields () =
  Error.unwrap ~context:"listing third-party users"
    (Matrix_client.Thirdparty.users (Client.base client) ~protocol ?fields ())

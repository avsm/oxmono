type profile = Matrix_client.Profile.profile = {
  displayname : string option;
  avatar_url : Matrix_client.Media.Mxc.t option;
  fields : (string * Jsont.json) list;
}

let get_profile client ~user_id =
  Error.unwrap ~context:"getting profile"
    (Matrix_client.Profile.get_profile (Client.base client) ~user_id)

let get_displayname client ~user_id =
  Error.unwrap ~context:"getting display name"
    (Matrix_client.Profile.get_displayname (Client.base client) ~user_id)

let set_displayname client ~displayname =
  Error.unwrap ~context:"setting display name"
    (Matrix_client.Profile.set_displayname (Client.base client) ~displayname)

let clear_displayname client =
  Error.unwrap ~context:"clearing display name"
    (Matrix_client.Profile.clear_displayname (Client.base client))

let get_avatar_url client ~user_id =
  Error.unwrap ~context:"getting avatar URL"
    (Matrix_client.Profile.get_avatar_url (Client.base client) ~user_id)

let set_avatar_url client ~avatar_url =
  Error.unwrap ~context:"setting avatar URL"
    (Matrix_client.Profile.set_avatar_url (Client.base client) ~avatar_url)

let clear_avatar_url client =
  Error.unwrap ~context:"clearing avatar URL"
    (Matrix_client.Profile.clear_avatar_url (Client.base client))

let find_field client ~user_id ~key =
  Error.unwrap ~context:"finding profile field"
    (Matrix_client.Profile.find_field (Client.base client) ~user_id ~key)

let set_field client ~key ~value =
  Error.unwrap ~context:"setting profile field"
    (Matrix_client.Profile.set_field (Client.base client) ~key ~value)

let delete_field client ~key =
  Error.unwrap ~context:"deleting profile field"
    (Matrix_client.Profile.delete_field (Client.base client) ~key)

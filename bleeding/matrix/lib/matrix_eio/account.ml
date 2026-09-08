type medium = Matrix_client.Account.medium = Email | Msisdn

type threepid = Matrix_client.Account.threepid = {
  medium : medium;
  address : string;
  validated_at : Matrix_proto.Event.Timestamp.t;
  added_at : Matrix_proto.Event.Timestamp.t;
}

let get_threepids client =
  Error.unwrap ~context:"getting account threepids"
    (Matrix_client.Account.get_threepids (Client.base client))

let request_email_token client ~email ~client_secret ~send_attempt =
  Error.unwrap ~context:"requesting account email token"
    (Matrix_client.Account.request_email_token (Client.base client) ~email
       ~client_secret ~send_attempt)

let request_msisdn_token client ~country ~phone_number ~client_secret
    ~send_attempt =
  Error.unwrap ~context:"requesting account MSISDN token"
    (Matrix_client.Account.request_msisdn_token (Client.base client) ~country
       ~phone_number ~client_secret ~send_attempt)

let add_threepid client ~client_secret ~sid =
  Error.unwrap ~context:"adding account threepid"
    (Matrix_client.Account.add_threepid (Client.base client) ~client_secret ~sid)

let delete_threepid client ~medium ~address =
  Error.unwrap ~context:"deleting account threepid"
    (Matrix_client.Account.delete_threepid (Client.base client) ~medium ~address)

let change_password client ~new_password ?logout_devices ?auth () =
  Error.unwrap ~context:"changing account password"
    (Matrix_client.Account.change_password (Client.base client) ~new_password
       ?logout_devices ?auth ())

let deactivate client ?erase ?auth () =
  Error.unwrap ~context:"deactivating account"
    (Matrix_client.Account.deactivate (Client.base client) ?erase ?auth ())

let get_ignored_users client =
  Error.unwrap ~context:"getting ignored users"
    (Matrix_client.Account.get_ignored_users (Client.base client))

let ignore_user client ~user_id =
  Error.unwrap ~context:"ignoring user"
    (Matrix_client.Account.ignore_user (Client.base client) ~user_id)

let unignore_user client ~user_id =
  Error.unwrap ~context:"unignoring user"
    (Matrix_client.Account.unignore_user (Client.base client) ~user_id)

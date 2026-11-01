type token = Matrix_client.Openid.token = {
  access_token : string;
  matrix_server_name : string;
  expires_in : int;
}

let request_token client ~user_id =
  Error.unwrap ~context:"requesting OpenID token"
    (Matrix_client.Openid.request_token (Client.base client) ~user_id)

let request_own_token client =
  Error.unwrap ~context:"requesting own OpenID token"
    (Matrix_client.Openid.request_own_token (Client.base client))

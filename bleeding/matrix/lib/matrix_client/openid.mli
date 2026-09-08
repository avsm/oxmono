(** openid — minting a token that proves a Matrix identity to a third party.

    [POST /_matrix/client/v3/user/{userId}/openid/request_token] (Matrix 1.0)
    mints a short-lived token. A third party hands it to the federation endpoint
    [GET /_matrix/federation/v1/openid/userinfo] to learn which Matrix user it
    belongs to. *)

type token = {
  access_token : string;
      (** The token. It is a bearer credential, so whoever holds it can prove
          this user gave it to them. *)
  matrix_server_name : string;
      (** The homeserver whose federation API validates the token. *)
  expires_in : int;  (** Lifetime in seconds from the moment of issue. *)
}
(** The type for an OpenID token. *)

val request_token :
  Client.t -> user_id:Matrix_proto.Id.User_id.t -> (token, Error.t) result
(** [request_token t ~user_id] is
    [POST /_matrix/client/v3/user/{userId}/openid/request_token] (Matrix 1.0).
    [user_id] must be the user the access token belongs to. *)

val request_own_token : Client.t -> (token, Error.t) result
(** [request_own_token t] is {!request_token} for the logged-in user.

    Fails with {!Error.No_session} when the client carries none. *)

(** openid — minting a token that proves a Matrix identity to a third party,
    raising instead of returning.

    Every function raises [Eio.Io] carrying [Error.E e] where
    {!Matrix_client.Openid} returns [Error e]. That module documents the token
    and what a third party does with it. *)

type token = Matrix_client.Openid.token = {
  access_token : string;
  matrix_server_name : string;
  expires_in : int;
}
(** A token a third party exchanges for the user's identity. *)

val request_token : Client.t -> user_id:Matrix_proto.Id.User_id.t -> token
(** [request_token c ~user_id] is {!Matrix_client.Openid.request_token} with the
    result unwrapped. *)

val request_own_token : Client.t -> token
(** [request_own_token c] is {!Matrix_client.Openid.request_own_token} with the
    result unwrapped, and so also raises when the client has no session. *)

open Result.Syntax

type token = {
  access_token : string;
  matrix_server_name : string;
  expires_in : int;
}

(* [token_type] is always "Bearer" and carries nothing a caller can act on. *)
let token_jsont =
  Jsont.Object.(
    map ~kind:"openid_token" (fun access_token matrix_server_name expires_in ->
        { access_token; matrix_server_name; expires_in })
    |> mem "access_token" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.access_token)
    |> mem "matrix_server_name" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.matrix_server_name)
    |> mem "expires_in" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.expires_in)
    |> skip_unknown |> finish)

let request_token_path = Route.v "/user/{user_id}/openid/request_token"

let request_token client ~user_id =
  let path =
    Route.expand_exn request_token_path
      [ ("user_id", Matrix_proto.Id.User_id.to_string user_id) ]
  in
  (* The spec defines the request body as an empty object. *)
  let* body = Client.Http.post client ~path ~body:"{}" () in
  Client.Http.decode_response token_jsont body

let request_own_token client =
  match Client.session client with
  | None -> Error Error.No_session
  | Some { Client.user_id; _ } -> request_token client ~user_id

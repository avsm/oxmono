open Result.Syntax
module Id = Matrix_proto.Id
module Field_map = Map.MakePortable (String)

type profile = {
  displayname : string option;
  avatar_url : Media.Mxc.t option;
  fields : (string * Jsont.json) list;
}

(* A profile is an open object: [displayname] and [avatar_url] are the two
   members the specification has always defined, and since Matrix 1.16 a user
   may set namespaced ones alongside them. *)
let profile_jsont =
  Jsont.Object.(
    map ~kind:"profile" (fun displayname avatar_url fields ->
        { displayname; avatar_url; fields = Field_map.bindings fields })
    |> mem "displayname"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.displayname)
    |> mem "avatar_url" Media.mxc_option_jsont
         ~dec_absent:(fun () -> None)
         ~enc_omit:Option.is_none
         ~enc:(fun t -> t.avatar_url)
    |> keep_unknown
         (Matrix_proto.Json.Codec.string_map_mems Matrix_proto.Json.Codec.json)
         ~enc:(fun t -> Field_map.of_seq (List.to_seq t.fields))
    |> finish)

let profile_path_route = Route.v "/profile/{user_id}"
let displayname_path_route = Route.v "/profile/{user_id}/displayname"
let avatar_url_path_route = Route.v "/profile/{user_id}/avatar_url"

let profile_path user_id =
  Route.expand_exn profile_path_route
    [ ("user_id", Id.User_id.to_string user_id) ]

let profile_subpath route user_id =
  Route.expand_exn route [ ("user_id", Id.User_id.to_string user_id) ]

let as_me client f =
  match Client.session client with
  | None -> Error Error.No_session
  | Some { Client.user_id; _ } -> f user_id

let get_profile client ~user_id =
  let* body = Client.Http.get client ~path:(profile_path user_id) () in
  Client.Http.decode_response profile_jsont body

let displayname_jsont =
  Jsont.Object.(
    map ~kind:"displayname" Fun.id
    |> mem "displayname"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:Fun.id
    |> finish)

let get_displayname client ~user_id =
  let* body =
    Client.Http.get client
      ~path:(profile_subpath displayname_path_route user_id)
      ()
  in
  Client.Http.decode_response displayname_jsont body

let set_displayname client ~displayname =
  as_me client @@ fun user_id ->
  let* body = Client.Http.encode_body displayname_jsont (Some displayname) in
  let+ _ =
    Client.Http.put client
      ~path:(profile_subpath displayname_path_route user_id)
      ~body ()
  in
  ()

let clear_displayname client =
  as_me client @@ fun user_id ->
  let* body = Client.Http.encode_body displayname_jsont None in
  let+ _ =
    Client.Http.put client
      ~path:(profile_subpath displayname_path_route user_id)
      ~body ()
  in
  ()

let avatar_url_jsont =
  Jsont.Object.(
    map ~kind:"avatar_url" Fun.id
    |> mem "avatar_url" Media.mxc_option_jsont
         ~dec_absent:(fun () -> None)
         ~enc:Fun.id
    |> finish)

let get_avatar_url client ~user_id =
  let* body =
    Client.Http.get client
      ~path:(profile_subpath avatar_url_path_route user_id)
      ()
  in
  Client.Http.decode_response avatar_url_jsont body

let set_avatar_url client ~avatar_url =
  as_me client @@ fun user_id ->
  let* body = Client.Http.encode_body avatar_url_jsont (Some avatar_url) in
  let+ _ =
    Client.Http.put client
      ~path:(profile_subpath avatar_url_path_route user_id)
      ~body ()
  in
  ()

let clear_avatar_url client =
  as_me client @@ fun user_id ->
  let* body = Client.Http.encode_body avatar_url_jsont None in
  let+ _ =
    Client.Http.put client
      ~path:(profile_subpath avatar_url_path_route user_id)
      ~body ()
  in
  ()

let field_map_jsont = Json_codec.string_map Matrix_proto.Json.Codec.json

let extended_field_stable_route =
  Route.v "/_matrix/client/v3/profile/{user_id}/{key}"

let extended_field_unstable_route =
  Route.v "/_matrix/client/unstable/uk.tcpip.msc4133/profile/{user_id}/{key}"

let extended_field_path client ~user_id ~key =
  let* prefix =
    (* [displayname] and [avatar_url] predate MSC4133 and therefore retain
       their stable profile route even when the server advertises only an old
       Matrix version (the distinction made by ruma's typed field enum). *)
    if key = "displayname" || key = "avatar_url" then Ok "/_matrix/client/v3"
    else
      let* versions = Server.get_versions client in
      (* The pinned ruma history lists [uk.tcpip.msc4133] as the final known
         fallback. Its selector uses [uk.tcpip.msc4133.stable] to opt into the
         stable spelling before Matrix 1.16; when neither signal is advertised,
         ruma still chooses that known unstable path. *)
      Ok
        (if
           Server.supports_version_at_least versions ~major:1 ~minor:16
           || Server.has_unstable_feature versions "uk.tcpip.msc4133.stable"
         then "/_matrix/client/v3"
         else "/_matrix/client/unstable/uk.tcpip.msc4133")
  in
  let route =
    if prefix = "/_matrix/client/v3" then extended_field_stable_route
    else extended_field_unstable_route
  in
  Ok
    (Route.expand_exn route
       [ ("user_id", Id.User_id.to_string user_id); ("key", key) ])

let find_field client ~user_id ~key =
  let* path = extended_field_path client ~user_id ~key in
  let* body = Client.Http.get_absolute client ~path () in
  (* The reply is an object with the field's own name as its single member,
     and an unset field comes back as [{}]. *)
  let+ obj = Client.Http.decode_response field_map_jsont body in
  List.assoc_opt key obj

let set_field client ~key ~value =
  as_me client @@ fun user_id ->
  let* path = extended_field_path client ~user_id ~key in
  (* The request body mirrors the reply: one member, named for the key. *)
  let* body = Client.Http.encode_body field_map_jsont [ (key, value) ] in
  let+ _ = Client.Http.put_absolute client ~path ~body () in
  ()

let delete_field client ~key =
  as_me client @@ fun user_id ->
  let* path = extended_field_path client ~user_id ~key in
  let+ _ = Client.Http.delete_absolute client ~path () in
  ()

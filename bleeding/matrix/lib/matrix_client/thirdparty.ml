open Result.Syntax

let string_map_of = Json_codec.string_map

(* Every third-party field value is a string. *)
let fields_jsont = string_map_of Matrix_proto.Json.Codec.string

type field_type = { regexp : string; placeholder : string }

let field_type_jsont =
  Jsont.Object.(
    map ~kind:"field_type" (fun regexp placeholder -> { regexp; placeholder })
    |> mem "regexp" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.regexp)
    |> mem "placeholder" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.placeholder)
    |> finish)

type protocol_instance = {
  network_id : string;
  desc : string;
  icon : string option;
  fields : (string * string) list;
  instance_id : string option;
}

let protocol_instance_jsont =
  Jsont.Object.(
    map ~kind:"protocol_instance"
      (fun network_id desc icon fields instance_id ->
        { network_id; desc; icon; fields; instance_id })
    |> mem "network_id" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.network_id)
    |> mem "desc" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.desc)
    |> opt_mem "icon" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.icon)
    |> mem "fields" fields_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.fields)
    |> opt_mem "instance_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
        t.instance_id)
    |> finish)

type protocol = {
  user_fields : string list;
  location_fields : string list;
  icon : string;
  field_types : (string * field_type) list;
  instances : protocol_instance list;
}

let protocol_jsont =
  Jsont.Object.(
    map ~kind:"protocol"
      (fun user_fields location_fields icon field_types instances ->
        { user_fields; location_fields; icon; field_types; instances })
    |> mem "user_fields"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.user_fields)
    |> mem "location_fields"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.location_fields)
    |> mem "icon" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun t -> t.icon)
    |> mem "field_types"
         (string_map_of field_type_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.field_types)
    |> mem "instances"
         (Jsont.list protocol_instance_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.instances)
    |> finish)

(* The reply is the protocol map itself, with no wrapper member. *)
let protocols_jsont = string_map_of protocol_jsont

let protocols client =
  let* body = Client.Http.get client ~path:"/thirdparty/protocols" () in
  Client.Http.decode_response protocols_jsont body

let protocol_path = Route.v "/thirdparty/protocol/{protocol}"

let get_protocol client ~name =
  let path = Route.expand_exn protocol_path [ ("protocol", name) ] in
  let* body = Client.Http.get client ~path () in
  Client.Http.decode_response protocol_jsont body

type location = {
  alias : Matrix_proto.Id.Room_alias.t;
  protocol : string;
  fields : (string * string) list;
}

let location_jsont =
  Jsont.Object.(
    map ~kind:"third_party_location" (fun alias protocol fields ->
        { alias; protocol; fields })
    |> mem "alias" Matrix_proto.Id.Room_alias.jsont ~enc:(fun t -> t.alias)
    |> mem "protocol" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun (t : location) -> t.protocol)
    |> mem "fields" fields_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : location) -> t.fields)
    |> finish)

let locations_jsont = Jsont.list location_jsont
let location_path = Route.v "/thirdparty/location/{protocol}"

let locations_of_alias client ~alias =
  let query = [ ("alias", Matrix_proto.Id.Room_alias.to_string alias) ] in
  let* body = Client.Http.get client ~path:"/thirdparty/location" ~query () in
  Client.Http.decode_response locations_jsont body

let locations client ~protocol ?(fields = []) () =
  let path = Route.expand_exn location_path [ ("protocol", protocol) ] in
  (* The protocol's own field names become query parameters directly. *)
  let* body = Client.Http.get client ~path ~query:fields () in
  Client.Http.decode_response locations_jsont body

type user = {
  userid : Matrix_proto.Id.User_id.t;
  protocol : string;
  fields : (string * string) list;
}

let user_jsont =
  Jsont.Object.(
    map ~kind:"third_party_user" (fun userid protocol fields ->
        { userid; protocol; fields })
    |> mem "userid" Matrix_proto.Id.User_id.jsont ~enc:(fun t -> t.userid)
    |> mem "protocol" Matrix_proto.Json.Codec.string
         ~dec_absent:(fun () -> "")
         ~enc:(fun (t : user) -> t.protocol)
    |> mem "fields" fields_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : user) -> t.fields)
    |> finish)

let users_jsont = Jsont.list user_jsont
let user_path = Route.v "/thirdparty/user/{protocol}"

let users_of_user_id client ~user_id =
  let query = [ ("userid", Matrix_proto.Id.User_id.to_string user_id) ] in
  let* body = Client.Http.get client ~path:"/thirdparty/user" ~query () in
  Client.Http.decode_response users_jsont body

let users client ~protocol ?(fields = []) () =
  let path = Route.expand_exn user_path [ ("protocol", protocol) ] in
  let* body = Client.Http.get client ~path ~query:fields () in
  Client.Http.decode_response users_jsont body

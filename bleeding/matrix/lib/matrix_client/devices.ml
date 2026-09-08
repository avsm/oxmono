open Result.Syntax
module Id = Matrix_proto.Id

type device = {
  device_id : Id.Device_id.t;
  display_name : string option;
  last_seen_ip : string option;
  last_seen_ts : Matrix_proto.Event.Timestamp.t option;
}

let device_jsont =
  Jsont.Object.(
    map ~kind:"device" (fun device_id display_name last_seen_ip last_seen_ts ->
        { device_id; display_name; last_seen_ip; last_seen_ts })
    |> mem "device_id" Id.Device_id.jsont ~enc:(fun t -> t.device_id)
    (* Homeservers commonly write explicit [null] for values that have never
       been observed.  Keep the optional record fields useful for both that
       representation and the spec's omitted representation. *)
    |> mem "display_name"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.display_name)
    |> mem "last_seen_ip"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.last_seen_ip)
    |> mem "last_seen_ts"
         (Jsont.option Matrix_proto.Event.Timestamp.jsont)
         ~dec_absent:(fun () -> None)
         ~enc:(fun t -> t.last_seen_ts)
    |> finish)

let devices_response_jsont =
  Jsont.Object.(
    map ~kind:"devices" Fun.id
    |> mem "devices" (Jsont.list device_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:Fun.id
    |> finish)

let device_path_route = Route.v "/devices/{device_id}"

let device_path device_id =
  Route.expand_exn device_path_route
    [ ("device_id", Id.Device_id.to_string device_id) ]

let get_devices client =
  let* body = Client.Http.get client ~path:"/devices" () in
  Client.Http.decode_response devices_response_jsont body

let get_device client ~device_id =
  let* body = Client.Http.get client ~path:(device_path device_id) () in
  Client.Http.decode_response device_jsont body

let update_device_request_jsont =
  Jsont.Object.(
    map ~kind:"update_device" Fun.id
    |> mem "display_name" Matrix_proto.Json.Codec.string ~enc:Fun.id
    |> finish)

let update_device client ~device_id ~display_name =
  let* body =
    Client.Http.encode_body update_device_request_jsont display_name
  in
  let+ _ = Client.Http.put client ~path:(device_path device_id) ~body () in
  ()

(* Neither delete drives the UIAA flow: a server that demands it answers 401
   with a flow description, which surfaces as an error. *)
let delete_device client ~device_id =
  let+ _ = Client.Http.delete client ~path:(device_path device_id) () in
  ()

let delete_devices_request_jsont =
  Jsont.Object.(
    map ~kind:"delete_devices" Fun.id
    |> mem "devices" (Jsont.list Id.Device_id.jsont) ~enc:Fun.id
    |> finish)

let delete_devices client ~device_ids =
  let* body = Client.Http.encode_body delete_devices_request_jsont device_ids in
  let+ _ = Client.Http.post client ~path:"/delete_devices" ~body () in
  ()

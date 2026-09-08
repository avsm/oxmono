open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

type call_id = string
type party_id = string

let call_id_of_string s = s
let party_id_of_string s = s

(* Call and party identifiers need the same unpredictable uniqueness as a
   transaction identifier, so they share its generator. *)
let generate_call_id client = Random.txn_id (Client.random client)
let generate_party_id client = Random.txn_id (Client.random client)

let send_call_event_route =
  Route.v "/rooms/{room_id}/send/{event_type}/{transaction_id}"

(* Every call event is an ordinary timeline send, so they differ only in the
   event type and the content codec. *)
let send_call_event client ~room_id ~event_type jsont content =
  let path =
    Route.expand_exn send_call_event_route
      [
        ("room_id", Id.Room_id.to_string room_id);
        ("event_type", Event.Event_type.to_string event_type);
        ("transaction_id", Random.txn_id (Client.random client));
      ]
  in
  let* body = Client.Http.encode_body jsont content in
  let* resp_body = Client.Http.put client ~path ~body () in
  let+ resp =
    Client.Http.decode_response Messages.send_response_jsont resp_body
  in
  resp.Messages.event_id

let send_invite client ~room_id ~call_id ~party_id ~offer ~lifetime
    ?(version = 1) ?invitee () =
  let content : Event.Call_invite_content.t =
    {
      call_id;
      party_id = Some party_id;
      version;
      lifetime;
      offer;
      invitee = Option.map Id.User_id.to_string invitee;
    }
  in
  send_call_event client ~room_id ~event_type:Event.Event_type.Call_invite
    Event.Call_invite_content.jsont content

let send_candidates client ~room_id ~call_id ~party_id ~candidates
    ?(version = 1) () =
  let content : Event.Call_candidates_content.t =
    { call_id; party_id = Some party_id; version; candidates }
  in
  send_call_event client ~room_id ~event_type:Event.Event_type.Call_candidates
    Event.Call_candidates_content.jsont content

let send_answer client ~room_id ~call_id ~party_id ~answer ?(version = 1) () =
  let content : Event.Call_answer_content.t =
    { call_id; party_id = Some party_id; version; answer }
  in
  send_call_event client ~room_id ~event_type:Event.Event_type.Call_answer
    Event.Call_answer_content.jsont content

let send_hangup client ~room_id ~call_id ~party_id ?reason ?(version = 1) () =
  let content : Event.Call_hangup_content.t =
    { call_id; party_id = Some party_id; version; reason }
  in
  send_call_event client ~room_id ~event_type:Event.Event_type.Call_hangup
    Event.Call_hangup_content.jsont content

let send_reject client ~room_id ~call_id ~party_id ?(version = 1) () =
  (* [m.call.reject] has the same content as [m.call.hangup]. *)
  let content : Event.Call_hangup_content.t =
    { call_id; party_id = Some party_id; version; reason = None }
  in
  send_call_event client ~room_id ~event_type:Event.Event_type.Call_reject
    Event.Call_hangup_content.jsont content

type turn_server = {
  username : string;
  password : string;
  uris : string list;
  ttl : int;
}

let turn_server_jsont =
  Jsont.Object.(
    map ~kind:"turn_server" (fun username password uris ttl ->
        { username; password; uris; ttl })
    |> mem "username" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.username)
    |> mem "password" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.password)
    |> mem "uris"
         (Jsont.list Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.uris)
    |> mem "ttl" Matrix_proto.Json.Codec.int ~enc:(fun t -> t.ttl)
    |> finish)

let get_turn_server client =
  let* body = Client.Http.get client ~path:"/voip/turnServer" () in
  Client.Http.decode_response turn_server_jsont body

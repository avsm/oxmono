open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

type space_child = Directory.space_child = {
  child_id : Id.Room_id.t;
  content : Event.Space_child_content.t;
}

let hierarchy_jsont =
  Matrix_proto.Common.Page.jsont ~chunk:"rooms" Directory.room_summary_jsont

let hierarchy_route = Route.v "/_matrix/client/v1/rooms/{room_id}/hierarchy"
let state_route = Route.v "/rooms/{room_id}/state/{event_type}/{state_key}"

let get_hierarchy client ~space ?suggested_only ?limit ?max_depth ?from () =
  (* This endpoint lives under [/_matrix/client/v1], so it goes through the
     absolute-path helper rather than {!Client.Http.get}, which prefixes v3. *)
  let path =
    Route.expand_exn hierarchy_route [ ("room_id", Id.Room_id.to_string space) ]
  in
  let query =
    List.filter_map Fun.id
      [
        (match suggested_only with
        | Some true -> Some ("suggested_only", "true")
        | _ -> None);
        Option.map (fun l -> ("limit", string_of_int l)) limit;
        Option.map (fun d -> ("max_depth", string_of_int d)) max_depth;
        Option.map (fun f -> ("from", f)) from;
      ]
  in
  let* body, _content_type = Client.Http.get_bytes client ~path ~query () in
  Client.Http.decode_response hierarchy_jsont body

(* A child or parent link is an ordinary state event, so the reply is the
   event identifier {!Messages.send_response_jsont} already reads. *)
let put_link client ~room ~event_type ~state_key ~body =
  let path =
    Route.expand_exn state_route
      [
        ("room_id", Id.Room_id.to_string room);
        ("event_type", event_type);
        ("state_key", Id.Room_id.to_string state_key);
      ]
  in
  let* resp = Client.Http.put client ~path ~body () in
  let+ resp = Client.Http.decode_response Messages.send_response_jsont resp in
  resp.Messages.event_id

let add_child client ~space ~child ?(via = []) ?order ?(suggested = false) () =
  let content =
    Event.Space_child_content.make
      ?via:(if via = [] then None else Some via)
      ?order
      ?suggested:(if suggested then Some true else None)
      ()
  in
  let* body = Client.Http.encode_body Event.Space_child_content.jsont content in
  put_link client ~room:space ~event_type:"m.space.child" ~state_key:child ~body

(* A link is removed by replacing its state event with empty content; there is
   no DELETE for state. *)
let remove_child client ~space ~child =
  put_link client ~room:space ~event_type:"m.space.child" ~state_key:child
    ~body:"{}"

let set_parent client ~room ~parent ?(via = []) ?(canonical = false) () =
  let content =
    Event.Space_parent_content.make
      ?via:(if via = [] then None else Some via)
      ?canonical:(if canonical then Some true else None)
      ()
  in
  let* body =
    Client.Http.encode_body Event.Space_parent_content.jsont content
  in
  put_link client ~room ~event_type:"m.space.parent" ~state_key:parent ~body

let remove_parent client ~room ~parent =
  put_link client ~room ~event_type:"m.space.parent" ~state_key:parent
    ~body:"{}"

let space_room_type = "m.space"
let is_space (r : Directory.room_summary) = r.room_type = Some space_room_type

let create_space client ?name ?topic ?visibility ?invite () =
  Rooms.create client ?name ?topic ?visibility ?invite
    ~room_type:space_room_type ()

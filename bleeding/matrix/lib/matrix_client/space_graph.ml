module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

type edge = {
  parent : Id.Room_id.t;
  child : Id.Room_id.t;
  order : string option;
  timestamp : Event.Timestamp.t option;
}

type t = {
  nodes : (string, Id.Room_id.t) Hashtbl.t;
  parents : (string, Id.Room_id.t list) Hashtbl.t;
  children : (string, edge list) Hashtbl.t;
}

let key = Id.Room_id.to_string

let decode_child (event : Store.state_event) =
  Result.to_option
    (Jsont.Json.decode Event.Space_child_content.jsont event.content)

let decode_parent (event : Store.state_event) =
  Result.to_option
    (Jsont.Json.decode Event.Space_parent_content.jsont event.content)

let has_via via = Option.exists (( <> ) []) via
let compare_room_id left right = String.compare (key left) (key right)

let compare_optional_order left right =
  match (left, right) with
  | Some left, Some right -> String.compare left right
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

let compare_optional_timestamp left right =
  match (left, right) with
  | Some left, Some right -> Event.Timestamp.compare left right
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

let compare_edge left right =
  let by_order = compare_optional_order left.order right.order in
  if by_order <> 0 then by_order
  else
    let by_timestamp =
      compare_optional_timestamp left.timestamp right.timestamp
    in
    if by_timestamp <> 0 then by_timestamp
    else compare_room_id left.child right.child

let compare_candidate left right =
  let by_parent = compare_room_id left.parent right.parent in
  if by_parent <> 0 then by_parent else compare_room_id left.child right.child

let candidate_edges state nodes rooms =
  let is_node room_id = Hashtbl.mem nodes (key room_id) in
  List.concat_map
    (fun (room : Store.room_info) ->
      Store.state_events_of_type room Event.Event_type.Space_child
      |> List.filter_map (fun (child_event : Store.state_event) ->
          let child_id =
            Result.to_option (Id.Room_id.of_string child_event.state_key)
          in
          match (child_id, decode_child child_event) with
          | Some child, Some child_content
            when is_node child
                 && has_via (Event.Space_child_content.via child_content) -> (
              match
                Base_client.find_state_event state child
                  ~event_type:Event.Event_type.Space_parent
                  ~state_key:(key room.room_id) ()
              with
              | Some parent_event -> (
                  match decode_parent parent_event with
                  | Some parent_content
                    when has_via (Event.Space_parent_content.via parent_content)
                    ->
                      Some
                        {
                          parent = room.room_id;
                          child;
                          order = Event.Space_child_content.order child_content;
                          timestamp = child_event.origin_server_ts;
                        }
                  | Some _ | None -> None)
              | None -> None)
          | Some _, Some _ | Some _, None | None, _ -> None))
    rooms
  |> List.sort compare_candidate

let path_exists children ~from ~target =
  let seen = Hashtbl.create 16 in
  let rec visit room =
    Id.Room_id.equal room target
    ||
    let room_key = key room in
    if Hashtbl.mem seen room_key then false
    else (
      Hashtbl.add seen room_key ();
      Option.value (Hashtbl.find_opt children room_key) ~default:[]
      |> List.exists (fun edge -> visit edge.child))
  in
  visit from

let of_state state =
  let rooms = Base_client.rooms_with state Base_client.Joined in
  let nodes = Hashtbl.create (List.length rooms) in
  List.iter
    (fun (room : Store.room_info) ->
      Hashtbl.replace nodes (key room.room_id) room.room_id)
    rooms;
  let parents = Hashtbl.create (List.length rooms) in
  let children = Hashtbl.create (List.length rooms) in
  List.iter
    (fun edge ->
      (* Candidate order is stable. Keeping an edge only when the graph built so
         far has no child-to-parent path retains a deterministic maximal DAG. *)
      if not (path_exists children ~from:edge.child ~target:edge.parent) then (
        let child_key = key edge.child in
        let parent_key = key edge.parent in
        let old_parents =
          Option.value (Hashtbl.find_opt parents child_key) ~default:[]
        in
        Hashtbl.replace parents child_key (edge.parent :: old_parents);
        let old_children =
          Option.value (Hashtbl.find_opt children parent_key) ~default:[]
        in
        Hashtbl.replace children parent_key (edge :: old_children)))
    (candidate_edges state nodes rooms);
  Hashtbl.iter
    (fun child values ->
      Hashtbl.replace parents child (List.sort compare_room_id values))
    parents;
  Hashtbl.iter
    (fun parent values ->
      Hashtbl.replace children parent (List.sort compare_edge values))
    children;
  { nodes; parents; children }

let parents t room =
  Option.value (Hashtbl.find_opt t.parents (key room)) ~default:[]

let children t room =
  Option.value (Hashtbl.find_opt t.children (key room)) ~default:[]
  |> List.map (fun edge -> edge.child)

let roots t =
  Hashtbl.to_seq t.nodes |> List.of_seq
  |> List.filter_map (fun (room_key, room_id) ->
      match Hashtbl.find_opt t.parents room_key with
      | None | Some [] -> Some room_id
      | Some (_ :: _) -> None)
  |> List.sort compare_room_id

let flattened_subtree t room =
  if not (Hashtbl.mem t.nodes (key room)) then []
  else
    let seen = Hashtbl.create (Hashtbl.length t.nodes) in
    let rec visit room =
      let room_key = key room in
      if Hashtbl.mem seen room_key then []
      else (
        Hashtbl.add seen room_key ();
        room :: List.concat_map visit (children t room))
    in
    visit room

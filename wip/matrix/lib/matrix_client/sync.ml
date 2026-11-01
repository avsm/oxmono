type params = {
  filter : string option;
  since : string option;
  full_state : bool;
  set_presence : [ `Online | `Offline | `Unavailable ] option;
  timeout : int;
}

let default_params =
  {
    filter = None;
    since = None;
    full_state = false;
    set_presence = None;
    timeout = 30000;
  }

let presence_to_string = function
  | `Online -> "online"
  | `Offline -> "offline"
  | `Unavailable -> "unavailable"

let wire_presence = function
  | Some `Online | None -> None
  | Some ((`Offline | `Unavailable) as presence) -> Some presence

let query_of_params p =
  let add name value q =
    match value with Some v -> (name, v) :: q | None -> q
  in
  [ ("timeout", string_of_int p.timeout) ]
  |> add "filter" p.filter |> add "since" p.since
  |> (fun q -> if p.full_state then ("full_state", "true") :: q else q)
  |> add "set_presence"
       (Option.map presence_to_string (wire_presence p.set_presence))

let sync_once client ?(params = default_params) () =
  let params =
    match params.set_presence with
    | Some _ -> params
    | None -> { params with set_presence = Some (Client.sync_presence client) }
  in
  match
    Client.Http.get client ~path:"/sync" ~query:(query_of_params params) ()
  with
  | Error e -> Error e
  | Ok body -> Client.Http.decode_response Matrix_proto.Sync.Response.jsont body

module Filter = struct
  type event = {
    limit : int option;
    not_senders : string list;
    not_types : string list;
    senders : string list;
    types : string list;
  }

  type room_event = {
    limit : int option;
    not_senders : string list;
    not_types : string list;
    senders : string list;
    types : string list;
    lazy_load_members : bool;
    include_redundant_members : bool;
    not_rooms : string list;
    rooms : string list;
    contains_url : bool option;
  }

  type room = {
    not_rooms : string list;
    rooms : string list;
    ephemeral : room_event option;
    include_leave : bool;
    state : room_event option;
    timeline : room_event option;
    account_data : room_event option;
  }

  type t = {
    event_fields : string list;
    event_format : [ `Client | `Federation ];
    presence : event option;
    account_data : event option;
    room : room option;
  }

  let default_event =
    { limit = None; not_senders = []; not_types = []; senders = []; types = [] }

  let default_room_event =
    {
      limit = None;
      not_senders = [];
      not_types = [];
      senders = [];
      types = [];
      lazy_load_members = true;
      include_redundant_members = false;
      not_rooms = [];
      rooms = [];
      contains_url = None;
    }

  let default_room =
    {
      not_rooms = [];
      rooms = [];
      ephemeral = None;
      include_leave = false;
      state = None;
      timeline = None;
      account_data = None;
    }

  let default =
    {
      event_fields = [];
      event_format = `Client;
      presence = None;
      account_data = None;
      room = None;
    }

  let strings = Jsont.list Matrix_proto.Json.Codec.string

  let event_jsont : event Jsont.t =
    let open Jsont.Object in
    map (fun limit not_senders not_types senders types ->
        ({ limit; not_senders; not_types; senders; types } : event))
    |> opt_mem "limit" Matrix_proto.Json.Codec.int ~enc:(fun (t : event) ->
        t.limit)
    |> mem "not_senders" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : event) -> t.not_senders)
    |> mem "not_types" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : event) -> t.not_types)
    |> mem "senders" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : event) -> t.senders)
    |> mem "types" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : event) -> t.types)
    |> finish

  let room_event_jsont : room_event Jsont.t =
    let open Jsont.Object in
    map
      (fun
        limit
        not_senders
        not_types
        senders
        types
        lazy_load_members
        include_redundant_members
        not_rooms
        rooms
        contains_url
      ->
        ({
           limit;
           not_senders;
           not_types;
           senders;
           types;
           lazy_load_members;
           include_redundant_members;
           not_rooms;
           rooms;
           contains_url;
         }
          : room_event))
    |> opt_mem "limit" Matrix_proto.Json.Codec.int ~enc:(fun (t : room_event) ->
        t.limit)
    |> mem "not_senders" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room_event) -> t.not_senders)
    |> mem "not_types" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room_event) -> t.not_types)
    |> mem "senders" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room_event) -> t.senders)
    |> mem "types" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room_event) -> t.types)
    |> mem "lazy_load_members" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (t : room_event) -> t.lazy_load_members)
    |> mem "include_redundant_members" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (t : room_event) -> t.include_redundant_members)
    |> mem "not_rooms" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room_event) -> t.not_rooms)
    |> mem "rooms" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room_event) -> t.rooms)
    |> opt_mem "contains_url" Jsont.bool ~enc:(fun (t : room_event) ->
        t.contains_url)
    |> finish

  let room_jsont : room Jsont.t =
    let open Jsont.Object in
    map
      (fun
        not_rooms rooms ephemeral include_leave state timeline account_data ->
        ({
           not_rooms;
           rooms;
           ephemeral;
           include_leave;
           state;
           timeline;
           account_data;
         }
          : room))
    |> mem "not_rooms" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room) -> t.not_rooms)
    |> mem "rooms" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : room) -> t.rooms)
    |> opt_mem "ephemeral" room_event_jsont ~enc:(fun (t : room) -> t.ephemeral)
    |> mem "include_leave" Jsont.bool
         ~dec_absent:(fun () -> false)
         ~enc:(fun (t : room) -> t.include_leave)
    |> opt_mem "state" room_event_jsont ~enc:(fun (t : room) -> t.state)
    |> opt_mem "timeline" room_event_jsont ~enc:(fun (t : room) -> t.timeline)
    |> opt_mem "account_data" room_event_jsont ~enc:(fun (t : room) ->
        t.account_data)
    |> finish

  let event_format_jsont : [ `Client | `Federation ] Jsont.t =
    Jsont.of_of_string ~kind:"event_format"
      ~enc:(function `Client -> "client" | `Federation -> "federation")
      (function
        | "client" -> Ok `Client
        | "federation" -> Ok `Federation
        | s -> Error ("Unknown event_format: " ^ s))

  let jsont : t Jsont.t =
    let open Jsont.Object in
    map (fun event_fields event_format presence account_data room ->
        ({ event_fields; event_format; presence; account_data; room } : t))
    |> mem "event_fields" strings
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : t) -> t.event_fields)
    |> mem "event_format" event_format_jsont
         ~dec_absent:(fun () -> `Client)
         ~enc:(fun (t : t) -> t.event_format)
    |> opt_mem "presence" event_jsont ~enc:(fun (t : t) -> t.presence)
    |> opt_mem "account_data" event_jsont ~enc:(fun (t : t) -> t.account_data)
    |> opt_mem "room" room_jsont ~enc:(fun (t : t) -> t.room)
    |> finish

  type response = { filter_id : string }

  let response_jsont =
    Jsont.Object.(
      map (fun filter_id -> { filter_id })
      |> mem "filter_id" Matrix_proto.Json.Codec.string ~enc:(fun r ->
          r.filter_id)
      |> finish)

  let filter_path_route = Route.v "/user/{user_id}/filter"
  let filter_id_path_route = Route.v "/user/{user_id}/filter/{filter_id}"

  let user_path client ?filter_id () =
    match Client.session client with
    | None -> Error Error.No_session
    | Some { user_id; _ } ->
        let bindings =
          [ ("user_id", Matrix_proto.Id.User_id.to_string user_id) ]
        in
        Ok
          (match filter_id with
          | None -> Route.expand_exn filter_path_route bindings
          | Some filter_id ->
              Route.expand_exn filter_id_path_route
                (("filter_id", filter_id) :: bindings))

  let create client ~filter =
    match user_path client () with
    | Error e -> Error e
    | Ok path -> (
        match Client.Http.encode_body jsont filter with
        | Error e -> Error e
        | Ok body -> (
            match Client.Http.post client ~path ~body () with
            | Error e -> Error e
            | Ok body -> (
                match Client.Http.decode_response response_jsont body with
                | Error e -> Error e
                | Ok resp -> Ok resp.filter_id)))

  let get client ~filter_id =
    match user_path client ~filter_id () with
    | Error e -> Error e
    | Ok path -> (
        match Client.Http.get client ~path () with
        | Error e -> Error e
        | Ok body -> Client.Http.decode_response jsont body)
end

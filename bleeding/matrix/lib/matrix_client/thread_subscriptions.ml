open Result.Syntax
module Id = Matrix_proto.Id

type status = { automatic : bool }

type subscription = {
  room_id : Id.Room_id.t;
  thread_root : Id.Event_id.t;
  automatic : bool;
  bump_stamp : int64;
}

type unsubscription = {
  room_id : Id.Room_id.t;
  thread_root : Id.Event_id.t;
  bump_stamp : int64;
}

type page = {
  subscribed : subscription list;
  unsubscribed : unsubscription list;
  end_token : string option;
}

type stored_status = Manual | Automatic | Unsubscribed
type stored_subscription = { status : stored_status; bump_stamp : int64 option }

type update = {
  room_id : Id.Room_id.t;
  thread_root : Id.Event_id.t;
  subscription : stored_subscription;
}

type catchup_token = { from_ : string; to_ : string option }
type subscription_value = { automatic : bool; bump_stamp : int64 }
type unsubscription_value = { bump_stamp : int64 }

type page_wire = {
  subscribed_wire :
    (Id.Room_id.t * (Id.Event_id.t * subscription_value) list) list;
  unsubscribed_wire :
    (Id.Room_id.t * (Id.Event_id.t * unsubscription_value) list) list;
  end_wire : string option;
}

let msc4306_base = "/_matrix/client/unstable/io.element.msc4306"
let msc4308_base = "/_matrix/client/unstable/io.element.msc4308"

let subscription_route =
  Route.v (msc4306_base ^ "/rooms/{room_id}/thread/{thread_root}/subscription")

let changes_route = Route.v (msc4308_base ^ "/thread_subscriptions")
let max_safe_integer = 9007199254740991L
let uint_jsont = Matrix_proto.Json.Codec.uint64

let is_supported client =
  let+ versions = Server.get_versions client in
  Server.has_unstable_feature versions "org.matrix.msc4306"

let stored_status_jsont =
  Jsont.enum
    [
      ("manual", Manual);
      ("automatic", Automatic);
      ("unsubscribed", Unsubscribed);
    ]

let bump_stamp_jsont = uint_jsont

let persisted_bump_stamp_jsont =
  let validate value =
    if value < 0L || value > max_safe_integer then
      Jsont.Error.msg Jsont.Meta.none
        "persisted thread subscription bump stamp is not a non-negative \
         JavaScript-safe integer"
  in
  Jsont.iter ~dec:validate ~enc:validate Matrix_proto.Json.Codec.Legacy.int64

type stored_entry = {
  entry_room_id : Id.Room_id.t;
  entry_thread_root : Id.Event_id.t;
  entry_subscription : stored_subscription;
}

type stored_state = {
  format_version : int;
  entries : stored_entry list;
  catchup_tokens : catchup_token list;
}

let stored_subscription_jsont =
  Jsont.Object.(
    map (fun status bump_stamp -> { status; bump_stamp })
    |> mem "status" stored_status_jsont ~enc:(fun (t : stored_subscription) ->
        t.status)
    |> opt_mem "bump_stamp" persisted_bump_stamp_jsont
         ~enc:(fun (t : stored_subscription) -> t.bump_stamp)
    |> finish)

let stored_entry_jsont =
  Jsont.Object.(
    map (fun room_id thread_root subscription ->
        {
          entry_room_id = room_id;
          entry_thread_root = thread_root;
          entry_subscription = subscription;
        })
    |> mem "room_id" Id.Room_id.jsont ~enc:(fun (t : stored_entry) ->
        t.entry_room_id)
    |> mem "thread_root" Id.Event_id.jsont ~enc:(fun (t : stored_entry) ->
        t.entry_thread_root)
    |> mem "subscription" stored_subscription_jsont
         ~enc:(fun (t : stored_entry) -> t.entry_subscription)
    |> finish)

let catchup_token_jsont =
  Jsont.Object.(
    map (fun from_ to_ -> { from_; to_ })
    |> mem "from" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : catchup_token) -> t.from_)
    |> opt_mem "to" Matrix_proto.Json.Codec.string
         ~enc:(fun (t : catchup_token) -> t.to_)
    |> finish)

let stored_state_jsont =
  let format_version_jsont =
    Jsont.iter
      ~dec:(fun version ->
        if version <> 1 then
          Jsont.Error.msg Jsont.Meta.none
            "unsupported thread subscription store format version")
      Matrix_proto.Json.Codec.Legacy.int
  in
  Jsont.Object.(
    map (fun format_version entries catchup_tokens ->
        { format_version; entries; catchup_tokens })
    |> mem "format_version" format_version_jsont
         ~dec_absent:(fun () -> 1)
         ~enc:(fun (t : stored_state) -> t.format_version)
    |> mem "entries"
         (Jsont.list stored_entry_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : stored_state) -> t.entries)
    |> mem "catchup_tokens"
         (Jsont.list catchup_token_jsont)
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : stored_state) -> t.catchup_tokens)
    |> finish)

let store_slot = Store.Slot.v ~name:"thread_subscriptions" stored_state_jsont

let key room_id thread_root =
  (Id.Room_id.to_string room_id, Id.Event_id.to_string thread_root)

let normalize_entries entries =
  let module M = Map.Make (struct
    type t = string * string

    let compare = compare
  end) in
  let entries =
    List.fold_left
      (fun map ({ entry_room_id; entry_thread_root; _ } as entry) ->
        M.add (key entry_room_id entry_thread_root) entry map)
      M.empty entries
  in
  M.bindings entries |> List.map snd

let normalize_tokens tokens =
  List.fold_left
    (fun kept token ->
      if List.exists (fun previous -> previous = token) kept then kept
      else kept @ [ token ])
    [] tokens

let normalize_state state =
  {
    state with
    entries = normalize_entries state.entries;
    catchup_tokens = normalize_tokens state.catchup_tokens;
  }

let state_result store =
  let+ state = Store.Slot.find store store_slot in
  Option.value state
    ~default:{ format_version = 1; entries = []; catchup_tokens = [] }
  |> normalize_state

let save_state store state =
  let* previous = Store.Slot.find store store_slot in
  let* () = Store.Slot.set store store_slot (normalize_state state) in
  let rollback () =
    match previous with
    | None ->
        Store.Slot.remove store store_slot;
        Ok ()
    | Some previous -> Store.Slot.set store store_slot previous
  in
  let format_rollback_error original rollback_error =
    Error.Policy_denied
      (Printf.sprintf
         "thread subscription flush failed: %s; rollback failed: %s"
         (Error.to_string original)
         (Error.to_string rollback_error))
  in
  match Store.flush store with
  | Ok () -> Ok ()
  | Error error -> (
      match rollback () with
      | Ok () -> Error error
      | Error rollback_error ->
          Error (format_rollback_error error rollback_error))
  | exception exn -> (
      let bt = Printexc.get_raw_backtrace () in
      match rollback () with
      | Ok () -> Printexc.raise_with_backtrace exn bt
      | Error rollback_error ->
          Logs.err (fun m ->
              m "thread subscription rollback failed after exception: %s"
                (Error.to_string rollback_error));
          Printexc.raise_with_backtrace exn bt)

let stored_of_entry entry =
  (entry.entry_room_id, entry.entry_thread_root, entry.entry_subscription)

let subscriptions store =
  let+ state = state_result store in
  List.map stored_of_entry state.entries

let find_stored store ~room_id ~thread_root =
  let+ entries = subscriptions store in
  List.find_map
    (fun (room, root, subscription) ->
      if
        fst (key room root) = fst (key room_id thread_root)
        && snd (key room root) = snd (key room_id thread_root)
      then Some subscription
      else None)
    entries

let merge ~(previous : stored_subscription option)
    (new_subscription : stored_subscription) =
  match (previous, new_subscription.bump_stamp) with
  | Some previous, None ->
      Some { new_subscription with bump_stamp = previous.bump_stamp }
  | Some previous, Some new_stamp -> (
      match previous.bump_stamp with
      | Some previous_stamp when new_stamp <= previous_stamp -> None
      | _ -> Some new_subscription)
  | None, _ -> Some new_subscription

let upsert_many store updates =
  let* state = state_result store in
  let entries =
    List.fold_left
      (fun entries ({ room_id; thread_root; subscription } : update) ->
        let k = key room_id thread_root in
        let previous =
          List.find_map
            (fun entry ->
              if key entry.entry_room_id entry.entry_thread_root = k then
                Some entry.entry_subscription
              else None)
            entries
        in
        match merge ~previous subscription with
        | None -> entries
        | Some subscription ->
            let entries =
              List.filter
                (fun entry ->
                  key entry.entry_room_id entry.entry_thread_root <> k)
                entries
            in
            {
              entry_room_id = room_id;
              entry_thread_root = thread_root;
              entry_subscription = subscription;
            }
            :: entries)
      state.entries updates
  in
  save_state store { state with entries }

let upsert store ~room_id ~thread_root subscription =
  upsert_many store [ { room_id; thread_root; subscription } ]

let remove store ~room_id ~thread_root =
  let* state = state_result store in
  let k = key room_id thread_root in
  let entries =
    List.filter
      (fun entry -> key entry.entry_room_id entry.entry_thread_root <> k)
      state.entries
  in
  if List.length entries = List.length state.entries then Ok ()
  else save_state store { state with entries }

let remove_room store ~room_id =
  let* state = state_result store in
  let room = Id.Room_id.to_string room_id in
  let entries =
    List.filter
      (fun entry -> Id.Room_id.to_string entry.entry_room_id <> room)
      state.entries
  in
  if List.length entries = List.length state.entries then Ok ()
  else save_state store { state with entries }

let catchup_tokens store =
  let+ state = state_result store in
  state.catchup_tokens

let queue_catchup_token store ~from_ ~to_ =
  let* state = state_result store in
  let token = { from_; to_ } in
  if List.exists (fun existing -> existing = token) state.catchup_tokens then
    Ok ()
  else
    save_state store
      { state with catchup_tokens = state.catchup_tokens @ [ token ] }

let subscription_path ~room_id ~thread_root =
  Route.expand_exn subscription_route
    [
      ("room_id", Id.Room_id.to_string room_id);
      ("thread_root", Id.Event_id.to_string thread_root);
    ]

let status_jsont =
  Jsont.Object.(
    map (fun automatic -> ({ automatic } : status))
    |> mem "automatic" Jsont.bool ~enc:(fun (t : status) -> t.automatic)
    |> finish)

let subscription_value_jsont =
  Jsont.Object.(
    map (fun automatic bump_stamp ->
        ({ automatic; bump_stamp } : subscription_value))
    |> mem "automatic" Jsont.bool ~enc:(fun (t : subscription_value) ->
        t.automatic)
    |> mem "bump_stamp" uint_jsont ~enc:(fun (t : subscription_value) ->
        t.bump_stamp)
    |> finish)

let unsubscription_value_jsont =
  Jsont.Object.(
    map (fun bump_stamp -> ({ bump_stamp } : unsubscription_value))
    |> mem "bump_stamp" uint_jsont ~enc:(fun (t : unsubscription_value) ->
        t.bump_stamp)
    |> finish)

let subscribed_map_jsont =
  Json_codec.keyed_map ~what:"thread subscription room id"
    ~of_string:Id.Room_id.of_string ~to_string:Id.Room_id.to_string
    (Json_codec.keyed_map ~what:"thread subscription event id"
       ~of_string:Id.Event_id.of_string ~to_string:Id.Event_id.to_string
       subscription_value_jsont)

let unsubscribed_map_jsont =
  Json_codec.keyed_map ~what:"thread unsubscription room id"
    ~of_string:Id.Room_id.of_string ~to_string:Id.Room_id.to_string
    (Json_codec.keyed_map ~what:"thread unsubscription event id"
       ~of_string:Id.Event_id.of_string ~to_string:Id.Event_id.to_string
       unsubscription_value_jsont)

let page_wire_jsont =
  Jsont.Object.(
    map (fun subscribed unsubscribed end_token ->
        ({
           subscribed_wire = subscribed;
           unsubscribed_wire = unsubscribed;
           end_wire = end_token;
         }
          : page_wire))
    |> mem "subscribed" subscribed_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : page_wire) -> t.subscribed_wire)
    |> mem "unsubscribed" unsubscribed_map_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun (t : page_wire) -> t.unsubscribed_wire)
    |> mem "end"
         (Jsont.option Matrix_proto.Json.Codec.string)
         ~dec_absent:(fun () -> None)
         ~enc:(fun (t : page_wire) -> t.end_wire)
    |> finish)

let flatten_subscription values =
  List.concat_map
    (fun (room_id, threads) ->
      List.map
        (fun (thread_root, value) ->
          {
            room_id;
            thread_root;
            automatic = value.automatic;
            bump_stamp = value.bump_stamp;
          })
        threads)
    values

let flatten_unsubscription values =
  List.concat_map
    (fun (room_id, threads) ->
      List.map
        (fun (thread_root, value) ->
          { room_id; thread_root; bump_stamp = value.bump_stamp })
        threads)
    values

let get client ~room_id ~thread_root =
  let result =
    let* body =
      Client.Http.get_absolute client
        ~path:(subscription_path ~room_id ~thread_root)
        ()
    in
    Client.Http.decode_response status_jsont body
  in
  match result with
  | Ok status -> Ok (Some status)
  | Error (Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ }) -> Ok None
  | Error error -> Error error

let subscribe client ~room_id ~thread_root ?automatic () =
  let json =
    match automatic with
    | None -> Json_codec.obj []
    | Some event_id ->
        Json_codec.obj
          [ ("automatic", Jsont.Json.string (Id.Event_id.to_string event_id)) ]
  in
  let* body = Client.Http.encode_body Matrix_proto.Json.Codec.json json in
  let* _ =
    Client.Http.put_absolute client
      ~path:(subscription_path ~room_id ~thread_root)
      ~body ()
  in
  Ok ()

let unsubscribe client ~room_id ~thread_root () =
  let* _ =
    Client.Http.delete_absolute client
      ~path:(subscription_path ~room_id ~thread_root)
      ()
  in
  Ok ()

let changes client ?from ?to_ ?limit () =
  Option.iter
    (fun value ->
      if value < 0 || Int64.of_int value > max_safe_integer then
        invalid_arg
          "Matrix_client.Thread_subscriptions.changes: limit is not a \
           non-negative JavaScript-safe integer")
    limit;
  let query =
    List.filter_map Fun.id
      [
        Some ("dir", "b");
        Option.map (fun value -> ("from", value)) from;
        Option.map (fun value -> ("to", value)) to_;
        Option.map (fun value -> ("limit", string_of_int value)) limit;
      ]
  in
  let* body =
    Client.Http.get_absolute client
      ~path:(Route.expand_exn changes_route [])
      ~query ()
  in
  let* { subscribed_wire; unsubscribed_wire; end_wire } =
    Client.Http.decode_response page_wire_jsont body
  in
  Ok
    {
      subscribed = flatten_subscription subscribed_wire;
      unsubscribed = flatten_unsubscription unsubscribed_wire;
      end_token = end_wire;
    }

let update_of_subscription (item : subscription) =
  {
    room_id = item.room_id;
    thread_root = item.thread_root;
    subscription =
      {
        status = (if item.automatic then Automatic else Manual);
        bump_stamp = Some item.bump_stamp;
      };
  }

let update_of_unsubscription (item : unsubscription) =
  {
    room_id = item.room_id;
    thread_root = item.thread_root;
    subscription = { status = Unsubscribed; bump_stamp = Some item.bump_stamp };
  }

let apply_page store page =
  upsert_many store
    (List.map update_of_subscription page.subscribed
    @ List.map update_of_unsubscription page.unsubscribed)

let advance_token store token end_token =
  let* state = state_result store in
  let tokens =
    match end_token with
    | Some from_ ->
        List.map
          (fun existing ->
            if existing = token then { from_; to_ = token.to_ } else existing)
          state.catchup_tokens
    | None ->
        List.filter (fun existing -> existing <> token) state.catchup_tokens
  in
  save_state store { state with catchup_tokens = normalize_tokens tokens }

let catch_up_once client ~store =
  let* tokens = catchup_tokens store in
  match List.rev tokens with
  | [] -> Ok false
  | token :: _ ->
      let* page = changes client ~from:token.from_ ?to_:token.to_ () in
      (* The subscription state is committed before the token is advanced. If
         either flush fails, retaining the old token makes the request safely
         retryable after a restart. *)
      let* () = apply_page store page in
      let* () = advance_token store token page.end_token in
      Ok true

let catch_up client ~store =
  let rec loop () =
    let* processed = catch_up_once client ~store in
    if processed then loop () else Ok ()
  in
  loop ()

let subscribe_and_store client ~store ~room_id ~thread_root ?automatic () =
  let* () = subscribe client ~room_id ~thread_root ?automatic () in
  upsert store ~room_id ~thread_root
    {
      status = (match automatic with None -> Manual | Some _ -> Automatic);
      bump_stamp = None;
    }

let unsubscribe_and_store client ~store ~room_id ~thread_root () =
  let* () = unsubscribe client ~room_id ~thread_root () in
  upsert store ~room_id ~thread_root
    { status = Unsubscribed; bump_stamp = None }

let get_and_store client ~store ~room_id ~thread_root =
  let* result = get client ~room_id ~thread_root in
  match result with
  | Some wire_status ->
      let status = if wire_status.automatic then Automatic else Manual in
      let* () =
        upsert store ~room_id ~thread_root { status; bump_stamp = None }
      in
      Ok (Some wire_status)
  | None ->
      let* () = remove store ~room_id ~thread_root in
      Ok None

let load_or_fetch client ~store ~room_id ~thread_root =
  let* tokens = catchup_tokens store in
  if tokens <> [] then get_and_store client ~store ~room_id ~thread_root
  else
    let+ stored = find_stored store ~room_id ~thread_root in
    Option.bind stored (fun { status; _ } ->
        match status with
        | Manual -> Some { automatic = false }
        | Automatic -> Some { automatic = true }
        | Unsubscribed -> None)

let is_conflicting_unsubscription = function
  | Error.Matrix_error
      {
        errcode =
          Error.M_UNKNOWN_CODE "IO.ELEMENT.MSC4306.M_CONFLICTING_UNSUBSCRIPTION";
        _;
      } ->
      true
  | _ -> false

let subscribe_if_needed client ~store ~room_id ~thread_root ?automatic () =
  let* previous = load_or_fetch client ~store ~room_id ~thread_root in
  match previous with
  | Some previous when (not previous.automatic) || Option.is_some automatic ->
      Ok ()
  | _ -> (
      match
        subscribe_and_store client ~store ~room_id ~thread_root ?automatic ()
      with
      | Ok () -> Ok ()
      | Error error
        when Option.is_some automatic && is_conflicting_unsubscription error ->
          Ok ()
      | Error error -> Error error)

let apply_sliding_extension store ~previous_pos
    (extension : Matrix_proto.Sliding_sync.Response.thread_subscriptions) =
  let subscribed =
    List.concat_map
      (fun (room_id, threads) ->
        List.map
          (fun ( thread_root,
                 (item : Matrix_proto.Sliding_sync.Response.thread_subscription)
               ) ->
            {
              room_id;
              thread_root;
              subscription =
                {
                  status = (if item.automatic then Automatic else Manual);
                  bump_stamp = Some item.bump_stamp;
                };
            })
          threads)
      extension.subscribed
  in
  let unsubscribed =
    List.concat_map
      (fun (room_id, threads) ->
        List.map
          (fun ( thread_root,
                 (item :
                   Matrix_proto.Sliding_sync.Response.thread_unsubscription) )
             ->
            {
              room_id;
              thread_root;
              subscription =
                { status = Unsubscribed; bump_stamp = Some item.bump_stamp };
            })
          threads)
      extension.unsubscribed
  in
  (* Rust records a newly reported gap before applying the response changes.
     A caller must finish this operation before accepting the response's new
     sliding-sync position. *)
  let* () =
    match extension.prev_batch with
    | None -> Ok ()
    | Some from_ -> queue_catchup_token store ~from_ ~to_:previous_pos
  in
  match unsubscribed @ subscribed with
  | [] -> Ok ()
  | updates -> upsert_many store updates

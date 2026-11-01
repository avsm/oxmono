type t = Jsont.json

let of_json json =
  match json with
  | Jsont.Object _ -> Ok json
  | _ -> Error (Error.Invalid_request "Registration state must be an object")

let raw t = t

let field t name =
  match t with
  | Jsont.Object (members, _) ->
      Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

let decode_field t name codec =
  match field t name with
  | None -> Ok None
  | Some json -> Codec.decode codec json |> Result.map Option.some

let with_active active = function
  | Jsont.Object (members, meta) as raw ->
      if Option.is_some (Jsont.Json.find_mem "is_active" members) then raw
      else
        Jsont.Object
          ( (("is_active", Jsont.Meta.none), Jsont.Json.bool active) :: members,
            meta )
  | raw -> raw

let user_list active =
  Jsont.map ~kind:"Registration users"
    ~enc:(fun users ->
      List.map
        (fun user -> Jsont.Json.encode Zulip.User.jsont user |> Result.get_ok)
        users)
    ~dec:(fun users ->
      List.map
        (fun raw ->
          match Jsont.Json.decode Zulip.User.jsont (with_active active raw) with
          | Ok user -> user
          | Error message -> Jsont.Error.msgf Jsont.Meta.none "%s" message)
        users)
    (Jsont.list Jsont.json)

let users t = decode_field t "realm_users" (user_list true)
let inactive_users t = decode_field t "realm_non_active_users" (user_list false)
let cross_realm_bots t = decode_field t "cross_realm_bots" (user_list true)
let user_id t = decode_field t "user_id" Zulip.Id.User.jsont

let subscriptions t =
  decode_field t "subscriptions" (Jsont.list Zulip.Channel.Subscription.jsont)

let channels t = decode_field t "streams" (Jsont.list Zulip.Channel.jsont)
let alert_words t = decode_field t "alert_words" (Jsont.list Jsont.string)
let feature_level t = decode_field t "zulip_feature_level" Jsont.int
let version t = decode_field t "zulip_version" Jsont.string

let settings_jsont =
  Jsont.iter ~kind:"registration user settings"
    ~dec:(function
      | Jsont.Object _ -> ()
      | json -> Jsont.Json.error_sort ~exp:Jsont.Sort.Object json)
    Jsont.json

let settings t = decode_field t "user_settings" settings_jsont

let setting t name codec =
  match settings t with
  | Error e -> Error e
  | Ok None -> Ok None
  | Ok (Some settings) -> decode_field settings name codec

type muted_user = { id : Zulip.Id.User.t; timestamp : float }

let muted_user_jsont =
  Jsont.Object.map (fun id timestamp -> { id; timestamp })
  |> Jsont.Object.mem "id" Zulip.Id.User.jsont ~enc:(fun m -> m.id)
  |> Jsont.Object.mem "timestamp" Jsont.number ~enc:(fun m -> m.timestamp)
  |> Jsont.Object.finish

let muted_users t = decode_field t "muted_users" (Jsont.list muted_user_jsont)

type topic = {
  channel_id : Zulip.Id.Channel.t;
  name : string;
  updated : float;
  visibility : Zulip.Topic_visibility.t;
}

let topic_jsont =
  Jsont.Object.map (fun channel_id name updated visibility ->
      { channel_id; name; updated; visibility })
  |> Jsont.Object.mem "stream_id" Zulip.Id.Channel.jsont ~enc:(fun t ->
      t.channel_id)
  |> Jsont.Object.mem "topic_name" Jsont.string ~enc:(fun t -> t.name)
  |> Jsont.Object.mem "last_updated" Jsont.number ~enc:(fun t -> t.updated)
  |> Jsont.Object.mem "visibility_policy" Zulip.Topic_visibility.jsont
       ~enc:(fun t -> t.visibility)
  |> Jsont.Object.finish

let topics t = decode_field t "user_topics" (Jsont.list topic_jsont)

let presence_map_jsont =
  let module Map = Jsont.String_map in
  Jsont.map
    ~dec:(fun map ->
      List.rev (Map.fold (fun key value acc -> (key, value) :: acc) map []))
    ~enc:(fun xs ->
      List.fold_left
        (fun map (key, value) -> Map.add key value map)
        (Map.create ()) xs)
    (Jsont.Object.as_string_map Presence.user_presence_jsont)

let presences t = decode_field t "presences" presence_map_jsont

let presence_last_update_id t =
  decode_field t "presence_last_update_id" Jsont.int

let server_timestamp t = decode_field t "server_timestamp" Jsont.number

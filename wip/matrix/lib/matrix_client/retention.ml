open Result.Syntax
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event

type policy = Event.Room_retention_content.t

let policy_min_lifetime = Event.Room_retention_content.min_lifetime
let policy_max_lifetime = Event.Room_retention_content.max_lifetime

let check_bounds ~error min_lifetime max_lifetime =
  Option.iter
    (fun value -> if value < 0L then error "negative lifetime")
    min_lifetime;
  Option.iter
    (fun value -> if value < 0L then error "negative lifetime")
    max_lifetime;
  match (min_lifetime, max_lifetime) with
  | Some min, Some max when min > max -> error "minimum exceeds maximum"
  | _ -> ()

let policy ?min_lifetime ?max_lifetime () =
  check_bounds
    ~error:(fun msg -> invalid_arg ("Retention.policy: " ^ msg))
    min_lifetime max_lifetime;
  Event.Room_retention_content.make ?min_lifetime ?max_lifetime ()

let validate_policy_json policy =
  check_bounds
    ~error:(fun msg -> Jsont.Error.msg Jsont.Meta.none msg)
    (policy_min_lifetime policy)
    (policy_max_lifetime policy);
  policy

let policy_jsont : policy Jsont.t =
  Jsont.map ~dec:validate_policy_json ~enc:validate_policy_json
    Event.Room_retention_content.jsont

type lifetime_limits = {
  min_lifetime : int64 option;
  max_lifetime : int64 option;
}

type limits = {
  min_lifetime : lifetime_limits option;
  max_lifetime : lifetime_limits option;
}

type configuration = { limits : limits; policies : (string * policy) list }

let validate_limits_json (limits : lifetime_limits) =
  check_bounds
    ~error:(fun msg -> Jsont.Error.msg Jsont.Meta.none msg)
    limits.min_lifetime limits.max_lifetime;
  limits

let lifetime_limits_jsont : lifetime_limits Jsont.t =
  Jsont.Object.(
    map (fun (min_lifetime : int64 option) (max_lifetime : int64 option) ->
        ({ min_lifetime; max_lifetime } : lifetime_limits))
    |> opt_mem "min" Matrix_proto.Json.Codec.int64
         ~enc:(fun (t : lifetime_limits) -> t.min_lifetime)
    |> opt_mem "max" Matrix_proto.Json.Codec.int64
         ~enc:(fun (t : lifetime_limits) -> t.max_lifetime)
    |> finish)
  |> Jsont.map ~dec:validate_limits_json ~enc:validate_limits_json

let valid_policy_key key =
  String.equal key "*" || Result.is_ok (Id.Room_id.of_string key)

let validate_policies policies =
  List.iter
    (fun (key, _policy) ->
      if not (valid_policy_key key) then
        Jsont.Error.msg Jsont.Meta.none ("invalid retention policy key: " ^ key))
    policies;
  policies

let policies_jsont : (string * policy) list Jsont.t =
  Json_codec.string_map policy_jsont
  |> Jsont.map ~dec:validate_policies ~enc:validate_policies

let limits_jsont : limits Jsont.t =
  Jsont.Object.(
    map (fun min_lifetime max_lifetime -> { min_lifetime; max_lifetime })
    |> opt_mem "min_lifetime" lifetime_limits_jsont ~enc:(fun t ->
        t.min_lifetime)
    |> opt_mem "max_lifetime" lifetime_limits_jsont ~enc:(fun t ->
        t.max_lifetime)
    |> finish)

let configuration_jsont : configuration Jsont.t =
  Jsont.Object.(
    map (fun limits policies -> { limits; policies })
    |> mem "limits" limits_jsont
         ~dec_absent:(fun () -> { min_lifetime = None; max_lifetime = None })
         ~enc:(fun t -> t.limits)
    |> mem "policies" policies_jsont
         ~dec_absent:(fun () -> [])
         ~enc:(fun t -> t.policies)
    |> finish)

let get_configuration client =
  let path =
    "/_matrix/client/unstable/org.matrix.msc1763/retention/configuration"
  in
  let* body, _content_type = Client.Http.get_bytes client ~path () in
  Client.Http.decode_response configuration_jsont body

let decode_policy = function
  | Ok json -> Result.to_option (Jsont.Json.decode policy_jsont json)
  | Error _ -> None

let get_room_policy client ~room_id =
  let get event_type =
    match State.get_state_event client ~room_id ~event_type () with
    | Error (Error.Matrix_error { errcode = Error.M_NOT_FOUND; _ }) -> Ok None
    | Error e -> Error e
    | Ok json -> Ok (decode_policy (Ok json))
  in
  match get Event.Event_type.Room_retention with
  | Ok (Some _ as p) -> Ok p
  | Ok None -> get Event.Event_type.Room_retention_unstable
  | Error e -> Error e

let set_room_policy client ~room_id policy =
  match Jsont.Json.encode policy_jsont policy with
  | Error e -> Error (Error.Json_error e)
  | Ok content ->
      State.set_state client ~room_id
        ~event_type:Event.Event_type.Room_retention ~content ()

let clamp value (limits : lifetime_limits option) =
  match limits with
  | None -> value
  | Some limits -> (
      match value with
      | None -> limits.min_lifetime
      | Some value ->
          let min = Option.value limits.min_lifetime ~default:0L in
          let max = Option.value limits.max_lifetime ~default:Int64.max_int in
          Some (Int64.max min (Int64.min max value)))

let effective_policy ~room_id ~room_policy config =
  let room_key = Id.Room_id.to_string room_id in
  match List.assoc_opt room_key config.policies with
  | Some override -> Some override
  | None -> (
      match room_policy with
      | None -> List.assoc_opt "*" config.policies
      | Some room ->
          let max_lifetime =
            clamp (policy_max_lifetime room) config.limits.max_lifetime
          in
          let min_lifetime =
            clamp (policy_min_lifetime room) config.limits.min_lifetime
          in
          let min_lifetime =
            match (max_lifetime, min_lifetime) with
            | Some max, Some min -> Some (Int64.min max min)
            | _ -> min_lifetime
          in
          Some (policy ?min_lifetime ?max_lifetime ()))

let unsupported = function
  | Error.Matrix_error { errcode = Error.M_UNRECOGNIZED | Error.M_NOT_FOUND; _ }
    ->
      true
  | Error.Http_error { status = 404 | 501; _ } -> true
  | _ -> false

let effective client ~room_id =
  match get_configuration client with
  | Ok config ->
      let* room_policy = get_room_policy client ~room_id in
      Ok (effective_policy ~room_id ~room_policy config)
  | Error e when unsupported e -> Ok None
  | Error e -> Error e

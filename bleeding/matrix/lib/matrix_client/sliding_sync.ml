module Wire = Matrix_proto.Sliding_sync

let src = Logs.Src.create "matrix.sliding_sync" ~doc:"Simplified sliding sync"

module Log = (val Logs.src_log src : Logs.LOG)

let path = "/_matrix/client/unstable/org.matrix.simplified_msc3575/sync"
let default_timeout_ms = 30_000
let native_feature = "org.matrix.simplified_msc3575"

let is_available_in versions =
  Server.has_unstable_feature versions native_feature

let is_available client =
  let open Result.Syntax in
  let+ versions = Server.get_versions client in
  is_available_in versions

let presence_to_string = function
  | `Online -> "online"
  | `Offline -> "offline"
  | `Unavailable -> "unavailable"

let wire_presence = function
  | Some `Online | None -> None
  | Some ((`Offline | `Unavailable) as presence) -> Some presence

let unsupported_error =
  Error.Matrix_error
    {
      Error.errcode = Error.M_UNRECOGNIZED;
      error =
        "the homeserver does not implement simplified sliding sync (MSC4186) \
         at " ^ path;
      retry_after_ms = None;
      soft_logout = None;
    }

(* A homeserver without MSC4186 answers the unstable path with a plain 404,
   or with a Matrix error whose code is [M_UNRECOGNIZED]. Both become one
   recognisable value so a caller can fall back to [Sync] without having to
   know which shape its homeserver picked. *)
let normalise_error = function
  | Error.Http_error { status = 404; _ } -> unsupported_error
  | Error.Matrix_error { errcode = Error.M_UNRECOGNIZED; _ } ->
      unsupported_error
  | e -> e

let is_unsupported = function
  | Error.Matrix_error { errcode = Error.M_UNRECOGNIZED; _ } -> true
  | Error.Http_error { status = 404; _ } -> true
  | _ -> false

let is_expired_pos = function
  | Error.Matrix_error { errcode = Error.M_UNKNOWN_CODE "M_UNKNOWN_POS"; _ } ->
      true
  | _ -> false

let sync_once client ?pos ?(timeout_ms = default_timeout_ms) ?set_presence
    request =
  let set_presence =
    match set_presence with
    | Some _ -> set_presence
    | None -> Some (Client.sync_presence client)
  in
  match Client.Http.encode_body Wire.Request.jsont request with
  | Error _ as e -> e
  | Ok body -> (
      let query =
        (match pos with Some p -> [ ("pos", p) ] | None -> [])
        @ (match wire_presence set_presence with
          | Some p -> [ ("set_presence", presence_to_string p) ]
          | None -> [])
        @ [ ("timeout", string_of_int timeout_ms) ]
      in
      Log.debug (fun m ->
          m "POST %s (pos=%s timeout=%dms)" path
            (Option.value pos ~default:"<none>")
            timeout_ms);
      match
        Client.Http.post_bytes client ~path ~query
          ~content_type:"application/json" ~body ()
      with
      | Error e ->
          let e = normalise_error e in
          if is_unsupported e then
            Log.warn (fun m -> m "%s" (Error.to_string e));
          Error e
      | Ok body -> Client.Http.decode_response Wire.Response.jsont body)

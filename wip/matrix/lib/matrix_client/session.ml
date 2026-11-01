module Server = struct
  type t = { homeserver : Uriz.t; user_id : Matrix_proto.Id.User_id.t }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun homeserver user_id -> { homeserver; user_id })
      |> mem "homeserver" Json_codec.uri ~enc:(fun t -> t.homeserver)
      |> mem "user_id" Matrix_proto.Id.User_id.jsont ~enc:(fun t -> t.user_id)
      |> finish)
end

module Auth = struct
  type method_ = Matrix | OAuth of { client_id : string }

  type t = {
    access_token : string;
    device_id : Matrix_proto.Id.Device_id.t;
    refresh_token : string option;
    access_token_expires_at : Ptime.t option;
    method_ : method_;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map
        (fun
          access_token
          device_id
          refresh_token
          access_token_expires_at
          oauth_client_id
        ->
          let method_ =
            match oauth_client_id with
            | None -> Matrix
            | Some client_id -> OAuth { client_id }
          in
          {
            access_token;
            device_id;
            refresh_token;
            access_token_expires_at;
            method_;
          })
      |> mem "access_token" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.access_token)
      |> mem "device_id" Matrix_proto.Id.Device_id.jsont ~enc:(fun t ->
          t.device_id)
      |> opt_mem "refresh_token" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.refresh_token)
      |> opt_mem "access_token_expires_at" Json_codec.ptime ~enc:(fun t ->
          t.access_token_expires_at)
      |> opt_mem "oauth_client_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          match t.method_ with
          | Matrix -> None
          | OAuth { client_id } -> Some client_id)
      |> finish)
end

module Sync_state = struct
  type t = { next_batch : string option; filter_id : string option }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun next_batch filter_id -> { next_batch; filter_id })
      |> opt_mem "next_batch" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.next_batch)
      |> opt_mem "filter_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.filter_id)
      |> finish)
end

module Metadata = struct
  type t = {
    created_at : Ptime.t;
    last_used_at : Ptime.t;
    client_name : string;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun created_at last_used_at client_name ->
          { created_at; last_used_at; client_name })
      |> mem "created_at" Json_codec.ptime ~enc:(fun t -> t.created_at)
      |> mem "last_used_at" Json_codec.ptime ~enc:(fun t -> t.last_used_at)
      |> mem "client_name" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.client_name)
      |> finish)
end

module Session_file = struct
  type t = {
    server : Server.t;
    auth : Auth.t;
    sync : Sync_state.t;
    metadata : Metadata.t;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun server auth sync metadata -> { server; auth; sync; metadata })
      |> mem "server" Server.jsont ~enc:(fun t -> t.server)
      |> mem "auth" Auth.jsont ~enc:(fun t -> t.auth)
      |> mem "sync" Sync_state.jsont ~enc:(fun t -> t.sync)
      |> mem "metadata" Metadata.jsont ~enc:(fun t -> t.metadata)
      |> finish)
end

module Device_keys = struct
  type t = {
    ed25519_public : string;
    ed25519_private : string;
    curve25519_public : string;
    curve25519_private : string;
    uploaded_at : Ptime.t option;
    algorithms : string list;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map
        (fun
          ed25519_public
          ed25519_private
          curve25519_public
          curve25519_private
          uploaded_at
          algorithms
        ->
          {
            ed25519_public;
            ed25519_private;
            curve25519_public;
            curve25519_private;
            uploaded_at;
            algorithms;
          })
      |> mem "ed25519_public" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.ed25519_public)
      |> mem "ed25519_private" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.ed25519_private)
      |> mem "curve25519_public" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.curve25519_public)
      |> mem "curve25519_private" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.curve25519_private)
      |> opt_mem "uploaded_at" Json_codec.ptime ~enc:(fun t -> t.uploaded_at)
      |> mem "algorithms"
           (Jsont.list Matrix_proto.Json.Codec.string)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.algorithms)
      |> finish)
end

module One_time_key = struct
  type t = {
    key_id : string;
    public : string;
    private_ : string;
    created_at : Ptime.t;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun key_id public private_ created_at ->
          { key_id; public; private_; created_at })
      |> mem "key_id" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.key_id)
      |> mem "public" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.public)
      |> mem "private" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.private_)
      |> mem "created_at" Json_codec.ptime ~enc:(fun t -> t.created_at)
      |> finish)
end

module One_time_keys_file = struct
  type t = {
    target_count : int;
    last_upload_at : Ptime.t option;
    next_key_id : int;
    keys : One_time_key.t list;
    fallback : One_time_key.t option;
    previous_fallback : One_time_key.t option;
    fallback_used : bool;
  }

  let config_jsont =
    Jsont.Object.(
      map (fun target_count last_upload_at next_key_id ->
          (target_count, last_upload_at, next_key_id))
      |> mem "target_count" Matrix_proto.Json.Codec.Legacy.int
           ~dec_absent:(fun () -> 50)
           ~enc:(fun (tc, _, _) -> tc)
      |> opt_mem "last_upload_at" Json_codec.ptime ~enc:(fun (_, lu, _) -> lu)
      |> mem "next_key_id" Matrix_proto.Json.Codec.Legacy.int
           ~dec_absent:(fun () -> 0)
           ~enc:(fun (_, _, nk) -> nk)
      |> finish)

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun config keys fallback previous_fallback fallback_used ->
          let target_count, last_upload_at, next_key_id = config in
          {
            target_count;
            last_upload_at;
            next_key_id;
            keys;
            fallback;
            previous_fallback;
            fallback_used;
          })
      |> mem "config" config_jsont ~enc:(fun t ->
          (t.target_count, t.last_upload_at, t.next_key_id))
      |> mem "keys"
           (Jsont.list One_time_key.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.keys)
      |> opt_mem "fallback" One_time_key.jsont ~enc:(fun t -> t.fallback)
      |> opt_mem "previous_fallback" One_time_key.jsont ~enc:(fun t ->
          t.previous_fallback)
      |> mem "fallback_used" Jsont.bool
           ~dec_absent:(fun () -> false)
           ~enc:(fun t -> t.fallback_used)
      |> finish)
end

module Olm_session = struct
  let ptime_precise : Ptime.t Jsont.t =
    Jsont.of_of_string ~kind:"RFC 3339 timestamp"
      ~enc:(fun value -> (Ptime.to_rfc3339 ~frac_s:12 ~tz_offset_s:0) value)
      (fun s ->
        match Ptime.of_rfc3339 s with
        | Ok (t, _, _) -> Ok t
        | Error (`RFC3339 (_, e)) ->
            Error (Format.asprintf "%a" Ptime.pp_rfc3339_error e))

  type t = {
    their_identity_key : string;
    session_id : string;
    pickle : string;
    created_at : Ptime.t;
    last_used_at : Ptime.t;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun their_identity_key session_id pickle created_at last_used_at ->
          { their_identity_key; session_id; pickle; created_at; last_used_at })
      |> mem "their_identity_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.their_identity_key)
      |> mem "session_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.session_id)
      |> mem "pickle" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.pickle)
      |> mem "created_at" Json_codec.ptime ~enc:(fun t -> t.created_at)
      |> mem "last_used_at" ptime_precise ~enc:(fun t -> t.last_used_at)
      |> finish)
end

module Olm_sessions_file = struct
  type t = { sessions : Olm_session.t list }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun sessions -> { sessions })
      |> mem "sessions"
           (Jsont.list Olm_session.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.sessions)
      |> finish)
end

module Megolm_inbound = struct
  type t = {
    room_id : Matrix_proto.Id.Room_id.t;
    session_id : string;
    sender_key : string;
    signing_key : string;
    pickle : string;
    first_known_index : int;
    created_at : Ptime.t;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map
        (fun
          room_id
          session_id
          sender_key
          signing_key
          pickle
          first_known_index
          created_at
        ->
          {
            room_id;
            session_id;
            sender_key;
            signing_key;
            pickle;
            first_known_index;
            created_at;
          })
      |> mem "room_id" Matrix_proto.Id.Room_id.jsont ~enc:(fun t -> t.room_id)
      |> mem "session_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.session_id)
      |> mem "sender_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.sender_key)
      |> mem "signing_key" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.signing_key)
      |> mem "pickle" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.pickle)
      |> mem "first_known_index" Matrix_proto.Json.Codec.Legacy.int
           ~enc:(fun t -> t.first_known_index)
      |> mem "created_at" Json_codec.ptime ~enc:(fun t -> t.created_at)
      |> finish)
end

module Megolm_inbound_file = struct
  type t = { sessions : Megolm_inbound.t list }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun sessions -> { sessions })
      |> mem "sessions"
           (Jsont.list Megolm_inbound.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.sessions)
      |> finish)
end

module Shared_with = struct
  type t = {
    user_id : Matrix_proto.Id.User_id.t;
    device_id : Matrix_proto.Id.Device_id.t;
    shared_at : Ptime.t;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun user_id device_id shared_at -> { user_id; device_id; shared_at })
      |> mem "user_id" Matrix_proto.Id.User_id.jsont ~enc:(fun t -> t.user_id)
      |> mem "device_id" Matrix_proto.Id.Device_id.jsont ~enc:(fun t ->
          t.device_id)
      |> mem "shared_at" Json_codec.ptime ~enc:(fun t -> t.shared_at)
      |> finish)
end

module Megolm_outbound = struct
  type t = {
    room_id : Matrix_proto.Id.Room_id.t;
    session_id : string;
    pickle : string;
    message_index : int;
    created_at : Ptime.t;
    message_count : int;
    max_age_ms : int64;
    shared_with : Shared_with.t list;
  }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map
        (fun
          room_id
          session_id
          pickle
          message_index
          created_at
          message_count
          max_age_ms
          shared_with
        ->
          {
            room_id;
            session_id;
            pickle;
            message_index;
            created_at;
            message_count;
            max_age_ms;
            shared_with;
          })
      |> mem "room_id" Matrix_proto.Id.Room_id.jsont ~enc:(fun t -> t.room_id)
      |> mem "session_id" Matrix_proto.Json.Codec.string ~enc:(fun t ->
          t.session_id)
      |> mem "pickle" Matrix_proto.Json.Codec.string ~enc:(fun t -> t.pickle)
      |> mem "message_index" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun t ->
          t.message_index)
      |> mem "created_at" Json_codec.ptime ~enc:(fun t -> t.created_at)
      |> mem "message_count" Matrix_proto.Json.Codec.Legacy.int ~enc:(fun t ->
          t.message_count)
      |> mem "max_age_ms" Matrix_proto.Json.Codec.Legacy.int64 ~enc:(fun t ->
          t.max_age_ms)
      |> mem "shared_with"
           (Jsont.list Shared_with.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.shared_with)
      |> finish)
end

module Megolm_outbound_file = struct
  type t = { sessions : Megolm_outbound.t list }

  let jsont : t Jsont.t =
    Jsont.Object.(
      map (fun sessions -> { sessions })
      |> mem "sessions"
           (Jsont.list Megolm_outbound.jsont)
           ~dec_absent:(fun () -> [])
           ~enc:(fun t -> t.sessions)
      |> finish)
end

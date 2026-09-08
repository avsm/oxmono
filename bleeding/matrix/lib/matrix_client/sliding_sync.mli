(** sliding_sync — the simplified sliding sync endpoint (MSC4186).

    One call posts a {!Matrix_proto.Sliding_sync.Request.t} and decodes a
    {!Matrix_proto.Sliding_sync.Response.t}. The request and response shapes,
    and the builders that assemble a request, are in
    {!Matrix_proto.Sliding_sync}. Responses are folded into the shared
    {!type:Base_client.state}; [Matrix_eio.Sliding_sync] has the loop that keeps
    calling {!sync_once} and threads [pos] across the calls. A legacy
    [sliding_sync_state] store slot is migrated into that common state rather
    than maintained as a second public fold.

    {[
    let request =
      Matrix_proto.Sliding_sync.Request.v ~conn_id:"room-list" ()
      |> Matrix_proto.Sliding_sync.Request.add_list ~name:"all"
           ~ranges:[ (0, 19) ]
           ~timeline_limit:1
    in
    match Sliding_sync.sync_once client request with
    | Ok response ->
        Base_client.apply_sliding (Base_client.create ~user_id ()) response
    | Error e -> prerr_endline (Error.to_string e)
    ]}

    @see <https://github.com/matrix-org/matrix-spec-proposals/pull/4186>
      MSC4186: Simplified Sliding Sync *)

val path : string
(** [path] is the unstable endpoint,
    [/_matrix/client/unstable/org.matrix.simplified_msc3575/sync]. MSC4186
    reuses MSC3575's unstable prefix, so the [3575] in the path does not mean
    the payload is MSC3575. There is no stable path. *)

val default_timeout_ms : int
(** [default_timeout_ms] is [30_000], the poll timeout {!sync_once} sends when
    it is given none. *)

val native_feature : string
(** [native_feature] is ["org.matrix.simplified_msc3575"], the
    [/_matrix/client/versions] flag used by the pinned matrix-rust-sdk to
    discover its native MSC4186 implementation. *)

val is_available_in : Server.versions -> bool
(** [is_available_in versions] is [true] only when {!native_feature} is present
    and enabled. An absent or explicitly false flag is unavailable. *)

val is_available : Client.t -> (bool, Error.t) result
(** [is_available client] fetches [/_matrix/client/versions] once and applies
    {!is_available_in}. Transport and decoding failures remain typed errors;
    [false] means the server answered successfully without enabling the native
    endpoint. The pinned Rust SDK supports no separate stable endpoint at this
    revision. *)

val sync_once :
  Client.t ->
  ?pos:string ->
  ?timeout_ms:int ->
  ?set_presence:[ `Online | `Offline | `Unavailable ] ->
  Matrix_proto.Sliding_sync.Request.t ->
  (Matrix_proto.Sliding_sync.Response.t, Error.t) result
(** [sync_once client request] posts [request] to {!path}.

    [pos] continues an existing session, and omitting it starts a new one, which
    can be expensive for the server. [timeout_ms] is how long the server may
    hold the request open before answering and defaults to
    {!default_timeout_ms}.

    [set_presence] is sent as the [set_presence] query parameter using the same
    [online], [offline], and [unavailable] values as {!Sync}, except that the
    effective default [`Online] is omitted on the wire. When omitted, it is read
    from the client-owned {!Client.val-sync_presence} default.

    The result is [Error e] with [is_unsupported e] when the homeserver does not
    implement MSC4186. *)

val is_unsupported : Error.t -> bool
(** [is_unsupported e] is [true] for the error a homeserver without MSC4186
    gives, which is a bare [404] or a Matrix error with code [M_UNRECOGNIZED].
    {!sync_once} normalises both into one [M_UNRECOGNIZED] {!Error.Matrix_error}
    whose message names the endpoint, so a caller can fall back to {!Sync}
    without knowing which shape its server picked. *)

val is_expired_pos : Error.t -> bool
(** [is_expired_pos e] is [true] for [M_UNKNOWN_POS], which the server sends
    when the session behind [pos] is gone. The cure is to sync again with no
    [pos], which the loop in [Matrix_eio.Sliding_sync] does. *)

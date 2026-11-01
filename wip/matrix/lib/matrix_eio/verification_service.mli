(** verification_service — SAS verification driven from the sync loop.

    {!Verification} supplies pure state machines that neither send nor ask a
    human anything. A service owns a {!Verification.Flow.t}, puts what the
    machines produce on the wire with [/sendToDevice], and calls the
    application's [confirm] callback at the one point where a person has to
    compare emoji. Feed it every [m.key.verification.*] to-device event with
    {!handle}. It drives each flow as far as it will go without another event
    from the peer, and marks a device {!Encryption.Verified} once its MACs check
    out.

    {[
    let verification =
      Matrix_eio.Verification_service.create ~client ~encryption:enc
        ~confirm:(fun p ->
          List.iter
            (fun (e : Matrix_eio.Verification_service.emoji) ->
              Printf.printf "%s %s\n" e.symbol e.description)
            p.emoji;
          read_line () = "y")
        ()
    ]}

    A service owns no switch of its own. The sync-facing functions run on the
    fiber that calls them, but confirmation callbacks are forked on the client's
    switch and therefore cannot block the sync loop. Cancelling that switch
    stops the loop and with it the driving of every flow. The service's state is
    in memory, so it is lost with the value and no flow survives a restart.

    A failed [/sendToDevice] raises [Eio.Io] carrying an {!Error.type-err} out
    of whichever function was driving the flow.

    QR flows are not driven. An [m.qr_code.*] method is left in the flow table
    for the caller to pick up with {!flow}. Both to-device and in-room
    verification are transported. In-room verification is intended for a DM;
    encrypted-room sends use the encryption machine and [room_members] (or the
    local and peer users as a safe fallback), so callers should provide the
    complete membership list for rooms with additional participants.

    @see <https://spec.matrix.org/v1.11/client-server-api/#device-verification>
      Device verification *)

type emoji = Matrix_client.Verification.Sas.emoji = {
  number : int;
  symbol : string;
  description : string;
}
(** The type for one of the emoji both sides compare. It is
    {!Matrix_client.Verification.Sas.type-emoji}. *)

type prompt = {
  their_user_id : Matrix_proto.Id.User_id.t;
  their_device_id : Matrix_proto.Id.Device_id.t option;
  emoji : emoji list;  (** The seven emoji both sides must be shown. *)
  decimals : int * int * int;
      (** The same secret as three numbers, for a device that cannot render
          emoji. *)
  we_started : bool;  (** Whether this side sent the request. *)
}
(** The type for what the user has to compare. Both sides of a flow see the same
    {!field-emoji} and the same {!field-decimals}. If they do not, somebody is
    in the middle. *)

(** How a flow ended. *)
type result =
  | Verified of {
      user_id : Matrix_proto.Id.User_id.t;
      device_id : Matrix_proto.Id.Device_id.t option;
    }
      (** The MACs matched and any requested cross-signing publication
          succeeded. A device whose own key was covered is marked
          {!Encryption.Verified}. Without [private_identity] or [secret_store],
          this retains the legacy local-verification behavior. *)
  | Publication_failed of {
      user_id : Matrix_proto.Id.User_id.t;
      device_id : Matrix_proto.Id.Device_id.t option;
      reason : string;
    }
      (** SAS completed, but a supplied cross-signing capability could not
          publish the verified key. Local trust is not changed. *)
  | Cancelled of {
      user_id : Matrix_proto.Id.User_id.t;
      code : Matrix_client.Verification.Cancel_code.t;
    }  (** The other side, or this one, cancelled the flow. *)

type t
(** The type for verification services. State transitions are serialized so that
    sync and UI response fibers cannot mutate a flow concurrently. A service
    drives the flows of the one device its {!Encryption.t} belongs to. *)

val create :
  ?methods:Matrix_client.Verification.Method.t list ->
  ?on_result:(result -> unit) ->
  ?now:(unit -> Matrix_proto.Event.Timestamp.t) ->
  ?private_identity:Matrix_client.Cross_signing.private_identity ->
  ?secret_store:Secrets.store ->
  ?on_prompt:(flow_id:string -> prompt -> unit) ->
  ?room_members:(Matrix_proto.Id.Room_id.t -> Matrix_proto.Id.User_id.t list) ->
  client:Client.t ->
  encryption:Encryption.t ->
  ?confirm:(prompt -> bool) ->
  unit ->
  t
(** [create ~client ~encryption ~confirm ()] is a driver for [encryption]'s
    device.

    [room_members] supplies the users who should receive encrypted in-room
    verification events. It is called for every room send; the local and peer
    users are always included. In-room verification is intended for a DM, and
    omitting this callback is safe only when those two users are the complete
    membership.

    [on_prompt] is called once per SAS flow, with its flow id and the emoji to
    show. It runs on the client's switch after the sync-facing function has
    returned and does not answer the flow; the application calls {!respond}
    after the user decides. As a compatibility adapter, [confirm] may instead
    return the decision directly, but it too runs on the client's switch and may
    wait for a human. The two callbacks are mutually exclusive. With neither
    callback, prompts can still be answered by a caller that tracks flow ids
    from {!sessions}.

    [on_result] is called once per flow, after the service mutex has been
    released, when it finishes, publication fails, or is cancelled. It may
    safely call back into this service; other exceptions are logged and do not
    interrupt sync. It defaults to doing nothing. [methods] defaults to
    {!Matrix_client.Verification.Method.all}. [now] is the clock a flow is timed
    against and whose reading goes on the wire as a request's timestamp; it
    defaults to the system clock in milliseconds. [private_identity] is an
    optional capability holding this user's cross-signing private keys. Its user
    must match [encryption]'s user (otherwise [Invalid_argument] is raised).
    When supplied, verified same-user devices are signed with the self-signing
    key and other-user validated master keys with the user-signing key. Missing
    secrets and rejected uploads are reported as [Publication_failed]; no
    private identity preserves legacy local trust. [secret_store] is an
    alternative to [private_identity]: it imports this user's private
    cross-signing keys from the supplied SSSS store before the service starts.
    The client, encryption machine and store must belong to the same account,
    and a store must have been opened against the client's homeserver. The two
    private-key options cannot be supplied together. Import or validation
    failures are raised by [create]. *)

val flow : t -> Matrix_client.Verification.Flow.t
(** [flow t] is the table of in-flight flows, for a caller that wants to inspect
    them or drive a QR flow the service leaves undriven. *)

val sessions : t -> Matrix_client.Verification.Flow.session list
(** [sessions t] is the active flows currently tracked by [t]. Terminal flows
    are reported through [on_result] and removed after their final transport
    message; callers must retain any information they need from the result
    callback rather than expecting finished sessions to remain here. *)

val handle :
  t ->
  Client.t ->
  sender:Matrix_proto.Id.User_id.t ->
  event_type:string ->
  content:Jsont.json ->
  unit
(** [handle t c ~sender ~event_type ~content] routes one [m.key.verification.*]
    to-device event into its flow and drives it, sending whatever the state
    machines produced.

    [sender] and [content] must come from a valid Olm channel addressed to this
    device, as {!Encryption.sync_hook}'s outcome supplies them. An event whose
    content does not parse is dropped and logged. An event for a flow that is
    gone is answered with [m.unknown_transaction]. *)

val handle_room :
  t ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  event_id:Matrix_proto.Id.Event_id.t ->
  sender:Matrix_proto.Id.User_id.t ->
  timestamp:Matrix_proto.Event.Timestamp.t ->
  event_type:string ->
  content:Jsont.json ->
  unit
(** [handle_room] routes one timeline verification event. The event may be a
    plaintext [m.room.message] request or a decrypted [m.key.verification.*]
    follow-up; responses are sent back into the same room. Requests addressed to
    another user, from an unknown device, or outside the permitted [timestamp]
    window are ignored. [event_id] identifies the event being routed; for the
    initial request it becomes the transaction identifier referenced by every
    follow-up. Callers must preserve timeline order. *)

val request :
  t ->
  Client.t ->
  ?device_id:Matrix_proto.Id.Device_id.t ->
  Matrix_proto.Id.User_id.t ->
  Matrix_client.Verification.Flow.session
(** [request t c their_user_id] asks [their_user_id]'s devices to verify with
    this one, and is the session opened for the flow, whose
    {!Matrix_client.Verification.Flow.session_transaction} identifies it in
    later callbacks. [device_id] narrows the request to one device and defaults
    to absent. When the encryption machine knows concrete devices, the request
    names them individually so the first answer can cancel the others with
    [m.accepted]; otherwise it falls back to asking every device. The local
    device is excluded when verifying another device of the same user. *)

val request_in_room :
  t ->
  Client.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?methods:Matrix_client.Verification.Method.t list ->
  ?body:string ->
  Matrix_proto.Id.User_id.t ->
  Matrix_client.Verification.Flow.session
(** [request_in_room t c ~room_id their_user_id] sends an in-room
    [m.key.verification.request] as [m.room.message], tracks it once the server
    returns its event id, and returns the resulting session. In an encrypted
    room the request is sent as [m.room.encrypted]. *)

val accept : t -> Client.t -> Matrix_client.Verification.Flow.session -> unit
(** [accept t c session] answers an incoming request with
    [m.key.verification.ready] and drives the flow on. An incoming request
    arrives as a session in {!Matrix_client.Verification.Flow.Requested} and is
    never answered automatically, because accepting is the user's decision. *)

val respond : t -> Client.t -> flow_id:string -> accept:bool -> unit
(** [respond t c ~flow_id ~accept] answers the one-shot SAS confirmation prompt
    for [flow_id]. [accept] selects {!Matrix_client.Verification.Flow.confirm}
    or {!Matrix_client.Verification.Flow.mismatch}. Responses for unknown,
    duplicate, or no-longer-SAS-ready flows are ignored. *)

val cancel :
  t ->
  Client.t ->
  Matrix_client.Verification.Flow.session ->
  Matrix_client.Verification.Cancel_code.t ->
  unit
(** [cancel t c session code] cancels a flow, telling the other side why. *)

val tick : t -> Client.t -> unit
(** [tick t c] fails every flow older than ten minutes and sends the
    cancellations. {!Sync_service.run} calls it once per sync response. *)

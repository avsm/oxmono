(** Scaffolding for the tests that run against a real homeserver.

    Everything here needs [MATRIX_TEST_HOMESERVER] to name a running homeserver.
    [test/integration/synapse.sh up] starts one and prints the export line.
    Nothing here mocks. {!connect} builds a {!Matrix_eio.Client} over
    {!Fetch_httpz.std}, and every request a scenario makes goes out over TCP.

    A scenario is a function of a {!t}, which carries the Eio environment, a
    switch, and the homeserver URL:

    {[
      let test_something () =
        Harness.run @@ fun h ->
        let alice = Harness.register_user h ~prefix:"alice" () in
        let sync = Harness.start_sync h alice in
        ...
    ]}

    {!run} tears the switch down when the scenario returns, which is what stops
    the sync and send-queue fibers {!start_sync} and {!start_send_queue} leave
    running.

    Users are registered with a random localpart per call, so a scenario can be
    run repeatedly against a homeserver whose database persists. *)

(** {1 The environment} *)

val homeserver_env : unit -> string option
(** [homeserver_env ()] is [MATRIX_TEST_HOMESERVER], if it is set to something
    other than the empty string. *)

type t
(** The type for a running scenario. It carries an Eio environment, a switch
    whose lifetime is the scenario's, and the homeserver under test. *)

val run : (t -> unit) -> unit
(** [run f] enters [Eio_main.run] and a fresh switch, and calls [f].

    When [f] returns the switch is cancelled, so the fibers started by
    {!start_sync} and {!start_send_queue} stop. An exception from [f]
    propagates, after the same teardown.

    @raise Failure
      if [MATRIX_TEST_HOMESERVER] is not set. Call {!homeserver_env} first;
      [test_homeserver.ml] does, and skips. *)

val env : t -> Eio_unix.Stdenv.base
(** [env t] is the Eio environment {!run} entered. *)

val switch : t -> Eio.Switch.t
(** [switch t] is the scenario's switch, which {!run} cancels on the way out. *)

val clock : t -> float Eio.Time.clock_ty Eio.Std.r
(** [clock t] is what the waits are measured against. *)

val homeserver : t -> Uriz.t
(** [homeserver t] is [MATRIX_TEST_HOMESERVER] as a URL. *)

val hex : t -> int -> string
(** [hex t n] is [n] bytes from the environment's secure random source,
    hex-encoded. Used for unique localparts, room names and message bodies, so
    that a rerun against a persistent database does not collide with the run
    before. *)

(** {1 Failing} *)

val ok : string -> ('a, Matrix_client.Error.t) result -> 'a
(** [ok what result] is the value, or an Alcotest failure naming [what] and the
    error. Every scenario call goes through this so that a homeserver rejection
    is reported as itself rather than as a pattern-match failure. *)

val is_error : ('a, Matrix_client.Error.t) result -> bool
(** [is_error result] is [true] when the homeserver refused, for a scenario
    asserting that it does. *)

(** {1 Users} *)

type user = {
  localpart : string;  (** The random localpart this run registered. *)
  password : string;
  user_id : Matrix_proto.Id.User_id.t;
  device_id : Matrix_proto.Id.Device_id.t;
  client : Matrix_eio.Client.t;
      (** The logged-in client, with the session from registration installed. *)
}

val connect : t -> Matrix_eio.Client.t
(** [connect t] is an anonymous client for the homeserver. Plain [http] is
    accepted by {!Matrix_client.Client.create}, which is what makes a loopback
    Synapse usable without a certificate. *)

val register_user : t -> ?prefix:string -> unit -> user
(** [register_user t ()] registers [<prefix>-<hex>] with a random password and
    returns a client carrying the session.

    Registration is user-interactive even when the server has open registration:
    Synapse answers the first [POST /register] with a 401 and the flow
    [["m.login.dummy"]]. This retries once with {!Matrix_client.Uiaa.dummy_auth}
    carrying the session the challenge named, which is all an open server asks
    for.

    [prefix] defaults to ["user"]. *)

val base : user -> Matrix_client.Client.t
(** [base user] is the result-returning client under {!user.client}, for the
    {!Matrix_client} endpoint modules. *)

(** {1 Waiting}

    A homeserver answers when it answers, so a scenario that has asked for
    something waits for it to show up rather than assuming the next request sees
    it. All of these poll and give up with an Alcotest failure. *)

val default_timeout : float
(** [default_timeout] is 30 seconds. *)

val wait_until : t -> ?timeout:float -> ?label:string -> (unit -> bool) -> unit
(** [wait_until t pred] polls [pred] until it holds. [timeout] is in seconds and
    defaults to {!default_timeout}. [label] is named in the failure message, and
    defaults to a generic one. *)

val wait_for : t -> ?timeout:float -> ?label:string -> (unit -> 'a option) -> 'a
(** [wait_for t pred] is {!wait_until} for a predicate that produces the thing
    waited for. *)

(** {1 Sync} *)

type sync
(** The type for a {!Matrix_eio.Sync_service} running in its own fiber, with
    everything it has reported kept for a scenario to wait on. *)

val start_sync :
  t ->
  ?encryption:Matrix_eio.Encryption.t ->
  ?verification:Matrix_eio.Verification_service.t ->
  user ->
  sync
(** [start_sync t user] forks {!Matrix_eio.Sync_service.run} on the scenario's
    switch and returns a handle on it.

    [encryption], when given, is run over every response before the base client
    folds it in. See {!Matrix_eio.Sync_service.run}. [verification] is where
    that machine's [m.key.verification.*] to-device events are routed, which is
    what makes a flow run itself. It does nothing without [encryption], which is
    what decrypts them. *)

val service : sync -> Matrix_eio.Sync_service.t
(** [service s] is the running sync service, for a scenario that reads its
    members or its receipts. *)

val state : sync -> Matrix_client.Base_client.state
(** [state s] is everything the service has folded in so far. *)

val responses : sync -> int
(** [responses s] is how many responses have been folded in. Waiting for this to
    increase is how a scenario waits for "one more round" without knowing what
    it will carry. *)

val room_changes : sync -> Matrix_client.Base_client.room_change list
(** [room_changes s] is every change reported so far, oldest first. *)

val timeline :
  sync -> Matrix_proto.Id.Room_id.t -> Matrix_proto.Event.Raw_event.t list
(** [timeline sync room] is every timeline event reported for [room] so far,
    oldest first. *)

val wait_for_state :
  t ->
  ?timeout:float ->
  ?label:string ->
  sync ->
  (Matrix_client.Base_client.state -> 'a option) ->
  'a
(** [wait_for_state t sync f] waits until [f] of the service's current state is
    [Some]. *)

val wait_for_room :
  t ->
  ?timeout:float ->
  ?label:string ->
  sync ->
  Matrix_proto.Id.Room_id.t ->
  (Matrix_client.Base_client.room_info -> bool) ->
  Matrix_client.Base_client.room_info
(** [wait_for_room t sync room pred] waits for the service to hold a
    {!Matrix_client.Base_client.room_info} for [room] satisfying [pred]. *)

val wait_for_room_change :
  t ->
  ?timeout:float ->
  ?label:string ->
  sync ->
  (Matrix_client.Base_client.room_change -> bool) ->
  Matrix_client.Base_client.room_change
(** [wait_for_room_change t sync pred] waits for a reported change satisfying
    [pred] and returns the first one. Changes already reported count, so a
    scenario cannot miss one by registering the wait late. *)

val wait_for_event :
  t ->
  ?timeout:float ->
  ?label:string ->
  sync ->
  Matrix_proto.Id.Room_id.t ->
  (Matrix_proto.Event.Raw_event.t -> bool) ->
  Matrix_proto.Event.Raw_event.t
(** [wait_for_event t sync room pred] waits for a timeline event of [room]
    satisfying [pred]. *)

(** {1 Sending} *)

val start_send_queue :
  t ->
  ?encryption:Matrix_eio.Encryption.t ->
  ?sync:sync ->
  user ->
  Matrix_eio.Send_queue.t
(** [start_send_queue t user] creates a queue and forks its fibers on the
    scenario's switch, so an enqueued request is sent without the scenario
    driving it.

    [encryption] and [sync] together make the queue encrypt for rooms the
    machine knows are encrypted: [sync]'s service supplies the members. *)

val wait_sent :
  t ->
  ?timeout:float ->
  Matrix_eio.Send_queue.request ->
  Matrix_proto.Id.Event_id.t
(** [wait_sent t request] waits for [request] to reach
    {!Matrix_eio.Send_queue.Sent} and returns the event id the server gave it. A
    {!Matrix_eio.Send_queue.Wedged} request fails immediately. *)

(** {1 JSON} *)

val string_member : string -> Jsont.json -> string option
(** [string_member name json] is the string value of [name] in a JSON object.
    Scenarios read event content this way rather than through a codec, because
    what is under test is what the server sent. *)

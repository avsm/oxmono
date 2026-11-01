(** Scaffolding for the tests that run against a real homeserver.

    See [harness.mli]. Nothing here is specific to a scenario: it registers
    users, starts sync loops and send queues, and waits for the server to catch
    up. The scenarios live in [scenario_*.ml]. *)

module Base = Matrix_client.Base_client
module Id = Matrix_proto.Id

let homeserver_env () =
  match Sys.getenv_opt "MATRIX_TEST_HOMESERVER" with
  | Some ("" | " ") | None -> None
  | Some url -> Some url

type t = {
  sw : Eio.Switch.t;
  env : Eio_unix.Stdenv.base;
  homeserver : Uriz.t;
  random : Matrix_client.Random.t;
}

let env t = t.env
let switch t = t.sw
let clock t = Eio.Stdenv.clock t.env
let homeserver t = t.homeserver

(* [Switch.run] waits for every fiber forked on the switch, and the sync and
   send-queue fibers never return. Raising out of the body is what cancels
   them: the switch turns itself off, absorbs the resulting [Cancelled], and
   re-raises what we threw — which we then swallow. *)
exception Scenario_finished

let run f =
  let url =
    match homeserver_env () with
    | Some url -> url
    | None ->
        failwith
          "MATRIX_TEST_HOMESERVER is not set; run test/integration/synapse.sh \
           up"
  in
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let t =
      {
        sw;
        env;
        homeserver = Uriz.of_string_exn url;
        random = Matrix_client.Random.of_env env;
      }
    in
    f t;
    raise Scenario_finished
  with Scenario_finished -> ()

let hex t n =
  let bytes = Matrix_client.Random.generate t.random n in
  String.concat ""
    (List.map
       (fun c -> Printf.sprintf "%02x" (Char.code c))
       (List.init (String.length bytes) (String.get bytes)))

let ok what = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: %s" what (Matrix_client.Error.to_string e)

let is_error = function Ok _ -> false | Error _ -> true

type user = {
  localpart : string;
  password : string;
  user_id : Id.User_id.t;
  device_id : Id.Device_id.t;
  client : Matrix_eio.Client.t;
}

let connect t =
  Matrix_eio.Client.create ~sw:t.sw ~env:t.env ~homeserver:t.homeserver
    ~user_agent:"ocaml-matrix-integration" ()

let base user = Matrix_eio.Client.base user.client

(* Synapse answers an unauthenticated [POST /register] with a 401 carrying
   the UIAA flows even when registration is wide open. The client-side UIAA
   helper retains the registration body while answering that first challenge.
   A challenge the harness cannot answer remains data, so this reports it with
   the server's flow count rather than pretending every server uses dummy. *)
let register_user t ?(prefix = "user") () =
  let localpart = Printf.sprintf "%s-%s" prefix (hex t 4) in
  let password = "pw-" ^ hex t 8 in
  let client = connect t in
  let session =
    match
      Matrix_client.Auth.register_uiaa
        (Matrix_eio.Client.base client)
        ~username:localpart ~password
        ~params:
          {
            device_id = None;
            initial_device_display_name = Some "ocaml-matrix integration test";
          }
        ~auth_callback:(fun uiaa ->
          if Matrix_client.Uiaa.has_dummy_flow uiaa then
            Some (Matrix_client.Uiaa.dummy_auth ?session:uiaa.session ())
          else None)
        ()
    with
    | Matrix_client.Uiaa.Uiaa_success session -> session
    | Matrix_client.Uiaa.Uiaa_auth_required uiaa ->
        Alcotest.failf
          "register: the server requires unsupported UIAA (%d flows)"
          (List.length uiaa.flows)
    | Matrix_client.Uiaa.Uiaa_error e ->
        Alcotest.failf "register: %s" (Matrix_client.Error.to_string e)
  in
  {
    localpart;
    password;
    user_id = session.Matrix_client.Client.user_id;
    device_id = session.Matrix_client.Client.device_id;
    client = Matrix_eio.Client.with_session client session;
  }

let default_timeout = 30.0
let poll_interval = 0.05

let wait_for t ?(timeout = default_timeout) ?(label = "condition") f =
  let clock = clock t in
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    match f () with
    | Some v -> v
    | None ->
        if Eio.Time.now clock >= deadline then
          Alcotest.failf "timed out after %.0fs waiting for %s" timeout label
        else (
          Eio.Time.sleep clock poll_interval;
          loop ())
  in
  loop ()

let wait_until t ?timeout ?label pred =
  wait_for t ?timeout ?label (fun () -> if pred () then Some () else None)

type sync = {
  svc : Matrix_eio.Sync_service.t;
  mutable count : int;
  mutable seen : Base.room_change list;  (** Newest first. *)
}

let start_sync t ?encryption ?verification user =
  let svc =
    Matrix_eio.Sync_service.of_user ~user_id:user.user_id
      ~display_name:user.localpart ()
  in
  let s = { svc; count = 0; seen = [] } in
  Matrix_eio.Sync_service.run ~sw:t.sw ~clock:(clock t) user.client svc
    ?encryption ?verification
    ~on_error:(fun e ->
      (* A failure here is the scenario's failure: raising inside the sync
         fiber fails the switch, which fails the test with the error rather
         than with a timeout thirty seconds later. *)
      Alcotest.failf "sync for %s: %a" user.localpart Matrix_eio.Error.pp_err e)
    ~on_change:(fun _state (changes : Base.changes) ->
      s.seen <- List.rev_append changes.room_changes s.seen;
      s.count <- s.count + 1)
    ();
  s

let service s = s.svc
let state s = Matrix_eio.Sync_service.state s.svc
let responses s = s.count
let room_changes s = List.rev s.seen

let changes_for s room_id =
  List.filter
    (fun (c : Base.room_change) ->
      String.equal
        (Id.Room_id.to_string c.changed_room_id)
        (Id.Room_id.to_string room_id))
    (room_changes s)

let timeline s room_id =
  List.concat_map
    (fun (c : Base.room_change) -> c.timeline)
    (changes_for s room_id)

let wait_for_state t ?timeout ?label s f =
  wait_for t ?timeout ?label (fun () -> f (state s))

let wait_for_room t ?timeout ?(label = "a room") s room_id pred =
  wait_for t ?timeout ~label (fun () ->
      match Base.find_room (state s) room_id with
      | Some info when pred info -> Some info
      | _ -> None)

let wait_for_room_change t ?timeout ?(label = "a room change") s pred =
  wait_for t ?timeout ~label (fun () -> List.find_opt pred (room_changes s))

let wait_for_event t ?timeout ?(label = "a timeline event") s room_id pred =
  wait_for t ?timeout ~label (fun () -> List.find_opt pred (timeline s room_id))

let start_send_queue t ?encryption ?sync user =
  let queue =
    Matrix_eio.Send_queue.create
      ~random:(Matrix_client.Client.random (base user))
      ~user_id:user.user_id ()
  in
  let members =
    Option.map
      (fun s room_id -> Matrix_eio.Sync_service.members s.svc room_id)
      sync
  in
  Matrix_eio.Send_queue.start ~sw:t.sw ~clock:(clock t) ?encryption ?members
    user.client queue;
  queue

let wait_sent t ?timeout request =
  wait_for t ?timeout ~label:"the send queue to deliver a request" (fun () ->
      match Matrix_eio.Send_queue.status request with
      | Matrix_eio.Send_queue.Sent event_id -> Some event_id
      | Uploaded _ -> Alcotest.fail "an upload node has no event id"
      | Wedged ->
          Alcotest.failf "the send queue wedged the request: %s"
            (match Matrix_eio.Send_queue.last_error request with
            | Some e -> Matrix_client.Error.to_string e
            | None -> "no error recorded")
      | Cancelled -> Alcotest.fail "the send queue cancelled the request"
      | Pending | Sending -> None)

let string_member name = function
  | Jsont.Object (o, _) -> (
      match Jsont.Json.find_mem name o with
      | Some (_, Jsont.String (s, _)) -> Some s
      | _ -> None)
  | _ -> None

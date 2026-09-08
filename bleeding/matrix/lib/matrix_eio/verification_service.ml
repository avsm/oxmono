module V = Matrix_client.Verification
module Id = Matrix_proto.Id
module Event = Matrix_proto.Event
module Ts = Matrix_proto.Event.Timestamp
module Flow = V.Flow
module Sas = V.Sas
module Cs = Matrix_client.Cross_signing
module Key_id = Matrix_client.Crypto_key.Key_id
module Ed25519 = Matrix_client.Crypto_key.Ed25519

type emoji = Sas.emoji = { number : int; symbol : string; description : string }

type prompt = {
  their_user_id : Id.User_id.t;
  their_device_id : Id.Device_id.t option;
  emoji : emoji list;
  decimals : int * int * int;
  we_started : bool;
}

type result =
  | Verified of { user_id : Id.User_id.t; device_id : Id.Device_id.t option }
  | Publication_failed of {
      user_id : Id.User_id.t;
      device_id : Id.Device_id.t option;
      reason : string;
    }
  | Cancelled of { user_id : Id.User_id.t; code : V.Cancel_code.t }

type t = {
  v_mutex : Eio.Mutex.t;
  v_flow : Flow.t;
  v_random : Matrix_client.Random.t;
  v_enc : Encryption.t;
  v_confirm : (prompt -> bool) option;
  v_on_prompt : (flow_id:string -> prompt -> unit) option;
  v_on_result : (result -> unit) option;
  v_methods : V.Method.t list;
  v_now : unit -> Ts.t;
  v_reported : (string, unit) Hashtbl.t;
  (* State-machine transitions enqueue results while holding [v_mutex].  The
     outer service wrapper drains them after releasing the mutex, so an
     application callback may safely call back into this service. *)
  mutable v_notifications : result list;
  (* A SAS-ready flow stays here until the application answers the prompt.
     Keeping this separate from [v_reported] makes repeated sync events
     harmless while still allowing a completed flow to be reported once. *)
  v_pending : (string, prompt) Hashtbl.t;
  v_private_identity : Cs.private_identity option;
  v_room_members : (Id.Room_id.t -> Id.User_id.t list) option;
}

(* The reading goes on the wire as the [timestamp] of an
   [m.key.verification.request], which a recipient is required to ignore when
   it is more than ten minutes in the past or five minutes in the future, so
   it must be the wall clock rather than any monotonic source. *)
let default_now () = Ts.of_ptime (Ptime_clock.now ())
let published_key ~role key = Cs.key_ed25519 (Cs.key ~role key)

let private_master_matches encryption identity =
  let own_user = Encryption.user_id encryption in
  match
    ( Option.bind
        (Encryption.identity_master_key encryption own_user)
        (published_key ~role:Cs.Master),
      Cs.master_public identity )
  with
  | Some expected, Some actual -> Ed25519.Public.equal expected actual
  | _ -> false

let private_self_signing_matches encryption identity =
  let own_user = Encryption.user_id encryption in
  match
    ( Option.bind
        (Encryption.identity_self_signing_key encryption own_user)
        (published_key ~role:Cs.Self_signing),
      Option.map Ed25519.Private.public (Cs.self_signing_secret identity) )
  with
  | Some expected, Some actual -> Ed25519.Public.equal expected actual
  | _ -> false

let private_user_signing_matches encryption identity =
  let own_user = Encryption.user_id encryption in
  match
    ( Option.bind
        (Encryption.identity_user_signing_key encryption own_user)
        (published_key ~role:Cs.User_signing),
      Option.map Ed25519.Private.public (Cs.user_signing_secret identity) )
  with
  | Some expected, Some actual -> Ed25519.Public.equal expected actual
  | _ -> false

let validate_private_identity encryption identity =
  let own_user = Encryption.user_id encryption in
  let check label expected actual =
    match (expected, actual) with
    | Some expected, Some actual when not (Ed25519.Public.equal expected actual)
      ->
        invalid_arg
          (Printf.sprintf
             "Verification_service.create: private %s key does not match the \
              validated identity"
             label)
    | _ -> ()
  in
  check "master"
    (Option.bind
       (Encryption.identity_master_key encryption own_user)
       (published_key ~role:Cs.Master))
    (Cs.master_public identity);
  check "self-signing"
    (Option.bind
       (Encryption.identity_self_signing_key encryption own_user)
       (published_key ~role:Cs.Self_signing))
    (Option.map Ed25519.Private.public (Cs.self_signing_secret identity));
  check "user-signing"
    (Option.bind
       (Encryption.identity_user_signing_key encryption own_user)
       (published_key ~role:Cs.User_signing))
    (Option.map Ed25519.Private.public (Cs.user_signing_secret identity))

let create ?methods ?on_result ?now ?private_identity ?secret_store ?on_prompt
    ?room_members ~client ~encryption ?confirm () =
  (match (on_prompt, confirm) with
  | Some _, Some _ ->
      invalid_arg
        "Verification_service.create: on_prompt and confirm are mutually \
         exclusive"
  | _ -> ());
  (match (private_identity, secret_store) with
  | Some _, Some _ ->
      invalid_arg
        "Verification_service.create: private_identity and secret_store are \
         mutually exclusive"
  | _ -> ());
  let encryption_user = Encryption.user_id encryption in
  let client_user = Client.user_id client in
  if not (Id.User_id.equal client_user encryption_user) then
    invalid_arg
      "Verification_service.create: client and encryption user mismatch";
  Option.iter
    (fun store ->
      if
        (not (Id.User_id.equal (Secrets.store_user_id store) client_user))
        || not
             (Uriz.equal
                (Secrets.store_homeserver store)
                (Client.homeserver client))
      then
        invalid_arg
          "Verification_service.create: secret store and client account \
           mismatch")
    secret_store;
  let private_identity =
    match private_identity with
    | Some identity -> Some identity
    | None ->
        Option.map
          (fun store -> Secrets.import_cross_signing store ~encryption)
          secret_store
  in
  Option.iter
    (fun identity ->
      if not (Id.User_id.equal (Cs.identity_user_id identity) encryption_user)
      then
        invalid_arg
          "Verification_service.create: private identity user mismatch"
      else validate_private_identity encryption identity)
    private_identity;
  {
    v_mutex = Eio.Mutex.create ();
    v_flow = Flow.create ();
    v_random = Matrix_client.Client.random (Client.base client);
    v_enc = encryption;
    v_confirm = confirm;
    v_on_prompt = on_prompt;
    v_on_result = on_result;
    v_methods = Option.value methods ~default:V.Method.all;
    v_now = Option.value now ~default:default_now;
    v_reported = Hashtbl.create 8;
    v_notifications = [];
    v_pending = Hashtbl.create 8;
    v_private_identity = private_identity;
    v_room_members = room_members;
  }

let deliver_notifications t notifications =
  match t.v_on_result with
  | None -> ()
  | Some callback ->
      List.iter
        (fun result ->
          try callback result with
          | Eio.Cancel.Cancelled _ as exn ->
              let bt = Printexc.get_raw_backtrace () in
              Printexc.raise_with_backtrace exn bt
          | exn ->
              Logs.err (fun m ->
                  m "verification: result callback failed: %s"
                    (Printexc.to_string exn)))
        notifications

let with_service t f =
  let outcome, notifications =
    Eio.Mutex.use_rw ~protect:true t.v_mutex (fun () ->
        let take_notifications () =
          let notifications = List.rev t.v_notifications in
          t.v_notifications <- [];
          notifications
        in
        match f () with
        | value -> (`Value value, take_notifications ())
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            (`Exception (exn, bt), take_notifications ()))
  in
  deliver_notifications t notifications;
  match outcome with
  | `Value value -> value
  | `Exception (exn, bt) -> Printexc.raise_with_backtrace exn bt

let flow t = t.v_flow
let sessions t = Flow.sessions t.v_flow
let our_device_id t = Encryption.device_id t.v_enc

let our_identity t =
  let ed25519, _curve = Encryption.identity_keys t.v_enc in
  (* A capability may have been supplied before the first key query, and its
     mutable secrets may subsequently have changed. Only advertise a master
     key which still matches the machine's validated own identity. *)
  let master_key =
    Option.bind t.v_private_identity (fun identity ->
        if private_master_matches t.v_enc identity then
          Cs.master_public identity
        else None)
  in
  Sas.identity
    ~user_id:(Encryption.user_id t.v_enc)
    ~device_id:(our_device_id t) ~device_key:ed25519 ?master_key ()

(* [None] for a device whose keys are unknown, which makes the state machine
   cancel rather than verify against a key it had to guess. *)
let lookup t ~user_id ~device_id =
  match Encryption.find_device t.v_enc user_id ~device_id with
  | None -> None
  | Some d ->
      Option.map
        (fun k ->
          let master_key =
            Option.bind (Encryption.identity_master_key t.v_enc user_id)
              (fun key -> Cs.key_ed25519 (Cs.key ~role:Cs.Master key))
          in
          Sas.identity ~user_id ~device_id ~device_key:k ?master_key ())
        (Encryption.device_ed25519 d)

let room_members t room_id peer_user_id =
  let supplied =
    match t.v_room_members with Some members -> members room_id | None -> []
  in
  let add user acc =
    if List.exists (Id.User_id.equal user) acc then acc else user :: acc
  in
  List.fold_right add supplied [ Encryption.user_id t.v_enc; peer_user_id ]

let send_room_event t client ~room_id ~peer_user_id ~event_type ~content =
  let members = room_members t room_id peer_user_id in
  if Encryption.is_room_encrypted t.v_enc room_id then
    Encryption.send_encrypted t.v_enc client room_id ~event_type ~content
      ~members
  else
    Error.unwrap
      ~context:
        (Fmt.str "sending a verification event in room %S"
           (Id.Room_id.to_string room_id))
      (Matrix_client.Messages.send_event (Client.base client) ~room_id
         ~event_type:(Event.Event_type.of_string event_type)
         ~content)

let send_room_messages t client ~room_id ~peer_user_id msgs =
  List.iter
    (fun msg ->
      match V.Message.to_json msg with
      | Error (`Msg m) -> raise (Error.err (Error.Json m))
      | Ok content ->
          ignore
            (send_room_event t client ~room_id ~peer_user_id
               ~event_type:(V.Message.event_type msg) ~content
              : Id.Event_id.t))
    msgs

let send_messages t client session msgs =
  match msgs with
  | [] -> ()
  | _ -> (
      match V.Transaction.room_id (Flow.session_transaction session) with
      | Some room_id ->
          send_room_messages t client ~room_id
            ~peer_user_id:(Flow.session_their_user_id session)
            msgs
      | None ->
          let devices =
            match Flow.session_their_device_id session with
            | Some d -> [ To_device.Device d ]
            | None -> (
                match Flow.session_requested_devices session with
                | [] -> [ To_device.All ]
                | devices -> devices)
          in
          List.iter
            (fun msg ->
              Verification.send_to_devices client
                ~their_user_id:(Flow.session_their_user_id session)
                ~devices msg)
            msgs)

let send_pending t client =
  let pending = Flow.take_pending_sends t.v_flow in
  List.iter
    (fun (session, msg) -> send_messages t client session [ msg ])
    pending;
  List.map fst pending

let send_directed_messages client session sends =
  List.iter
    (fun (device_id, msg) ->
      Verification.send_to_devices client
        ~their_user_id:(Flow.session_their_user_id session)
        ~devices:[ To_device.Device device_id ]
        msg)
    sends

let send_step t client session (step : Flow.step) =
  send_messages t client session step.Flow.send;
  send_directed_messages client session step.Flow.send_to

let report t r = t.v_notifications <- r :: t.v_notifications
let flow_id session = V.Transaction.id (Flow.session_transaction session)

let once t session f =
  let id = flow_id session in
  if not (Hashtbl.mem t.v_reported id) then (
    Hashtbl.replace t.v_reported id ();
    f ())

(* Terminal sessions have already had their final message sent by the caller
   of [advance]. Retain them only for that one reporting turn, then drop all
   bookkeeping so a long-lived sync service cannot grow with every flow. *)
let prune_terminal t session =
  let id = flow_id session in
  Hashtbl.remove t.v_pending id;
  Flow.remove t.v_flow id;
  Hashtbl.remove t.v_reported id

(* Only the keys whose MAC was checked count, so a flow that completed
   without covering the device key leaves the device's trust where it was. *)
let jmem k v = Jsont.Json.mem (Jsont.Json.name k) v
let jobject mems = Jsont.Json.object' mems
let jstring = Jsont.Json.string

let jkey_map l =
  jobject (List.map (fun (k, v) -> jmem (Key_id.to_string k) (jstring v)) l)

let signatures_json (sigs : Keys.signatures) =
  jobject
    (List.map
       (fun (user_id, keys) ->
         jmem
           (Id.User_id.to_string user_id)
           (jkey_map
              (List.map
                 (fun (key_id, signature) ->
                   ( key_id,
                     Matrix_client.Crypto_key.Signature.to_base64 signature ))
                 keys)))
       sigs)

let device_json (d : Keys.device_keys) =
  let fields =
    [
      jmem "user_id" (jstring (Id.User_id.to_string d.user_id));
      jmem "device_id" (jstring (Id.Device_id.to_string d.device_id));
      jmem "algorithms" (Jsont.Json.list (List.map jstring d.algorithms));
      jmem "keys" (jkey_map d.keys);
      jmem "signatures" (signatures_json d.signatures);
    ]
  in
  jobject
    (match d.unsigned with
    | None -> fields
    | Some u -> fields @ [ jmem "unsigned" u ])

let cross_signing_json (k : Keys.cross_signing_key) =
  let usage_to_string = function
    | Keys.Master -> "master"
    | Keys.Self_signing -> "self_signing"
    | Keys.User_signing -> "user_signing"
    | Keys.Other s -> s
  in
  jobject
    [
      jmem "user_id" (jstring (Id.User_id.to_string k.user_id));
      jmem "usage"
        (Jsont.Json.list
           (List.map (fun u -> jstring (usage_to_string u)) k.usage));
      jmem "keys" (jkey_map k.keys);
      jmem "signatures" (signatures_json k.signatures);
    ]

let failure_reason () = "the homeserver rejected a cross-signing signature"

let upload t client ~user_id ~target_key ~json =
  try
    let response =
      Keys.upload_signatures client [ (user_id, [ (target_key, json) ]) ]
    in
    if response.failures = [] then Ok () else Error (failure_reason ())
  with
  | Eio.Cancel.Cancelled _ as exn ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace exn bt
  | exn -> Error (Printexc.to_string exn)

let has_verified_key verified key_id value =
  List.exists
    (fun (k, v) -> Key_id.equal k key_id && String.equal v value)
    verified

(* Publish at most once per flow. A missing capability keeps the historical
   local-verification behavior; when a capability was supplied, a missing
   signer or validated target is reported instead of claiming publication. *)
let publish_verified t client session verified =
  match t.v_private_identity with
  | None -> Ok ()
  | Some identity -> (
      let user_id = Flow.session_their_user_id session in
      let device_id = Flow.session_their_device_id session in
      let our_user = Encryption.user_id t.v_enc in
      match device_id with
      | Some device_id when Id.User_id.equal user_id our_user -> (
          if not (private_master_matches t.v_enc identity) then
            Error "the private master key does not match the validated identity"
          else if not (private_self_signing_matches t.v_enc identity) then
            Error
              "the private self-signing key does not match the validated \
               identity"
          else
            match Encryption.find_device t.v_enc user_id ~device_id with
            | None -> Error "the verified device key is unavailable"
            | Some d -> (
                let qd : Matrix_client.Keys.device_keys =
                  {
                    user_id = d.user_id;
                    device_id = d.device_id;
                    algorithms = d.algorithms;
                    keys = d.keys;
                    signatures = d.signatures;
                    dehydrated = d.dehydrated;
                    unsigned = None;
                  }
                in
                let key_id = Key_id.of_device ~algorithm:"ed25519" device_id in
                let value =
                  Option.value
                    (Encryption.device_key d ~algorithm:"ed25519")
                    ~default:""
                in
                let key_id_seen =
                  List.exists (fun (k, _) -> Key_id.equal k key_id) verified
                in
                if key_id_seen && not (has_verified_key verified key_id value)
                then
                  Error
                    "the current device key differs from the SAS-verified key"
                else if not key_id_seen then Ok ()
                else
                  match Cs.self_signing_secret identity with
                  | None -> Error "self-signing secret is unavailable"
                  | Some secret ->
                      let signed =
                        Cs.sign_device_keys ~signer:secret
                          ~signer_user_id:our_user qd
                      in
                      upload t client ~user_id
                        ~target_key:(Id.Device_id.to_string device_id)
                        ~json:(device_json signed)))
      | _ -> (
          if not (private_master_matches t.v_enc identity) then
            Error "the private master key does not match the validated identity"
          else
            match Encryption.identity_master_key t.v_enc user_id with
            | None -> Ok ()
            | Some master -> (
                if not (private_user_signing_matches t.v_enc identity) then
                  Error
                    "the private user-signing key does not match the validated \
                     identity"
                else
                  let key_id, value =
                    match master.Keys.keys with
                    | [ (key_id, value) ] -> (key_id, value)
                    | _ -> (Key_id.v ~algorithm:"ed25519" ~id:"", "")
                  in
                  if not (has_verified_key verified key_id value) then Ok ()
                  else
                    match Cs.user_signing_secret identity with
                    | None -> Error "user-signing secret is unavailable"
                    | Some secret ->
                        let signed =
                          Cs.sign_cross_signing_key ~signer:secret
                            ~signer_user_id:our_user master
                        in
                        upload t client ~user_id ~target_key:value
                          ~json:(cross_signing_json signed))))

let record_verified t client session =
  let verified =
    match Flow.session_sas session with
    | Some sas -> Sas.verified_keys sas
    | None -> []
  in
  let user_id = Flow.session_their_user_id session in
  let device_id = Flow.session_their_device_id session in
  let device_key_mismatch =
    match device_id with
    | None -> false
    | Some device_id -> (
        let key_id = Key_id.of_device ~algorithm:"ed25519" device_id in
        let key_id_seen =
          List.exists (fun (k, _) -> Key_id.equal k key_id) verified
        in
        if not key_id_seen then false
        else
          match Encryption.find_device t.v_enc user_id ~device_id with
          | None -> true
          | Some d -> (
              match Encryption.device_key d ~algorithm:"ed25519" with
              | None -> true
              | Some value -> not (has_verified_key verified key_id value)))
  in
  let master_verified =
    match Encryption.identity_master_key t.v_enc user_id with
    | None -> false
    | Some master -> (
        match master.Keys.keys with
        | [ (key_id, value) ] -> has_verified_key verified key_id value
        | _ -> false)
  in
  once t session (fun () ->
      match
        if device_key_mismatch then
          Error "the current device key differs from the SAS-verified key"
        else publish_verified t client session verified
      with
      | Error reason ->
          report t (Publication_failed { user_id; device_id; reason })
      | Ok () ->
          (match device_id with
          | Some device_id
            when List.exists
                   (fun (key_id, _) ->
                     Key_id.equal key_id
                       (Key_id.of_device ~algorithm:"ed25519" device_id))
                   verified -> (
              match Encryption.find_device t.v_enc user_id ~device_id with
              | Some d -> (
                  match Encryption.device_key d ~algorithm:"ed25519" with
                  | Some value
                    when has_verified_key verified
                           (Key_id.of_device ~algorithm:"ed25519" device_id)
                           value ->
                      Encryption.set_device_trust t.v_enc user_id ~device_id
                        Encryption.Verified
                  | _ -> ())
              | None -> ())
          | _ -> ());
          if master_verified then Encryption.trust_user_identity t.v_enc user_id;
          report t (Verified { user_id; device_id }))

let prompt_of session sas =
  match (Sas.emoji sas, Sas.decimals sas) with
  | Some emoji, Some decimals ->
      Some
        {
          their_user_id = Flow.session_their_user_id session;
          their_device_id = Flow.session_their_device_id session;
          emoji;
          decimals;
          we_started = Flow.session_we_requested session;
        }
  | _ -> None

(* Confirmation used to happen inline in [advance], which meant that a UI
   callback waiting for a user could stop the sync loop.  The callback remains
   as a compatibility adapter, but is always run on the client's switch. *)
let rec ask_confirmation t client session prompt =
  let id = flow_id session in
  if not (Hashtbl.mem t.v_pending id) then (
    Hashtbl.add t.v_pending id prompt;
    Eio.Fiber.fork_daemon ~sw:(Client.switch client) (fun () ->
        try
          (* An explicit UI response may win before this compatibility fiber
             gets scheduled. In that case do not invoke a possibly blocking
             legacy callback at all. *)
          (match (t.v_on_prompt, t.v_confirm) with
          | Some on_prompt, None -> on_prompt ~flow_id:id prompt
          | None, Some confirm ->
              if
                Eio.Mutex.use_ro t.v_mutex (fun () ->
                    Hashtbl.mem t.v_pending id)
              then
                let accept = confirm prompt in
                (* [respond] removes the pending entry before changing the
                   flow, so duplicate callback completions cannot advance it
                   twice. *)
                with_service t (fun () ->
                    respond_unlocked t client ~flow_id:id ~accept)
          | None, None | Some _, Some _ -> ());
          `Stop_daemon
        with
        | Eio.Cancel.Cancelled _ -> `Stop_daemon
        | exn ->
            Logs.err (fun m ->
                m "verification: confirmation callback for %s failed: %s" id
                  (Printexc.to_string exn));
            (try
               with_service t (fun () ->
                   cancel_prompt_unlocked t client ~flow_id:id)
             with
            | Eio.Cancel.Cancelled _ -> ()
            | exn ->
                Logs.err (fun m ->
                    m "verification: cancelling failed prompt %s failed: %s" id
                      (Printexc.to_string exn)));
            `Stop_daemon)
    |> ignore)

and respond_unlocked t client ~flow_id:id ~accept =
  match Hashtbl.find_opt t.v_pending id with
  | None -> ()
  | Some _prompt -> (
      (* Consume the one-shot prompt before looking up the session.  A peer can
         cancel between prompt delivery and the UI response; in that case the
         response is deliberately ignored and cannot resurrect the flow. *)
      Hashtbl.remove t.v_pending id;
      match Flow.find t.v_flow id with
      | Some session -> (
          match Flow.session_stage session with
          | Flow.Sas sas when Sas.stage sas = Sas.Sas_ready ->
              let o =
                if accept then Flow.confirm session else Flow.mismatch session
              in
              send_step t client session o;
              advance ~fuel:7 t client session
          | _ -> ())
      | None -> ())

and cancel_prompt_unlocked t client ~flow_id:id =
  if Hashtbl.mem t.v_pending id then begin
    Hashtbl.remove t.v_pending id;
    match Flow.find t.v_flow id with
    | None -> ()
    | Some session ->
        let step = Flow.cancel session V.Cancel_code.User in
        send_step t client session step;
        advance ~fuel:7 t client session
  end

(* Each pass may change the stage, so this loops; [fuel] guards against a
   state machine that never settles rather than marking an expected path. *)
and advance ?(fuel = 8) t client session =
  if fuel <= 0 then ()
  else
    let tracked =
      match Flow.find t.v_flow (flow_id session) with
      | Some current -> current == session
      | None -> false
    in
    if not tracked then ()
    else
      match Flow.session_stage session with
      | Flow.Ready methods when Flow.session_we_requested session -> (
          if List.exists (V.Method.equal V.Method.Sas_v1) methods then
            match lookup_peer t session with
            | None -> ()
            | Some theirs ->
                let o =
                  Flow.start_sas ~random:t.v_random ~now:(t.v_now ())
                    ~ours:(our_identity t) ~theirs session
                in
                send_step t client session o;
                advance ~fuel:(fuel - 1) t client session)
      | Flow.Sas sas when Sas.stage sas = Sas.Sas_ready -> (
          (* The one moment a person is needed.  Queue a one-shot prompt and
           return immediately; [respond] performs the state transition. *)
          match prompt_of session sas with
          | None -> ()
          | Some p -> ask_confirmation t client session p)
      | Flow.Done ->
          Hashtbl.remove t.v_pending (flow_id session);
          record_verified t client session;
          prune_terminal t session
      | Flow.Cancelled code ->
          Hashtbl.remove t.v_pending (flow_id session);
          once t session (fun () ->
              report t
                (Cancelled
                   { user_id = Flow.session_their_user_id session; code }));
          prune_terminal t session
      | _ -> ()

and lookup_peer t session =
  match Flow.session_their_device_id session with
  | None -> None
  | Some device_id ->
      lookup t ~user_id:(Flow.session_their_user_id session) ~device_id

let handle t client ~sender ~event_type ~content =
  with_service t (fun () ->
      match V.Message.of_json ~event_type content with
      | Error (`Msg msg) ->
          Logs.warn (fun m ->
              m "verification: unparseable %s: %s" event_type msg)
      | Ok message -> (
          let o =
            Flow.handle t.v_flow ~random:t.v_random ~now:(t.v_now ())
              ~ours:(our_identity t) ~lookup:(lookup t) ~sender message
          in
          match o.Flow.session with
          | Some session ->
              send_step t client session o;
              let cancelled = send_pending t client in
              advance t client session;
              List.iter (advance t client) cancelled
          | None ->
              (* The flow has gone, so the only thing to send is an
                 [m.unknown_transaction], addressed to the whole user. *)
              List.iter
                (fun msg ->
                  Verification.send_to_devices client ~their_user_id:sender
                    ~devices:[ To_device.All ] msg)
                o.Flow.send;
              List.iter (advance t client) (send_pending t client)))

let room_request_is_for_us t content =
  match
    Jsont.Json.decode Event.Key_verification_request_message_content.jsont
      content
  with
  | Ok request -> (
      match
        Id.User_id.of_string
          (Event.Key_verification_request_message_content.to_ request)
      with
      | Ok user_id -> Id.User_id.equal user_id (Encryption.user_id t.v_enc)
      | Error _ -> false)
  | Error _ -> false

let handle_room t client ~room_id ~event_id ~sender ~timestamp ~event_type
    ~content =
  with_service t (fun () ->
      if
        String.equal event_type "m.room.message"
        && not (room_request_is_for_us t content)
      then
        Logs.warn (fun m ->
            m
              "verification: ignoring in-room request not addressed to this \
               user")
      else
        match V.Message.of_json ~event_type ~room_id ~event_id content with
        | Error (`Msg msg) ->
            Logs.warn (fun m ->
                m "verification: unparseable room %s: %s" event_type msg)
        | Ok message -> (
            let o =
              Flow.handle t.v_flow ~random:t.v_random ~now:(t.v_now ())
                ~event_timestamp:timestamp ~ours:(our_identity t)
                ~lookup:(lookup t) ~sender message
            in
            match o.Flow.session with
            | Some session ->
                send_step t client session o;
                let cancelled = send_pending t client in
                advance t client session;
                List.iter (advance t client) cancelled
            | None ->
                List.iter
                  (fun msg ->
                    send_room_messages t client ~room_id ~peer_user_id:sender
                      [ msg ])
                  o.Flow.send;
                List.iter (advance t client) (send_pending t client)))

let request t client ?device_id their_user_id =
  with_service t (fun () ->
      let devices =
        match device_id with
        | Some d -> [ To_device.Device d ]
        | None ->
            let same_user =
              Id.User_id.equal their_user_id (Encryption.user_id t.v_enc)
            in
            let known =
              Encryption.devices_of t.v_enc their_user_id
              |> List.filter (fun (device : Encryption.device) ->
                  not
                    (same_user
                    && Id.Device_id.equal device.device_id (our_device_id t)))
              |> List.map (fun (device : Encryption.device) ->
                  To_device.Device device.device_id)
            in
            if known = [] then [ To_device.All ] else known
      in
      let r =
        Flow.request ~random:t.v_random ~now:(t.v_now ())
          ~from_device:(our_device_id t) ~methods:t.v_methods ~their_user_id
          ~devices t.v_flow
      in
      To_device.send_with_new_txn client
        ~event_type:"m.key.verification.request" r.Flow.to_device;
      List.iter (advance t client) (send_pending t client);
      r.Flow.session)

let request_in_room t client ~room_id ?methods ?body their_user_id =
  with_service t (fun () ->
      let content =
        V.request_in_room ~from_device:(our_device_id t) ?methods ~their_user_id
          ?body ()
      in
      let json =
        match
          Jsont.Json.encode Event.Key_verification_request_message_content.jsont
            content
        with
        | Ok json -> json
        | Error m -> raise (Error.err (Error.Json m))
      in
      let event_id =
        send_room_event t client ~room_id ~peer_user_id:their_user_id
          ~event_type:"m.room.message" ~content:json
      in
      let r =
        Flow.request_in_room ~now:(t.v_now ()) ~from_device:(our_device_id t)
          ~room_id ~event_id ~their_user_id ~content t.v_flow
      in
      List.iter (advance t client) (send_pending t client);
      r.Flow.session)

let accept t client session =
  with_service t (fun () ->
      let o =
        Flow.accept session ~from_device:(our_device_id t)
          ~our_methods:t.v_methods
      in
      send_step t client session o;
      advance t client session)

let respond t client ~flow_id ~accept =
  with_service t (fun () -> respond_unlocked t client ~flow_id ~accept)

let cancel t client session code =
  with_service t (fun () ->
      let o = Flow.cancel session code in
      send_step t client session o;
      advance t client session)

let tick t client =
  with_service t (fun () ->
      List.iter
        (fun msg ->
          let id = V.Transaction.id (V.Message.transaction msg) in
          match Flow.find t.v_flow id with
          | Some session -> send_messages t client session [ msg ]
          | None -> ())
        (Flow.tick t.v_flow ~now:(t.v_now ()));
      List.iter (advance t client) (Flow.sessions t.v_flow))

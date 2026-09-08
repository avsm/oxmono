(** End-to-end encryption, end to end.

    [test_encryption.ml] drives two {!Matrix_client.Encryption} machines by
    hand, passing their to-device traffic between them in OCaml. This one does
    not: Alice and Bob each get a {!Matrix_eio.Client}, a
    {!Matrix_eio.Sync_service} and a {!Matrix_eio.Encryption}, and the only
    thing between them is a mock homeserver that behaves like one — it takes
    whatever [/keys/upload] gives it and hands it back from [/keys/query] and
    [/keys/claim], it queues what [/sendToDevice] sends and delivers it in the
    recipient's next [/sync], and it appends what [/rooms/…/send] posts to the
    room's timeline.

    So what is under test is the wiring rather than the cryptography: that
    {!Matrix_eio.Sync_service.run} runs the encryption machine over a response
    before the base client folds it in (both halves of a message — the
    [m.room_key] in [to_device] and the [m.room.encrypted] in the timeline —
    arrive in the {e same} response, and the wrong order silently loses the
    message), that the room's members are tracked from its state, and that
    {!Matrix_eio.Send_queue} encrypts when the room says it is encrypted.

    Alice sends "hello bob" through the send queue; Bob syncs once; the
    plaintext comes back out of [room_change.decrypted]. *)

module Id = Matrix_proto.Id
module Encryption = Matrix_client.Encryption
module Sync_service = Matrix_client.Base_client
module Rnd = Matrix_client.Random
module Cs = Matrix_client.Cross_signing
module Keys = Matrix_client.Keys
module String_map = Map.Make (String)

let uid s = Result.get_ok (Id.User_id.of_string s)
let rid s = Result.get_ok (Id.Room_id.of_string s)
let did s = Result.get_ok (Id.Device_id.of_string s)
let alice_id = uid "@alice:example.org"
let bob_id = uid "@bob:example.org"
let room = rid "!room:example.org"
let room_str = Id.Room_id.to_string room
let homeserver = Uriz.of_string_exn "https://hs.example"
let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* {1 JSON helpers} *)

let jname n = (n, Jsont.Meta.none)
let jstr = Jsont.Json.string
let jint n = Jsont.Json.number (float_of_int n)

let jobj l =
  Jsont.Json.object' (List.map (fun (k, v) -> Jsont.Json.mem (jname k) v) l)

let jarr l = Jsont.Json.list l

let to_string j =
  Result.get_ok (Jsont_bytesrw.encode_string Matrix_proto.Json.Codec.json j)

let of_string s =
  Result.get_ok (Jsont_bytesrw.decode_string Matrix_proto.Json.Codec.json s)

let field name j =
  match j with
  | Jsont.Object (o, _) -> (
      match Jsont.Json.find_mem name o with
      | Some (_, v) -> Some v
      | None -> None)
  | _ -> None

let str_field name j =
  match field name j with Some (Jsont.String (s, _)) -> Some s | _ -> None

(* An object's members as an association list, for the [{user: {device: …}}]
   shape every E2EE endpoint uses. *)
let members_of j =
  match j with
  | Jsont.Object (o, _) -> List.map (fun ((k, _), v) -> (k, v)) o
  | _ -> []

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.sub haystack i n = needle || go (i + 1))
  in
  go 0

(* {1 The homeserver}

   One record, shared by both clients. Keys are ["user|device"]. *)

type hs = {
  mutable device_keys : (string * Jsont.json) list;
      (** What [/keys/upload] was given, served back by [/keys/query]. *)
  mutable one_time_keys : (string * (string * Jsont.json) list) list;
      (** The unclaimed pool, served one at a time by [/keys/claim]. *)
  mutable inbox : (string * Jsont.json list) list;
      (** Queued to-device events, drained by the recipient's [/sync]. *)
  mutable timeline : Jsont.json list;  (** The room's events, oldest first. *)
  mutable next_event : int;
  mutable cross_signing : (string * Cs.upload) list;
      (** Public cross-signing identities served from [/keys/query]. *)
  mutable signature_uploads : (string * string * Jsont.json) list;
      (** User, bare target key and signed object received by the endpoint. *)
  mutable reject_signatures : bool;
  mutable account_data : (string * string * Jsont.json) list;
      (** User, account-data event type and its content. *)
}

let hs_create () =
  {
    device_keys = [];
    one_time_keys = [];
    inbox = [];
    timeline = [];
    next_event = 0;
    cross_signing = [];
    signature_uploads = [];
    reject_signatures = false;
    account_data = [];
  }

let key user device = Id.User_id.to_string user ^ "|" ^ device
let assoc_get k l = Option.value (List.assoc_opt k l) ~default:[]
let assoc_set k v l = (k, v) :: List.remove_assoc k l

let account_data_set hs ~user ~event_type content =
  hs.account_data <-
    (Id.User_id.to_string user, event_type, content)
    :: List.filter
         (fun (u, ty, _) ->
           not
             (String.equal u (Id.User_id.to_string user)
             && String.equal ty event_type))
         hs.account_data

let account_data_get hs ~user ~event_type =
  List.find_opt
    (fun (u, ty, _) ->
      String.equal u (Id.User_id.to_string user) && String.equal ty event_type)
    hs.account_data
  |> Option.map (fun (_, _, content) -> content)

(* {2 The state a joined room is in}

   Enough for the base client to see an encrypted room with two members, and
   for [Sync_service.members] to name them. *)

let state_event ~ty ~sender ~state_key ~content =
  jobj
    [
      ("type", jstr ty);
      ("sender", jstr sender);
      ("state_key", jstr state_key);
      ("content", content);
      ("event_id", jstr ("$state-" ^ ty ^ "-" ^ state_key));
      ("origin_server_ts", jint 1000);
    ]

let member_event user =
  state_event ~ty:"m.room.member" ~sender:user ~state_key:user
    ~content:(jobj [ ("membership", jstr "join") ])

let room_state =
  [
    state_event ~ty:"m.room.create"
      ~sender:(Id.User_id.to_string alice_id)
      ~state_key:""
      ~content:(jobj [ ("creator", jstr (Id.User_id.to_string alice_id)) ]);
    state_event ~ty:"m.room.encryption"
      ~sender:(Id.User_id.to_string alice_id)
      ~state_key:""
      ~content:(jobj [ ("algorithm", jstr "m.megolm.v1.aes-sha2") ]);
    member_event (Id.User_id.to_string alice_id);
    member_event (Id.User_id.to_string bob_id);
  ]

(* {2 Handlers} *)

let respond j req = Fetch_mock.respond (to_string j) req

(* [POST /keys/upload]: remember this device's identity keys and add its
   one-time keys to the pool. The reply's count is what tells the machine
   whether to top the pool up again. *)
let keys_upload hs ~me body =
  let j = of_string body in
  (match field "device_keys" j with
  | Some dk -> hs.device_keys <- assoc_set me dk hs.device_keys
  | None -> ());
  (match field "one_time_keys" j with
  | Some otks ->
      hs.one_time_keys <-
        assoc_set me
          (assoc_get me hs.one_time_keys @ members_of otks)
          hs.one_time_keys
  | None -> ());
  jobj
    [
      ( "one_time_key_counts",
        jobj
          [
            ( "signed_curve25519",
              jint (List.length (assoc_get me hs.one_time_keys)) );
          ] );
    ]

(* [POST /keys/query]: every device we hold for each user asked about. *)
let cross_signing_json key =
  Keys.cross_signing_key_jsont |> fun codec ->
  Result.get_ok (Jsont_bytesrw.encode_string codec key) |> of_string

let keys_query hs ~me_user body =
  let j = of_string body in
  let wanted =
    match field "device_keys" j with
    | Some d -> List.map fst (members_of d)
    | None -> []
  in
  let for_user user =
    List.filter_map
      (fun (k, dk) ->
        match String.index_opt k '|' with
        | Some i when String.sub k 0 i = user ->
            Some (String.sub k (i + 1) (String.length k - i - 1), dk)
        | _ -> None)
      hs.device_keys
  in
  let cross_map select users =
    jobj
      (List.filter_map
         (fun user ->
           Option.map
             (fun upload -> (user, cross_signing_json (select upload)))
             (List.assoc_opt user hs.cross_signing))
         users)
  in
  let own = Id.User_id.to_string me_user in
  jobj
    [
      ( "device_keys",
        jobj
          (List.filter_map
             (fun u ->
               match for_user u with [] -> None | ds -> Some (u, jobj ds))
             wanted) );
      ("master_keys", cross_map (fun u -> u.Cs.master_key) wanted);
      ("self_signing_keys", cross_map (fun u -> u.Cs.self_signing_key) wanted);
      ( "user_signing_keys",
        cross_map
          (fun u -> u.Cs.user_signing_key)
          (List.filter (String.equal own) wanted) );
    ]

let signature_upload hs body =
  let request = of_string body in
  List.iter
    (fun (user, targets) ->
      List.iter
        (fun (target, signed) ->
          hs.signature_uploads <-
            hs.signature_uploads @ [ (user, target, signed) ])
        (members_of targets))
    (members_of request);
  jobj [ ("failures", if hs.reject_signatures then request else jobj []) ]

(* [POST /keys/claim]: pop one key per requested device. A claimed key is
   gone, as on a real server, so claiming twice opens two Olm sessions
   rather than reusing one key. *)
let keys_claim hs body =
  let j = of_string body in
  let requested =
    match field "one_time_keys" j with
    | Some o ->
        List.concat_map
          (fun (user, devices) ->
            List.map (fun (device, _alg) -> (user, device)) (members_of devices))
          (members_of o)
    | None -> []
  in
  let claimed =
    List.filter_map
      (fun (user, device) ->
        let k = user ^ "|" ^ device in
        match assoc_get k hs.one_time_keys with
        | [] -> None
        | (key_id, key) :: rest ->
            hs.one_time_keys <- assoc_set k rest hs.one_time_keys;
            Some (user, device, key_id, key))
      requested
  in
  let by_user =
    List.fold_left
      (fun acc (user, device, key_id, key) ->
        let devices = Option.value (List.assoc_opt user acc) ~default:[] in
        assoc_set user ((device, jobj [ (key_id, key) ]) :: devices) acc)
      [] claimed
  in
  jobj
    [ ("one_time_keys", jobj (List.map (fun (u, ds) -> (u, jobj ds)) by_user)) ]

(* [PUT /sendToDevice/{type}/{txn}]: queue one event per addressed device. *)
let send_to_device hs ~me_user ~event_type body =
  let j = of_string body in
  (match field "messages" j with
  | None -> ()
  | Some msgs ->
      List.iter
        (fun (user, devices) ->
          List.iter
            (fun (device, content) ->
              let k = user ^ "|" ^ device in
              let event =
                jobj
                  [
                    ("type", jstr event_type);
                    ("sender", jstr (Id.User_id.to_string me_user));
                    ("content", content);
                  ]
              in
              hs.inbox <-
                assoc_set k (assoc_get k hs.inbox @ [ event ]) hs.inbox)
            (members_of devices))
        (members_of msgs));
  jobj []

(* [PUT /rooms/{id}/send/{type}/{txn}]: append to the room. *)
let room_send hs ~me_user ~event_type body =
  hs.next_event <- hs.next_event + 1;
  let event_id = Printf.sprintf "$evt%d:example.org" hs.next_event in
  let event =
    jobj
      [
        ("type", jstr event_type);
        ("sender", jstr (Id.User_id.to_string me_user));
        ("content", of_string body);
        ("event_id", jstr event_id);
        ("origin_server_ts", jint (2000 + hs.next_event));
      ]
  in
  hs.timeline <- hs.timeline @ [ event ];
  jobj [ ("event_id", jstr event_id) ]

(* [GET /sync]: this device's queued to-device events, the room's state, and
   whatever timeline it has not seen. The cursor is per client, so the two
   peers advance independently. *)
let sync hs ~me ~cursor =
  let events = assoc_get me hs.inbox in
  hs.inbox <- assoc_set me [] hs.inbox;
  let seen = !cursor in
  let fresh = List.filteri (fun i _ -> i >= seen) hs.timeline in
  cursor := List.length hs.timeline;
  jobj
    [
      ("next_batch", jstr (string_of_int (List.length hs.timeline)));
      ("to_device", jobj [ ("events", jarr events) ]);
      ( "device_one_time_keys_count",
        jobj
          [
            ( "signed_curve25519",
              jint (List.length (assoc_get me hs.one_time_keys)) );
          ] );
      ( "device_lists",
        jobj
          [
            ( "changed",
              jarr
                [
                  jstr (Id.User_id.to_string alice_id);
                  jstr (Id.User_id.to_string bob_id);
                ] );
            ("left", jarr []);
          ] );
      ( "rooms",
        jobj
          [
            ( "join",
              jobj
                [
                  ( room_str,
                    jobj
                      [
                        ("state", jobj [ ("events", jarr room_state) ]);
                        ( "timeline",
                          jobj
                            [
                              ("events", jarr fresh);
                              ("limited", Jsont.Json.bool false);
                              ("prev_batch", jstr "p");
                            ] );
                      ] );
                ] );
          ] );
    ]

let body_of (req : Fetch.Middleware.request) =
  match req.body with Fetch.String s -> s | _ -> "{}"

(* The last two path segments of a [/sendToDevice/{type}/{txn}] or
   [/rooms/{id}/send/{type}/{txn}] URL name the event type. *)
let segment_after marker url =
  let parts = String.split_on_char '/' url in
  let rec go = function
    | a :: b :: _ when String.equal a marker -> Some b
    | _ :: rest -> go rest
    | [] -> None
  in
  go parts

let suffix_after marker url =
  let marker_length = String.length marker in
  let rec find i =
    if i + marker_length > String.length url then None
    else if String.sub url i marker_length = marker then
      Some
        (String.sub url (i + marker_length)
           (String.length url - i - marker_length))
    else find (i + 1)
  in
  Option.bind (find 0) (fun value ->
      match Uriz.pct_decode value with
      | This decoded -> Some decoded
      | Null -> Alcotest.fail "invalid percent escape in request URL")

let handler hs ~me_user ~me_device ~cursor (req : Fetch.Middleware.request) =
  let url = Fetch.Middleware.Url.to_string req.url in
  let me = key me_user me_device in
  let body = body_of req in
  if contains "/account_data/" url then
    match suffix_after "/account_data/" url with
    | Some event_type -> (
        match account_data_get hs ~user:me_user ~event_type with
        | Some content -> respond content req
        | None ->
            Fetch_mock.respond ~status:404
              {|{"errcode":"M_NOT_FOUND","error":"account data not found"}|} req
        )
    | None -> Fetch_mock.respond ~status:400 {|{"errcode":"M_BAD"}|} req
  else if contains "/keys/signatures/upload" url then
    respond (signature_upload hs body) req
  else if contains "/keys/upload" url then respond (keys_upload hs ~me body) req
  else if contains "/keys/query" url then
    respond (keys_query hs ~me_user body) req
  else if contains "/keys/claim" url then respond (keys_claim hs body) req
  else if contains "/sendToDevice/" url then
    let event_type =
      Option.value (segment_after "sendToDevice" url) ~default:""
    in
    respond (send_to_device hs ~me_user ~event_type body) req
  else if contains "/send/" url then
    let event_type = Option.value (segment_after "send" url) ~default:"" in
    respond (room_send hs ~me_user ~event_type body) req
  else if contains "/sync" url then respond (sync hs ~me ~cursor) req
  else respond (jobj []) req

(* {1 Peers} *)

type peer = {
  p_user : Id.User_id.t;
  p_device : string;
  p_client : Matrix_eio.Client.t;
  p_enc : Matrix_eio.Encryption.t;
  p_svc : Matrix_eio.Sync_service.t;
  p_queue : Matrix_eio.Send_queue.t;
  p_ver : Matrix_eio.Verification_service.t;
  p_prompts : Matrix_eio.Verification_service.prompt list ref;
      (** Every SAS this peer was asked to confirm. *)
  p_results : Matrix_eio.Verification_service.result list ref;
  p_answer : bool ref;  (** What this peer's user says to the next prompt. *)
}

(* A deterministic keystream, so a failure in the crypto reproduces. The
   clients' own randomness (transaction ids) still comes from the
   environment. *)
let keystream seed n =
  let b = Buffer.create (n + 32) in
  let i = ref 0 in
  while Buffer.length b < n do
    Buffer.add_string b
      Digestif.SHA256.(to_raw_string (digest_string (seed ^ string_of_int !i)));
    incr i
  done;
  Buffer.sub b 0 n

let random_of seed =
  Rnd.of_source (Eio.Flow.string_source (keystream seed 400_000))

let make_peer ?private_identity ?secret_credential ?confirm ?on_prompt
    ?room_members ~sw ~env hs ~seed ~user ~device =
  let cursor = ref 0 in
  let fetch =
    Fetch_mock.client (handler hs ~me_user:user ~me_device:device ~cursor)
  in
  let client = Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () in
  let client =
    Matrix_eio.Client.with_session client
      {
        Matrix_client.Client.user_id = user;
        device_id = did device;
        access_token = "syt_" ^ device;
        refresh_token = None;
      }
  in
  let secret_store =
    Option.map
      (fun credential ->
        Matrix_eio.Secrets.open_secret_store client ~credential)
      secret_credential
  in
  let enc =
    Matrix_eio.Encryption.create ~random:(random_of seed) ~user_id:user
      ~device_id:(did device) ()
  in
  let svc = Matrix_eio.Sync_service.of_user ~user_id:user () in
  let queue =
    Matrix_eio.Send_queue.create
      ~random:(Matrix_client.Client.random (Matrix_eio.Client.base client))
      ~user_id:user ()
  in
  let prompts = ref [] and results = ref [] and answer = ref true in
  let confirm =
    match (confirm, on_prompt) with
    | None, None ->
        Some
          (fun p ->
            prompts := !prompts @ [ p ];
            !answer)
    | confirm, _ -> confirm
  in
  let ver =
    (* A fixed clock: the flows are driven by hand here, and a real one would
       only make the ten-minute timeout a source of flakiness. *)
    Matrix_eio.Verification_service.create ~client ~encryption:enc
      ~now:(fun () -> Matrix_proto.Event.Timestamp.of_ms 0L)
      ?private_identity ?secret_store ?on_prompt ?room_members
      ~on_result:(fun r -> results := !results @ [ r ])
      ?confirm ()
  in
  {
    p_user = user;
    p_device = device;
    p_client = client;
    p_enc = enc;
    p_svc = svc;
    p_queue = queue;
    p_ver = ver;
    p_prompts = prompts;
    p_results = results;
    p_answer = answer;
  }

(* One [/sync], through the whole chain: the encryption machine sees the
   response first and performs what it asks for, then the base client folds
   it in with the machine as its decryptor. *)
let sync_peer p =
  let _response, changes =
    Matrix_eio.Sync_service.sync_once p.p_client p.p_svc ~encryption:p.p_enc
      ~verification:p.p_ver ()
  in
  changes

let change_for (changes : Sync_service.changes) =
  List.find_opt
    (fun (c : Sync_service.room_change) ->
      String.equal (Id.Room_id.to_string c.changed_room_id) room_str)
    changes.room_changes

(* Alice's send goes through the queue exactly as the running fibers would
   drive it: the encrypting sender is chosen by the room's settings, and the
   request keeps its transaction id. *)
let send_through_queue p ~body =
  let request = Matrix_eio.Send_queue.send_text p.p_queue ~room_id:room ~body in
  let send =
    Matrix_eio.Send_queue.sender_for ~encryption:p.p_enc
      ~members:(Matrix_eio.Sync_service.members p.p_svc)
      request
  in
  (request, Matrix_eio.Send_queue.send_one ?send p.p_queue p.p_client request)

(* {1 The test} *)

(* A presence change cancels the blocked HTTP poll.  The first response is
   deliberately released only after cancellation has had a scheduling turn,
   so a response accidentally accepted after the wake would be observable. *)
let test_sync_presence_wakeup () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let first_started, first_started_u = Eio.Promise.create () in
  let release_first, release_first_u = Eio.Promise.create () in
  let second_started, second_started_u = Eio.Promise.create () in
  let requests = ref [] and count = ref 0 in
  let fetch =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        requests := url :: !requests;
        incr count;
        match !count with
        | 1 ->
            Eio.Promise.resolve first_started_u ();
            Eio.Promise.await release_first;
            Fetch_mock.respond {|{"next_batch":"stale"}|} req
        | 2 ->
            Eio.Promise.resolve second_started_u ();
            Fetch_mock.respond {|{"next_batch":"accepted"}|} req
        | n -> Alcotest.failf "unexpected sync request %d" n)
  in
  let client =
    Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () |> fun client ->
    Matrix_eio.Client.with_session client
      {
        Matrix_client.Client.user_id = alice_id;
        access_token = "syt_presence";
        device_id = did "PRESENCE";
        refresh_token = None;
      }
  in
  let service =
    Matrix_eio.Sync_service.create
      (Matrix_client.Base_client.create ~user_id:alice_id ())
  in
  let seed =
    match
      Jsont.Json.decode Matrix_proto.Sync.Response.jsont
        (of_string {|{"next_batch":"seed"}|})
    with
    | Ok response -> response
    | Error error -> Alcotest.failf "seed response: %s" error
  in
  ignore (Matrix_eio.Sync_service.apply client service seed);
  let changes = ref 0 and errors = ref 0 and responses = ref 0 in
  Matrix_eio.Sync_service.run ~sw ~clock:(Eio.Stdenv.clock env) client service
    ~on_change:(fun _ _ -> incr changes)
    ~on_response:(fun _ ->
      incr responses;
      Matrix_eio.Sync_service.Stop)
    ~on_error:(fun _ ->
      incr errors;
      Matrix_eio.Sync_service.Stop)
    ();
  Eio.Promise.await first_started;
  Matrix_eio.Client.set_sync_presence client `Unavailable;
  Eio.Fiber.yield ();
  Eio.Promise.resolve release_first_u ();
  Eio.Promise.await second_started;
  check_int "two polls" 2 !count;
  check_bool "same since token on restart" true
    (List.for_all (fun url -> contains "since=seed" url) !requests);
  check_bool "latest presence on restarted poll" true
    (contains "set_presence=unavailable" (List.hd !requests));
  check_int "one applied response" 1 !changes;
  check_int "no response callback for stale poll" 1 !responses;
  check_int "no error callback for stale poll" 0 !errors

let test_sync_presence_wakeup_during_retry () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let second_started, second_started_u = Eio.Promise.create () in
  let requests = ref [] and count = ref 0 in
  let fetch =
    Fetch_mock.client (fun req ->
        let url = Fetch.Middleware.Url.to_string req.url in
        requests := url :: !requests;
        incr count;
        if !count = 1 then Fetch_mock.respond ~status:500 "{}" req
        else begin
          Eio.Promise.resolve second_started_u ();
          Fetch_mock.respond {|{"next_batch":"accepted"}|} req
        end)
  in
  let client =
    Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () |> fun client ->
    Matrix_eio.Client.with_session client
      {
        Matrix_client.Client.user_id = alice_id;
        access_token = "syt_presence_retry";
        device_id = did "PRESENCE_RETRY";
        refresh_token = None;
      }
  in
  let service =
    Matrix_eio.Sync_service.create
      (Matrix_client.Base_client.create ~user_id:alice_id ())
  in
  let changes = ref 0 and errors = ref 0 and responses = ref 0 in
  let on_error _ =
    (* The updater is queued before the retry sleep starts. Its zero-duration
       yield therefore wakes the sleep, rather than racing the first request. *)
    Eio.Fiber.fork ~sw (fun () ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.;
        Matrix_eio.Client.set_sync_presence client `Unavailable);
    incr errors;
    Matrix_eio.Sync_service.Retry_after 10.
  in
  Matrix_eio.Sync_service.run ~sw ~clock:(Eio.Stdenv.clock env) client service
    ~on_change:(fun _ _ -> incr changes)
    ~on_response:(fun _ ->
      incr responses;
      Matrix_eio.Sync_service.Stop)
    ~on_error ();
  Eio.Promise.await second_started;
  check_int "retry was woken by presence" 2 !count;
  check_bool "latest presence after retry wake" true
    (contains "set_presence=unavailable" (List.hd !requests));
  check_int "one ordinary request error" 1 !errors;
  check_int "one applied response" 1 !changes;
  check_int "one response callback" 1 !responses

let test_sync_presence_noop_does_not_restart () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let started, started_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let finished, finished_u = Eio.Promise.create () in
  let count = ref 0 in
  let fetch =
    Fetch_mock.client (fun req ->
        incr count;
        Eio.Promise.resolve started_u ();
        Eio.Promise.await release;
        Eio.Promise.resolve finished_u ();
        Fetch_mock.respond {|{"next_batch":"accepted"}|} req)
  in
  let client =
    Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () |> fun client ->
    Matrix_eio.Client.with_session client
      {
        Matrix_client.Client.user_id = alice_id;
        access_token = "syt_presence_noop";
        device_id = did "PRESENCE_NOOP";
        refresh_token = None;
      }
  in
  let service =
    Matrix_eio.Sync_service.create
      (Matrix_client.Base_client.create ~user_id:alice_id ())
  in
  Matrix_eio.Sync_service.run ~sw ~clock:(Eio.Stdenv.clock env) client service
    ~on_response:(fun _ -> Matrix_eio.Sync_service.Stop)
    ~on_change:(fun _ _ -> ())
    ();
  Eio.Promise.await started;
  Matrix_eio.Client.set_sync_presence client `Online;
  Eio.Fiber.yield ();
  check_int "no-op presence leaves poll in flight" 1 !count;
  Eio.Promise.resolve release_u ();
  Eio.Promise.await finished;
  check_int "no-op presence makes no restart" 1 !count

let test_sync_presence_override_ignores_client_change () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let started, started_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let finished, finished_u = Eio.Promise.create () in
  let count = ref 0 and requests = ref [] in
  let fetch =
    Fetch_mock.client (fun req ->
        incr count;
        requests := Fetch.Middleware.Url.to_string req.url :: !requests;
        Eio.Promise.resolve started_u ();
        Eio.Promise.await release;
        Eio.Promise.resolve finished_u ();
        Fetch_mock.respond {|{"next_batch":"accepted"}|} req)
  in
  let client =
    Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () |> fun client ->
    Matrix_eio.Client.with_session client
      {
        Matrix_client.Client.user_id = alice_id;
        access_token = "syt_presence_override";
        device_id = did "PRESENCE_OVERRIDE";
        refresh_token = None;
      }
  in
  let service =
    Matrix_eio.Sync_service.create
      (Matrix_client.Base_client.create ~user_id:alice_id ())
  in
  let params =
    { Matrix_client.Sync.default_params with set_presence = Some `Offline }
  in
  Matrix_eio.Sync_service.run ~sw ~clock:(Eio.Stdenv.clock env) client service
    ~params
    ~on_response:(fun _ -> Matrix_eio.Sync_service.Stop)
    ~on_change:(fun _ _ -> ())
    ();
  Eio.Promise.await started;
  Matrix_eio.Client.set_sync_presence client `Unavailable;
  Eio.Fiber.yield ();
  check_int "override leaves poll in flight" 1 !count;
  Eio.Promise.resolve release_u ();
  Eio.Promise.await finished;
  check_int "override makes no restart" 1 !count;
  check_bool "override remains on wire" true
    (contains "set_presence=offline" (List.hd !requests))

let test_round_trip () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let alice =
    make_peer ~sw ~env hs ~seed:"alice" ~user:alice_id ~device:"ALICEDEV"
  in
  let bob = make_peer ~sw ~env hs ~seed:"bob" ~user:bob_id ~device:"BOBDEV" in

  (* Two rounds each. The first publishes device keys and one-time keys and
     starts tracking the room's members; the second is when the [/keys/query]
     that tracking asked for actually finds the other side's keys. *)
  ignore (sync_peer bob);
  ignore (sync_peer alice);
  ignore (sync_peer bob);
  let alice_changes = sync_peer alice in

  (match change_for alice_changes with
  | None -> Alcotest.fail "Alice never saw the room"
  | Some c ->
      check_bool "Alice sees the room as encrypted" true
        (c.info.encryption <> None));
  check_bool "the machine agrees the room is encrypted" true
    (Matrix_eio.Encryption.is_room_encrypted alice.p_enc room);
  let tracked =
    List.map Id.User_id.to_string
      (Matrix_eio.Sync_service.members alice.p_svc room)
    |> List.sort String.compare
  in
  Alcotest.(check (list string))
    "both members are tracked"
    [ "@alice:example.org"; "@bob:example.org" ]
    tracked;
  check_bool "Alice holds Bob's device" true
    (Matrix_eio.Encryption.find_device alice.p_enc bob_id
       ~device_id:(did "BOBDEV")
    <> None);

  (* Alice sends. *)
  let _request, outcome = send_through_queue alice ~body:"hello bob" in
  (match outcome with
  | Matrix_eio.Send_queue.Sent_ok _ -> ()
  | Matrix_eio.Send_queue.Uploaded_ok _ ->
      Alcotest.fail "unexpected upload outcome"
  | Matrix_eio.Send_queue.Retry_in d ->
      Alcotest.failf "send asked to retry in %.1fs" d
  | Matrix_eio.Send_queue.Failed e ->
      Alcotest.failf "send failed: %s" (Matrix_client.Error.to_string e));

  (* What reached the server must be ciphertext. *)
  (match List.rev hs.timeline with
  | [] -> Alcotest.fail "nothing was sent to the room"
  | last :: _ ->
      check_string "the room event is encrypted" "m.room.encrypted"
        (Option.value (str_field "type" last) ~default:"");
      let content = Option.value (field "content" last) ~default:(jobj []) in
      check_string "with the Megolm algorithm" "m.megolm.v1.aes-sha2"
        (Option.value (str_field "algorithm" content) ~default:"");
      check_bool "and the plaintext is nowhere in it" false
        (contains "hello bob" (to_string content)));

  (* Bob syncs once. The room key and the message are in the same response. *)
  let bob_changes = sync_peer bob in
  match change_for bob_changes with
  | None -> Alcotest.fail "Bob never saw the room"
  | Some c ->
      check_int "nothing was left undecrypted" 0 (List.length c.undecrypted);
      check_int "one event was decrypted" 1 (List.length c.decrypted);
      let d = List.hd c.decrypted in
      check_string "it came in as m.room.encrypted" "m.room.encrypted"
        (Matrix_proto.Event.Event_type.to_string d.encrypted.type_);
      check_string "and came out as a message" "m.room.message"
        (Matrix_proto.Event.Event_type.to_string d.plaintext.type_);
      check_string "the body is Alice's" "hello bob"
        (Option.value (str_field "body" d.plaintext.content) ~default:"");
      check_string "the envelope survives"
        (Id.User_id.to_string alice_id)
        (Id.User_id.to_string d.plaintext.sender);
      check_bool "the event id survives" true (d.plaintext.event_id <> None);
      check_bool "the sender key is Alice's" true
        (Matrix_client.Crypto_key.Curve25519.Public.equal
           (snd (Matrix_eio.Encryption.identity_keys alice.p_enc))
           d.info.decrypted_sender_key);
      check_bool "Alice's device has signed device info but no owner chain" true
        (d.info.decrypted_verification = Encryption.Device_info)

(* {1 Verification}

   The same two clients, the same mock homeserver, and a SAS flow driven
   entirely by the sync loop: every [m.key.verification.*] the machine
   surfaces is routed into {!Matrix_eio.Verification_service}, which answers
   it and sends the reply through [/sendToDevice]. Nothing in this test
   touches the state machines directly; it only syncs, and answers the one
   question a person would be asked. *)

module Vs = Matrix_eio.Verification_service

(* Sync both sides until [stop] or the rounds run out. Each round is one
   [/sync] each, which carries at most one step of the protocol in each
   direction. *)
let rec pump ?(rounds = 12) a b stop =
  if rounds <= 0 || stop () then ()
  else (
    ignore (sync_peer a);
    ignore (sync_peer b);
    pump ~rounds:(rounds - 1) a b stop)

let introduce alice bob =
  ignore (sync_peer bob);
  ignore (sync_peer alice);
  ignore (sync_peer bob);
  ignore (sync_peer alice)

let private_identity ~seed user_id =
  let identity = Cs.create_private_identity ~user_id in
  Cs.generate_private_keys ~random:(random_of seed) identity;
  let upload =
    match Cs.build_upload identity with
    | Some upload -> upload
    | None -> Alcotest.fail "generated cross-signing identity is incomplete"
  in
  (identity, upload)

let install_identity hs user_id upload =
  hs.cross_signing <-
    assoc_set (Id.User_id.to_string user_id) upload hs.cross_signing

let install_secret_store hs user_id identity =
  let key =
    match
      Matrix_client.Secret_storage.key_of_bytes
        (keystream "alice-secret-storage-key" 32)
    with
    | Ok key -> key
    | Error (`Msg msg) -> Alcotest.failf "invalid test SSSS key: %s" msg
  in
  let key_id = "alice-test-key" in
  let description =
    Matrix_client.Secret_storage.Key_description.v
      ~random:(random_of "alice-secret-storage-description")
      ~name:"integration test cross-signing key" key
  in
  let json codec value =
    match Jsont.Json.encode codec value with
    | Ok value -> value
    | Error error -> Alcotest.failf "cannot encode test SSSS data: %s" error
  in
  let secret name value =
    let encrypted =
      Matrix_client.Secret_storage.encrypt
        ~random:(random_of ("alice-secret-storage-" ^ name))
        key ~name value
    in
    jobj
      [
        ( "encrypted",
          jobj
            [
              ( key_id,
                json Matrix_client.Secret_storage.Encrypted.jsont encrypted );
            ] );
      ]
  in
  let module S = Matrix_client.Secret_storage in
  let module K = Matrix_client.Crypto_key.Ed25519.Private in
  let encode_seed secret = Matrix_proto.Base64.encode (K.to_bytes secret) in
  account_data_set hs ~user:user_id
    ~event_type:Matrix_client.Secrets.default_key_event_type
    (jobj [ ("key", jstr key_id) ]);
  account_data_set hs ~user:user_id
    ~event_type:(Matrix_client.Secrets.key_event_type ~key_id)
    (json S.Key_description.jsont description);
  let put_secret name value =
    account_data_set hs ~user:user_id ~event_type:name (secret name value)
  in
  put_secret S.secret_cross_signing_master
    (encode_seed (Option.get (Cs.master_secret identity)));
  put_secret S.secret_cross_signing_self_signing
    (encode_seed (Option.get (Cs.self_signing_secret identity)));
  put_secret S.secret_cross_signing_user_signing
    (encode_seed (Option.get (Cs.user_signing_secret identity)));
  S.Recovery_key.encode key

let complete_sas alice bob =
  ignore
    (Vs.request alice.p_ver alice.p_client ~device_id:(did bob.p_device)
       bob.p_user);
  ignore (sync_peer bob);
  let pending =
    match
      List.find_opt
        (fun session ->
          Matrix_client.Verification.Flow.session_stage session
          = Matrix_client.Verification.Flow.Requested)
        (Vs.sessions bob.p_ver)
    with
    | Some session -> session
    | None -> Alcotest.fail "peer has no pending SAS request"
  in
  Vs.accept bob.p_ver bob.p_client pending;
  pump alice bob (fun () -> !(alice.p_results) <> [] && !(bob.p_results) <> [])

let decode_signed codec json =
  match Jsont_bytesrw.decode_string codec (to_string json) with
  | Ok value -> value
  | Error error -> Alcotest.failf "signed upload did not decode: %s" error

let one_key (key : Keys.cross_signing_key) =
  match key.keys with
  | [ (_, value) ] -> value
  | _ -> Alcotest.fail "cross-signing key did not contain exactly one key"

let publication_failed results =
  List.exists
    (function
      | Vs.Publication_failed _ -> true
      | Vs.Verified _ | Vs.Cancelled _ -> false)
    !results

let result_summary results =
  String.concat ", "
    (List.map
       (function
         | Vs.Verified _ -> "verified"
         | Vs.Cancelled { code; _ } ->
             "cancelled: "
             ^ Matrix_client.Verification.Cancel_code.to_string code
         | Vs.Publication_failed { reason; _ } ->
             "publication failed: " ^ reason)
       !results)

let verified_key_summary peer =
  peer.p_ver |> Vs.sessions
  |> List.concat_map (fun session ->
      match Matrix_client.Verification.Flow.session_sas session with
      | None -> []
      | Some sas -> Matrix_client.Verification.Sas.verified_keys sas)
  |> List.map (fun (key_id, value) ->
      Matrix_client.Crypto_key.Key_id.to_string key_id ^ "=" ^ value)
  |> String.concat ", "

let test_verification_service_rejects_client_mismatch () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let private_, upload = private_identity ~seed:"binding-identity" alice_id in
  install_identity hs alice_id upload;
  let credential = install_secret_store hs alice_id private_ in
  let make_client ~homeserver ~user ~device =
    let cursor = ref 0 in
    let fetch =
      Fetch_mock.client (handler hs ~me_user:user ~me_device:device ~cursor)
    in
    Matrix_eio.Client.create ~sw ~env ~homeserver ~fetch () |> fun client ->
    Matrix_eio.Client.with_session client
      {
        Matrix_client.Client.user_id = user;
        device_id = did device;
        access_token = "syt_" ^ device;
        refresh_token = None;
      }
  in
  let matching_client =
    make_client ~homeserver ~user:alice_id ~device:"ALICEDEV"
  in
  let store =
    Matrix_eio.Secrets.open_secret_store matching_client ~credential
  in
  let alice_encryption =
    Matrix_eio.Encryption.create
      ~random:(random_of "binding-alice")
      ~user_id:alice_id ~device_id:(did "ALICEDEV") ()
  in
  let expect_invalid label f =
    match f () with
    | exception Invalid_argument _ -> ()
    | _ -> Alcotest.failf "%s: expected Invalid_argument" label
  in
  let other_homeserver_client =
    make_client
      ~homeserver:(Uriz.of_string_exn "https://other.example")
      ~user:alice_id ~device:"ALICEDEV"
  in
  expect_invalid "store homeserver mismatch" (fun () ->
      Matrix_eio.Verification_service.create ~client:other_homeserver_client
        ~encryption:alice_encryption ~secret_store:store
        ~confirm:(fun _ -> true)
        ());
  let bob_encryption =
    Matrix_eio.Encryption.create ~random:(random_of "binding-bob")
      ~user_id:bob_id ~device_id:(did "BOBDEV") ()
  in
  expect_invalid "client user mismatch" (fun () ->
      Matrix_eio.Verification_service.create ~client:matching_client
        ~encryption:bob_encryption
        ~confirm:(fun _ -> true)
        ())

let test_sas_publishes_other_identity () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let alice_private, alice_upload =
    private_identity ~seed:"alice-cross-signing" alice_id
  in
  let bob_private, bob_upload =
    private_identity ~seed:"bob-cross-signing" bob_id
  in
  install_identity hs alice_id alice_upload;
  install_identity hs bob_id bob_upload;
  let alice_credential = install_secret_store hs alice_id alice_private in
  let alice =
    make_peer ~secret_credential:alice_credential ~sw ~env hs
      ~seed:"alice-sas-publish" ~user:alice_id ~device:"ALICEDEV"
  in
  let bob =
    make_peer ~private_identity:bob_private ~sw ~env hs ~seed:"bob-sas-publish"
      ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  check_bool "Alice loaded her own cross-signing identity" true
    (Option.is_some
       (Encryption.identity_master_key
          (Matrix_eio.Encryption.machine alice.p_enc)
          alice_id));
  check_bool "Alice loaded Bob's cross-signing identity" true
    (Option.is_some
       (Encryption.identity_master_key
          (Matrix_eio.Encryption.machine alice.p_enc)
          bob_id));
  complete_sas alice bob;
  let bob_target = one_key bob_upload.master_key in
  (match
     List.find_opt
       (fun (user, target, _) ->
         String.equal user (Id.User_id.to_string bob_id)
         && String.equal target bob_target)
       hs.signature_uploads
   with
  | Some (user, target, json) ->
      check_string "signed user" (Id.User_id.to_string bob_id) user;
      check_string "bare master-key target" bob_target target;
      let signed = decode_signed Keys.cross_signing_key_jsont json in
      check_bool "master is signed by Alice's user-signing key" true
        (Cs.verify_key
           ~signer:(Cs.key ~role:Cs.User_signing alice_upload.user_signing_key)
           ~signed:(Cs.key ~role:Cs.Master signed))
  | None ->
      Alcotest.failf "expected Bob's signature upload, got %d (%s; keys: %s)"
        (List.length hs.signature_uploads)
        (result_summary alice.p_results)
        (verified_key_summary alice));
  check_bool "publication succeeded" false (publication_failed alice.p_results);
  check_bool "the published identity is trusted" true
    (Encryption.identity_status
       (Matrix_eio.Encryption.machine alice.p_enc)
       bob_id
    = Some Encryption.Identity_verified)

let test_sas_publishes_own_device () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let private_, upload = private_identity ~seed:"alice-own-signing" alice_id in
  install_identity hs alice_id upload;
  let first =
    make_peer ~private_identity:private_ ~sw ~env hs ~seed:"alice-first"
      ~user:alice_id ~device:"ALICEDEV"
  in
  let second =
    make_peer ~private_identity:private_ ~sw ~env hs ~seed:"alice-second"
      ~user:alice_id ~device:"ALICE2"
  in
  introduce first second;
  check_bool "first device loaded its cross-signing identity" true
    (Option.is_some
       (Encryption.identity_master_key
          (Matrix_eio.Encryption.machine first.p_enc)
          alice_id));
  let peer_key peer device =
    match
      Matrix_eio.Encryption.find_device peer.p_enc alice_id
        ~device_id:(did device)
    with
    | Some found -> Option.get (Matrix_eio.Encryption.device_ed25519 found)
    | None -> Alcotest.failf "device %s was not queried" device
  in
  check_bool "first queried the second device's exact key" true
    (Matrix_client.Crypto_key.Ed25519.Public.equal (peer_key first "ALICE2")
       (fst (Matrix_eio.Encryption.identity_keys second.p_enc)));
  check_bool "second queried the first device's exact key" true
    (Matrix_client.Crypto_key.Ed25519.Public.equal
       (peer_key second "ALICEDEV")
       (fst (Matrix_eio.Encryption.identity_keys first.p_enc)));
  complete_sas first second;
  (match
     List.find_opt
       (fun (user, target, _) ->
         String.equal user (Id.User_id.to_string alice_id)
         && String.equal target "ALICE2")
       hs.signature_uploads
   with
  | Some (user, target, json) ->
      check_string "own signed user" (Id.User_id.to_string alice_id) user;
      check_string "bare device target" "ALICE2" target;
      let signed = decode_signed Keys.device_keys_jsont json in
      check_bool "device is signed by the self-signing key" true
        (Cs.verify_device_signature
           ~self_signing_key:
             (Cs.key ~role:Cs.Self_signing upload.self_signing_key)
           ~device:(Cs.create_device signed))
  | None ->
      Alcotest.failf
        "expected one device-signature upload, got %d (%s; keys: %s)"
        (List.length hs.signature_uploads)
        (result_summary first.p_results)
        (verified_key_summary first));
  check_bool "the exact verified device becomes trusted" true
    (match
       Matrix_eio.Encryption.find_device first.p_enc alice_id
         ~device_id:(did "ALICE2")
     with
    | Some device -> device.trust = Encryption.Verified
    | None -> false)

let test_sas_publication_failure_suppresses_trust () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  hs.reject_signatures <- true;
  let alice_private, alice_upload =
    private_identity ~seed:"alice-rejected-signing" alice_id
  in
  let bob_private, bob_upload =
    private_identity ~seed:"bob-rejected-signing" bob_id
  in
  install_identity hs alice_id alice_upload;
  install_identity hs bob_id bob_upload;
  let alice =
    make_peer ~private_identity:alice_private ~sw ~env hs ~seed:"alice-rejected"
      ~user:alice_id ~device:"ALICEDEV"
  in
  let bob =
    make_peer ~private_identity:bob_private ~sw ~env hs ~seed:"bob-rejected"
      ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  complete_sas alice bob;
  check_bool "rejection is surfaced" true (publication_failed alice.p_results);
  check_bool "rejected identity is not trusted" true
    (Encryption.identity_status
       (Matrix_eio.Encryption.machine alice.p_enc)
       bob_id
    <> Some Encryption.Identity_verified);
  check_bool "rejected device is not trusted" true
    (match
       Matrix_eio.Encryption.find_device alice.p_enc bob_id
         ~device_id:(did "BOBDEV")
     with
    | Some device -> device.trust <> Encryption.Verified
    | None -> true)

let test_sas_rejects_mismatched_private_identity () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let _, alice_published =
    private_identity ~seed:"alice-published-identity" alice_id
  in
  let alice_wrong, _ =
    private_identity ~seed:"alice-wrong-private-identity" alice_id
  in
  let bob_private, bob_upload =
    private_identity ~seed:"bob-private-identity" bob_id
  in
  install_identity hs alice_id alice_published;
  install_identity hs bob_id bob_upload;
  let alice =
    make_peer ~private_identity:alice_wrong ~sw ~env hs
      ~seed:"alice-wrong-identity-device" ~user:alice_id ~device:"ALICEDEV"
  in
  let bob =
    make_peer ~private_identity:bob_private ~sw ~env hs
      ~seed:"bob-right-identity-device" ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  complete_sas alice bob;
  check_bool "mismatched private identity is surfaced" true
    (publication_failed alice.p_results);
  check_bool "a mismatched identity publishes nothing" true
    (not
       (List.exists
          (fun (user, target, _) ->
            String.equal user (Id.User_id.to_string bob_id)
            && String.equal target (one_key bob_upload.master_key))
          hs.signature_uploads));
  check_bool "mismatch does not grant device trust" true
    (match
       Matrix_eio.Encryption.find_device alice.p_enc bob_id
         ~device_id:(did "BOBDEV")
     with
    | Some device -> device.trust <> Encryption.Verified
    | None -> true)

let test_sas_rejects_mismatched_user_signing_secret () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let alice_private, alice_upload =
    private_identity ~seed:"alice-matching-master-self" alice_id
  in
  let bob_private, bob_upload =
    private_identity ~seed:"bob-matching-master-self" bob_id
  in
  (* Keep Alice's master and self-signing halves matching the published
     identity, but replace only the private user-signing half. *)
  let wrong_user_signing, _ =
    Matrix_client.Crypto_key.Ed25519.generate
      ~random:(random_of "alice-wrong-user-signing")
      ()
  in
  Cs.set_user_signing_secret alice_private (Some wrong_user_signing);
  install_identity hs alice_id alice_upload;
  install_identity hs bob_id bob_upload;
  let alice =
    make_peer ~private_identity:alice_private ~sw ~env hs
      ~seed:"alice-user-signing-mismatch" ~user:alice_id ~device:"ALICEDEV"
  in
  let bob =
    make_peer ~private_identity:bob_private ~sw ~env hs
      ~seed:"bob-user-signing-match" ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  complete_sas alice bob;
  check_bool "mismatched user-signing secret is surfaced" true
    (publication_failed alice.p_results);
  check_bool "mismatched user-signing secret publishes nothing" true
    (not
       (List.exists
          (fun (user, target, _) ->
            String.equal user (Id.User_id.to_string bob_id)
            && String.equal target (one_key bob_upload.master_key))
          hs.signature_uploads));
  check_bool "mismatched user-signing secret grants no identity trust" true
    (Encryption.identity_status
       (Matrix_eio.Encryption.machine alice.p_enc)
       bob_id
    <> Some Encryption.Identity_verified)

let test_sas () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let alice =
    make_peer ~sw ~env hs ~seed:"alice-sas" ~user:alice_id ~device:"ALICEDEV"
  in
  let bob =
    make_peer ~sw ~env hs ~seed:"bob-sas" ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  check_bool "Alice holds Bob's device" true
    (Matrix_eio.Encryption.find_device alice.p_enc bob_id
       ~device_id:(did "BOBDEV")
    <> None);
  check_bool "Bob holds Alice's device" true
    (Matrix_eio.Encryption.find_device bob.p_enc alice_id
       ~device_id:(did "ALICEDEV")
    <> None);

  (* Alice asks Bob's device to verify. *)
  let _session =
    Vs.request alice.p_ver alice.p_client ~device_id:(did "BOBDEV") bob_id
  in
  ignore (sync_peer bob);
  (match Vs.sessions bob.p_ver with
  | [ s ] ->
      check_bool "Bob has a pending request" true
        (Matrix_client.Verification.Flow.session_stage s
        = Matrix_client.Verification.Flow.Requested);
      (* Accepting is a decision, so nothing answered it automatically. *)
      Vs.accept bob.p_ver bob.p_client s
  | ss -> Alcotest.failf "Bob has %d sessions, expected one" (List.length ss));

  (* From here the flow runs itself: ready, start, accept, key, key, then the
     one prompt each side shows its user, then the MACs and the dones. *)
  pump alice bob (fun () -> !(alice.p_results) <> [] && !(bob.p_results) <> []);

  check_int "Alice was asked once" 1 (List.length !(alice.p_prompts));
  check_int "Bob was asked once" 1 (List.length !(bob.p_prompts));
  let a_emoji = (List.hd !(alice.p_prompts)).Vs.emoji in
  let b_emoji = (List.hd !(bob.p_prompts)).Vs.emoji in
  check_int "seven emoji" 7 (List.length a_emoji);
  Alcotest.(check (list string))
    "both sides see the same emoji"
    (List.map (fun (e : Vs.emoji) -> e.symbol) a_emoji)
    (List.map (fun (e : Vs.emoji) -> e.symbol) b_emoji);
  check_bool "and the same decimals" true
    ((List.hd !(alice.p_prompts)).Vs.decimals
   = (List.hd !(bob.p_prompts)).Vs.decimals);
  check_bool "Alice knows she started it" true
    (List.hd !(alice.p_prompts)).Vs.we_started;
  check_bool "Bob knows he did not" false
    (List.hd !(bob.p_prompts)).Vs.we_started;

  let verified = function
    | Vs.Verified _ -> true
    | Vs.Cancelled _ | Vs.Publication_failed _ -> false
  in
  check_bool "Alice's flow succeeded" true
    (List.exists verified !(alice.p_results));
  check_bool "Bob's flow succeeded" true (List.exists verified !(bob.p_results));
  check_int "Alice's terminal flow is pruned" 0
    (List.length (Vs.sessions alice.p_ver));
  check_int "Bob's terminal flow is pruned" 0
    (List.length (Vs.sessions bob.p_ver));

  (* And the point of the whole exercise: each side now trusts the other's
     device, which is what the gossip policy and the decrypted events'
     verification state rest on. *)
  let trust_of enc user device =
    match Matrix_eio.Encryption.find_device enc user ~device_id:device with
    | None -> Alcotest.failf "no device %a" Id.Device_id.pp device
    | Some d -> d.trust
  in
  check_bool "Alice marked Bob's device verified" true
    (trust_of alice.p_enc bob_id (did "BOBDEV") = Encryption.Verified);
  check_bool "Bob marked Alice's device verified" true
    (trust_of bob.p_enc alice_id (did "ALICEDEV") = Encryption.Verified)

let test_sas_in_room () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let members _ = [ alice_id; bob_id ] in
  let alice =
    make_peer ~room_members:members ~sw ~env hs ~seed:"alice-room-sas"
      ~user:alice_id ~device:"ALICEDEV"
  in
  let bob =
    make_peer ~room_members:members ~sw ~env hs ~seed:"bob-room-sas"
      ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  ignore (Vs.request_in_room alice.p_ver alice.p_client ~room_id:room bob_id);
  (match List.rev hs.timeline with
  | event :: _ ->
      check_string "in-room request uses encrypted envelope" "m.room.encrypted"
        (Option.value (str_field "type" event) ~default:"")
  | [] -> Alcotest.fail "in-room request did not reach the room");
  ignore (sync_peer bob);
  let pending =
    match
      List.find_opt
        (fun session ->
          Matrix_client.Verification.Flow.session_stage session
          = Matrix_client.Verification.Flow.Requested)
        (Vs.sessions bob.p_ver)
    with
    | Some session -> session
    | None -> Alcotest.fail "Bob did not receive the in-room request"
  in
  Vs.accept bob.p_ver bob.p_client pending;
  pump alice bob (fun () -> !(alice.p_results) <> [] && !(bob.p_results) <> []);
  let verified = function
    | Vs.Verified _ -> true
    | Vs.Cancelled _ | Vs.Publication_failed _ -> false
  in
  check_bool "Alice's in-room flow succeeded" true
    (List.exists verified !(alice.p_results));
  check_bool "Bob's in-room flow succeeded" true
    (List.exists verified !(bob.p_results));
  check_bool "every verification room event stays encrypted" true
    (List.for_all
       (fun event ->
         String.equal
           (Option.value (str_field "type" event) ~default:"")
           "m.room.encrypted")
       hs.timeline)

let test_sas_mismatch () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let alice =
    make_peer ~sw ~env hs ~seed:"alice-no" ~user:alice_id ~device:"ALICEDEV"
  in
  let bob =
    make_peer ~sw ~env hs ~seed:"bob-no" ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  (* Bob's user says the emoji do not match. *)
  bob.p_answer := false;
  let _session =
    Vs.request alice.p_ver alice.p_client ~device_id:(did "BOBDEV") bob_id
  in
  ignore (sync_peer bob);
  (match Vs.sessions bob.p_ver with
  | [ s ] -> Vs.accept bob.p_ver bob.p_client s
  | _ -> Alcotest.fail "Bob has no pending request");
  pump alice bob (fun () -> !(alice.p_results) <> []);
  let cancelled = function
    | Vs.Cancelled { code; _ } ->
        code = Matrix_client.Verification.Cancel_code.Mismatched_sas
    | Vs.Verified _ | Vs.Publication_failed _ -> false
  in
  check_bool "Alice is told the strings did not match" true
    (List.exists cancelled !(alice.p_results));
  check_bool "and Bob's device is not verified" false
    (match
       Matrix_eio.Encryption.find_device alice.p_enc bob_id
         ~device_id:(did "BOBDEV")
     with
    | Some d -> d.trust = Encryption.Verified
    | None -> false)

(* A confirmation callback may wait for a UI. It must run off the sync fiber,
   while an explicit response still advances the exact one-shot prompt. *)
let test_verification_confirmation_is_nonblocking () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let entered, entered_r = Eio.Promise.create () in
  let release, release_r = Eio.Promise.create () in
  let prompts = ref [] in
  let alice =
    make_peer ~sw ~env hs ~seed:"alice-async" ~user:alice_id ~device:"ALICEDEV"
      ~confirm:(fun p ->
        prompts := p :: !prompts;
        Eio.Promise.resolve entered_r ();
        Eio.Promise.await release;
        true)
  in
  let bob =
    make_peer ~sw ~env hs ~seed:"bob-async" ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  ignore
    (Vs.request alice.p_ver alice.p_client ~device_id:(did bob.p_device) bob_id);
  ignore (sync_peer bob);
  (match Vs.sessions bob.p_ver with
  | [ s ] -> Vs.accept bob.p_ver bob.p_client s
  | _ -> Alcotest.fail "Bob has no pending SAS request");
  (* This would deadlock when [confirm] was called inline by [advance]. *)
  pump ~rounds:12 alice bob (fun () -> false);
  Eio.Promise.await entered;
  let session =
    match
      List.find_opt
        (fun s ->
          match Matrix_client.Verification.Flow.session_stage s with
          | Matrix_client.Verification.Flow.Sas sas ->
              Matrix_client.Verification.Sas.stage sas
              = Matrix_client.Verification.Sas.Sas_ready
          | _ -> false)
        (Vs.sessions alice.p_ver)
    with
    | Some s -> s
    | None -> Alcotest.fail "Alice has no pending SAS confirmation"
  in
  let flow_id =
    Matrix_client.Verification.Transaction.id
      (Matrix_client.Verification.Flow.session_transaction session)
  in
  Vs.respond alice.p_ver alice.p_client ~flow_id ~accept:true;
  (* A duplicate response and the old callback's eventual response are both
     ignored after the one-shot pending entry is consumed. *)
  Vs.respond alice.p_ver alice.p_client ~flow_id ~accept:false;
  Eio.Promise.resolve release_r ();
  pump alice bob (fun () -> !(alice.p_results) <> [] && !(bob.p_results) <> []);
  check_int "one confirmation prompt" 1 (List.length !prompts);
  check_int "one Alice result" 1 (List.length !(alice.p_results));
  check_int "one Bob result" 1 (List.length !(bob.p_results))

let test_verification_prompt_exception_is_isolated () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let hs = hs_create () in
  let callback_ran = ref false in
  let alice =
    make_peer ~sw ~env hs ~seed:"alice-prompt-exn" ~user:alice_id
      ~device:"ALICEDEV" ~on_prompt:(fun ~flow_id:_ _ ->
        callback_ran := true;
        failwith "prompt renderer failed")
  in
  let bob =
    make_peer ~sw ~env hs ~seed:"bob-prompt-exn" ~user:bob_id ~device:"BOBDEV"
  in
  introduce alice bob;
  ignore
    (Vs.request alice.p_ver alice.p_client ~device_id:(did bob.p_device) bob_id);
  ignore (sync_peer bob);
  (match Vs.sessions bob.p_ver with
  | [ session ] -> Vs.accept bob.p_ver bob.p_client session
  | _ -> Alcotest.fail "Bob has no pending SAS request");
  pump alice bob (fun () ->
      !callback_ran && !(alice.p_results) <> [] && !(bob.p_results) <> []);
  check_bool "raising prompt callback ran" true !callback_ran;
  let cancelled_by_user = function
    | Vs.Cancelled { code = Matrix_client.Verification.Cancel_code.User; _ } ->
        true
    | Vs.Verified _ | Vs.Publication_failed _ | Vs.Cancelled _ -> false
  in
  check_bool "callback failure cleanly cancels the local flow" true
    (List.exists cancelled_by_user !(alice.p_results));
  check_int "callback failure reports once" 1 (List.length !(alice.p_results))

let () =
  Alcotest.run "e2ee integration"
    [
      ( "sync service",
        [
          Alcotest.test_case "presence wake restarts HTTP poll" `Quick
            test_sync_presence_wakeup;
          Alcotest.test_case "presence wake interrupts retry delay" `Quick
            test_sync_presence_wakeup_during_retry;
          Alcotest.test_case "no-op presence does not restart poll" `Quick
            test_sync_presence_noop_does_not_restart;
          Alcotest.test_case "explicit presence override is fixed" `Quick
            test_sync_presence_override_ignores_client_change;
        ] );
      ( "two clients",
        [
          Alcotest.test_case "send encrypted, sync, decrypt" `Quick
            test_round_trip;
        ] );
      ( "verification",
        [
          Alcotest.test_case "SAS over the sync loop" `Quick test_sas;
          Alcotest.test_case "SAS in an encrypted room" `Quick test_sas_in_room;
          Alcotest.test_case "service rejects client/account mismatch" `Quick
            test_verification_service_rejects_client_mismatch;
          Alcotest.test_case "SAS publishes another user's identity" `Quick
            test_sas_publishes_other_identity;
          Alcotest.test_case "SAS publishes an own-device signature" `Quick
            test_sas_publishes_own_device;
          Alcotest.test_case "signature rejection suppresses trust" `Quick
            test_sas_publication_failure_suppresses_trust;
          Alcotest.test_case "mismatched private identity is rejected" `Quick
            test_sas_rejects_mismatched_private_identity;
          Alcotest.test_case "mismatched user-signing secret is rejected" `Quick
            test_sas_rejects_mismatched_user_signing_secret;
          Alcotest.test_case "the user says no" `Quick test_sas_mismatch;
          Alcotest.test_case "SAS confirmation is non-blocking" `Quick
            test_verification_confirmation_is_nonblocking;
          Alcotest.test_case "prompt callback exception is isolated" `Quick
            test_verification_prompt_exception_is_isolated;
        ] );
    ]

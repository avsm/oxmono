module Slice = Mqttz.Slice
module V3 = Mqttz.V3.Packet
module V5 = Mqttz.V5.Packet
module Property = Mqttz.V5.Property

type config = {
  client_id : string;
  version : Mqttz.Protocol_version.t;
  keep_alive : int;
  credentials : Mqttz.Credentials.t option;
  will : Mqttz.Will.t option;
  max_packet_size : int;
  message_capacity : int;
  operation_timeout : float;
}

let default_config ~client_id =
  {
    client_id;
    version = `V5_0;
    keep_alive = 60;
    credentials = None;
    will = None;
    max_packet_size = Mqttz.Frame.default_max_size;
    message_capacity = 32;
    operation_timeout = 30.;
  }

let validate_config c =
  if c.message_capacity < 1 || c.message_capacity > 65535 then
    invalid_arg "message_capacity must be 1..65535";
  if c.max_packet_size < 2 || c.max_packet_size > 268435460 then
    invalid_arg "max_packet_size must be 2..268435460";
  if not (c.operation_timeout > 0. && c.operation_timeout < infinity) then
    invalid_arg "operation_timeout must be finite and positive";
  if c.keep_alive < 0 || c.keep_alive > 65535 then
    invalid_arg "keep_alive must be 0..65535"

type message = {
  topic : string;
  payload : Slice.t;
  qos : Mqttz.Qos.t;
  retain : bool;
  properties : Property.t list;
}

exception Closed
exception Protocol_error of string
exception Rejected of string

type packet = V3 of V3.t | V5 of V5.t

type t = {
  config : config;
  transport : Transport.t;
  timeout : Eio.Time.Timeout.t;
  tx : Eio.Mutex.t;
  operation : Eio.Mutex.t;
  messages : message Eio.Stream.t;
  ended : exn Eio.Promise.t;
  end_session : exn Eio.Promise.u;
  mutable closed : bool;
  mutable pending : (int * packet Eio.Stream.t) option;
  mutable next_id : int;
  received_qos2 : (int, unit) Hashtbl.t;
  mutable awaiting_ping : bool;
  mutable keep_alive : int;
  mutable peer_max_size : int;
  mutable peer_max_qos : Mqttz.Qos.t;
  mutable peer_retain : bool;
  mutable peer_wildcards : bool;
  mutable peer_shared : bool;
}

let is_connected t = not t.closed

let fail t ex =
  if not t.closed then begin
    t.closed <- true;
    Eio.Promise.resolve t.end_session ex;
    Eio.Cancel.protect (fun () -> try t.transport.close () with _ -> ())
  end

let close t = fail t Closed
let check_open t = if t.closed then raise (Eio.Promise.await t.ended)
let protocol message = raise (Protocol_error message)
let encoded = function V3 p -> V3.encode p | V5 p -> V5.encode p

let prepare t packet =
  let slices = encoded packet in
  let size = List.fold_left (fun n s -> n + Slice.length s) 0 slices in
  if size > t.peer_max_size then invalid_arg "packet exceeds broker size limit";
  slices

let send t packet =
  (* Validate before taking the write lock. A failure here has sent no bytes. *)
  let slices = prepare t packet in
  Eio.Mutex.use_ro t.tx (fun () ->
      check_open t;
      match
        Eio.Time.Timeout.run_exn t.timeout (fun () -> t.transport.write slices)
      with
      | () -> ()
      | exception ex ->
          fail t ex;
          raise ex)

let read t =
  let frame =
    try Transport.read_frame t.transport ~max_size:t.config.max_packet_size
    with Mqttz.Frame.Malformed message -> protocol message
  in
  match t.config.version with
  | `V3_1_1 -> (
      match V3.decode ~max_size:t.config.max_packet_size frame with
      | Ok p -> V3 p
      | Error e -> protocol e)
  | `V5_0 -> (
      match V5.decode ~max_size:t.config.max_packet_size frame with
      | Ok p -> V5 p
      | Error e -> protocol e)

let await t queue =
  Eio.Fiber.first
    (fun () -> Eio.Stream.take queue)
    (fun () -> raise (Eio.Promise.await t.ended))

let receive t =
  match Eio.Stream.take_nonblocking t.messages with
  | Some message -> message
  | None -> await t t.messages

let deliver t message =
  if Eio.Stream.length t.messages >= t.config.message_capacity then
    protocol "incoming message queue is full";
  Eio.Stream.add t.messages message

let acknowledge t id packet =
  match t.pending with
  | Some (expected, queue) when id = expected ->
      if Eio.Stream.length queue >= 4 then protocol "excess acknowledgements";
      Eio.Stream.add queue packet
  | _ -> protocol "acknowledgement for an unknown packet identifier"

let ack_packet version kind id =
  match (version, kind) with
  | `V3_1_1, `Puback -> V3 (V3.Puback id)
  | `V3_1_1, `Pubrec -> V3 (V3.Pubrec id)
  | `V3_1_1, `Pubrel -> V3 (V3.Pubrel id)
  | `V3_1_1, `Pubcomp -> V3 (V3.Pubcomp id)
  | `V5_0, `Puback ->
      V5 (V5.Puback { packet_id = id; reason_code = `Success; properties = [] })
  | `V5_0, `Pubrec ->
      V5 (V5.Pubrec { packet_id = id; reason_code = `Success; properties = [] })
  | `V5_0, `Pubrel ->
      V5 (V5.Pubrel { packet_id = id; reason_code = `Success; properties = [] })
  | `V5_0, `Pubcomp ->
      V5
        (V5.Pubcomp { packet_id = id; reason_code = `Success; properties = [] })

let incoming_publish t message id =
  match (message.qos, id) with
  | `At_most_once, None -> deliver t message
  | `At_least_once, Some id ->
      deliver t message;
      send t (ack_packet t.config.version `Puback id)
  | `Exactly_once, Some id ->
      if not (Hashtbl.mem t.received_qos2 id) then begin
        if Hashtbl.length t.received_qos2 >= t.config.message_capacity then
          protocol "too many incomplete QoS 2 exchanges";
        deliver t message;
        Hashtbl.add t.received_qos2 id ()
      end;
      send t (ack_packet t.config.version `Pubrec id)
  | _ -> protocol "invalid incoming PUBLISH identifier"

let incoming_pubrel t id =
  let known = Hashtbl.mem t.received_qos2 id in
  Hashtbl.remove t.received_qos2 id;
  match t.config.version with
  | `V5_0 when not known ->
      send t
        (V5
           (V5.Pubcomp
              {
                packet_id = id;
                reason_code = `Packet_identifier_not_found;
                properties = [];
              }))
  | version -> send t (ack_packet version `Pubcomp id)

let handle t = function
  | V3 (V3.Publish p) ->
      incoming_publish t
        {
          topic = p.topic;
          payload = p.payload;
          qos = p.qos;
          retain = p.retain;
          properties = [];
        }
        p.packet_id
  | V5 (V5.Publish p) ->
      (* CONNECT advertises no topic aliases. Accepting an alias here would
         allow an unnegotiated empty topic into the application. *)
      if
        List.exists
          (function Property.Topic_alias _ -> true | _ -> false)
          p.properties
      then protocol "broker sent an unnegotiated topic alias";
      incoming_publish t
        {
          topic = p.topic;
          payload = p.payload;
          qos = p.qos;
          retain = p.retain;
          properties = p.properties;
        }
        p.packet_id
  | V3 (V3.Pubrel id) -> incoming_pubrel t id
  | V5 (V5.Pubrel p) -> incoming_pubrel t p.packet_id
  | V3 (V3.Puback id | V3.Pubrec id | V3.Pubcomp id | V3.Unsuback id) as p ->
      acknowledge t id p
  | V3 (V3.Suback a) as p -> acknowledge t a.packet_id p
  | V5 (V5.Puback a) as p -> acknowledge t a.packet_id p
  | V5 (V5.Pubrec a) as p -> acknowledge t a.packet_id p
  | V5 (V5.Pubcomp a) as p -> acknowledge t a.packet_id p
  | V5 (V5.Suback a) as p -> acknowledge t a.packet_id p
  | V5 (V5.Unsuback a) as p -> acknowledge t a.packet_id p
  | V3 V3.Pingresp | V5 V5.Pingresp -> t.awaiting_ping <- false
  | V5 (V5.Disconnect d) ->
      raise (Rejected (Mqttz.V5.Reason_code.to_string d.reason_code))
  | _ -> protocol "unexpected server packet"

let reader t () =
  (try
     while not t.closed do
       handle t (read t)
     done
   with ex -> fail t ex);
  `Stop_daemon

let pinger t clock () =
  (try
     while not t.closed do
       Eio.Fiber.first
         (fun () -> Eio.Time.Mono.sleep clock (float t.keep_alive *. 0.5))
         (fun () -> ignore (Eio.Promise.await t.ended));
       if not t.closed then begin
         if t.awaiting_ping then protocol "broker did not respond to PINGREQ";
         t.awaiting_ping <- true;
         send t
           (match t.config.version with
           | `V3_1_1 -> V3 V3.Pingreq
           | `V5_0 -> V5 V5.Pingreq)
       end
     done
   with ex -> fail t ex);
  `Stop_daemon

let connect_packet config =
  match config.version with
  | `V3_1_1 ->
      V3
        (V3.Connect
           {
             client_id = config.client_id;
             clean_session = true;
             keep_alive = config.keep_alive;
             credentials = config.credentials;
             will = config.will;
           })
  | `V5_0 ->
      let will =
        Option.map
          (fun w ->
            Mqttz.V5.Will_properties.
              {
                will_topic = Mqttz.Will.topic w;
                will_payload = Mqttz.Will.payload w;
                will_qos = Mqttz.Will.qos w;
                will_retain = Mqttz.Will.retain w;
                will_properties = [];
              })
          config.will
      in
      V5
        (V5.Connect
           {
             client_id = config.client_id;
             clean_start = true;
             keep_alive = config.keep_alive;
             credentials = config.credentials;
             will;
             properties =
               [
                 Property.Maximum_packet_size
                   (Int32.of_int config.max_packet_size);
                 Property.Receive_maximum config.message_capacity;
               ];
           })

let connack t = function
  | V3 (V3.Connack c) ->
      if c.return_code <> `Accepted then
        raise (Rejected (Mqttz.V3.Return_code.to_string c.return_code));
      if c.session_present then protocol "session present after clean CONNECT"
  | V5 (V5.Connack c) ->
      if Mqttz.V5.Reason_code.to_int c.reason_code <> 0 then
        raise (Rejected (Mqttz.V5.Reason_code.to_string c.reason_code));
      if c.session_present then protocol "session present after clean CONNECT";
      if
        t.config.client_id = ""
        && not
             (List.exists
                (function
                  | Property.Assigned_client_identifier _ -> true | _ -> false)
                c.properties)
      then protocol "missing assigned client identifier";
      List.iter
        (function
          | Property.Server_keep_alive n -> t.keep_alive <- n
          | Property.Maximum_packet_size n ->
              let unsigned = Int64.logand (Int64.of_int32 n) 0xffffffffL in
              t.peer_max_size <- Int64.to_int (Int64.min unsigned 268435460L)
          | Property.Maximum_qos qos -> t.peer_max_qos <- qos
          | Property.Retain_available enabled -> t.peer_retain <- enabled
          | Property.Wildcard_subscription_available enabled ->
              t.peer_wildcards <- enabled
          | Property.Shared_subscription_available enabled ->
              t.peer_shared <- enabled
          | Property.Authentication_method _ | Property.Authentication_data _ ->
              protocol "enhanced authentication is not configured"
          | _ -> ())
        c.properties
  | _ -> protocol "expected CONNACK"

let start ~sw ~clock ~config transport =
  let ended, end_session = Eio.Promise.create () in
  let t =
    {
      config;
      transport;
      timeout = Eio.Time.Timeout.seconds clock config.operation_timeout;
      tx = Eio.Mutex.create ();
      operation = Eio.Mutex.create ();
      messages = Eio.Stream.create config.message_capacity;
      ended;
      end_session;
      closed = false;
      pending = None;
      next_id = 1;
      received_qos2 = Hashtbl.create 16;
      awaiting_ping = false;
      keep_alive = config.keep_alive;
      peer_max_size = 268435460;
      peer_max_qos = `Exactly_once;
      peer_retain = true;
      peer_wildcards = true;
      peer_shared = true;
    }
  in
  try
    Eio.Time.Timeout.run_exn t.timeout (fun () ->
        send t (connect_packet config);
        connack t (read t));
    Eio.Switch.on_release sw (fun () -> close t);
    Eio.Fiber.fork_daemon ~sw (reader t);
    if t.keep_alive > 0 then Eio.Fiber.fork_daemon ~sw (pinger t clock);
    t
  with ex ->
    fail t ex;
    raise ex

let of_flow ~sw ~clock ~config flow =
  validate_config config;
  start ~sw ~clock ~config (Transport.of_flow flow)

let connect ~sw ~net ~clock ~config ~host ~port () =
  validate_config config;
  if port < 1 || port > 65535 then invalid_arg "invalid MQTT port";
  Eio.Time.Timeout.run_exn
    (Eio.Time.Timeout.seconds clock config.operation_timeout) (fun () ->
      let rec attempt = function
        | [] -> invalid_arg "MQTT host has no addresses"
        | address :: rest -> (
            try Eio.Net.connect ~sw net address
            with Eio.Io _ when rest <> [] -> attempt rest)
      in
      let socket =
        attempt
          (Eio.Net.getaddrinfo_stream net host ~service:(string_of_int port))
      in
      start ~sw ~clock ~config (Transport.of_socket socket))

let exchange t f =
  Eio.Mutex.use_ro t.operation (fun () ->
      check_open t;
      let id = t.next_id in
      t.next_id <- (if id = 65535 then 1 else id + 1);
      let queue = Eio.Stream.create 4 in
      t.pending <- Some (id, queue);
      Fun.protect
        ~finally:(fun () -> t.pending <- None)
        (fun () ->
          try Eio.Time.Timeout.run_exn t.timeout (fun () -> f id queue) with
          | Rejected _ as ex -> raise ex
          | ex ->
              fail t ex;
              raise ex))

let accepted reason =
  if Mqttz.V5.Reason_code.to_int reason >= 128 then
    raise (Rejected (Mqttz.V5.Reason_code.to_string reason))

let publish ?(qos = `At_most_once) ?(retain = false) ?(properties = []) t ~topic
    payload =
  if Mqttz.Qos.to_int qos > Mqttz.Qos.to_int t.peer_max_qos then
    invalid_arg "QoS exceeds broker maximum";
  if retain && not t.peer_retain then
    invalid_arg "broker does not support retain";
  if
    List.exists
      (function
        | Property.Topic_alias _ | Property.Subscription_identifier _ -> true
        | _ -> false)
      properties
  then
    invalid_arg
      "client PUBLISH cannot set topic aliases or subscription identifiers";
  let make packet_id =
    match t.config.version with
    | `V3_1_1 ->
        if properties <> [] then invalid_arg "MQTT 3.1.1 has no properties";
        V3 (V3.Publish { dup = false; qos; retain; topic; packet_id; payload })
    | `V5_0 ->
        V5
          (V5.Publish
             { dup = false; qos; retain; topic; packet_id; payload; properties })
  in
  (* Validate before registering an exchange. *)
  ignore (prepare t (make (if qos = `At_most_once then None else Some 1)));
  if qos = `At_most_once then send t (make None)
  else
    exchange t (fun id queue ->
        send t (make (Some id));
        match (qos, await t queue) with
        | `At_least_once, V3 (V3.Puback _) -> ()
        | `At_least_once, V5 (V5.Puback p) -> accepted p.reason_code
        | `Exactly_once, ((V3 (V3.Pubrec _) | V5 (V5.Pubrec _)) as packet) ->
            (match packet with
            | V5 (V5.Pubrec p) -> accepted p.reason_code
            | _ -> ());
            let rec complete () =
              send t (ack_packet t.config.version `Pubrel id);
              match await t queue with
              | V3 (V3.Pubcomp _) -> ()
              | V5 (V5.Pubcomp p) -> accepted p.reason_code
              | V3 (V3.Pubrec _) -> complete ()
              | V5 (V5.Pubrec p) ->
                  accepted p.reason_code;
                  complete ()
              | _ -> protocol "expected PUBCOMP"
            in
            complete ()
        | _ -> protocol "unexpected publish acknowledgement")

let subscribe ?(qos = `At_most_once) t filters =
  if filters = [] then invalid_arg "empty subscription";
  List.iter
    (fun filter ->
      if not (Mqttz.Topic.Filter.validate filter) then
        invalid_arg "invalid topic filter";
      if
        (not t.peer_wildcards)
        && (String.contains filter '+' || String.contains filter '#')
      then invalid_arg "broker does not support wildcard subscriptions";
      if (not t.peer_shared) && String.starts_with ~prefix:"$share/" filter then
        invalid_arg "broker does not support shared subscriptions")
    filters;
  exchange t (fun packet_id queue ->
      let packet =
        match t.config.version with
        | `V3_1_1 ->
            V3
              (V3.Subscribe
                 {
                   packet_id;
                   topics =
                     List.map
                       (fun filter -> Mqttz.V3.Subscription.{ filter; qos })
                       filters;
                 })
        | `V5_0 ->
            V5
              (V5.Subscribe
                 {
                   packet_id;
                   properties = [];
                   topics =
                     List.map
                       (fun filter ->
                         Mqttz.V5.Subscription.
                           {
                             filter;
                             options = Mqttz.V5.Subscription_options.default qos;
                           })
                       filters;
                 })
      in
      send t packet;
      let codes =
        match await t queue with
        | V3 (V3.Suback s) ->
            List.map Mqttz.V3.Suback_code.to_int s.return_codes
        | V5 (V5.Suback s) ->
            List.map Mqttz.V5.Reason_code.to_int s.reason_codes
        | _ -> protocol "expected SUBACK"
      in
      if List.length codes <> List.length filters then
        protocol "wrong SUBACK count";
      if List.exists (fun code -> code >= 128) codes then
        raise (Rejected "subscription rejected");
      if List.exists (fun code -> code > Mqttz.Qos.to_int qos) codes then
        protocol "SUBACK granted a higher QoS than requested")

let unsubscribe t topics =
  if topics = [] then invalid_arg "empty unsubscribe";
  List.iter
    (fun filter ->
      if not (Mqttz.Topic.Filter.validate filter) then
        invalid_arg "invalid topic filter")
    topics;
  exchange t (fun packet_id queue ->
      send t
        (match t.config.version with
        | `V3_1_1 -> V3 (V3.Unsubscribe { packet_id; topics })
        | `V5_0 -> V5 (V5.Unsubscribe { packet_id; topics; properties = [] }));
      match await t queue with
      | V3 (V3.Unsuback _) -> ()
      | V5 (V5.Unsuback a) ->
          if List.length topics <> List.length a.reason_codes then
            protocol "wrong UNSUBACK count";
          List.iter accepted a.reason_codes
      | _ -> protocol "expected UNSUBACK")

let disconnect t =
  if not t.closed then
    Fun.protect
      ~finally:(fun () -> close t)
      (fun () ->
        Eio.Mutex.use_ro t.operation (fun () ->
            if not t.closed then
              send t
                (match t.config.version with
                | `V3_1_1 -> V3 V3.Disconnect
                | `V5_0 ->
                    V5
                      (V5.Disconnect
                         {
                           reason_code = `Normal_disconnection;
                           properties = [];
                         }))))

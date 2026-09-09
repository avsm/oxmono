(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** MQTT v3.1.1 Protocol *)


module Return_code = struct
  type t =
    [ `Accepted
    | `Unacceptable_protocol_version
    | `Identifier_rejected
    | `Server_unavailable
    | `Bad_username_or_password
    | `Not_authorized ]

  let to_int = function
    | `Accepted -> 0
    | `Unacceptable_protocol_version -> 1
    | `Identifier_rejected -> 2
    | `Server_unavailable -> 3
    | `Bad_username_or_password -> 4
    | `Not_authorized -> 5

  let of_int = function
    | 0 -> `Accepted
    | 1 -> `Unacceptable_protocol_version
    | 2 -> `Identifier_rejected
    | 3 -> `Server_unavailable
    | 4 -> `Bad_username_or_password
    | 5 -> `Not_authorized
    | n -> invalid_arg (Printf.sprintf "Return_code.of_int: invalid value %d" n)

  let to_string = function
    | `Accepted -> "accepted"
    | `Unacceptable_protocol_version -> "unacceptable_protocol_version"
    | `Identifier_rejected -> "identifier_rejected"
    | `Server_unavailable -> "server_unavailable"
    | `Bad_username_or_password -> "bad_username_or_password"
    | `Not_authorized -> "not_authorized"

  let pp ppf t = Format.fprintf ppf "%s" (to_string t)
end

module Suback_code = struct
  type t = [ `Granted_qos_0 | `Granted_qos_1 | `Granted_qos_2 | `Failure ]

  let to_int = function
    | `Granted_qos_0 -> 0x00
    | `Granted_qos_1 -> 0x01
    | `Granted_qos_2 -> 0x02
    | `Failure -> 0x80

  let of_int = function
    | 0x00 -> `Granted_qos_0
    | 0x01 -> `Granted_qos_1
    | 0x02 -> `Granted_qos_2
    | 0x80 -> `Failure
    | _ -> invalid_arg "invalid SUBACK return code"

  let to_string = function
    | `Granted_qos_0 -> "granted_qos_0"
    | `Granted_qos_1 -> "granted_qos_1"
    | `Granted_qos_2 -> "granted_qos_2"
    | `Failure -> "failure"

  let pp ppf t = Format.fprintf ppf "%s" (to_string t)
end

module Subscription = struct
  type t = { filter : Shared.Topic.Filter.t; qos : Shared.Qos.t }

  let pp ppf t =
    Format.fprintf ppf "{filter=%a; qos=%a}" Shared.Topic.Filter.pp t.filter
      Shared.Qos.pp t.qos
end

module Packet = struct
  module P = Parser

  module Connect = struct
    type t = {
      clean_session : bool;
      keep_alive : int;
      client_id : string;
      credentials : Shared.Credentials.t option;
      will : Shared.Will.t option;
    }

    let pp ppf t =
      Format.fprintf ppf
        "Connect{client_id=%s; clean_session=%b; keep_alive=%d}" t.client_id
        t.clean_session t.keep_alive
  end

  module Connack = struct
    type t = { session_present : bool; return_code : Return_code.t }

    let pp ppf t =
      Format.fprintf ppf "Connack{session_present=%b; return_code=%a}"
        t.session_present Return_code.pp t.return_code
  end

  module Publish = struct
    type t = {
      dup : bool;
      qos : Shared.Qos.t;
      retain : bool;
      topic : Shared.Topic.Name.t;
      packet_id : Shared.Packet_id.t option;
      payload : Slice.t;
    }

    let pp ppf t =
      Format.fprintf ppf
        "Publish{topic=%s; qos=%a; dup=%b; retain=%b; payload=<%d bytes>}"
        t.topic Shared.Qos.pp t.qos t.dup t.retain (Slice.length t.payload)
  end

  module Subscribe = struct
    type t = { packet_id : Shared.Packet_id.t; topics : Subscription.t list }

    let pp ppf t =
      Format.fprintf ppf "Subscribe{packet_id=%d; topics=[%a]}" t.packet_id
        (Format.pp_print_list ~pp_sep:Shared.pp_semi Subscription.pp)
        t.topics
  end

  module Suback = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      return_codes : Suback_code.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Suback{packet_id=%d; return_codes=[%a]}" t.packet_id
        (Format.pp_print_list ~pp_sep:Shared.pp_semi Suback_code.pp)
        t.return_codes
  end

  module Unsubscribe = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      topics : Shared.Topic.Filter.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Unsubscribe{packet_id=%d; topics=[%a]}" t.packet_id
        (Format.pp_print_list ~pp_sep:Shared.pp_semi Shared.Topic.Filter.pp)
        t.topics
  end

  type t =
    | Connect of Connect.t
    | Connack of Connack.t
    | Publish of Publish.t
    | Puback of Shared.Packet_id.t
    | Pubrec of Shared.Packet_id.t
    | Pubrel of Shared.Packet_id.t
    | Pubcomp of Shared.Packet_id.t
    | Subscribe of Subscribe.t
    | Suback of Suback.t
    | Unsubscribe of Unsubscribe.t
    | Unsuback of Shared.Packet_id.t
    | Pingreq
    | Pingresp
    | Disconnect

  let pp ppf = function
    | Connect c -> Connect.pp ppf c
    | Connack c -> Connack.pp ppf c
    | Publish p -> Publish.pp ppf p
    | Puback id -> Format.fprintf ppf "Puback(%d)" id
    | Pubrec id -> Format.fprintf ppf "Pubrec(%d)" id
    | Pubrel id -> Format.fprintf ppf "Pubrel(%d)" id
    | Pubcomp id -> Format.fprintf ppf "Pubcomp(%d)" id
    | Subscribe s -> Subscribe.pp ppf s
    | Suback s -> Suback.pp ppf s
    | Unsubscribe u -> Unsubscribe.pp ppf u
    | Unsuback id -> Format.fprintf ppf "Unsuback(%d)" id
    | Pingreq -> Format.fprintf ppf "Pingreq"
    | Pingresp -> Format.fprintf ppf "Pingresp"
    | Disconnect -> Format.fprintf ppf "Disconnect"

  let validate = function
    | Connect c ->
        P.check (c.client_id <> "" || c.clean_session)
          "empty client id with persistent session";
        P.check (Utf8.valid c.client_id && String.length c.client_id <= 65535)
          "invalid client id";
        P.check (c.keep_alive >= 0 && c.keep_alive <= 65535) "invalid keep alive";
        (match c.credentials with
         | Some (`Password _) -> invalid_arg "v3 password requires username"
         | _ -> ());
        Option.iter (fun w -> P.topic (Shared.Will.topic w)) c.will
    | Connack c ->
        P.check (not c.session_present || c.return_code = `Accepted)
          "session present on refused connection"
    | Publish p ->
        P.topic p.topic;
        P.check (not p.dup || p.qos <> `At_most_once) "DUP on QoS 0";
        (match p.qos, p.packet_id with
         | `At_most_once, None -> ()
         | (`At_least_once | `Exactly_once), Some id -> P.packet_id id
         | _ -> invalid_arg "PUBLISH packet id does not match QoS")
    | Puback id | Pubrec id | Pubrel id | Pubcomp id | Unsuback id ->
        P.packet_id id
    | Subscribe s ->
        P.packet_id s.packet_id;
        P.check (s.topics <> []) "empty subscription";
        List.iter (fun (s : Subscription.t) -> P.filter s.filter) s.topics
    | Suback s ->
        P.packet_id s.packet_id;
        P.check (s.return_codes <> []) "empty SUBACK"
    | Unsubscribe s ->
        P.packet_id s.packet_id;
        P.check (s.topics <> []) "empty unsubscribe";
        List.iter P.filter s.topics
    | Pingreq | Pingresp | Disconnect -> ()

  (** {1 Encoding} *)

  let write_connect writer (c : Connect.t) =
    let payload =
      P.to_string (fun w ->
          P.write_mqtt_string w "MQTT";
          P.write_uint8 w 4;
          let flags = ref 0 in
          if c.clean_session then flags := !flags lor 0x02;
          Option.iter
            (fun w' ->
              flags := !flags lor 0x04;
              flags := !flags lor (Shared.Qos.to_int (Shared.Will.qos w') lsl 3);
              if Shared.Will.retain w' then flags := !flags lor 0x20)
            c.will;
          (match c.credentials with
          | Some (`Password _) -> invalid_arg "v3 password requires username"
          | Some (`Username _) -> flags := !flags lor 0x80
          | Some (`Username_password _) -> flags := !flags lor 0xC0
          | None -> ());
          P.write_uint8 w !flags;
          P.write_uint16_be w c.keep_alive;
          P.write_mqtt_string w c.client_id;
          Option.iter
            (fun w' ->
              P.write_mqtt_string w (Shared.Will.topic w');
              P.write_mqtt_binary w (Shared.Will.payload w'))
            c.will;
          match c.credentials with
          | Some (`Password _) -> invalid_arg "v3 password requires username"
          | Some (`Username u) -> P.write_mqtt_string w u
          | Some (`Username_password (username, password)) ->
              P.write_mqtt_string w username;
              P.write_mqtt_binary w password
          | None -> ())
    in
    P.write_fixed_header writer `CONNECT 0 (String.length payload);
    P.write_string writer payload

  let write_connack writer (c : Connack.t) =
    P.write_fixed_header writer `CONNACK 0 2;
    P.write_uint8 writer (if c.session_present then 0x01 else 0x00);
    P.write_uint8 writer (Return_code.to_int c.return_code)

  let publish_header (p : Publish.t) =
    let variable = P.to_string (fun w ->
      P.write_mqtt_string w p.topic;
      Option.iter (P.write_uint16_be w) p.packet_id;
      ()) in
    let flags = (if p.dup then 8 else 0)
      lor (Shared.Qos.to_int p.qos lsl 1)
      lor (if p.retain then 1 else 0) in
    P.to_string (fun w ->
      P.write_fixed_header w `PUBLISH flags
        (String.length variable + Slice.length p.payload);
      P.write_string w variable)

  let write_pubx writer packet_type ?(flags = 0) id =
    P.write_fixed_header writer packet_type flags 2;
    P.write_uint16_be writer id

  let write_puback writer id = write_pubx writer `PUBACK id
  let write_pubrec writer id = write_pubx writer `PUBREC id
  let write_pubrel writer id = write_pubx writer `PUBREL ~flags:0x02 id
  let write_pubcomp writer id = write_pubx writer `PUBCOMP id

  let write_subscribe writer (s : Subscribe.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint16_be w s.packet_id;
          List.iter
            (fun (t : Subscription.t) ->
              P.write_mqtt_string w t.filter;
              P.write_uint8 w (Shared.Qos.to_int t.qos))
            s.topics)
    in
    P.write_fixed_header writer `SUBSCRIBE 0x02 (String.length payload);
    P.write_string writer payload

  let write_suback writer (s : Suback.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint16_be w s.packet_id;
          List.iter
            (fun rc -> P.write_uint8 w (Suback_code.to_int rc))
            s.return_codes)
    in
    P.write_fixed_header writer `SUBACK 0 (String.length payload);
    P.write_string writer payload

  let write_unsubscribe writer (u : Unsubscribe.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint16_be w u.packet_id;
          List.iter (fun topic -> P.write_mqtt_string w topic) u.topics)
    in
    P.write_fixed_header writer `UNSUBSCRIBE 0x02 (String.length payload);
    P.write_string writer payload

  let write_unsuback writer id =
    P.write_fixed_header writer `UNSUBACK 0 2;
    P.write_uint16_be writer id

  let write_pingreq writer = P.write_fixed_header writer `PINGREQ 0 0
  let write_pingresp writer = P.write_fixed_header writer `PINGRESP 0 0
  let write_disconnect writer = P.write_fixed_header writer `DISCONNECT 0 0

  let write writer = function
    | Connect c -> write_connect writer c
    | Connack c -> write_connack writer c
    | Publish _ -> assert false
    | Puback id -> write_puback writer id
    | Pubrec id -> write_pubrec writer id
    | Pubrel id -> write_pubrel writer id
    | Pubcomp id -> write_pubcomp writer id
    | Subscribe s -> write_subscribe writer s
    | Suback s -> write_suback writer s
    | Unsubscribe u -> write_unsubscribe writer u
    | Unsuback id -> write_unsuback writer id
    | Pingreq -> write_pingreq writer
    | Pingresp -> write_pingresp writer
    | Disconnect -> write_disconnect writer

  (** {1 Decoding} *)

  let read_connect (reader @ local) =
    let proto_name = P.mqtt_string reader in
    if proto_name <> "MQTT" then invalid_arg "Invalid protocol name";
    let proto_level = P.uint8 reader in
    if proto_level <> 4 then invalid_arg "Unsupported protocol level";
    let flags = P.uint8 reader in
    P.connect_flags flags;
    P.check (flags land 0x40 = 0 || flags land 0x80 <> 0) "password without username";
    let clean_session = flags land 0x02 <> 0 in
    let will_flag = flags land 0x04 <> 0 in
    let will_qos = Shared.Qos.of_int ((flags land 0x18) lsr 3) in
    let will_retain = flags land 0x20 <> 0 in
    let password_flag = flags land 0x40 <> 0 in
    let username_flag = flags land 0x80 <> 0 in
    let keep_alive = P.uint16_be reader in
    let client_id = P.mqtt_string reader in
    let will =
      if will_flag then
        let topic = P.mqtt_string reader in
        let payload = P.mqtt_binary reader in
        Some
          (Shared.Will.create ~topic ~payload ~qos:will_qos ~retain:will_retain)
      else None
    in
    let credentials =
      if username_flag then
        let username = P.mqtt_string reader in
        if password_flag then
          let password = P.mqtt_binary reader in
          Some (`Username_password (username, password))
        else Some (`Username username)
      else None
    in
    Connect Connect.{ client_id; clean_session; keep_alive; credentials; will }

  let read_connack (reader @ local) =
    let flags = P.uint8 reader in
    P.check (flags land 0xfe = 0) "reserved CONNACK flags";
    let session_present = flags land 0x01 <> 0 in
    let return_code = P.uint8 reader in
    Connack
      Connack.{ session_present; return_code = Return_code.of_int return_code }

  let read_publish ~flags (reader @ local) =
    let dup = flags land 0x08 <> 0 in
    let qos = Shared.Qos.of_int ((flags land 0x06) lsr 1) in
    let retain = flags land 0x01 <> 0 in
    let topic = P.mqtt_string reader in
    let packet_id =
      if qos <> `At_most_once then Some (P.uint16_be reader) else None
    in
    let payload = P.take_rest reader in
    Publish Publish.{ dup; qos; retain; topic; packet_id; payload }

  let read_puback (reader @ local) = Puback (P.uint16_be reader)
  let read_pubrec (reader @ local) = Pubrec (P.uint16_be reader)
  let read_pubrel (reader @ local) = Pubrel (P.uint16_be reader)
  let read_pubcomp (reader @ local) = Pubcomp (P.uint16_be reader)

  let read_subscribe (reader @ local) =
    let packet_id = P.uint16_be reader in
    let read_topic (reader @ local) =
      let filter = P.mqtt_string reader in
      let qos_byte = P.uint8 reader in
      let qos = Shared.Qos.of_int qos_byte in
      Subscription.{ filter; qos }
    in
    let topics = P.many1 read_topic reader in
    Subscribe Subscribe.{ packet_id; topics }

  let read_suback ~remaining_length (reader @ local) =
    let packet_id = P.uint16_be reader in
    let num_codes = remaining_length - 2 in
    let read_code (reader @ local) = Suback_code.of_int (P.uint8 reader) in
    let return_codes = P.count num_codes read_code reader in
    Suback Suback.{ packet_id; return_codes }

  let read_unsubscribe (reader @ local) =
    let packet_id = P.uint16_be reader in
    let topics = P.many1 P.mqtt_string reader in
    Unsubscribe Unsubscribe.{ packet_id; topics }

  let read_unsuback (reader @ local) = Unsuback (P.uint16_be reader)

  let read (reader @ local) =
    let first_byte = P.uint8 reader in
    let packet_type = Shared.Packet_type.of_int (first_byte lsr 4) in
    let flags = first_byte land 0x0F in
    let remaining_length = P.variable_length reader in
    let local_ payload_reader = P.sub remaining_length reader in
    let packet = match packet_type with
    | `CONNECT -> read_connect payload_reader
    | `CONNACK -> read_connack payload_reader
    | `PUBLISH -> read_publish ~flags payload_reader
    | `PUBACK -> read_puback payload_reader
    | `PUBREC -> read_pubrec payload_reader
    | `PUBREL -> read_pubrel payload_reader
    | `PUBCOMP -> read_pubcomp payload_reader
    | `SUBSCRIBE -> read_subscribe payload_reader
    | `SUBACK -> read_suback ~remaining_length payload_reader
    | `UNSUBSCRIBE -> read_unsubscribe payload_reader
    | `UNSUBACK -> read_unsuback payload_reader
    | `PINGREQ -> Pingreq
    | `PINGRESP -> Pingresp
    | `DISCONNECT -> Disconnect
    | `RESERVED -> invalid_arg "Reserved packet type"
    | `AUTH -> invalid_arg "AUTH not supported in v3.1.1"
    in
    P.finish payload_reader;
    validate packet;
    packet

  let decode ?max_size data = P.decode read ?max_size data
  let encode packet =
    validate packet;
    match packet with
    | Publish p -> [Slice.of_string (publish_header p); p.payload]
    | _ -> [Slice.of_string (P.to_string (fun w -> write w packet))]
  let to_bytes packet =
    let parts = encode packet in
    let size = List.fold_left (fun n s -> n + Slice.length s) 0 parts in
    let b = Bytes.create size in
    ignore (List.fold_left (fun off (s : Slice.t) ->
      Bytes.blit s.bytes s.off b off s.len; off + s.len) 0 parts);
    b

end

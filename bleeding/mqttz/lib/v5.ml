(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** MQTT v5.0 Protocol *)


module Reason_code = struct
  type t =
    [ `Success
    | `Normal_disconnection
    | `Granted_qos_0
    | `Granted_qos_1
    | `Granted_qos_2
    | `Disconnect_with_will
    | `No_matching_subscribers
    | `No_subscription_existed
    | `Continue_authentication
    | `Re_authenticate
    | `Unspecified_error
    | `Malformed_packet
    | `Protocol_error
    | `Implementation_specific_error
    | `Unsupported_protocol_version
    | `Client_identifier_not_valid
    | `Bad_user_name_or_password
    | `Not_authorized
    | `Server_unavailable
    | `Server_busy
    | `Banned
    | `Server_shutting_down
    | `Bad_authentication_method
    | `Keep_alive_timeout
    | `Session_taken_over
    | `Topic_filter_invalid
    | `Topic_name_invalid
    | `Packet_identifier_in_use
    | `Packet_identifier_not_found
    | `Receive_maximum_exceeded
    | `Topic_alias_invalid
    | `Packet_too_large
    | `Message_rate_too_high
    | `Quota_exceeded
    | `Administrative_action
    | `Payload_format_invalid
    | `Retain_not_supported
    | `Qos_not_supported
    | `Use_another_server
    | `Server_moved
    | `Shared_subscriptions_not_supported
    | `Connection_rate_exceeded
    | `Maximum_connect_time
    | `Subscription_identifiers_not_supported
    | `Wildcard_subscriptions_not_supported ]

  let to_int = function
    | `Success -> 0x00
    | `Normal_disconnection -> 0x00
    | `Granted_qos_0 -> 0x00
    | `Granted_qos_1 -> 0x01
    | `Granted_qos_2 -> 0x02
    | `Disconnect_with_will -> 0x04
    | `No_matching_subscribers -> 0x10
    | `No_subscription_existed -> 0x11
    | `Continue_authentication -> 0x18
    | `Re_authenticate -> 0x19
    | `Unspecified_error -> 0x80
    | `Malformed_packet -> 0x81
    | `Protocol_error -> 0x82
    | `Implementation_specific_error -> 0x83
    | `Unsupported_protocol_version -> 0x84
    | `Client_identifier_not_valid -> 0x85
    | `Bad_user_name_or_password -> 0x86
    | `Not_authorized -> 0x87
    | `Server_unavailable -> 0x88
    | `Server_busy -> 0x89
    | `Banned -> 0x8A
    | `Server_shutting_down -> 0x8B
    | `Bad_authentication_method -> 0x8C
    | `Keep_alive_timeout -> 0x8D
    | `Session_taken_over -> 0x8E
    | `Topic_filter_invalid -> 0x8F
    | `Topic_name_invalid -> 0x90
    | `Packet_identifier_in_use -> 0x91
    | `Packet_identifier_not_found -> 0x92
    | `Receive_maximum_exceeded -> 0x93
    | `Topic_alias_invalid -> 0x94
    | `Packet_too_large -> 0x95
    | `Message_rate_too_high -> 0x96
    | `Quota_exceeded -> 0x97
    | `Administrative_action -> 0x98
    | `Payload_format_invalid -> 0x99
    | `Retain_not_supported -> 0x9A
    | `Qos_not_supported -> 0x9B
    | `Use_another_server -> 0x9C
    | `Server_moved -> 0x9D
    | `Shared_subscriptions_not_supported -> 0x9E
    | `Connection_rate_exceeded -> 0x9F
    | `Maximum_connect_time -> 0xA0
    | `Subscription_identifiers_not_supported -> 0xA1
    | `Wildcard_subscriptions_not_supported -> 0xA2

  let of_int = function
    | 0x00 -> `Success
    | 0x01 -> `Granted_qos_1
    | 0x02 -> `Granted_qos_2
    | 0x04 -> `Disconnect_with_will
    | 0x10 -> `No_matching_subscribers
    | 0x11 -> `No_subscription_existed
    | 0x18 -> `Continue_authentication
    | 0x19 -> `Re_authenticate
    | 0x80 -> `Unspecified_error
    | 0x81 -> `Malformed_packet
    | 0x82 -> `Protocol_error
    | 0x83 -> `Implementation_specific_error
    | 0x84 -> `Unsupported_protocol_version
    | 0x85 -> `Client_identifier_not_valid
    | 0x86 -> `Bad_user_name_or_password
    | 0x87 -> `Not_authorized
    | 0x88 -> `Server_unavailable
    | 0x89 -> `Server_busy
    | 0x8A -> `Banned
    | 0x8B -> `Server_shutting_down
    | 0x8C -> `Bad_authentication_method
    | 0x8D -> `Keep_alive_timeout
    | 0x8E -> `Session_taken_over
    | 0x8F -> `Topic_filter_invalid
    | 0x90 -> `Topic_name_invalid
    | 0x91 -> `Packet_identifier_in_use
    | 0x92 -> `Packet_identifier_not_found
    | 0x93 -> `Receive_maximum_exceeded
    | 0x94 -> `Topic_alias_invalid
    | 0x95 -> `Packet_too_large
    | 0x96 -> `Message_rate_too_high
    | 0x97 -> `Quota_exceeded
    | 0x98 -> `Administrative_action
    | 0x99 -> `Payload_format_invalid
    | 0x9A -> `Retain_not_supported
    | 0x9B -> `Qos_not_supported
    | 0x9C -> `Use_another_server
    | 0x9D -> `Server_moved
    | 0x9E -> `Shared_subscriptions_not_supported
    | 0x9F -> `Connection_rate_exceeded
    | 0xA0 -> `Maximum_connect_time
    | 0xA1 -> `Subscription_identifiers_not_supported
    | 0xA2 -> `Wildcard_subscriptions_not_supported
    | n -> invalid_arg (Printf.sprintf "Unknown reason code: 0x%02X" n)

  (* OASIS MQTT 5.0, reason-code tables in sections 3.2 and 3.4--3.15.
     PUBREC shares PUBACK's codes. PUBREL shares PUBCOMP's codes. *)
  let allowed_on packet code =
    let code = match code with
      | `Normal_disconnection | `Granted_qos_0 -> `Success
      | code -> code
    in
    match packet, code with
    | `Connack,
        (`Success
        | `Unspecified_error
        | `Malformed_packet
        | `Protocol_error
        | `Implementation_specific_error
        | `Unsupported_protocol_version
        | `Client_identifier_not_valid
        | `Bad_user_name_or_password
        | `Not_authorized
        | `Server_unavailable
        | `Server_busy
        | `Banned
        | `Bad_authentication_method
        | `Topic_name_invalid
        | `Packet_too_large
        | `Quota_exceeded
        | `Payload_format_invalid
        | `Retain_not_supported
        | `Qos_not_supported
        | `Use_another_server
        | `Server_moved
        | `Connection_rate_exceeded) -> true
    | `Puback,
        (`Success
        | `No_matching_subscribers
        | `Unspecified_error
        | `Implementation_specific_error
        | `Not_authorized
        | `Topic_name_invalid
        | `Packet_identifier_in_use
        | `Quota_exceeded
        | `Payload_format_invalid) -> true
    | `Pubcomp,
        (`Success
        | `Packet_identifier_not_found) -> true
    | `Suback,
        (`Success
        | `Granted_qos_1
        | `Granted_qos_2
        | `Unspecified_error
        | `Implementation_specific_error
        | `Not_authorized
        | `Topic_filter_invalid
        | `Packet_identifier_in_use
        | `Quota_exceeded
        | `Shared_subscriptions_not_supported
        | `Subscription_identifiers_not_supported
        | `Wildcard_subscriptions_not_supported) -> true
    | `Unsuback,
        (`Success
        | `No_subscription_existed
        | `Unspecified_error
        | `Implementation_specific_error
        | `Not_authorized
        | `Topic_filter_invalid
        | `Packet_identifier_in_use) -> true
    | `Disconnect,
        (`Success
        | `Disconnect_with_will
        | `Unspecified_error
        | `Malformed_packet
        | `Protocol_error
        | `Implementation_specific_error
        | `Not_authorized
        | `Server_busy
        | `Server_shutting_down
        | `Keep_alive_timeout
        | `Session_taken_over
        | `Topic_filter_invalid
        | `Topic_name_invalid
        | `Receive_maximum_exceeded
        | `Topic_alias_invalid
        | `Packet_too_large
        | `Message_rate_too_high
        | `Quota_exceeded
        | `Administrative_action
        | `Payload_format_invalid
        | `Retain_not_supported
        | `Qos_not_supported
        | `Use_another_server
        | `Server_moved
        | `Shared_subscriptions_not_supported
        | `Connection_rate_exceeded
        | `Maximum_connect_time
        | `Subscription_identifiers_not_supported
        | `Wildcard_subscriptions_not_supported) -> true
    | `Auth,
        (`Success
        | `Continue_authentication
        | `Re_authenticate) -> true
    | _ -> false

  let to_string = function
    | `Success -> "Success"
    | `Normal_disconnection -> "Normal disconnection"
    | `Granted_qos_0 -> "Granted QoS 0"
    | `Granted_qos_1 -> "Granted QoS 1"
    | `Granted_qos_2 -> "Granted QoS 2"
    | `Disconnect_with_will -> "Disconnect with Will Message"
    | `No_matching_subscribers -> "No matching subscribers"
    | `No_subscription_existed -> "No subscription existed"
    | `Continue_authentication -> "Continue authentication"
    | `Re_authenticate -> "Re-authenticate"
    | `Unspecified_error -> "Unspecified error"
    | `Malformed_packet -> "Malformed Packet"
    | `Protocol_error -> "Protocol Error"
    | `Implementation_specific_error -> "Implementation specific error"
    | `Unsupported_protocol_version -> "Unsupported Protocol Version"
    | `Client_identifier_not_valid -> "Client Identifier not valid"
    | `Bad_user_name_or_password -> "Bad User Name or Password"
    | `Not_authorized -> "Not authorized"
    | `Server_unavailable -> "Server unavailable"
    | `Server_busy -> "Server busy"
    | `Banned -> "Banned"
    | `Server_shutting_down -> "Server shutting down"
    | `Bad_authentication_method -> "Bad authentication method"
    | `Keep_alive_timeout -> "Keep Alive timeout"
    | `Session_taken_over -> "Session taken over"
    | `Topic_filter_invalid -> "Topic Filter invalid"
    | `Topic_name_invalid -> "Topic Name invalid"
    | `Packet_identifier_in_use -> "Packet Identifier in use"
    | `Packet_identifier_not_found -> "Packet Identifier not found"
    | `Receive_maximum_exceeded -> "Receive Maximum exceeded"
    | `Topic_alias_invalid -> "Topic Alias invalid"
    | `Packet_too_large -> "Packet too large"
    | `Message_rate_too_high -> "Message rate too high"
    | `Quota_exceeded -> "Quota exceeded"
    | `Administrative_action -> "Administrative action"
    | `Payload_format_invalid -> "Payload format invalid"
    | `Retain_not_supported -> "Retain not supported"
    | `Qos_not_supported -> "QoS not supported"
    | `Use_another_server -> "Use another server"
    | `Server_moved -> "Server moved"
    | `Shared_subscriptions_not_supported ->
        "Shared Subscriptions not supported"
    | `Connection_rate_exceeded -> "Connection rate exceeded"
    | `Maximum_connect_time -> "Maximum connect time"
    | `Subscription_identifiers_not_supported ->
        "Subscription Identifiers not supported"
    | `Wildcard_subscriptions_not_supported ->
        "Wildcard Subscriptions not supported"

  let pp ppf t = Format.fprintf ppf "%s" (to_string t)
end

(** {1 Properties} *)

module Property = struct
  module P = Parser

  type t =
    | Payload_format_indicator of int
    | Message_expiry_interval of int32
    | Content_type of string
    | Response_topic of string
    | Correlation_data of string
    | Subscription_identifier of int
    | Session_expiry_interval of int32
    | Assigned_client_identifier of string
    | Server_keep_alive of int
    | Authentication_method of string
    | Authentication_data of string
    | Request_problem_information of int
    | Will_delay_interval of int32
    | Request_response_information of int
    | Response_information of string
    | Server_reference of string
    | Reason_string of string
    | Receive_maximum of int
    | Topic_alias_maximum of int
    | Topic_alias of int
    | Maximum_qos of Shared.Qos.t
    | Retain_available of bool
    | User_property of string * string
    | Maximum_packet_size of int32
    | Wildcard_subscription_available of bool
    | Subscription_identifier_available of bool
    | Shared_subscription_available of bool

  let id_to_int = function
    | `Payload_format_indicator -> 0x01
    | `Message_expiry_interval -> 0x02
    | `Content_type -> 0x03
    | `Response_topic -> 0x08
    | `Correlation_data -> 0x09
    | `Subscription_identifier -> 0x0B
    | `Session_expiry_interval -> 0x11
    | `Assigned_client_identifier -> 0x12
    | `Server_keep_alive -> 0x13
    | `Authentication_method -> 0x15
    | `Authentication_data -> 0x16
    | `Request_problem_information -> 0x17
    | `Will_delay_interval -> 0x18
    | `Request_response_information -> 0x19
    | `Response_information -> 0x1A
    | `Server_reference -> 0x1C
    | `Reason_string -> 0x1F
    | `Receive_maximum -> 0x21
    | `Topic_alias_maximum -> 0x22
    | `Topic_alias -> 0x23
    | `Maximum_qos -> 0x24
    | `Retain_available -> 0x25
    | `User_property -> 0x26
    | `Maximum_packet_size -> 0x27
    | `Wildcard_subscription_available -> 0x28
    | `Subscription_identifier_available -> 0x29
    | `Shared_subscription_available -> 0x2A

  let id_of_int = function
    | 0x01 -> `Payload_format_indicator
    | 0x02 -> `Message_expiry_interval
    | 0x03 -> `Content_type
    | 0x08 -> `Response_topic
    | 0x09 -> `Correlation_data
    | 0x0B -> `Subscription_identifier
    | 0x11 -> `Session_expiry_interval
    | 0x12 -> `Assigned_client_identifier
    | 0x13 -> `Server_keep_alive
    | 0x15 -> `Authentication_method
    | 0x16 -> `Authentication_data
    | 0x17 -> `Request_problem_information
    | 0x18 -> `Will_delay_interval
    | 0x19 -> `Request_response_information
    | 0x1A -> `Response_information
    | 0x1C -> `Server_reference
    | 0x1F -> `Reason_string
    | 0x21 -> `Receive_maximum
    | 0x22 -> `Topic_alias_maximum
    | 0x23 -> `Topic_alias
    | 0x24 -> `Maximum_qos
    | 0x25 -> `Retain_available
    | 0x26 -> `User_property
    | 0x27 -> `Maximum_packet_size
    | 0x28 -> `Wildcard_subscription_available
    | 0x29 -> `Subscription_identifier_available
    | 0x2A -> `Shared_subscription_available
    | n -> invalid_arg (Printf.sprintf "Unknown property ID: 0x%02X" n)

  (** {2 Writing Properties} *)

  let write_property writer prop =
    match prop with
    | Payload_format_indicator v ->
        P.write_variable_length writer (id_to_int `Payload_format_indicator);
        P.write_uint8 writer v
    | Message_expiry_interval v ->
        P.write_variable_length writer (id_to_int `Message_expiry_interval);
        P.write_uint32_be writer v
    | Content_type v ->
        P.write_variable_length writer (id_to_int `Content_type);
        P.write_mqtt_string writer v
    | Response_topic v ->
        P.write_variable_length writer (id_to_int `Response_topic);
        P.write_mqtt_string writer v
    | Correlation_data v ->
        P.write_variable_length writer (id_to_int `Correlation_data);
        P.write_mqtt_binary writer v
    | Subscription_identifier v ->
        P.write_variable_length writer (id_to_int `Subscription_identifier);
        P.write_variable_length writer v
    | Session_expiry_interval v ->
        P.write_variable_length writer (id_to_int `Session_expiry_interval);
        P.write_uint32_be writer v
    | Assigned_client_identifier v ->
        P.write_variable_length writer (id_to_int `Assigned_client_identifier);
        P.write_mqtt_string writer v
    | Server_keep_alive v ->
        P.write_variable_length writer (id_to_int `Server_keep_alive);
        P.write_uint16_be writer v
    | Authentication_method v ->
        P.write_variable_length writer (id_to_int `Authentication_method);
        P.write_mqtt_string writer v
    | Authentication_data v ->
        P.write_variable_length writer (id_to_int `Authentication_data);
        P.write_mqtt_binary writer v
    | Request_problem_information v ->
        P.write_variable_length writer (id_to_int `Request_problem_information);
        P.write_uint8 writer v
    | Will_delay_interval v ->
        P.write_variable_length writer (id_to_int `Will_delay_interval);
        P.write_uint32_be writer v
    | Request_response_information v ->
        P.write_variable_length writer (id_to_int `Request_response_information);
        P.write_uint8 writer v
    | Response_information v ->
        P.write_variable_length writer (id_to_int `Response_information);
        P.write_mqtt_string writer v
    | Server_reference v ->
        P.write_variable_length writer (id_to_int `Server_reference);
        P.write_mqtt_string writer v
    | Reason_string v ->
        P.write_variable_length writer (id_to_int `Reason_string);
        P.write_mqtt_string writer v
    | Receive_maximum v ->
        P.write_variable_length writer (id_to_int `Receive_maximum);
        P.write_uint16_be writer v
    | Topic_alias_maximum v ->
        P.write_variable_length writer (id_to_int `Topic_alias_maximum);
        P.write_uint16_be writer v
    | Topic_alias v ->
        P.write_variable_length writer (id_to_int `Topic_alias);
        P.write_uint16_be writer v
    | Maximum_qos v ->
        P.write_variable_length writer (id_to_int `Maximum_qos);
        P.write_uint8 writer (Shared.Qos.to_int v)
    | Retain_available v ->
        P.write_variable_length writer (id_to_int `Retain_available);
        P.write_uint8 writer (if v then 1 else 0)
    | User_property (k, v) ->
        P.write_variable_length writer (id_to_int `User_property);
        P.write_mqtt_string writer k;
        P.write_mqtt_string writer v
    | Maximum_packet_size v ->
        P.write_variable_length writer (id_to_int `Maximum_packet_size);
        P.write_uint32_be writer v
    | Wildcard_subscription_available v ->
        P.write_variable_length writer
          (id_to_int `Wildcard_subscription_available);
        P.write_uint8 writer (if v then 1 else 0)
    | Subscription_identifier_available v ->
        P.write_variable_length writer
          (id_to_int `Subscription_identifier_available);
        P.write_uint8 writer (if v then 1 else 0)
    | Shared_subscription_available v ->
        P.write_variable_length writer
          (id_to_int `Shared_subscription_available);
        P.write_uint8 writer (if v then 1 else 0)

  let identifier = function
    | Payload_format_indicator _ -> `Payload_format_indicator
    | Message_expiry_interval _ -> `Message_expiry_interval
    | Content_type _ -> `Content_type
    | Response_topic _ -> `Response_topic
    | Correlation_data _ -> `Correlation_data
    | Subscription_identifier _ -> `Subscription_identifier
    | Session_expiry_interval _ -> `Session_expiry_interval
    | Assigned_client_identifier _ -> `Assigned_client_identifier
    | Server_keep_alive _ -> `Server_keep_alive
    | Authentication_method _ -> `Authentication_method
    | Authentication_data _ -> `Authentication_data
    | Request_problem_information _ -> `Request_problem_information
    | Will_delay_interval _ -> `Will_delay_interval
    | Request_response_information _ -> `Request_response_information
    | Response_information _ -> `Response_information
    | Server_reference _ -> `Server_reference
    | Reason_string _ -> `Reason_string
    | Receive_maximum _ -> `Receive_maximum
    | Topic_alias_maximum _ -> `Topic_alias_maximum
    | Topic_alias _ -> `Topic_alias
    | Maximum_qos _ -> `Maximum_qos
    | Retain_available _ -> `Retain_available
    | User_property _ -> `User_property
    | Maximum_packet_size _ -> `Maximum_packet_size
    | Wildcard_subscription_available _ -> `Wildcard_subscription_available
    | Subscription_identifier_available _ -> `Subscription_identifier_available
    | Shared_subscription_available _ -> `Shared_subscription_available

  (* OASIS MQTT 5.0, Table 2-4 and the Will property table in 3.1.3.2. *)
  let allowed_on context property =
    match context, property with
    | `Connect,
        (`Session_expiry_interval
        | `Authentication_method
        | `Authentication_data
        | `Request_problem_information
        | `Request_response_information
        | `Receive_maximum
        | `Topic_alias_maximum
        | `User_property
        | `Maximum_packet_size) -> true
    | `Will,
        (`Payload_format_indicator
        | `Message_expiry_interval
        | `Content_type
        | `Response_topic
        | `Correlation_data
        | `Will_delay_interval
        | `User_property) -> true
    | `Connack,
        (`Session_expiry_interval
        | `Assigned_client_identifier
        | `Server_keep_alive
        | `Authentication_method
        | `Authentication_data
        | `Response_information
        | `Server_reference
        | `Reason_string
        | `Receive_maximum
        | `Topic_alias_maximum
        | `Maximum_qos
        | `Retain_available
        | `User_property
        | `Maximum_packet_size
        | `Wildcard_subscription_available
        | `Subscription_identifier_available
        | `Shared_subscription_available) -> true
    | `Publish,
        (`Payload_format_indicator
        | `Message_expiry_interval
        | `Content_type
        | `Response_topic
        | `Correlation_data
        | `Subscription_identifier
        | `Topic_alias
        | `User_property) -> true
    | `Subscribe,
        (`Subscription_identifier
        | `User_property) -> true
    | `Unsubscribe,
        (`User_property) -> true
    | `Ack,
        (`Reason_string
        | `User_property) -> true
    | `Disconnect,
        (`Session_expiry_interval
        | `Server_reference
        | `Reason_string
        | `User_property) -> true
    | `Auth,
        (`Authentication_method
        | `Authentication_data
        | `Reason_string
        | `User_property) -> true
    | _ -> false

  (* Two small bitmaps also fit OCaml's 31-bit integer representation. *)
  type seen = { mutable low : int; mutable high : int }

  let contains (seen @ local) property =
    let id = id_to_int property in
    if id < 30 then seen.low land (1 lsl id) <> 0
    else seen.high land (1 lsl (id - 30)) <> 0

  let remember (seen @ local) property =
    let id = id_to_int property in
    if id < 30 then seen.low <- seen.low lor (1 lsl id)
    else seen.high <- seen.high lor (1 lsl (id - 30))

  let validate context props =
    let local_ seen = stack_ { low = 0; high = 0 } in
    let rec loop context (seen @ local) = function
      | [] -> ()
      | prop :: rest ->
          let property = identifier prop in
          P.check (allowed_on context property)
            "property not allowed on this packet";
          let repeat = match property with
            | `User_property -> true
            | `Subscription_identifier -> context = `Publish
            | _ -> false
          in
          P.check (repeat || not (contains seen property))
            "duplicate singleton property";
          remember seen property;
          (match prop with
          | Payload_format_indicator n | Request_problem_information n
          | Request_response_information n ->
              P.check (n = 0 || n = 1) "property must be 0 or 1"
          | Receive_maximum n | Topic_alias n -> P.packet_id n
          | Topic_alias_maximum n | Server_keep_alive n ->
              P.check (n >= 0 && n <= 65535) "property outside uint16 range"
          | Subscription_identifier n ->
              P.check (n > 0 && n <= 268435455)
                "invalid subscription identifier"
          | Maximum_packet_size n ->
              P.check (n <> 0l) "zero maximum packet size"
          | Maximum_qos q ->
              P.check (q <> `Exactly_once) "maximum QoS must be 0 or 1"
          | Response_topic s -> P.topic s
          | Assigned_client_identifier s -> P.check (s <> "") "empty assigned id"
          | _ -> ());
          loop context seen rest
    in
    loop context seen props;
    if context = `Connect then
      P.check (not (contains seen `Authentication_data)
               || contains seen `Authentication_method)
        "authentication data without method"

  let write_properties writer props =
    let props_data =
      P.to_string (fun w -> List.iter (write_property w) props)
    in
    P.write_variable_length writer (String.length props_data);
    P.write_string writer props_data

  (** {2 Reading Properties} *)

  let read_property (reader @ local) =
    let prop_id = P.variable_length reader in
    match id_of_int prop_id with
    | `Payload_format_indicator -> Payload_format_indicator (P.uint8 reader)
    | `Message_expiry_interval -> Message_expiry_interval (P.uint32_be reader)
    | `Content_type -> Content_type (P.mqtt_string reader)
    | `Response_topic -> Response_topic (P.mqtt_string reader)
    | `Correlation_data -> Correlation_data (P.mqtt_binary reader)
    | `Subscription_identifier ->
        Subscription_identifier (P.variable_length reader)
    | `Session_expiry_interval -> Session_expiry_interval (P.uint32_be reader)
    | `Assigned_client_identifier ->
        Assigned_client_identifier (P.mqtt_string reader)
    | `Server_keep_alive -> Server_keep_alive (P.uint16_be reader)
    | `Authentication_method -> Authentication_method (P.mqtt_string reader)
    | `Authentication_data -> Authentication_data (P.mqtt_binary reader)
    | `Request_problem_information ->
        Request_problem_information (P.uint8 reader)
    | `Will_delay_interval -> Will_delay_interval (P.uint32_be reader)
    | `Request_response_information ->
        Request_response_information (P.uint8 reader)
    | `Response_information -> Response_information (P.mqtt_string reader)
    | `Server_reference -> Server_reference (P.mqtt_string reader)
    | `Reason_string -> Reason_string (P.mqtt_string reader)
    | `Receive_maximum -> Receive_maximum (P.uint16_be reader)
    | `Topic_alias_maximum -> Topic_alias_maximum (P.uint16_be reader)
    | `Topic_alias -> Topic_alias (P.uint16_be reader)
    | `Maximum_qos -> Maximum_qos (Shared.Qos.of_int (P.uint8 reader))
    | `Retain_available -> Retain_available (P.bool reader)
    | `User_property ->
        let k = P.mqtt_string reader in
        let v = P.mqtt_string reader in
        User_property (k, v)
    | `Maximum_packet_size -> Maximum_packet_size (P.uint32_be reader)
    | `Wildcard_subscription_available ->
        Wildcard_subscription_available (P.bool reader)
    | `Subscription_identifier_available ->
        Subscription_identifier_available (P.bool reader)
    | `Shared_subscription_available ->
        Shared_subscription_available (P.bool reader)

  let read_properties (reader @ local) =
    let props_len = P.variable_length reader in
    if props_len = 0 then []
    else
      let local_ props_reader = P.sub props_len reader in
      let props = P.many read_property props_reader in
      P.finish props_reader;
      props

  let pp ppf = function
    | Payload_format_indicator v ->
        Format.fprintf ppf "Payload_format_indicator(%d)" v
    | Message_expiry_interval v ->
        Format.fprintf ppf "Message_expiry_interval(%ld)" v
    | Content_type v -> Format.fprintf ppf "Content_type(%s)" v
    | Response_topic v -> Format.fprintf ppf "Response_topic(%s)" v
    | Correlation_data v ->
        Format.fprintf ppf "Correlation_data(<%d bytes>)" (String.length v)
    | Subscription_identifier v ->
        Format.fprintf ppf "Subscription_identifier(%d)" v
    | Session_expiry_interval v ->
        Format.fprintf ppf "Session_expiry_interval(%ld)" v
    | Assigned_client_identifier v ->
        Format.fprintf ppf "Assigned_client_identifier(%s)" v
    | Server_keep_alive v -> Format.fprintf ppf "Server_keep_alive(%d)" v
    | Authentication_method v ->
        Format.fprintf ppf "Authentication_method(%s)" v
    | Authentication_data _ ->
        Format.fprintf ppf "Authentication_data(<hidden>)"
    | Request_problem_information v ->
        Format.fprintf ppf "Request_problem_information(%d)" v
    | Will_delay_interval v -> Format.fprintf ppf "Will_delay_interval(%ld)" v
    | Request_response_information v ->
        Format.fprintf ppf "Request_response_information(%d)" v
    | Response_information v -> Format.fprintf ppf "Response_information(%s)" v
    | Server_reference v -> Format.fprintf ppf "Server_reference(%s)" v
    | Reason_string v -> Format.fprintf ppf "Reason_string(%s)" v
    | Receive_maximum v -> Format.fprintf ppf "Receive_maximum(%d)" v
    | Topic_alias_maximum v -> Format.fprintf ppf "Topic_alias_maximum(%d)" v
    | Topic_alias v -> Format.fprintf ppf "Topic_alias(%d)" v
    | Maximum_qos v -> Format.fprintf ppf "Maximum_qos(%a)" Shared.Qos.pp v
    | Retain_available v -> Format.fprintf ppf "Retain_available(%b)" v
    | User_property (k, v) -> Format.fprintf ppf "User_property(%s, %s)" k v
    | Maximum_packet_size v -> Format.fprintf ppf "Maximum_packet_size(%ld)" v
    | Wildcard_subscription_available v ->
        Format.fprintf ppf "Wildcard_subscription_available(%b)" v
    | Subscription_identifier_available v ->
        Format.fprintf ppf "Subscription_identifier_available(%b)" v
    | Shared_subscription_available v ->
        Format.fprintf ppf "Shared_subscription_available(%b)" v
end

(** {1 Subscription Options} *)

module Subscription_options = struct
  type t = {
    qos : Shared.Qos.t;
    no_local : bool;
    retain_as_published : bool;
    retain_handling : int;
  }

  let pp ppf t =
    Format.fprintf ppf
      "{qos=%a; no_local=%b; retain_as_published=%b; retain_handling=%d}"
      Shared.Qos.pp t.qos t.no_local t.retain_as_published t.retain_handling

  let default qos =
    { qos; no_local = false; retain_as_published = false; retain_handling = 0 }
end

module Subscription = struct
  type t = { filter : Shared.Topic.Filter.t; options : Subscription_options.t }

  let pp ppf t =
    Format.fprintf ppf "{filter=%a; options=%a}" Shared.Topic.Filter.pp t.filter
      Subscription_options.pp t.options
end

module Will_properties = struct
  type t = {
    will_topic : string;
    will_payload : string;
    will_qos : Shared.Qos.t;
    will_retain : bool;
    will_properties : Property.t list;
  }

  let pp ppf t =
    Format.fprintf ppf "{will_topic=%s; will_qos=%a; will_retain=%b}"
      t.will_topic Shared.Qos.pp t.will_qos t.will_retain
end

(** {1 Packet Definitions} *)

module Packet = struct
  module P = Parser

  module Connect = struct
    type t = {
      clean_start : bool;
      keep_alive : int;
      client_id : string;
      credentials : Shared.Credentials.t option;
      will : Will_properties.t option;
      properties : Property.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Connect{client_id=%s; clean_start=%b; keep_alive=%d}"
        t.client_id t.clean_start t.keep_alive
  end

  module Connack = struct
    type t = {
      session_present : bool;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Connack{session_present=%b; reason_code=%a}"
        t.session_present Reason_code.pp t.reason_code
  end

  module Publish = struct
    type t = {
      dup : bool;
      qos : Shared.Qos.t;
      retain : bool;
      topic : Shared.Topic.Name.t;
      packet_id : Shared.Packet_id.t option;
      payload : Slice.t;
      properties : Property.t list;
    }

    let pp ppf t =
      Format.fprintf ppf
        "Publish{topic=%s; qos=%a; dup=%b; retain=%b; payload=<%d bytes>}"
        t.topic Shared.Qos.pp t.qos t.dup t.retain (Slice.length t.payload)
  end

  module Puback = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Puback{packet_id=%d; reason_code=%a}" t.packet_id
        Reason_code.pp t.reason_code
  end

  module Pubrec = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Pubrec{packet_id=%d; reason_code=%a}" t.packet_id
        Reason_code.pp t.reason_code
  end

  module Pubrel = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Pubrel{packet_id=%d; reason_code=%a}" t.packet_id
        Reason_code.pp t.reason_code
  end

  module Pubcomp = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      reason_code : Reason_code.t;
      properties : Property.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Pubcomp{packet_id=%d; reason_code=%a}" t.packet_id
        Reason_code.pp t.reason_code
  end

  module Subscribe = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      properties : Property.t list;
      topics : Subscription.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Subscribe{packet_id=%d; topics=[%a]}" t.packet_id
        (Format.pp_print_list ~pp_sep:Shared.pp_semi Subscription.pp)
        t.topics
  end

  module Suback = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      properties : Property.t list;
      reason_codes : Reason_code.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Suback{packet_id=%d; reason_codes=[%a]}" t.packet_id
        (Format.pp_print_list ~pp_sep:Shared.pp_semi Reason_code.pp)
        t.reason_codes
  end

  module Unsubscribe = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      properties : Property.t list;
      topics : Shared.Topic.Filter.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Unsubscribe{packet_id=%d; topics=[%a]}" t.packet_id
        (Format.pp_print_list ~pp_sep:Shared.pp_semi Shared.Topic.Filter.pp)
        t.topics
  end

  module Unsuback = struct
    type t = {
      packet_id : Shared.Packet_id.t;
      properties : Property.t list;
      reason_codes : Reason_code.t list;
    }

    let pp ppf t =
      Format.fprintf ppf "Unsuback{packet_id=%d; reason_codes=[%a]}" t.packet_id
        (Format.pp_print_list ~pp_sep:Shared.pp_semi Reason_code.pp)
        t.reason_codes
  end

  module Disconnect = struct
    type t = { reason_code : Reason_code.t; properties : Property.t list }

    let pp ppf t =
      Format.fprintf ppf "Disconnect{reason_code=%a}" Reason_code.pp
        t.reason_code
  end

  module Auth = struct
    type t = { reason_code : Reason_code.t; properties : Property.t list }

    let pp ppf t =
      Format.fprintf ppf "Auth{reason_code=%a}" Reason_code.pp t.reason_code
  end

  type t =
    | Connect of Connect.t
    | Connack of Connack.t
    | Publish of Publish.t
    | Puback of Puback.t
    | Pubrec of Pubrec.t
    | Pubrel of Pubrel.t
    | Pubcomp of Pubcomp.t
    | Subscribe of Subscribe.t
    | Suback of Suback.t
    | Unsubscribe of Unsubscribe.t
    | Unsuback of Unsuback.t
    | Pingreq
    | Pingresp
    | Disconnect of Disconnect.t
    | Auth of Auth.t

  let pp ppf = function
    | Connect c -> Connect.pp ppf c
    | Connack c -> Connack.pp ppf c
    | Publish p -> Publish.pp ppf p
    | Puback p -> Puback.pp ppf p
    | Pubrec p -> Pubrec.pp ppf p
    | Pubrel p -> Pubrel.pp ppf p
    | Pubcomp p -> Pubcomp.pp ppf p
    | Subscribe s -> Subscribe.pp ppf s
    | Suback s -> Suback.pp ppf s
    | Unsubscribe u -> Unsubscribe.pp ppf u
    | Unsuback u -> Unsuback.pp ppf u
    | Pingreq -> Format.fprintf ppf "Pingreq"
    | Pingresp -> Format.fprintf ppf "Pingresp"
    | Disconnect d -> Disconnect.pp ppf d
    | Auth a -> Auth.pp ppf a

  (** {1 Encoding} *)

  let check_reason packet code =
    P.check (Reason_code.allowed_on packet code)
      "reason code not allowed on this packet"

  let validate packet =
    let ack id rc props allowed =
      P.packet_id id; check_reason allowed rc; Property.validate `Ack props
    in
    match packet with
    | Connect c ->
        P.check (Utf8.valid c.client_id && String.length c.client_id <= 65535)
          "invalid client id";
        P.check (c.keep_alive >= 0 && c.keep_alive <= 65535) "invalid keep alive";
        Property.validate `Connect c.properties;
        Option.iter (fun (w : Will_properties.t) ->
          P.topic w.will_topic;
          Property.validate `Will w.will_properties;
          if List.mem (Property.Payload_format_indicator 1) w.will_properties then
            P.check (Utf8.valid_payload (Slice.of_string w.will_payload))
              "invalid UTF-8 Will payload") c.will
    | Connack c ->
        check_reason `Connack c.reason_code;
        P.check (not c.session_present || Reason_code.to_int c.reason_code = 0)
          "session present on refused connection";
        Property.validate `Connack c.properties
    | Publish p ->
        Property.validate `Publish p.properties;
        if p.topic = "" then
          P.check (List.exists (function Property.Topic_alias _ -> true
            | _ -> false) p.properties) "empty topic without alias"
        else P.topic p.topic;
        P.check (not p.dup || p.qos <> `At_most_once) "DUP on QoS 0";
        (match p.qos, p.packet_id with
         | `At_most_once, None -> ()
         | (`At_least_once | `Exactly_once), Some id -> P.packet_id id
         | _ -> invalid_arg "PUBLISH packet id does not match QoS");
        if List.mem (Property.Payload_format_indicator 1) p.properties then
          P.check (Utf8.valid_payload p.payload) "invalid UTF-8 PUBLISH payload"
    | Puback p -> ack p.packet_id p.reason_code p.properties
        `Puback
    | Pubrec p -> ack p.packet_id p.reason_code p.properties
        `Puback
    | Pubrel p -> ack p.packet_id p.reason_code p.properties `Pubcomp
    | Pubcomp p -> ack p.packet_id p.reason_code p.properties `Pubcomp
    | Subscribe s ->
        P.packet_id s.packet_id;
        P.check (s.topics <> []) "empty subscription";
        Property.validate `Subscribe s.properties;
        List.iter (fun (s : Subscription.t) ->
          P.check (Shared.Topic.Filter.validate_shared s.filter)
            "invalid shared topic filter";
          P.check (s.options.retain_handling >= 0 &&
                   s.options.retain_handling <= 2) "invalid retain handling";
          if String.starts_with ~prefix:"$share/" s.filter then
            P.check (not s.options.no_local) "No Local on shared subscription"
        ) s.topics
    | Suback s ->
        P.packet_id s.packet_id;
        Property.validate `Ack s.properties;
        P.check (s.reason_codes <> []) "empty SUBACK";
        List.iter (check_reason `Suback)
          s.reason_codes
    | Unsubscribe s ->
        P.packet_id s.packet_id;
        Property.validate `Unsubscribe s.properties;
        P.check (s.topics <> []) "empty unsubscribe";
        List.iter (fun filter ->
          P.check (Shared.Topic.Filter.validate_shared filter)
            "invalid shared topic filter") s.topics
    | Unsuback s ->
        P.packet_id s.packet_id;
        Property.validate `Ack s.properties;
        P.check (s.reason_codes <> []) "empty UNSUBACK";
        List.iter (check_reason `Unsuback) s.reason_codes
    | Disconnect d ->
        Property.validate `Disconnect d.properties;
        check_reason `Disconnect
          d.reason_code
    | Auth a -> Property.validate `Auth a.properties; check_reason `Auth a.reason_code
    | Pingreq | Pingresp -> ()

  let write_connect writer (c : Connect.t) =
    let payload =
      P.to_string (fun w ->
          P.write_mqtt_string w "MQTT";
          P.write_uint8 w 5;
          let flags = ref 0 in
          if c.clean_start then flags := !flags lor 0x02;
          (match c.will with
          | Some (will : Will_properties.t) ->
              flags := !flags lor 0x04;
              flags := !flags lor (Shared.Qos.to_int will.will_qos lsl 3);
              if will.will_retain then flags := !flags lor 0x20
          | None -> ());
          (match c.credentials with
          | Some (`Password _) -> flags := !flags lor 0x40
          | Some (`Username _) -> flags := !flags lor 0x80
          | Some (`Username_password _) -> flags := !flags lor 0xC0
          | None -> ());
          P.write_uint8 w !flags;
          P.write_uint16_be w c.keep_alive;
          Property.write_properties w c.properties;
          P.write_mqtt_string w c.client_id;
          (match c.will with
          | Some (will : Will_properties.t) ->
              Property.write_properties w will.will_properties;
              P.write_mqtt_string w will.will_topic;
              P.write_mqtt_binary w will.will_payload
          | None -> ());
          match c.credentials with
          | Some (`Password p) -> P.write_mqtt_binary w p
          | Some (`Username u) -> P.write_mqtt_string w u
          | Some (`Username_password (username, password)) ->
              P.write_mqtt_string w username;
              P.write_mqtt_binary w password
          | None -> ())
    in
    P.write_fixed_header writer `CONNECT 0 (String.length payload);
    P.write_string writer payload

  let write_connack writer (c : Connack.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint8 w (if c.session_present then 0x01 else 0x00);
          P.write_uint8 w (Reason_code.to_int c.reason_code);
          Property.write_properties w c.properties)
    in
    P.write_fixed_header writer `CONNACK 0 (String.length payload);
    P.write_string writer payload

  let publish_header (p : Publish.t) =
    let variable = P.to_string (fun w ->
      P.write_mqtt_string w p.topic;
      Option.iter (P.write_uint16_be w) p.packet_id;
          Property.write_properties w p.properties;
      ()) in
    let flags = (if p.dup then 8 else 0)
      lor (Shared.Qos.to_int p.qos lsl 1)
      lor (if p.retain then 1 else 0) in
    P.to_string (fun w ->
      P.write_fixed_header w `PUBLISH flags
        (String.length variable + Slice.length p.payload);
      P.write_string w variable)

  let write_pubx writer packet_type ~flags ~packet_id ~reason_code ~properties =
    let payload =
      P.to_string (fun w ->
          P.write_uint16_be w packet_id;
          if reason_code <> `Success || properties <> [] then begin
            P.write_uint8 w (Reason_code.to_int reason_code);
            if properties <> [] then Property.write_properties w properties
          end)
    in
    P.write_fixed_header writer packet_type flags (String.length payload);
    P.write_string writer payload

  let write_puback writer (p : Puback.t) =
    write_pubx writer `PUBACK ~flags:0 ~packet_id:p.packet_id
      ~reason_code:p.reason_code ~properties:p.properties

  let write_pubrec writer (p : Pubrec.t) =
    write_pubx writer `PUBREC ~flags:0 ~packet_id:p.packet_id
      ~reason_code:p.reason_code ~properties:p.properties

  let write_pubrel writer (p : Pubrel.t) =
    write_pubx writer `PUBREL ~flags:0x02 ~packet_id:p.packet_id
      ~reason_code:p.reason_code ~properties:p.properties

  let write_pubcomp writer (p : Pubcomp.t) =
    write_pubx writer `PUBCOMP ~flags:0 ~packet_id:p.packet_id
      ~reason_code:p.reason_code ~properties:p.properties

  let write_subscribe writer (s : Subscribe.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint16_be w s.packet_id;
          Property.write_properties w s.properties;
          List.iter
            (fun (t : Subscription.t) ->
              P.write_mqtt_string w t.filter;
              let opts =
                Shared.Qos.to_int t.options.qos
                lor (if t.options.no_local then 0x04 else 0)
                lor (if t.options.retain_as_published then 0x08 else 0)
                lor (t.options.retain_handling lsl 4)
              in
              P.write_uint8 w opts)
            s.topics)
    in
    P.write_fixed_header writer `SUBSCRIBE 0x02 (String.length payload);
    P.write_string writer payload

  let write_suback writer (s : Suback.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint16_be w s.packet_id;
          Property.write_properties w s.properties;
          List.iter
            (fun rc -> P.write_uint8 w (Reason_code.to_int rc))
            s.reason_codes)
    in
    P.write_fixed_header writer `SUBACK 0 (String.length payload);
    P.write_string writer payload

  let write_unsubscribe writer (u : Unsubscribe.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint16_be w u.packet_id;
          Property.write_properties w u.properties;
          List.iter (fun topic -> P.write_mqtt_string w topic) u.topics)
    in
    P.write_fixed_header writer `UNSUBSCRIBE 0x02 (String.length payload);
    P.write_string writer payload

  let write_unsuback writer (u : Unsuback.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint16_be w u.packet_id;
          Property.write_properties w u.properties;
          List.iter
            (fun rc -> P.write_uint8 w (Reason_code.to_int rc))
            u.reason_codes)
    in
    P.write_fixed_header writer `UNSUBACK 0 (String.length payload);
    P.write_string writer payload

  let write_pingreq writer = P.write_fixed_header writer `PINGREQ 0 0
  let write_pingresp writer = P.write_fixed_header writer `PINGRESP 0 0

  let write_disconnect writer (d : Disconnect.t) =
    let payload =
      P.to_string (fun w ->
          if d.reason_code <> `Normal_disconnection || d.properties <> [] then begin
            P.write_uint8 w (Reason_code.to_int d.reason_code);
            if d.properties <> [] then Property.write_properties w d.properties
          end)
    in
    P.write_fixed_header writer `DISCONNECT 0 (String.length payload);
    P.write_string writer payload

  let write_auth writer (a : Auth.t) =
    let payload =
      P.to_string (fun w ->
          P.write_uint8 w (Reason_code.to_int a.reason_code);
          Property.write_properties w a.properties)
    in
    P.write_fixed_header writer `AUTH 0 (String.length payload);
    P.write_string writer payload

  let write writer = function
    | Connect c -> write_connect writer c
    | Connack c -> write_connack writer c
    | Publish _ -> assert false
    | Puback p -> write_puback writer p
    | Pubrec p -> write_pubrec writer p
    | Pubrel p -> write_pubrel writer p
    | Pubcomp p -> write_pubcomp writer p
    | Subscribe s -> write_subscribe writer s
    | Suback s -> write_suback writer s
    | Unsubscribe u -> write_unsubscribe writer u
    | Unsuback u -> write_unsuback writer u
    | Pingreq -> write_pingreq writer
    | Pingresp -> write_pingresp writer
    | Disconnect d -> write_disconnect writer d
    | Auth a -> write_auth writer a

  (** {1 Decoding} *)

  let read_connect (reader @ local) =
    let proto_name = P.mqtt_string reader in
    if proto_name <> "MQTT" then invalid_arg "Invalid protocol name";
    let proto_version = P.uint8 reader in
    if proto_version <> 5 then invalid_arg "Unsupported protocol version";
    let flags = P.uint8 reader in
    P.connect_flags flags;
    let clean_start = flags land 0x02 <> 0 in
    let will_flag = flags land 0x04 <> 0 in
    let will_qos = Shared.Qos.of_int ((flags land 0x18) lsr 3) in
    let will_retain = flags land 0x20 <> 0 in
    let password_flag = flags land 0x40 <> 0 in
    let username_flag = flags land 0x80 <> 0 in
    let keep_alive = P.uint16_be reader in
    let properties = Property.read_properties reader in
    let client_id = P.mqtt_string reader in
    let will =
      if will_flag then
        let will_properties = Property.read_properties reader in
        let will_topic = P.mqtt_string reader in
        let will_payload = P.mqtt_binary reader in
        Some
          Will_properties.
            { will_properties; will_topic; will_payload; will_qos; will_retain }
      else None
    in
    let credentials =
      if username_flag then
        let username = P.mqtt_string reader in
        if password_flag then
          let password = P.mqtt_binary reader in
          Some (`Username_password (username, password))
        else Some (`Username username)
      else if password_flag then Some (`Password (P.mqtt_binary reader))
      else None
    in
    Connect
      Connect.
        { clean_start; keep_alive; client_id; credentials; will; properties }

  let read_connack ~remaining_length:_ (reader @ local) =
    let flags = P.uint8 reader in
    P.check (flags land 0xfe = 0) "reserved CONNACK flags";
    let session_present = flags land 0x01 <> 0 in
    let reason_code_byte = P.uint8 reader in
    let reason_code = Reason_code.of_int reason_code_byte in
    let properties =
      Property.read_properties reader
    in
    Connack Connack.{ session_present; reason_code; properties }

  let read_publish ~flags (reader @ local) =
    let dup = flags land 0x08 <> 0 in
    let qos = Shared.Qos.of_int ((flags land 0x06) lsr 1) in
    let retain = flags land 0x01 <> 0 in
    let topic = P.mqtt_string reader in
    let packet_id =
      if qos <> `At_most_once then Some (P.uint16_be reader) else None
    in
    let properties = Property.read_properties reader in
    let payload = P.take_rest reader in
    Publish Publish.{ dup; qos; retain; topic; packet_id; payload; properties }

  let read_pubx_common ~remaining_length (reader @ local) =
    let packet_id = P.uint16_be reader in
    if remaining_length = 2 then (packet_id, `Success, [])
    else
      let reason_code_byte = P.uint8 reader in
      let reason_code = Reason_code.of_int reason_code_byte in
      let properties =
        if remaining_length > 3 then Property.read_properties reader else []
      in
      (packet_id, reason_code, properties)

  let read_puback ~remaining_length (reader @ local) =
    let packet_id, reason_code, properties =
      read_pubx_common ~remaining_length reader
    in
    Puback Puback.{ packet_id; reason_code; properties }

  let read_pubrec ~remaining_length (reader @ local) =
    let packet_id, reason_code, properties =
      read_pubx_common ~remaining_length reader
    in
    Pubrec Pubrec.{ packet_id; reason_code; properties }

  let read_pubrel ~remaining_length (reader @ local) =
    let packet_id, reason_code, properties =
      read_pubx_common ~remaining_length reader
    in
    Pubrel Pubrel.{ packet_id; reason_code; properties }

  let read_pubcomp ~remaining_length (reader @ local) =
    let packet_id, reason_code, properties =
      read_pubx_common ~remaining_length reader
    in
    Pubcomp Pubcomp.{ packet_id; reason_code; properties }

  let read_subscribe (reader @ local) =
    let packet_id = P.uint16_be reader in
    let properties = Property.read_properties reader in
    let read_topic (reader @ local) =
      let filter = P.mqtt_string reader in
      let opts_byte = P.uint8 reader in
      P.check (opts_byte land 0xc0 = 0) "reserved subscription options";
      let options =
        Subscription_options.
          {
            qos = Shared.Qos.of_int (opts_byte land 0x03);
            no_local = opts_byte land 0x04 <> 0;
            retain_as_published = opts_byte land 0x08 <> 0;
            retain_handling = (opts_byte land 0x30) lsr 4;
          }
      in
      Subscription.{ filter; options }
    in
    let topics = P.many1 read_topic reader in
    Subscribe Subscribe.{ packet_id; properties; topics }

  let read_suback (reader @ local) =
    let packet_id = P.uint16_be reader in
    let properties = Property.read_properties reader in
    let read_reason_code (reader @ local) = Reason_code.of_int (P.uint8 reader) in
    let reason_codes = P.many read_reason_code reader in
    Suback Suback.{ packet_id; properties; reason_codes }

  let read_unsubscribe (reader @ local) =
    let packet_id = P.uint16_be reader in
    let properties = Property.read_properties reader in
    let topics = P.many1 P.mqtt_string reader in
    Unsubscribe Unsubscribe.{ packet_id; properties; topics }

  let read_unsuback (reader @ local) =
    let packet_id = P.uint16_be reader in
    let properties = Property.read_properties reader in
    let read_reason_code (reader @ local) = Reason_code.of_int (P.uint8 reader) in
    let reason_codes = P.many read_reason_code reader in
    Unsuback Unsuback.{ packet_id; properties; reason_codes }

  let read_disconnect ~remaining_length (reader @ local) =
    if remaining_length = 0 then
      Disconnect
        Disconnect.{ reason_code = `Normal_disconnection; properties = [] }
    else
      let reason_code_byte = P.uint8 reader in
      let reason_code = Reason_code.of_int reason_code_byte in
      let properties =
        if remaining_length > 1 then Property.read_properties reader else []
      in
      Disconnect Disconnect.{ reason_code; properties }

  let read_auth (reader @ local) =
    let reason_code = if P.is_eod reader then `Success
      else Reason_code.of_int (P.uint8 reader) in
    let properties = if P.is_eod reader then []
      else Property.read_properties reader in
    Auth Auth.{ reason_code; properties }

  let read (reader @ local) =
    let first_byte = P.uint8 reader in
    let packet_type = Shared.Packet_type.of_int (first_byte lsr 4) in
    let flags = first_byte land 0x0F in
    let remaining_length = P.variable_length reader in
    let local_ payload_reader = P.sub remaining_length reader in
    let packet = match packet_type with
    | `CONNECT -> read_connect payload_reader
    | `CONNACK -> read_connack ~remaining_length payload_reader
    | `PUBLISH -> read_publish ~flags payload_reader
    | `PUBACK -> read_puback ~remaining_length payload_reader
    | `PUBREC -> read_pubrec ~remaining_length payload_reader
    | `PUBREL -> read_pubrel ~remaining_length payload_reader
    | `PUBCOMP -> read_pubcomp ~remaining_length payload_reader
    | `SUBSCRIBE -> read_subscribe payload_reader
    | `SUBACK -> read_suback payload_reader
    | `UNSUBSCRIBE -> read_unsubscribe payload_reader
    | `UNSUBACK -> read_unsuback payload_reader
    | `PINGREQ -> Pingreq
    | `PINGRESP -> Pingresp
    | `DISCONNECT -> read_disconnect ~remaining_length payload_reader
    | `AUTH -> read_auth payload_reader
    | `RESERVED -> invalid_arg "Reserved packet type"
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

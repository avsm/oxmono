(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** HTTP/2 Frame Layer.

    Implements frame parsing and serialization per
    {{:https://datatracker.ietf.org/doc/html/rfc9113#section-4}RFC 9113 Section 4}
    and {{:https://datatracker.ietf.org/doc/html/rfc9113#section-6}Section 6}. *)

(* ============================================================
   Stream Identifier
   ============================================================ *)

type stream_id = int32

let stream_id_is_client_initiated id =
  Int32.(logand id 1l = 1l) && id <> 0l

let stream_id_is_server_initiated id =
  Int32.(logand id 1l = 0l) && id <> 0l

(* ============================================================
   Frame Types
   ============================================================ *)

type frame_type =
  | Data
  | Headers
  | Priority
  | Rst_stream
  | Settings
  | Push_promise
  | Ping
  | Goaway
  | Window_update
  | Continuation
  | Unknown of int

let frame_type_to_int = function
  | Data -> 0x00
  | Headers -> 0x01
  | Priority -> 0x02
  | Rst_stream -> 0x03
  | Settings -> 0x04
  | Push_promise -> 0x05
  | Ping -> 0x06
  | Goaway -> 0x07
  | Window_update -> 0x08
  | Continuation -> 0x09
  | Unknown n -> n

let frame_type_of_int = function
  | 0x00 -> Data
  | 0x01 -> Headers
  | 0x02 -> Priority
  | 0x03 -> Rst_stream
  | 0x04 -> Settings
  | 0x05 -> Push_promise
  | 0x06 -> Ping
  | 0x07 -> Goaway
  | 0x08 -> Window_update
  | 0x09 -> Continuation
  | n -> Unknown n

let pp_frame_type ppf ft =
  Format.pp_print_string ppf (match ft with
    | Data -> "DATA"
    | Headers -> "HEADERS"
    | Priority -> "PRIORITY"
    | Rst_stream -> "RST_STREAM"
    | Settings -> "SETTINGS"
    | Push_promise -> "PUSH_PROMISE"
    | Ping -> "PING"
    | Goaway -> "GOAWAY"
    | Window_update -> "WINDOW_UPDATE"
    | Continuation -> "CONTINUATION"
    | Unknown n -> Printf.sprintf "UNKNOWN(0x%02x)" n)

(* ============================================================
   Frame Flags
   ============================================================ *)

module Flags = struct
  type t = int

  let none = 0x00
  let end_stream = 0x01
  let ack = 0x01  (* Same bit, different frames *)
  let end_headers = 0x04
  let padded = 0x08
  let priority = 0x20

  let test flags flag = flags land flag <> 0
  let set flags flag = flags lor flag
  let clear flags flag = flags land (lnot flag)
  (* Conditionally set a flag: [set_if flag cond flags] sets [flag] in [flags] if [cond] is true.
     Designed for piping: [Flags.none |> set_if end_stream true |> set_if ack false] *)
  let set_if flag cond flags = if cond then set flags flag else flags

  let pp ppf flags =
    let flag_names = [
      (end_stream, "END_STREAM");
      (end_headers, "END_HEADERS");
      (padded, "PADDED");
      (priority, "PRIORITY");
    ] in
    let active = List.filter_map (fun (flag, name) ->
      if test flags flag then Some name else None
    ) flag_names in
    match active with
    | [] -> Format.pp_print_string ppf "0x00"
    | _ -> Format.pp_print_string ppf (String.concat "|" active)
end

(* ============================================================
   Frame Header
   ============================================================ *)

type frame_header = {
  length : int;
  frame_type : frame_type;
  flags : Flags.t;
  stream_id : stream_id;
}

let frame_header_length = 9

let pp_frame_header ppf h =
  Format.fprintf ppf "{len=%d type=%a flags=%a stream=%ld}"
    h.length pp_frame_type h.frame_type Flags.pp h.flags h.stream_id

(* ============================================================
   Error Codes
   ============================================================ *)

type error_code =
  | No_error
  | Protocol_error
  | Internal_error
  | Flow_control_error
  | Settings_timeout
  | Stream_closed
  | Frame_size_error
  | Refused_stream
  | Cancel
  | Compression_error
  | Connect_error
  | Enhance_your_calm
  | Inadequate_security
  | Http_1_1_required
  | Unknown_error of int32

let error_code_to_int32 = function
  | No_error -> 0x0l
  | Protocol_error -> 0x1l
  | Internal_error -> 0x2l
  | Flow_control_error -> 0x3l
  | Settings_timeout -> 0x4l
  | Stream_closed -> 0x5l
  | Frame_size_error -> 0x6l
  | Refused_stream -> 0x7l
  | Cancel -> 0x8l
  | Compression_error -> 0x9l
  | Connect_error -> 0xal
  | Enhance_your_calm -> 0xbl
  | Inadequate_security -> 0xcl
  | Http_1_1_required -> 0xdl
  | Unknown_error n -> n

let error_code_of_int32 = function
  | 0x0l -> No_error
  | 0x1l -> Protocol_error
  | 0x2l -> Internal_error
  | 0x3l -> Flow_control_error
  | 0x4l -> Settings_timeout
  | 0x5l -> Stream_closed
  | 0x6l -> Frame_size_error
  | 0x7l -> Refused_stream
  | 0x8l -> Cancel
  | 0x9l -> Compression_error
  | 0xal -> Connect_error
  | 0xbl -> Enhance_your_calm
  | 0xcl -> Inadequate_security
  | 0xdl -> Http_1_1_required
  | n -> Unknown_error n

let error_code_to_string = function
  | No_error -> "NO_ERROR"
  | Protocol_error -> "PROTOCOL_ERROR"
  | Internal_error -> "INTERNAL_ERROR"
  | Flow_control_error -> "FLOW_CONTROL_ERROR"
  | Settings_timeout -> "SETTINGS_TIMEOUT"
  | Stream_closed -> "STREAM_CLOSED"
  | Frame_size_error -> "FRAME_SIZE_ERROR"
  | Refused_stream -> "REFUSED_STREAM"
  | Cancel -> "CANCEL"
  | Compression_error -> "COMPRESSION_ERROR"
  | Connect_error -> "CONNECT_ERROR"
  | Enhance_your_calm -> "ENHANCE_YOUR_CALM"
  | Inadequate_security -> "INADEQUATE_SECURITY"
  | Http_1_1_required -> "HTTP_1_1_REQUIRED"
  | Unknown_error n -> Printf.sprintf "UNKNOWN(0x%lx)" n

let pp_error_code ppf ec =
  Format.pp_print_string ppf (error_code_to_string ec)

(* ============================================================
   Settings
   ============================================================ *)

type setting =
  | Header_table_size of int
  | Enable_push of bool
  | Max_concurrent_streams of int32
  | Initial_window_size of int32
  | Max_frame_size of int
  | Max_header_list_size of int
  | No_rfc7540_priorities of bool  (** RFC 9113 Section 5.3.2: Deprecate RFC 7540 priorities *)
  | Unknown_setting of int * int32

let setting_to_pair = function
  | Header_table_size n -> (0x1, Int32.of_int n)
  | Enable_push b -> (0x2, if b then 1l else 0l)
  | Max_concurrent_streams n -> (0x3, n)
  | Initial_window_size n -> (0x4, n)
  | Max_frame_size n -> (0x5, Int32.of_int n)
  | Max_header_list_size n -> (0x6, Int32.of_int n)
  | No_rfc7540_priorities b -> (0x9, if b then 1l else 0l)
  | Unknown_setting (id, value) -> (id, value)

let setting_of_pair id value =
  match id with
  | 0x1 -> Header_table_size (Int32.to_int value)
  | 0x2 -> Enable_push (value <> 0l)
  | 0x3 -> Max_concurrent_streams value
  | 0x4 -> Initial_window_size value
  | 0x5 -> Max_frame_size (Int32.to_int value)
  | 0x6 -> Max_header_list_size (Int32.to_int value)
  | 0x9 -> No_rfc7540_priorities (value <> 0l)
  | _ -> Unknown_setting (id, value)

let pp_setting ppf s =
  match s with
  | Header_table_size n ->
      Format.fprintf ppf "HEADER_TABLE_SIZE=%d" n
  | Enable_push b ->
      Format.fprintf ppf "ENABLE_PUSH=%b" b
  | Max_concurrent_streams n ->
      Format.fprintf ppf "MAX_CONCURRENT_STREAMS=%ld" n
  | Initial_window_size n ->
      Format.fprintf ppf "INITIAL_WINDOW_SIZE=%ld" n
  | Max_frame_size n ->
      Format.fprintf ppf "MAX_FRAME_SIZE=%d" n
  | Max_header_list_size n ->
      Format.fprintf ppf "MAX_HEADER_LIST_SIZE=%d" n
  | No_rfc7540_priorities b ->
      Format.fprintf ppf "NO_RFC7540_PRIORITIES=%b" b
  | Unknown_setting (id, value) ->
      Format.fprintf ppf "UNKNOWN(%d)=%ld" id value

(* ============================================================
   Priority
   ============================================================ *)

type priority = {
  exclusive : bool;
  stream_dependency : stream_id;
  weight : int;
}

let default_priority = {
  exclusive = false;
  stream_dependency = 0l;
  weight = 16;
}

let pp_priority ppf p =
  Format.fprintf ppf "{exclusive=%b dep=%ld weight=%d}"
    p.exclusive p.stream_dependency p.weight

(* ============================================================
   Frame Payloads
   ============================================================ *)

type frame_payload =
  | Data_payload of { data : Cstruct.t }
  | Headers_payload of {
      priority : priority option;
      header_block : Cstruct.t;
    }
  | Priority_payload of priority
  | Rst_stream_payload of error_code
  | Settings_payload of setting list
  | Push_promise_payload of {
      promised_stream_id : stream_id;
      header_block : Cstruct.t;
    }
  | Ping_payload of Cstruct.t
  | Goaway_payload of {
      last_stream_id : stream_id;
      error_code : error_code;
      debug_data : Cstruct.t;
    }
  | Window_update_payload of int32
  | Continuation_payload of { header_block : Cstruct.t }
  | Unknown_payload of Cstruct.t

(* ============================================================
   Complete Frame
   ============================================================ *)

type frame = {
  header : frame_header;
  payload : frame_payload;
}

let pp_frame ppf f =
  Format.fprintf ppf "Frame %a" pp_frame_header f.header

(* ============================================================
   Parse Errors
   ============================================================ *)

type parse_error =
  | Incomplete
  | Frame_size_error of string
  | Protocol_error of string

let pp_parse_error ppf = function
  | Incomplete -> Format.pp_print_string ppf "Incomplete"
  | Frame_size_error msg -> Format.fprintf ppf "Frame_size_error: %s" msg
  | Protocol_error msg -> Format.fprintf ppf "Protocol_error: %s" msg

(* ============================================================
   Frame Parsing
   ============================================================ *)

(** Result bind operator for cleaner error handling. *)
let ( let* ) = Result.bind

(** Parse padding from a frame buffer.
    Returns (pad_length, offset after padding byte) or error. *)
let parse_padding flags buf frame_name =
  if Flags.test flags Flags.padded then
    if Cstruct.length buf < 1 then
      Error (Protocol_error (frame_name ^ " frame too short for padding length"))
    else
      Ok (Cstruct.get_uint8 buf 0, 1)
  else
    Ok (0, 0)

let parse_frame_header buf =
  if Cstruct.length buf < frame_header_length then
    Error Incomplete
  else
    (* Length is 24-bit big-endian *)
    let length =
      (Cstruct.get_uint8 buf 0 lsl 16) lor
      (Cstruct.get_uint8 buf 1 lsl 8) lor
      (Cstruct.get_uint8 buf 2)
    in
    let frame_type = frame_type_of_int (Cstruct.get_uint8 buf 3) in
    let flags = Cstruct.get_uint8 buf 4 in
    (* Stream ID is 31-bit, clear reserved bit *)
    let stream_id = Int32.logand (Cstruct.BE.get_uint32 buf 5) 0x7fffffffl in
    Ok { length; frame_type; flags; stream_id }

let parse_priority buf off =
  let dep_and_exclusive = Cstruct.BE.get_uint32 buf off in
  let exclusive = Int32.(logand dep_and_exclusive 0x80000000l <> 0l) in
  let stream_dependency = Int32.logand dep_and_exclusive 0x7fffffffl in
  let weight = Cstruct.get_uint8 buf (off + 4) + 1 in (* 0-255 -> 1-256 *)
  { exclusive; stream_dependency; weight }

let parse_settings buf =
  let len = Cstruct.length buf in
  if len mod 6 <> 0 then
    Error (Protocol_error "SETTINGS payload must be multiple of 6 bytes")
  else
    let rec loop acc off =
      if off >= len then Ok (List.rev acc)
      else
        let id = Cstruct.BE.get_uint16 buf off in
        let value = Cstruct.BE.get_uint32 buf (off + 2) in
        loop (setting_of_pair id value :: acc) (off + 6)
    in
    loop [] 0

let parse_frame_payload header buf =
  let flags = header.flags in
  match header.frame_type with
  | Data ->
      let* (pad_length, off) = parse_padding flags buf "DATA" in
      let data_length = Cstruct.length buf - off - pad_length in
      if data_length < 0 then
        Error (Protocol_error "DATA padding exceeds frame size")
      else
        Ok (Data_payload { data = Cstruct.sub buf off data_length })

  | Headers ->
      let* (pad_length, off) = parse_padding flags buf "HEADERS" in
      (* Handle priority *)
      let* (priority, off) =
        if Flags.test flags Flags.priority then
          if Cstruct.length buf < off + 5 then
            Error (Protocol_error "HEADERS frame too short for priority")
          else
            Ok (Some (parse_priority buf off), off + 5)
        else
          Ok (None, off)
      in
      let header_block_length = Cstruct.length buf - off - pad_length in
      if header_block_length < 0 then
        Error (Protocol_error "HEADERS padding exceeds frame size")
      else
        Ok (Headers_payload {
          priority;
          header_block = Cstruct.sub buf off header_block_length;
        })

  | Priority ->
      if Cstruct.length buf <> 5 then
        Error (Frame_size_error "PRIORITY frame must be 5 bytes")
      else
        Ok (Priority_payload (parse_priority buf 0))

  | Rst_stream ->
      if Cstruct.length buf <> 4 then
        Error (Frame_size_error "RST_STREAM frame must be 4 bytes")
      else
        let code = error_code_of_int32 (Cstruct.BE.get_uint32 buf 0) in
        Ok (Rst_stream_payload code)

  | Settings ->
      if Flags.test flags Flags.ack then
        if Cstruct.length buf <> 0 then
          Error (Frame_size_error "SETTINGS ACK must have empty payload")
        else
          Ok (Settings_payload [])
      else
        (match parse_settings buf with
         | Ok settings -> Ok (Settings_payload settings)
         | Error e -> Error e)

  | Push_promise ->
      let* (pad_length, off) = parse_padding flags buf "PUSH_PROMISE" in
      if Cstruct.length buf < off + 4 then
        Error (Protocol_error "PUSH_PROMISE too short for promised stream ID")
      else
        let promised_stream_id =
          Int32.logand (Cstruct.BE.get_uint32 buf off) 0x7fffffffl
        in
        let off = off + 4 in
        let header_block_length = Cstruct.length buf - off - pad_length in
        if header_block_length < 0 then
          Error (Protocol_error "PUSH_PROMISE padding exceeds frame size")
        else
          Ok (Push_promise_payload {
            promised_stream_id;
            header_block = Cstruct.sub buf off header_block_length;
          })

  | Ping ->
      if Cstruct.length buf <> 8 then
        Error (Frame_size_error "PING frame must be 8 bytes")
      else
        Ok (Ping_payload buf)

  | Goaway ->
      if Cstruct.length buf < 8 then
        Error (Frame_size_error "GOAWAY frame must be at least 8 bytes")
      else
        let last_stream_id =
          Int32.logand (Cstruct.BE.get_uint32 buf 0) 0x7fffffffl
        in
        let error_code = error_code_of_int32 (Cstruct.BE.get_uint32 buf 4) in
        let debug_data = Cstruct.shift buf 8 in
        Ok (Goaway_payload { last_stream_id; error_code; debug_data })

  | Window_update ->
      if Cstruct.length buf <> 4 then
        Error (Frame_size_error "WINDOW_UPDATE frame must be 4 bytes")
      else
        let increment = Int32.logand (Cstruct.BE.get_uint32 buf 0) 0x7fffffffl in
        if increment = 0l then
          Error (Protocol_error "WINDOW_UPDATE increment must be > 0")
        else
          Ok (Window_update_payload increment)

  | Continuation ->
      Ok (Continuation_payload { header_block = buf })

  | Unknown _ ->
      Ok (Unknown_payload buf)

let parse_frame buf ~max_frame_size =
  match parse_frame_header buf with
  | Error e -> Error e
  | Ok header ->
      if header.length > max_frame_size then
        Error (Frame_size_error (Printf.sprintf
          "Frame payload %d exceeds max %d" header.length max_frame_size))
      else
        let total_length = frame_header_length + header.length in
        if Cstruct.length buf < total_length then
          Error Incomplete
        else
          let payload_buf = Cstruct.sub buf frame_header_length header.length in
          match parse_frame_payload header payload_buf with
          | Ok payload -> Ok ({ header; payload }, total_length)
          | Error e -> Error e

(* ============================================================
   Frame Serialization
   ============================================================ *)

let serialize_frame_header header =
  let buf = Cstruct.create frame_header_length in
  (* Length - 24-bit big-endian *)
  Cstruct.set_uint8 buf 0 ((header.length lsr 16) land 0xff);
  Cstruct.set_uint8 buf 1 ((header.length lsr 8) land 0xff);
  Cstruct.set_uint8 buf 2 (header.length land 0xff);
  (* Type *)
  Cstruct.set_uint8 buf 3 (frame_type_to_int header.frame_type);
  (* Flags *)
  Cstruct.set_uint8 buf 4 header.flags;
  (* Stream ID - clear reserved bit *)
  Cstruct.BE.set_uint32 buf 5 (Int32.logand header.stream_id 0x7fffffffl);
  buf

let serialize_priority p =
  let buf = Cstruct.create 5 in
  let dep = if p.exclusive
    then Int32.logor p.stream_dependency 0x80000000l
    else p.stream_dependency
  in
  Cstruct.BE.set_uint32 buf 0 dep;
  Cstruct.set_uint8 buf 4 (p.weight - 1);  (* 1-256 -> 0-255 *)
  buf

let serialize_settings settings =
  let buf = Cstruct.create (List.length settings * 6) in
  List.iteri (fun i s ->
    let id, value = setting_to_pair s in
    let off = i * 6 in
    Cstruct.BE.set_uint16 buf off id;
    Cstruct.BE.set_uint32 buf (off + 2) value
  ) settings;
  buf

let serialize_frame frame =
  let payload_buf = match frame.payload with
    | Data_payload { data } -> data
    | Headers_payload { priority; header_block } ->
        (match priority with
         | None -> header_block
         | Some p -> Cstruct.concat [serialize_priority p; header_block])
    | Priority_payload p -> serialize_priority p
    | Rst_stream_payload code ->
        let buf = Cstruct.create 4 in
        Cstruct.BE.set_uint32 buf 0 (error_code_to_int32 code);
        buf
    | Settings_payload settings -> serialize_settings settings
    | Push_promise_payload { promised_stream_id; header_block } ->
        let buf = Cstruct.create 4 in
        Cstruct.BE.set_uint32 buf 0 (Int32.logand promised_stream_id 0x7fffffffl);
        Cstruct.concat [buf; header_block]
    | Ping_payload data -> data
    | Goaway_payload { last_stream_id; error_code; debug_data } ->
        let buf = Cstruct.create 8 in
        Cstruct.BE.set_uint32 buf 0 (Int32.logand last_stream_id 0x7fffffffl);
        Cstruct.BE.set_uint32 buf 4 (error_code_to_int32 error_code);
        Cstruct.concat [buf; debug_data]
    | Window_update_payload increment ->
        let buf = Cstruct.create 4 in
        Cstruct.BE.set_uint32 buf 0 (Int32.logand increment 0x7fffffffl);
        buf
    | Continuation_payload { header_block } -> header_block
    | Unknown_payload data -> data
  in
  let header = { frame.header with length = Cstruct.length payload_buf } in
  Cstruct.concat [serialize_frame_header header; payload_buf]

let write_frame writer frame =
  let buf = serialize_frame frame in
  Eio.Buf_write.cstruct writer buf

(* ============================================================
   Frame Construction Helpers
   ============================================================ *)

let make_data ~stream_id ?(end_stream=false) data =
  let flags = Flags.none |> Flags.set_if Flags.end_stream end_stream in
  {
    header = { length = Cstruct.length data; frame_type = Data; flags; stream_id };
    payload = Data_payload { data };
  }

let make_headers ~stream_id ?(end_stream=false) ?(end_headers=true) ?priority block =
  let has_priority = Option.is_some priority in
  let flags = Flags.none
    |> Flags.set_if Flags.end_stream end_stream
    |> Flags.set_if Flags.end_headers end_headers
    |> Flags.set_if Flags.priority has_priority
  in
  let length = Cstruct.length block + (if has_priority then 5 else 0) in
  {
    header = { length; frame_type = Headers; flags; stream_id };
    payload = Headers_payload { priority; header_block = block };
  }

let make_rst_stream ~stream_id code =
  {
    header = { length = 4; frame_type = Rst_stream; flags = Flags.none; stream_id };
    payload = Rst_stream_payload code;
  }

let make_settings ?(ack=false) settings =
  let flags = Flags.none |> Flags.set_if Flags.ack ack in
  let length = if ack then 0 else List.length settings * 6 in
  {
    header = { length; frame_type = Settings; flags; stream_id = 0l };
    payload = Settings_payload settings;
  }

let make_ping ?(ack=false) data =
  if Cstruct.length data <> 8 then
    invalid_arg "PING data must be exactly 8 bytes";
  let flags = Flags.none |> Flags.set_if Flags.ack ack in
  {
    header = { length = 8; frame_type = Ping; flags; stream_id = 0l };
    payload = Ping_payload data;
  }

let make_goaway ~last_stream_id code ?(debug="") () =
  let debug_data = Cstruct.of_string debug in
  let length = 8 + Cstruct.length debug_data in
  {
    header = { length; frame_type = Goaway; flags = Flags.none; stream_id = 0l };
    payload = Goaway_payload { last_stream_id; error_code = code; debug_data };
  }

let make_window_update ~stream_id increment =
  if increment <= 0l then
    invalid_arg "WINDOW_UPDATE increment must be > 0";
  {
    header = { length = 4; frame_type = Window_update; flags = Flags.none; stream_id };
    payload = Window_update_payload increment;
  }

let make_continuation ~stream_id ?(end_headers=true) block =
  let flags = Flags.none |> Flags.set_if Flags.end_headers end_headers in
  {
    header = { length = Cstruct.length block; frame_type = Continuation; flags; stream_id };
    payload = Continuation_payload { header_block = block };
  }

(* ============================================================
   Constants
   ============================================================ *)

let default_max_frame_size = 16384        (* 2^14 *)
let max_max_frame_size = 16777215         (* 2^24 - 1 *)
let default_initial_window_size = 65535l  (* 2^16 - 1 *)
let max_window_size = 0x7fffffffl         (* 2^31 - 1 *)

let connection_preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
let connection_preface_length = 24

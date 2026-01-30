# HTTP/2 Implementation Plan for ocaml-requests

## Overview

This document outlines the plan for adding native HTTP/2 support to the ocaml-requests library, implementing RFC 9113 (HTTP/2), RFC 7541 (HPACK header compression), and integrating seamlessly with the existing Eio-based architecture.

### Design Goals

1. **Eio-Native**: Full integration with Eio's structured concurrency primitives
2. **Zero-Copy Where Possible**: Use `Cstruct` and `Bigstringaf` for efficient buffer management
3. **Protocol Transparency**: Users should be able to use the same API for HTTP/1.1 and HTTP/2
4. **Connection Multiplexing**: True stream multiplexing within a single TCP connection
5. **Shared Types**: Maximize type sharing between HTTP/1.1 and HTTP/2 implementations
6. **Backwards Compatibility Not Required**: This is a fresh implementation

## License Attribution

When deriving code or patterns from the [ocaml-h2](https://github.com/anmonteiro/ocaml-h2) library, files MUST include the following license header:

```ocaml
(*---------------------------------------------------------------------------
  Copyright (c) 2019 António Nuno Monteiro.
  Portions Copyright (c) 2017 Inhabited Type LLC.
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  SPDX-License-Identifier: BSD-3-Clause
 ---------------------------------------------------------------------------*)
```

Files NOT derived from h2 continue to use the ISC license header.

## Specification References

| RFC | Title | Status |
|-----|-------|--------|
| [RFC 9113](spec/rfc9113.txt) | HTTP/2 | Primary spec (obsoletes 7540) |
| [RFC 7541](spec/rfc7541.txt) | HPACK: Header Compression for HTTP/2 | Required |
| [RFC 9110](spec/rfc9110.txt) | HTTP Semantics | Shared with HTTP/1.1 |

## Shared Types (HTTP/1.1 and HTTP/2)

The requests library already provides protocol-agnostic types that will be reused for HTTP/2:

| Module | Type | Description | Shared? |
|--------|------|-------------|---------|
| `Method` | `Method.t` | HTTP methods (GET, POST, etc.) | Yes - RFC 9110 |
| `Status` | `Status.t` | HTTP status codes | Yes - RFC 9110 |
| `Headers` | `Headers.t` | Header collection | Yes - with H2 pseudo-header support |
| `Body` | `Body.t` | Request body construction | Yes |
| `Response` | `Response.t` | Response representation | Yes |
| `Uri` | `Uri.t` | URI parsing and manipulation | Yes - RFC 3986 |
| `Mime` | `Mime.t` | MIME types | Yes |
| `Error` | `Error.error` | Error types | Extended for H2 |
| `Timeout` | `Timeout.t` | Timeout configuration | Yes |
| `Retry` | `Retry.config` | Retry configuration | Yes |
| `Auth` | `Auth.t` | Authentication | Yes |

### HTTP/2-Specific Types (New)

| Module | Type | Description |
|--------|------|-------------|
| `H2_frame` | `frame_type`, `frame` | Frame definitions per RFC 9113 §6 |
| `H2_stream` | `stream_id`, `state` | Stream state machine per RFC 9113 §5.1 |
| `H2_settings` | `settings`, `setting` | Connection settings per RFC 9113 §6.5 |
| `H2_hpack` | `Encoder.t`, `Decoder.t` | HPACK compression per RFC 7541 |
| `H2_error` | `error_code` | H2 error codes per RFC 9113 §7 |

### Headers Module Extension

The existing `Headers` module needs extension for HTTP/2 pseudo-headers:

```ocaml
(** HTTP/2 pseudo-headers per RFC 9113 §8.3 *)

val get_pseudo : string -> t -> string option
(** [get_pseudo name headers] retrieves a pseudo-header (without the colon prefix).
    Example: [get_pseudo "method" headers] for [:method] *)

val set_pseudo : string -> string -> t -> t
(** [set_pseudo name value headers] sets a pseudo-header.
    Pseudo-headers are placed before regular headers per RFC 9113 §8.3. *)

val has_pseudo_headers : t -> bool
(** [has_pseudo_headers headers] returns true if any pseudo-headers are present. *)

val validate_h2_request : t -> (unit, string) result
(** Validate headers for HTTP/2 request constraints per RFC 9113 §8.2.1 *)

val validate_h2_response : t -> (unit, string) result
(** Validate headers for HTTP/2 response constraints per RFC 9113 §8.2.2 *)
```

### Error Module Extension

Extend `Error.error` with HTTP/2-specific variants:

```ocaml
(* Add to Error.error type *)

(* HTTP/2 protocol errors *)
| H2_protocol_error of { code: int; message: string }
    (** HTTP/2 connection error per RFC 9113 §5.4.1 *)
| H2_stream_error of { stream_id: int32; code: int; message: string }
    (** HTTP/2 stream error per RFC 9113 §5.4.2 *)
| H2_flow_control_error of { stream_id: int32 option }
    (** Flow control window exceeded per RFC 9113 §5.2 *)
| H2_compression_error of { message: string }
    (** HPACK decompression failed per RFC 7541 *)
| H2_settings_timeout
    (** SETTINGS acknowledgment timeout per RFC 9113 §6.5.3 *)
| H2_goaway of { last_stream_id: int32; code: int; debug: string }
    (** Server sent GOAWAY per RFC 9113 §6.8 *)
```

## Architecture Overview

### Module Structure

```
lib/
├── # ══════════════════════════════════════════════════════════════
├── # SHARED TYPES (Protocol-Agnostic, RFC 9110)
├── # ══════════════════════════════════════════════════════════════
├── method.ml[i]               # HTTP methods (existing)
├── status.ml[i]               # Status codes (existing)
├── headers.ml[i]              # Headers - EXTENDED for H2 pseudo-headers
├── body.ml[i]                 # Request body (existing)
├── response.ml[i]             # Response type (existing)
├── uri.ml[i]                  # URI parsing (existing)
├── error.ml[i]                # Errors - EXTENDED for H2 errors
├── auth.ml[i]                 # Authentication (existing)
├── timeout.ml[i]              # Timeout config (existing)
├── retry.ml[i]                # Retry config (existing)
│
├── # ══════════════════════════════════════════════════════════════
├── # HTTP/1.1 IMPLEMENTATION (Existing)
├── # ══════════════════════════════════════════════════════════════
├── http_read.ml[i]            # HTTP/1.1 response parsing
├── http_write.ml[i]           # HTTP/1.1 request serialization
├── http_client.ml[i]          # HTTP/1.1 client
│
├── # ══════════════════════════════════════════════════════════════
├── # HTTP/2 IMPLEMENTATION (New - BSD-3-Clause where h2-derived)
├── # ══════════════════════════════════════════════════════════════
├── h2/
│   ├── h2_frame.ml[i]         # Frame types and serialization (RFC 9113 §4, §6)
│   ├── h2_hpack.ml[i]         # HPACK encoder/decoder (RFC 7541) - BSD-3-Clause
│   ├── h2_hpack_static.ml     # Static table data (RFC 7541 Appendix A)
│   ├── h2_huffman.ml[i]       # Huffman coding (RFC 7541 Appendix B) - BSD-3-Clause
│   ├── h2_stream.ml[i]        # Stream state machine (RFC 9113 §5.1) - BSD-3-Clause
│   ├── h2_flow_control.ml[i]  # Flow control windows (RFC 9113 §5.2)
│   ├── h2_settings.ml[i]      # Settings negotiation (RFC 9113 §6.5)
│   ├── h2_connection.ml[i]    # Connection lifecycle management
│   └── h2_client.ml[i]        # Client-side HTTP/2 implementation
│
├── # ══════════════════════════════════════════════════════════════
├── # PROTOCOL ABSTRACTION LAYER (New)
├── # ══════════════════════════════════════════════════════════════
├── http_version.ml[i]         # Version enum and ALPN identifiers
├── connection.ml[i]           # Unified HTTP/1.1 + HTTP/2 connection
├── protocol.ml[i]             # Protocol selection and negotiation
│
├── # ══════════════════════════════════════════════════════════════
├── # PUBLIC API (Existing - Updated)
├── # ══════════════════════════════════════════════════════════════
├── requests.ml[i]             # Session API (protocol-transparent)
└── one.ml[i]                  # One-shot API (protocol-transparent)
```

### File License Summary

| File Pattern | License | Reason |
|--------------|---------|--------|
| `h2_hpack.ml[i]` | BSD-3-Clause | Derived from h2 HPACK implementation |
| `h2_huffman.ml[i]` | BSD-3-Clause | Derived from h2 Huffman tables |
| `h2_stream.ml[i]` | BSD-3-Clause | State machine patterns from h2 |
| All other files | ISC | Original implementation |

### Data Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                         Requests API                             │
│  (get, post, put, delete - unchanged interface)                  │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                     Connection Router                            │
│  - ALPN negotiation for protocol selection                       │
│  - Protocol-specific handler dispatch                            │
└──────────────┬──────────────────────────────────┬───────────────┘
               │                                  │
               ▼                                  ▼
┌──────────────────────────┐       ┌──────────────────────────────┐
│     HTTP/1.1 Handler     │       │      HTTP/2 Handler          │
│  (existing http_client)  │       │  (new h2_client)             │
└──────────────────────────┘       └──────────────┬───────────────┘
                                                  │
                          ┌───────────────────────┼───────────────────────┐
                          │                       │                       │
                          ▼                       ▼                       ▼
                   ┌────────────┐          ┌────────────┐          ┌────────────┐
                   │  Stream 1  │          │  Stream 3  │          │  Stream 5  │
                   │  Request A │          │  Request B │          │  Request C │
                   └────────────┘          └────────────┘          └────────────┘
```

## Phase 1: Core Frame Layer

### h2_frame.ml - Frame Parsing and Serialization

Implements RFC 9113 Section 4 (HTTP Frames) and Section 6 (Frame Definitions).

```ocaml
(** RFC 9113 Frame Types *)

(** Frame header - 9 octets fixed per RFC 9113 §4.1 *)
type frame_header = {
  length : int;              (** 24-bit payload length *)
  frame_type : frame_type;   (** 8-bit type *)
  flags : flags;             (** 8-bit flags *)
  stream_id : stream_id;     (** 31-bit stream identifier *)
}

(** Frame types per RFC 9113 §6 *)
type frame_type =
  | Data          (** 0x00 - RFC 9113 §6.1 *)
  | Headers       (** 0x01 - RFC 9113 §6.2 *)
  | Priority      (** 0x02 - RFC 9113 §6.3 (deprecated but must parse) *)
  | Rst_stream    (** 0x03 - RFC 9113 §6.4 *)
  | Settings      (** 0x04 - RFC 9113 §6.5 *)
  | Push_promise  (** 0x05 - RFC 9113 §6.6 *)
  | Ping          (** 0x06 - RFC 9113 §6.7 *)
  | Goaway        (** 0x07 - RFC 9113 §6.8 *)
  | Window_update (** 0x08 - RFC 9113 §6.9 *)
  | Continuation  (** 0x09 - RFC 9113 §6.10 *)
  | Unknown of int

(** Frame payload variants *)
type frame_payload =
  | Data_payload of {
      data : Cstruct.t;
      padding : int option;
      end_stream : bool;
    }
  | Headers_payload of {
      header_block : Cstruct.t;
      priority : priority option;
      end_stream : bool;
      end_headers : bool;
    }
  | Settings_payload of setting list
  | Window_update_payload of int32
  | Rst_stream_payload of error_code
  | Ping_payload of Cstruct.t  (** 8 bytes exactly *)
  | Goaway_payload of {
      last_stream_id : stream_id;
      error_code : error_code;
      debug_data : Cstruct.t;
    }
  | Continuation_payload of {
      header_block : Cstruct.t;
      end_headers : bool;
    }
  | Push_promise_payload of {
      promised_stream_id : stream_id;
      header_block : Cstruct.t;
      end_headers : bool;
    }

type frame = {
  header : frame_header;
  payload : frame_payload;
}

(** Eio-native frame reading *)
val read_frame :
  Eio.Buf_read.t ->
  max_frame_size:int ->
  (frame, error_code * string) result

(** Eio-native frame writing *)
val write_frame :
  (Eio.Buf_write.t -> unit) ->
  frame ->
  unit
```

**Key Implementation Notes:**

1. Use `Cstruct` for zero-copy buffer management
2. Frame size validation per SETTINGS_MAX_FRAME_SIZE
3. Reserved bit handling (must ignore on receive, set to 0 on send)
4. Unknown frame type handling (MUST ignore per §5.5)

### h2_error.ml - Error Codes

Implements RFC 9113 Section 7 (Error Codes).

```ocaml
type error_code =
  | No_error              (** 0x00 - Graceful shutdown *)
  | Protocol_error        (** 0x01 - Protocol error detected *)
  | Internal_error        (** 0x02 - Implementation fault *)
  | Flow_control_error    (** 0x03 - Flow control violated *)
  | Settings_timeout      (** 0x04 - Settings not acknowledged *)
  | Stream_closed         (** 0x05 - Frame on closed stream *)
  | Frame_size_error      (** 0x06 - Frame size incorrect *)
  | Refused_stream        (** 0x07 - Stream not processed *)
  | Cancel                (** 0x08 - Stream cancelled *)
  | Compression_error     (** 0x09 - Compression state error *)
  | Connect_error         (** 0x0a - TCP connection error for CONNECT *)
  | Enhance_your_calm     (** 0x0b - Processing capacity exceeded *)
  | Inadequate_security   (** 0x0c - Negotiated TLS insufficient *)
  | Http_1_1_required     (** 0x0d - Use HTTP/1.1 *)
  | Unknown of int32

type error =
  | Connection_error of error_code * string
  | Stream_error of stream_id * error_code
```

## Phase 2: HPACK Header Compression

### h2_hpack.ml - Header Compression

Implements RFC 7541 (HPACK).

```ocaml
(** HPACK encoding context *)
module Encoder : sig
  type t

  val create : capacity:int -> t
  val set_capacity : t -> int -> unit

  (** Encode headers to a buffer *)
  val encode :
    t ->
    Eio.Buf_write.t ->
    (string * string) list ->
    unit
end

(** HPACK decoding context *)
module Decoder : sig
  type t

  val create : capacity:int -> t
  val set_capacity : t -> int -> unit

  (** Decode a header block fragment *)
  val decode :
    t ->
    Cstruct.t ->
    ((string * string) list, error_code * string) result
end

(** Static table (RFC 7541 Appendix A) - 61 entries *)
val static_table : (string * string) array

(** Huffman encoding/decoding (RFC 7541 Appendix B) *)
module Huffman : sig
  val encode : string -> Cstruct.t
  val decode : Cstruct.t -> (string, string) result
end
```

**Key Implementation Notes:**

1. Dynamic table uses FIFO eviction
2. Static table lookup must be O(1) - use hash table
3. Never-indexed literals for sensitive headers (cookies, auth)
4. Integer encoding with variable-length prefix per §5.1

## Phase 3: Stream State Machine

### h2_stream.ml - Stream Management

Implements RFC 9113 Section 5.1 (Stream States).

```ocaml
(** Stream states per RFC 9113 §5.1 Figure 2 *)
type state =
  | Idle
  | Reserved_local
  | Reserved_remote
  | Open
  | Half_closed_local
  | Half_closed_remote
  | Closed of closed_reason

type closed_reason =
  | Finished              (** Normal completion with END_STREAM *)
  | Reset_by_us of error_code
  | Reset_by_peer of error_code

(** Stream identifier - odd for client-initiated, even for server *)
type stream_id = int32

(** A single HTTP/2 stream *)
type t = {
  id : stream_id;
  mutable state : state;
  mutable send_window : int32;
  mutable recv_window : int32;

  (** Request data *)
  request : Request.t option;
  request_body : Body.Writer.t option;

  (** Response handling *)
  mutable response : Response.t option;
  response_body : Eio.Stream.t;  (** Backpressure-aware body stream *)

  (** Completion signaling *)
  promise : (Response.t, error) result Eio.Promise.t;
  resolver : (Response.t, error) result Eio.Promise.u;
}

(** State transition validation *)
val transition : t -> event -> (unit, error_code) result

type event =
  | Send_headers of { end_stream : bool }
  | Recv_headers of { end_stream : bool }
  | Send_data of { end_stream : bool }
  | Recv_data of { end_stream : bool }
  | Send_rst_stream
  | Recv_rst_stream
  | Send_push_promise
  | Recv_push_promise

(** Stream identifier allocation *)
val next_stream_id : t -> stream_id
```

**Key Implementation Notes:**

1. Client streams use odd IDs starting at 1
2. Server-pushed streams use even IDs
3. Stream IDs cannot be reused
4. Maximum concurrent streams governed by SETTINGS

## Phase 4: Flow Control

### h2_flow_control.ml - Flow Control Windows

Implements RFC 9113 Section 5.2 (Flow Control).

```ocaml
(** Flow control window *)
type window = {
  mutable size : int32;
  mutable pending_updates : int32;
}

(** Initial window size: 65535 bytes per RFC 9113 §6.9.2 *)
val default_window_size : int32

(** Maximum window size: 2^31 - 1 *)
val max_window_size : int32

(** Connection-level flow control *)
type connection_flow = {
  send_window : window;
  recv_window : window;
}

(** Stream-level flow control *)
type stream_flow = {
  send_window : window;
  recv_window : window;
}

(** Consume bytes from send window, blocking if insufficient *)
val consume_send :
  connection_flow ->
  stream_flow ->
  int ->
  unit

(** Process received WINDOW_UPDATE *)
val apply_window_update :
  window ->
  int32 ->
  (unit, error_code) result

(** Generate WINDOW_UPDATE when bytes consumed *)
val update_recv_window :
  window ->
  int ->
  int32 option  (** Returns increment to send, if any *)
```

**Key Implementation Notes:**

1. DATA frames are the only flow-controlled frames
2. Both connection and stream level windows must have space
3. WINDOW_UPDATE overflow is FLOW_CONTROL_ERROR
4. Zero window size pauses sending (no busy waiting with Eio)

## Phase 5: Settings Negotiation

### h2_settings.ml - Connection Settings

Implements RFC 9113 Section 6.5 (SETTINGS).

```ocaml
(** Settings parameters per RFC 9113 §6.5.2 *)
type setting =
  | Header_table_size of int          (** 0x01 - Default: 4096 *)
  | Enable_push of bool               (** 0x02 - Default: true *)
  | Max_concurrent_streams of int32   (** 0x03 - Default: unlimited *)
  | Initial_window_size of int32      (** 0x04 - Default: 65535 *)
  | Max_frame_size of int             (** 0x05 - Default: 16384 *)
  | Max_header_list_size of int       (** 0x06 - Default: unlimited *)

type t = {
  header_table_size : int;
  enable_push : bool;
  max_concurrent_streams : int32 option;
  initial_window_size : int32;
  max_frame_size : int;
  max_header_list_size : int option;
}

val default : t

(** Validate setting values per RFC 9113 §6.5.2 *)
val validate : setting -> (unit, error_code * string) result

(** Apply settings to connection state *)
val apply : t -> setting list -> t
```

**Key Implementation Notes:**

1. SETTINGS must be acknowledged within reasonable time
2. Initial SETTINGS in connection preface cannot be empty
3. ENABLE_PUSH=0 from client means server MUST NOT push
4. MAX_FRAME_SIZE range: 16384 to 16777215

## Phase 6: Connection Management

### h2_connection.ml - Multiplexed Connection

```ocaml
(** HTTP/2 connection state *)
type t = {
  flow : Eio.Flow.two_way_ty Eio.Resource.t;
  hpack_encoder : Hpack.Encoder.t;
  hpack_decoder : Hpack.Decoder.t;
  mutable local_settings : Settings.t;
  mutable remote_settings : Settings.t;
  connection_flow : Flow_control.connection_flow;
  streams : (stream_id, Stream.t) Hashtbl.t;
  mutable next_stream_id : stream_id;
  mutable goaway_received : bool;
  mutable goaway_sent : bool;

  (** Eio synchronization *)
  write_mutex : Eio.Mutex.t;
  pending_writes : Frame.frame Eio.Stream.t;
}

(** Connection preface - client sends magic + SETTINGS *)
val connection_preface : string
(** "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" *)

(** Establish HTTP/2 connection *)
val create :
  sw:Eio.Switch.t ->
  flow:Eio.Flow.two_way_ty Eio.Resource.t ->
  settings:Settings.t ->
  t

(** Run the connection (spawns reader/writer fibers) *)
val run : t -> unit

(** Initiate a new stream for a request *)
val open_stream :
  t ->
  Request.t ->
  (Stream.t, error) result

(** Graceful shutdown *)
val shutdown : t -> error_code -> unit
```

### Connection Lifecycle

```
Client                                          Server
  |                                               |
  |-- Connection Preface (magic string) -------->|
  |-- SETTINGS frame -------------------------->|
  |                                               |
  |<-- SETTINGS frame --------------------------|
  |-- SETTINGS ACK ---------------------------->|
  |<-- SETTINGS ACK ----------------------------|
  |                                               |
  |== Connection Established ===================|
  |                                               |
  |-- HEADERS (stream 1) ---------------------->|
  |<-- HEADERS (stream 1) ----------------------|
  |-- DATA (stream 1, END_STREAM) ------------->|
  |<-- DATA (stream 1, END_STREAM) -------------|
  |                                               |
  |-- GOAWAY ---------------------------------->|
  |<-- GOAWAY ---------------------------------|
  |                                               |
```

## Phase 7: Client Implementation

### h2_client.ml - HTTP/2 Client

This module uses shared types throughout, ensuring HTTP/2 responses are
indistinguishable from HTTP/1.1 responses at the API level.

```ocaml
(** Make an HTTP/2 request over an existing connection.
    Uses shared types: Method.t, Uri.t, Headers.t, Body.t → Response.t *)
val request :
  sw:Eio.Switch.t ->
  connection:H2_connection.t ->
  meth:Method.t ->              (* Shared: method.ml *)
  uri:Uri.t ->                  (* Shared: uri.ml *)
  headers:Headers.t ->          (* Shared: headers.ml *)
  body:Body.t ->                (* Shared: body.ml *)
  Response.t Eio.Promise.t      (* Shared: response.ml *)

(** HTTP/2 pseudo-headers for requests per RFC 9113 §8.3.1
    These are extracted from the shared types, not exposed to users *)
type request_pseudo_headers = {
  method_ : string;        (** :method - from Method.to_string *)
  scheme : string;         (** :scheme - from Uri.scheme *)
  authority : string;      (** :authority - from Uri.host_with_port *)
  path : string;           (** :path - from Uri.path_and_query *)
}

(** Extract pseudo-headers from shared request types *)
val pseudo_headers_of_request :
  meth:Method.t ->
  uri:Uri.t ->
  request_pseudo_headers

(** Encode request headers for HEADERS frame.
    Combines pseudo-headers with regular Headers.t *)
val encode_request_headers :
  Hpack.Encoder.t ->
  pseudo:request_pseudo_headers ->
  headers:Headers.t ->           (* Shared: headers.ml *)
  Cstruct.t

(** Build Response.t from HTTP/2 response headers and body stream.
    This is where HTTP/2 frames are converted to the shared Response type. *)
val response_of_h2 :
  sw:Eio.Switch.t ->
  status:int ->                         (* From :status pseudo-header *)
  headers:Headers.t ->                  (* Shared: headers.ml, pseudo-headers stripped *)
  body:Eio.Flow.source_ty Eio.Resource.t ->  (* DATA frames as Eio flow *)
  url:string ->                         (* Original request URL *)
  elapsed:float ->                      (* Request timing *)
  Response.t                            (* Shared: response.ml *)
(** The returned Response.t is identical in structure to HTTP/1.1 responses.
    Users cannot distinguish between protocols without explicit inspection. *)

(** Handle PUSH_PROMISE - RFC 9113 §8.4
    Server push is disabled by default for clients (SETTINGS_ENABLE_PUSH=0) *)
val handle_push_promise :
  connection:H2_connection.t ->
  promised_stream_id:H2_stream.stream_id ->
  headers:Headers.t ->
  unit
```

### Response Construction Flow

```
HTTP/2 HEADERS Frame                      HTTP/1.1 Status Line + Headers
       │                                           │
       ▼                                           ▼
┌──────────────────┐                     ┌──────────────────┐
│ Decode HPACK     │                     │ Parse headers    │
│ Extract :status  │                     │ Parse status     │
│ Strip pseudos    │                     │                  │
└────────┬─────────┘                     └────────┬─────────┘
         │                                        │
         │    ┌────────────────────────┐          │
         └───►│  Response.Private.make │◄─────────┘
              │  ~status ~headers      │
              │  ~body ~url ~elapsed   │
              └───────────┬────────────┘
                          │
                          ▼
                  ┌───────────────┐
                  │  Response.t   │  ◄── Same type for both protocols
                  │  (shared)     │
                  └───────────────┘
```

## Phase 8: Protocol Selection (ALPN)

### http_version.ml - Protocol Detection

```ocaml
(** Supported HTTP versions *)
type version =
  | Http_1_0
  | Http_1_1
  | Http_2

(** Pretty printer *)
val pp : Format.formatter -> version -> unit

(** String conversion *)
val to_string : version -> string

(** ALPN protocol identifiers per RFC 9113 §3.1 *)
val alpn_h2 : string        (** "h2" - HTTP/2 over TLS *)
val alpn_http_1_1 : string  (** "http/1.1" *)

(** Build ALPN list for TLS configuration *)
val alpn_protocols : preferred:version list -> string list
(** Returns ALPN identifiers in preference order.
    Example: [alpn_protocols ~preferred:[Http_2; Http_1_1]] returns ["h2"; "http/1.1"] *)
```

### protocol.ml - Protocol Negotiation

```ocaml
(** Protocol negotiation and connection establishment *)

(** Preferred protocol configuration *)
type preference =
  | Prefer_h2      (** Try HTTP/2 first, fall back to HTTP/1.1 *)
  | Http2_only     (** HTTP/2 only, fail if not supported *)
  | Http1_only     (** HTTP/1.1 only, no ALPN negotiation *)
  | Auto           (** Use ALPN result, default to HTTP/1.1 *)

(** Establish connection with protocol negotiation *)
val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  tls_config:Tls.Config.client option ->
  preference:preference ->
  host:string ->
  port:int ->
  Connection.t
(** Performs:
    1. TCP connection
    2. TLS handshake with ALPN (if HTTPS)
    3. Protocol detection from ALPN result
    4. HTTP/2 connection preface (if HTTP/2)
    5. Returns unified Connection.t *)

(** Detect protocol from established TLS connection *)
val detect_from_alpn : Tls_eio.t -> Http_version.version option

(** Check if server supports HTTP/2 (for connection reuse decisions) *)
val supports_h2 : Connection.t -> bool
```

## Phase 9: Unified Connection Abstraction

### connection.ml - Protocol-Agnostic Connection

This module provides a unified interface that works with both HTTP/1.1 and HTTP/2,
using the shared types from the requests library.

```ocaml
(** HTTP protocol version *)
type version =
  | Http_1_1
  | Http_2

(** A connection that can be HTTP/1.1 or HTTP/2 *)
type t =
  | Http1 of {
      flow : Eio.Flow.two_way_ty Eio.Resource.t;
      version : [ `Http_1_0 | `Http_1_1 ];
    }
  | Http2 of {
      connection : H2_connection.t;
    }

(** Connection pool key - for HTTP/2, one connection handles all streams *)
type pool_key = {
  host : string;
  port : int;
  scheme : [ `Http | `Https ];
}

(** Request using shared types - protocol handled internally *)
type request = {
  meth : Method.t;              (** Shared: method.ml *)
  uri : Uri.t;                  (** Shared: uri.ml *)
  headers : Headers.t;          (** Shared: headers.ml *)
  body : Body.t;                (** Shared: body.ml *)
}

(** Execute a request on the appropriate protocol.
    Returns Response.t (shared type) regardless of underlying protocol. *)
val execute :
  t ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  request:request ->
  auto_decompress:bool ->
  Response.t
(** The response uses shared types:
    - [Response.status] returns [Status.t] (shared)
    - [Response.headers] returns [Headers.t] (shared)
    - [Response.body] returns [Eio.Flow.source] (protocol-agnostic stream) *)

(** Check if connection can accept more streams.
    - HTTP/1.1: Always true (one request at a time, pipelining not supported)
    - HTTP/2: True if under MAX_CONCURRENT_STREAMS limit *)
val can_accept_stream : t -> bool

(** Get the negotiated protocol version *)
val version : t -> version

(** HTTP/2-specific: number of active streams *)
val active_streams : t -> int
```

### Type Flow Diagram

```
                    ┌─────────────────────────────────────────┐
                    │           User Code                      │
                    │   Requests.get session url               │
                    └───────────────────┬─────────────────────┘
                                        │
                    ┌───────────────────▼─────────────────────┐
                    │         Shared Request Types             │
                    │  Method.t, Uri.t, Headers.t, Body.t     │
                    └───────────────────┬─────────────────────┘
                                        │
              ┌─────────────────────────┼─────────────────────────┐
              │                         │                         │
              ▼                         ▼                         ▼
    ┌─────────────────┐      ┌─────────────────┐      ┌─────────────────┐
    │   HTTP/1.1      │      │   Connection    │      │    HTTP/2       │
    │   http_write    │◄─────│   Abstraction   │─────►│   h2_client     │
    │   http_read     │      │   connection.ml │      │   h2_frame      │
    └────────┬────────┘      └─────────────────┘      └────────┬────────┘
             │                                                  │
             │         ┌─────────────────────────┐              │
             └────────►│   Shared Response Type  │◄─────────────┘
                       │      Response.t         │
                       │  Status.t, Headers.t    │
                       │  Eio.Flow.source (body) │
                       └─────────────────────────┘
```

## Phase 10: Updated Requests API

### Changes to requests.ml

The main API remains unchanged from the user's perspective. Internal changes:

1. **Connection Pool Changes**: HTTP/2 connections are multiplexed, so pooling strategy differs
2. **ALPN Negotiation**: Automatic protocol selection for HTTPS
3. **Stream Multiplexing**: Multiple concurrent requests on single connection

```ocaml
(** Internal: choose protocol and execute request *)
let make_request_internal t ~method_ ~uri ~headers ~body =
  let scheme = Uri.scheme uri in
  let pool = match scheme with
    | "http" -> t.http_pool
    | "https" -> t.https_pool
    | _ -> raise (Invalid_argument "unsupported scheme")
  in

  Conpool.with_connection pool (Uri.host_with_port uri) @@ fun conn ->
  match conn with
  | Connection.Http1 _ ->
      (* Existing HTTP/1.1 code path *)
      Http_client.make_request ...
  | Connection.Http2 { connection } ->
      (* New HTTP/2 code path *)
      let stream = H2_connection.open_stream connection request in
      Eio.Promise.await stream.promise
```

## Implementation Phases

### Phase 0: Shared Type Extensions (Week 1) - COMPLETED
- [x] Extend `headers.ml` with pseudo-header support for HTTP/2
- [x] Extend `error.ml` with HTTP/2 error variants
- [x] Add `http_version.ml` - version type and ALPN identifiers
- [x] Unit tests for header pseudo-header validation (28 tests passing)

### Phase 1: Frame Layer (Week 1-2) - COMPLETED
- [x] `h2/h2_frame.ml` - Frame types, parsing, serialization
- [x] Unit tests for all frame types (37 tests passing)
- [ ] Fuzz tests for frame parsing robustness

### Phase 2: HPACK (Week 2-3) - BSD-3-Clause files
- [ ] `h2/h2_hpack_static.ml` - Static table (RFC 7541 Appendix A)
- [ ] `h2/h2_huffman.ml` - Huffman encode/decode (RFC 7541 Appendix B)
- [ ] `h2/h2_hpack.ml` - Encoder and Decoder
  - [ ] Integer encoding with prefix
  - [ ] String literal encoding
  - [ ] Dynamic table with eviction
  - [ ] Indexed vs literal header representations
- [ ] Unit tests with RFC 7541 Appendix C examples

### Phase 3: Stream State Machine (Week 3-4) - BSD-3-Clause
- [ ] `h2/h2_stream.ml` - Stream state machine
  - [ ] State transitions per RFC 9113 §5.1
  - [ ] Stream ID allocation (odd for client)
  - [ ] Stream lifecycle (idle → open → half-closed → closed)
- [ ] `h2/h2_flow_control.ml` - Window management
  - [ ] Connection and stream level windows
  - [ ] WINDOW_UPDATE generation
  - [ ] Backpressure via Eio.Stream
- [ ] Unit tests for state transitions and flow control

### Phase 4: Connection Management (Week 4-5)
- [ ] `h2/h2_settings.ml` - Settings frame handling
- [ ] `h2/h2_connection.ml` - Connection lifecycle
  - [ ] Connection preface exchange
  - [ ] Settings negotiation
  - [ ] GOAWAY handling
  - [ ] PING/PONG
  - [ ] Reader/writer fiber management
- [ ] `h2/h2_client.ml` - Client request handling
  - [ ] Request → HEADERS frame conversion
  - [ ] Response handling with shared Response.t
  - [ ] Trailer support
- [ ] Integration tests with mock HTTP/2 server

### Phase 5: Protocol Abstraction (Week 5-6)
- [ ] `protocol.ml` - Protocol negotiation
- [ ] `connection.ml` - Unified connection type
- [ ] Update `requests.ml`:
  - [ ] ALPN preference configuration
  - [ ] Protocol-transparent request execution
  - [ ] Connection pool adaptation for HTTP/2 multiplexing
- [ ] Update `one.ml` for HTTP/2 support
- [ ] Integration tests with real HTTP/2 servers

### Phase 6: Testing & Documentation (Week 6-7)
- [ ] h2spec compliance testing (all test groups)
- [ ] Interoperability testing (nginx, Cloudflare, Google)
- [ ] Performance benchmarks vs HTTP/1.1
- [ ] Update module documentation with RFC references
- [ ] Usage examples in documentation
- [ ] CHANGELOG entry

## Key Differences from ocaml-h2

| Aspect | ocaml-h2 | Our Implementation |
|--------|----------|-------------------|
| I/O Model | Callback-based, runtime-agnostic | Native Eio with structured concurrency |
| Parsing | Angstrom | Direct Eio.Buf_read parsing |
| Serialization | Faraday | Eio.Buf_write |
| Concurrency | External via Lwt/Async/Eio adapters | Built-in Eio fibers and promises |
| Flow Control | Manual callbacks | Eio.Stream backpressure |
| Error Handling | Result types + callbacks | Eio exceptions + Result |
| Types | Standalone Request/Response types | Shared with HTTP/1.1 (Method, Status, Headers, Body, Response) |
| Integration | Separate library | Built into requests library |

### Shared Type Benefits

1. **Single API Surface**: Users work with the same types regardless of protocol
2. **No Conversion Overhead**: Response from HTTP/2 uses same `Response.t` as HTTP/1.1
3. **Consistent Error Handling**: Extended `Error.error` type covers both protocols
4. **Unified Headers**: `Headers.t` works for both, with H2 pseudo-header extensions
5. **Protocol Transparency**: `Requests.get` works the same for HTTP/1.1 and HTTP/2

## OCamldoc Templates

### Module-level reference:
```ocaml
(** RFC 9113 HTTP/2 Frame handling.

    This module implements HTTP/2 frame parsing and serialization as specified in
    {{:https://datatracker.ietf.org/doc/html/rfc9113#section-4}RFC 9113 Section 4}
    and {{:https://datatracker.ietf.org/doc/html/rfc9113#section-6}Section 6}. *)
```

### Section-specific reference:
```ocaml
(** Stream state machine per
    {{:https://datatracker.ietf.org/doc/html/rfc9113#section-5.1}RFC 9113 Section 5.1}. *)
```

### HPACK reference:
```ocaml
(** HPACK header compression per
    {{:https://datatracker.ietf.org/doc/html/rfc7541}RFC 7541}. *)
```

## Testing Strategy

### Unit Tests
- Each module has comprehensive unit tests
- Use RFC examples as test vectors (especially HPACK Appendix C)
- Property-based tests for frame parsing/serialization roundtrips

### Integration Tests
- Mock HTTP/2 server for controlled testing
- Full request/response cycles
- Error condition handling (RST_STREAM, GOAWAY)
- Flow control behavior under load

### h2spec Compliance
Run the [h2spec](https://github.com/summerwind/h2spec) test suite:
```bash
h2spec -h localhost -p 8443 --tls --insecure
```
All test groups must pass:
- Generic tests
- HPACK tests
- HTTP/2 tests (all sections)

### Interoperability Testing
Test against major HTTP/2 servers:
- nginx (most common)
- Cloudflare (strict implementation)
- Google (reference implementation)
- Apache (with mod_http2)

### Shared Type Verification
Verify that responses from HTTP/2 are indistinguishable from HTTP/1.1:
```ocaml
(* This test should pass regardless of protocol *)
let test_shared_response protocol =
  let response = match protocol with
    | `Http1 -> make_http1_request ()
    | `Http2 -> make_http2_request ()
  in
  (* All these use shared types *)
  assert (Response.status response = `OK);
  assert (Headers.get `Content_type (Response.headers response) = Some "application/json");
  let body = Response.text response in
  assert (String.length body > 0)
```

### Fuzz Testing
Property-based testing for:
- Frame parsing (malformed frames should not crash)
- HPACK decoding (compression bombs, invalid sequences)
- Header validation (injection attacks)

## Dependencies

- `eio` - Structured concurrency runtime (existing)
- `cstruct` - Zero-copy buffer management (may already exist)
- `tls-eio` - TLS with ALPN support (existing)
- No new external dependencies for core HTTP/2

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| HPACK complexity | Follow RFC 7541 examples exactly, extensive testing |
| Flow control deadlocks | Use Eio.Stream for backpressure, careful window management |
| Connection multiplexing bugs | State machine validation, property-based testing |
| Performance regression | Benchmark early and often, profile critical paths |

## References

1. [RFC 9113 - HTTP/2](https://datatracker.ietf.org/doc/html/rfc9113)
2. [RFC 7541 - HPACK](https://datatracker.ietf.org/doc/html/rfc7541)
3. [RFC 9110 - HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110)
4. [ocaml-h2 Reference Implementation](https://github.com/anmonteiro/ocaml-h2)
5. [h2spec Test Suite](https://github.com/summerwind/h2spec)

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** WebSocket Protocol Support (RFC 6455)

    This module provides functions for the WebSocket HTTP upgrade handshake.
    WebSocket connections are established by upgrading an HTTP/1.1 connection
    using the Upgrade mechanism.

    {2 Basic Usage}

    {[
      (* Client: initiate WebSocket upgrade *)
      let key = Websocket.generate_key () in
      let headers = Websocket.make_upgrade_headers ~key () in
      (* ... send request with these headers ... *)

      (* Client: validate server response *)
      match Websocket.validate_upgrade_response ~key ~status ~headers with
      | Ok () -> (* Connection upgraded successfully *)
      | Error reason -> (* Handshake failed *)
    ]}

    @see <https://www.rfc-editor.org/rfc/rfc6455> RFC 6455: The WebSocket Protocol *)

(** {1 Constants} *)

val protocol_version : string
(** The WebSocket protocol version per RFC 6455.
    This is always ["13"]. *)

val magic_guid : string
(** The magic GUID used in Sec-WebSocket-Accept computation.
    @see <https://www.rfc-editor.org/rfc/rfc6455#section-1.3> RFC 6455 Section 1.3 *)

(** {1 Sec-WebSocket-Key}

    @see <https://www.rfc-editor.org/rfc/rfc6455#section-4.1> RFC 6455 Section 4.1 *)

val generate_key : unit -> string
(** [generate_key ()] creates a random Sec-WebSocket-Key value.

    Generates a cryptographically random 16-byte nonce and base64-encodes it.
    The result is suitable for use in the Sec-WebSocket-Key header. *)

(** {1 Sec-WebSocket-Accept}

    @see <https://www.rfc-editor.org/rfc/rfc6455#section-4.2.2> RFC 6455 Section 4.2.2 *)

val compute_accept : key:string -> string
(** [compute_accept ~key] computes the expected Sec-WebSocket-Accept value.

    The computation is: [base64(SHA-1(key + magic_guid))]

    @param key The Sec-WebSocket-Key sent by the client
    @return The expected Sec-WebSocket-Accept value *)

val validate_accept : key:string -> accept:string -> bool
(** [validate_accept ~key ~accept] validates a server's Sec-WebSocket-Accept.

    @param key The Sec-WebSocket-Key that was sent
    @param accept The Sec-WebSocket-Accept received from the server
    @return [true] if the accept value is correct *)

(** {1 Sec-WebSocket-Protocol}

    @see <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.4> RFC 6455 Section 11.3.4 *)

val parse_protocols : string -> string list
(** [parse_protocols s] parses a Sec-WebSocket-Protocol header value.

    Example: ["graphql-ws, graphql-transport-ws"] -> [["graphql-ws"; "graphql-transport-ws"]] *)

val protocols_to_string : string list -> string
(** [protocols_to_string protocols] formats protocols as a header value. *)

val select_protocol : offered:string list -> supported:string list -> string option
(** [select_protocol ~offered ~supported] selects a mutually acceptable protocol.

    @param offered The protocols offered by the client
    @param supported The protocols we support (in preference order)
    @return The selected protocol, or [None] if no match *)

(** {1 Sec-WebSocket-Extensions}

    @see <https://www.rfc-editor.org/rfc/rfc6455#section-9> RFC 6455 Section 9
    @see <https://www.rfc-editor.org/rfc/rfc7692> RFC 7692: Compression Extensions *)

(** An extension with optional parameters.

    Example: [permessage-deflate; client_max_window_bits] *)
type extension = {
  name : string;
  params : (string * string option) list;
}

val parse_extensions : string -> extension list
(** [parse_extensions s] parses a Sec-WebSocket-Extensions header value.

    Example: ["permessage-deflate; client_max_window_bits"] *)

val extensions_to_string : extension list -> string
(** [extensions_to_string extensions] formats extensions as a header value. *)

val has_extension : name:string -> extension list -> bool
(** [has_extension ~name extensions] checks if an extension is present. *)

val get_extension_params : name:string -> extension list -> (string * string option) list option
(** [get_extension_params ~name extensions] gets parameters for an extension. *)

(** {1 Handshake Helpers} *)

val make_upgrade_headers :
  key:string ->
  ?protocols:string list ->
  ?extensions:extension list ->
  ?origin:string ->
  unit ->
  Headers.t
(** [make_upgrade_headers ~key ?protocols ?extensions ?origin ()] builds
    headers for a WebSocket upgrade request.

    Sets the following headers:
    - [Upgrade: websocket]
    - [Connection: Upgrade]
    - [Sec-WebSocket-Key: {key}]
    - [Sec-WebSocket-Version: 13]
    - [Sec-WebSocket-Protocol: ...] (if protocols provided)
    - [Sec-WebSocket-Extensions: ...] (if extensions provided)
    - [Origin: ...] (if origin provided)

    @param key The Sec-WebSocket-Key (use {!generate_key} to create)
    @param protocols Optional list of subprotocols to request
    @param extensions Optional list of extensions to request
    @param origin Optional Origin header value *)

val validate_upgrade_response :
  key:string ->
  status:int ->
  headers:Headers.t ->
  (unit, string) result
(** [validate_upgrade_response ~key ~status ~headers] validates a WebSocket
    upgrade response.

    Checks that:
    - Status code is 101 (Switching Protocols)
    - Upgrade header is "websocket"
    - Connection header includes "Upgrade"
    - Sec-WebSocket-Accept is correct for the given key

    @param key The Sec-WebSocket-Key that was sent
    @param status The HTTP status code
    @param headers The response headers
    @return [Ok ()] if valid, [Error reason] if invalid *)

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** WebSocket Protocol Support (RFC 6455)

    This module provides functions for the WebSocket HTTP upgrade handshake.
    WebSocket connections are established by upgrading an HTTP/1.1 connection
    using the Upgrade mechanism.

    @see <https://www.rfc-editor.org/rfc/rfc6455> RFC 6455: The WebSocket Protocol *)

let src = Logs.Src.create "requests.websocket" ~doc:"WebSocket Support"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Constants} *)

(** The WebSocket protocol version per RFC 6455.
    This is the only version defined by the RFC. *)
let protocol_version = "13"

(** The magic GUID used in Sec-WebSocket-Accept computation.
    @see <https://www.rfc-editor.org/rfc/rfc6455#section-1.3> RFC 6455 Section 1.3 *)
let magic_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

(** {1 Sec-WebSocket-Key Generation}

    The client generates a random 16-byte value, base64-encodes it, and sends
    it in the Sec-WebSocket-Key header. This proves the server understands
    the WebSocket protocol.

    @see <https://www.rfc-editor.org/rfc/rfc6455#section-4.1> RFC 6455 Section 4.1 *)

(** Generate a random Sec-WebSocket-Key value.

    Creates a cryptographically random 16-byte nonce and base64-encodes it.
    The result is suitable for use in the Sec-WebSocket-Key header. *)
let generate_key () =
  let random_bytes = Mirage_crypto_rng.generate 16 in
  let key = Base64.encode_exn random_bytes in
  Log.debug (fun m -> m "Generated WebSocket key: %s" key);
  key

(** {1 Sec-WebSocket-Accept Computation}

    The server computes Sec-WebSocket-Accept as:
    [base64(SHA-1(Sec-WebSocket-Key + magic_guid))]

    This proves the server received the client's handshake and understands
    the WebSocket protocol.

    @see <https://www.rfc-editor.org/rfc/rfc6455#section-4.2.2> RFC 6455 Section 4.2.2 *)

(** Compute the expected Sec-WebSocket-Accept value for a given key.

    @param key The Sec-WebSocket-Key sent by the client
    @return The expected Sec-WebSocket-Accept value *)
let compute_accept ~key =
  let combined = key ^ magic_guid in
  let hash = Digestif.SHA1.(digest_string combined |> to_raw_string) in
  let accept = Base64.encode_exn hash in
  Log.debug (fun m -> m "Computed WebSocket accept for key %s: %s" key accept);
  accept

(** Validate a server's Sec-WebSocket-Accept value.

    @param key The Sec-WebSocket-Key that was sent
    @param accept The Sec-WebSocket-Accept received from the server
    @return [true] if the accept value is correct *)
let validate_accept ~key ~accept =
  let expected = compute_accept ~key in
  let valid = String.equal expected accept in
  if not valid then
    Log.warn (fun m -> m "WebSocket accept validation failed: expected %s, got %s"
      expected accept);
  valid

(** {1 Sec-WebSocket-Protocol Negotiation}

    The client sends a list of desired subprotocols; the server selects one.
    Common subprotocols include "graphql-ws", "graphql-transport-ws", "wamp.2.json".

    @see <https://www.rfc-editor.org/rfc/rfc6455#section-11.3.4> RFC 6455 Section 11.3.4 *)

(** Parse a Sec-WebSocket-Protocol header value into a list of protocols.

    The header value is a comma-separated list of protocol identifiers. *)
let parse_protocols s =
  String.split_on_char ',' s
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)

(** Format a list of protocols as a Sec-WebSocket-Protocol header value. *)
let protocols_to_string protocols =
  String.concat ", " protocols

(** Select a protocol from the offered list that matches one we support.

    @param offered The protocols offered by the client
    @param supported The protocols we support (in preference order)
    @return The selected protocol, or [None] if no match *)
let select_protocol ~offered ~supported =
  List.find_opt (fun s -> List.mem s offered) supported

(** {1 Sec-WebSocket-Extensions Parsing}

    Extensions provide additional capabilities like compression.
    The most common extension is "permessage-deflate" (RFC 7692).

    @see <https://www.rfc-editor.org/rfc/rfc6455#section-9> RFC 6455 Section 9
    @see <https://www.rfc-editor.org/rfc/rfc7692> RFC 7692: Compression Extensions *)

(** An extension with optional parameters.

    Example: [("permessage-deflate", [("client_max_window_bits", None)])] *)
type extension = {
  name : string;
  params : (string * string option) list;
}

(** Parse a single extension (name with optional parameters).

    Format: [name; param1; param2=value; ...] *)
let parse_single_extension s =
  let parts = String.split_on_char ';' s |> List.map String.trim in
  match parts with
  | [] -> None
  | name :: params ->
      let parse_param p =
        match String.index_opt p '=' with
        | None -> (String.trim p, None)
        | Some eq_idx ->
            let key = String.trim (String.sub p 0 eq_idx) in
            let value = String.trim (String.sub p (eq_idx + 1) (String.length p - eq_idx - 1)) in
            (* Remove quotes if present *)
            let value = if String.length value >= 2 && value.[0] = '"' then
              String.sub value 1 (String.length value - 2)
            else value
            in
            (key, Some value)
      in
      Some {
        name = String.trim name;
        params = List.map parse_param params;
      }

(** Parse a Sec-WebSocket-Extensions header value.

    Format is comma-separated extensions, each with semicolon-separated parameters:
    [permessage-deflate; client_max_window_bits, another-ext] *)
let parse_extensions s =
  (* Split on commas, but be careful of quoted values *)
  let extensions = String.split_on_char ',' s in
  List.filter_map parse_single_extension extensions

(** Format extensions as a Sec-WebSocket-Extensions header value. *)
let extensions_to_string extensions =
  let ext_to_string ext =
    let params_str = List.map (fun (k, v) ->
      match v with
      | None -> k
      | Some v -> Printf.sprintf "%s=%s" k v
    ) ext.params in
    String.concat "; " (ext.name :: params_str)
  in
  String.concat ", " (List.map ext_to_string extensions)

(** Check if an extension is present in a list. *)
let has_extension ~name extensions =
  List.exists (fun ext -> String.equal ext.name name) extensions

(** Get parameters for a specific extension. *)
let get_extension_params ~name extensions =
  match List.find_opt (fun ext -> String.equal ext.name name) extensions with
  | Some ext -> Some ext.params
  | None -> None

(** {1 Handshake Header Helpers} *)

(** Build the headers for a WebSocket upgrade request.

    @param key The Sec-WebSocket-Key (use {!generate_key} to create)
    @param protocols Optional list of subprotocols to request
    @param extensions Optional list of extensions to request
    @param origin Optional Origin header value *)
let make_upgrade_headers ~key ?protocols ?extensions ?origin () =
  let headers = Headers.empty
    |> Headers.set `Upgrade "websocket"
    |> Headers.set `Connection "Upgrade"
    |> Headers.set `Sec_websocket_key key
    |> Headers.set `Sec_websocket_version protocol_version
  in
  let headers = match protocols with
    | Some ps when ps <> [] ->
        Headers.set `Sec_websocket_protocol (protocols_to_string ps) headers
    | _ -> headers
  in
  let headers = match extensions with
    | Some exts when exts <> [] ->
        Headers.set `Sec_websocket_extensions (extensions_to_string exts) headers
    | _ -> headers
  in
  let headers = match origin with
    | Some o -> Headers.set `Origin o headers
    | None -> headers
  in
  headers

(** Helper to check if a string contains a substring. *)
let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let rec check i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else check (i + 1)
    in
    check 0

(** Validate a WebSocket upgrade response.

    Checks that:
    - Status code is 101 (Switching Protocols)
    - Upgrade header is "websocket"
    - Connection header includes "Upgrade"
    - Sec-WebSocket-Accept is correct for the given key

    @param key The Sec-WebSocket-Key that was sent
    @param status The HTTP status code
    @param headers The response headers
    @return [Ok ()] if valid, [Error reason] if invalid *)
let validate_upgrade_response ~key ~status ~headers =
  (* Check status code *)
  if status <> 101 then
    Error (Printf.sprintf "Expected status 101, got %d" status)
  (* Check Upgrade header *)
  else match Headers.get `Upgrade headers with
  | None -> Error "Missing Upgrade header"
  | Some upgrade when String.lowercase_ascii upgrade <> "websocket" ->
      Error (Printf.sprintf "Upgrade header is '%s', expected 'websocket'" upgrade)
  | Some _ ->
      (* Check Connection header *)
      match Headers.get `Connection headers with
      | None -> Error "Missing Connection header"
      | Some conn ->
          let conn_lower = String.lowercase_ascii conn in
          if not (string_contains ~needle:"upgrade" conn_lower) then
            Error (Printf.sprintf "Connection header is '%s', expected 'Upgrade'" conn)
          else
            (* Check Sec-WebSocket-Accept *)
            match Headers.get `Sec_websocket_accept headers with
            | None -> Error "Missing Sec-WebSocket-Accept header"
            | Some accept ->
                if validate_accept ~key ~accept then
                  Ok ()
                else
                  Error "Sec-WebSocket-Accept validation failed"

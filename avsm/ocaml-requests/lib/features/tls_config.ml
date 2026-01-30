(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** TLS configuration utilities

    This module provides shared TLS configuration creation to ensure consistent
    behavior across session-based and one-shot request modes.

    Supports ALPN (Application-Layer Protocol Negotiation) for HTTP/2 upgrade
    per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3.3}RFC 9113 Section 3.3}. *)

let src = Logs.Src.create "requests.tls_config" ~doc:"TLS Configuration"
module Log = (val Logs.src_log src : Logs.LOG)

(** {1 TLS Version Types} *)

(** Minimum TLS version configuration.
    Per Recommendation #6: Allow enforcing minimum TLS version. *)
type tls_version =
  | TLS_1_2  (** TLS 1.2 minimum (default, widely compatible) *)
  | TLS_1_3  (** TLS 1.3 minimum (most secure, may not work with older servers) *)

let tls_version_to_tls = function
  | TLS_1_2 -> `TLS_1_2
  | TLS_1_3 -> `TLS_1_3

(** {1 ALPN Protocol Negotiation}

    Per {{:https://datatracker.ietf.org/doc/html/rfc9113#section-3.3}RFC 9113 Section 3.3},
    HTTP/2 connections over TLS use ALPN to negotiate the protocol. *)

(** ALPN protocol identifiers. *)
let alpn_h2 = "h2"
let alpn_http11 = "http/1.1"

(** HTTP protocol mode for ALPN negotiation. *)
type protocol_mode =
  | Auto        (** Prefer HTTP/2 if available, fall back to HTTP/1.1 *)
  | Http1_only  (** Use HTTP/1.1 only *)
  | Http2_only  (** Require HTTP/2 *)

(** Get ALPN protocols for the given mode. *)
let alpn_protocols = function
  | Auto -> [alpn_h2; alpn_http11]
  | Http1_only -> [alpn_http11]
  | Http2_only -> [alpn_h2]

(** {1 Configuration Creation} *)

(** Create a TLS client configuration.

    @param verify_tls If true, use system CA certificates for verification
    @param min_tls_version Minimum TLS version to accept (default TLS_1_2)
    @param protocol_mode HTTP protocol mode for ALPN (default Auto)
    @param host Hostname for error messages
    @return TLS client configuration
    @raise Error.Tls_handshake_failed if configuration cannot be created *)
let create_client ?(verify_tls = true) ?(min_tls_version = TLS_1_2) ?(protocol_mode = Auto) ~host () =
  let min_version = tls_version_to_tls min_tls_version in
  let alpn = alpn_protocols protocol_mode in
  Log.debug (fun m -> m "Creating TLS config with ALPN protocols: [%s]"
    (String.concat "; " alpn));
  match verify_tls with
  | true ->
      (* Use CA certificates for verification with minimum TLS version *)
      let authenticator = match Ca_certs.authenticator () with
        | Ok auth -> auth
        | Error (`Msg msg) ->
            Log.err (fun m -> m "Failed to load CA certificates: %s" msg);
            raise (Error.err (Error.Tls_handshake_failed {
              host;
              reason = "CA certificates error: " ^ msg
            }))
      in
      (match Tls.Config.client ~authenticator ~version:(min_version, `TLS_1_3)
               ~alpn_protocols:alpn () with
       | Ok cfg -> cfg
       | Error (`Msg msg) ->
           Log.err (fun m -> m "Failed to create TLS config: %s" msg);
           raise (Error.err (Error.Tls_handshake_failed {
             host;
             reason = "TLS config error: " ^ msg
           })))
  | false ->
      (* No verification but still enforce minimum TLS version *)
      match Tls.Config.client
        ~authenticator:(fun ?ip:_ ~host:_ _ -> Ok None)
        ~version:(min_version, `TLS_1_3)
        ~alpn_protocols:alpn
        () with
      | Ok cfg -> cfg
      | Error (`Msg msg) ->
          Log.err (fun m -> m "Failed to create TLS config: %s" msg);
          raise (Error.err (Error.Tls_handshake_failed {
            host;
            reason = "TLS config error: " ^ msg
          }))

(** Create a TLS client configuration, returning an option.

    @param verify_tls If true, use system CA certificates for verification
    @param min_tls_version Minimum TLS version to accept (default TLS_1_2)
    @param protocol_mode HTTP protocol mode for ALPN (default Auto)
    @param host Hostname for error messages
    @return Some TLS client configuration, or raises on error *)
let create_client_opt ?existing_config ~verify_tls ~min_tls_version ?(protocol_mode = Auto) ~host () =
  match existing_config with
  | Some cfg -> Some cfg
  | None -> Some (create_client ~verify_tls ~min_tls_version ~protocol_mode ~host ())

(** {1 ALPN Result Extraction}

    Helper functions for extracting negotiated protocol from TLS epoch. *)

(** Negotiated HTTP protocol from ALPN. *)
type negotiated_protocol =
  | Http1_1  (** HTTP/1.1 *)
  | Http2    (** HTTP/2 *)

(** Get the negotiated ALPN protocol from TLS epoch data.
    Returns [None] if ALPN was not negotiated. *)
let get_alpn_from_epoch (epoch : Tls.Core.epoch_data) : string option =
  epoch.alpn_protocol

(** Parse ALPN result string to negotiated protocol. *)
let negotiated_of_alpn = function
  | "h2" -> Some Http2
  | "http/1.1" -> Some Http1_1
  | _ -> None

(** Default protocol when ALPN is not available. *)
let default_protocol = Http1_1

(** Detect the protocol to use based on mode and ALPN result.
    @raise Failure if Http2_only mode but HTTP/2 not negotiated *)
let detect_protocol ~mode alpn_result =
  match mode, alpn_result with
  | _, Some "h2" -> Http2
  | _, Some "http/1.1" -> Http1_1
  | Http2_only, None ->
      failwith "HTTP/2 required but not negotiated via ALPN"
  | Http2_only, Some other ->
      failwith (Printf.sprintf "HTTP/2 required but ALPN returned: %s" other)
  | (Auto | Http1_only), _ -> Http1_1

(** Convert negotiated protocol to string. *)
let negotiated_to_string = function
  | Http1_1 -> "HTTP/1.1"
  | Http2 -> "HTTP/2"

(** Pretty print negotiated protocol. *)
let pp_negotiated ppf p =
  Format.pp_print_string ppf (negotiated_to_string p)

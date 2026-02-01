(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** XRPC client for AT Protocol.

    This module provides the low-level XRPC client for making queries and
    procedure calls to AT Protocol services. It uses the [requests] library for
    HTTP communication.

    {2 XRPC Overview}

    XRPC (eXtensible RPC) is AT Protocol's RPC mechanism. Methods are identified
    by namespace identifiers (NSIDs) like ["app.bsky.feed.getTimeline"].

    Two method types:
    - {b Query}: HTTP GET, idempotent, for reading data
    - {b Procedure}: HTTP POST, may have side effects, for mutations

    {2 Usage}

    {[
      let client =
        Xrpc_client.create ~sw ~env ~service:"https://bsky.social" ()
      in

      (* Public query (no auth) *)
      let profile =
        Xrpc_client.query client ~nsid:"app.bsky.actor.getProfile"
          ~params:[ ("actor", "alice.bsky.social") ]
          ~decoder:profile_jsont
      in

      (* After setting session *)
      Xrpc_client.set_session client session;

      (* Authenticated procedure *)
      Xrpc_client.procedure client ~nsid:"app.bsky.feed.post" ~params:[]
        ~input:(Some post_jsont) ~input_data:(Some my_post)
        ~decoder:post_ref_jsont
    ]}

    @see <https://atproto.com/specs/xrpc> AT Protocol XRPC Specification *)

(** {1 Client} *)

type t
(** XRPC client with HTTP session and optional authentication. *)

val create :
  sw:Eio.Switch.t ->
  env:
    < clock : _ Eio.Time.clock
    ; net : _ Eio.Net.t
    ; fs : Eio.Fs.dir_ty Eio.Path.t
    ; .. > ->
  service:string ->
  ?on_request:(t -> unit) ->
  unit ->
  t
(** [create ~sw ~env ~service ()] creates an XRPC client.

    @param sw Eio switch for resource management
    @param env Eio environment with clock, network, and filesystem
    @param service Base URL of the PDS (e.g., ["https://bsky.social"])
    @param on_request
      Optional callback invoked before each request, useful for token refresh in
      credential managers *)

(** {1 Session Management} *)

val set_session : t -> Xrpc_types.session -> unit
(** [set_session client session] sets the authentication session. Subsequent
    requests will include the Authorization header. *)

val clear_session : t -> unit
(** [clear_session client] removes the authentication session. *)

val get_session : t -> Xrpc_types.session option
(** [get_session client] returns the current session, if any. *)

val get_service : t -> string
(** [get_service client] returns the service base URL. *)

(** {1 XRPC Operations} *)

val query :
  t -> nsid:string -> params:(string * string) list -> decoder:'a Jsont.t -> 'a
(** [query client ~nsid ~params ~decoder] executes an XRPC query (GET).

    @param nsid Namespace identifier (e.g., ["app.bsky.feed.getTimeline"])
    @param params Query parameters as key-value pairs
    @param decoder jsont codec for decoding the response

    @raise Eio.Io with [Xrpc_error.E] on failure *)

val procedure :
  t ->
  nsid:string ->
  params:(string * string) list ->
  input:'a Jsont.t option ->
  input_data:'a option ->
  decoder:'b Jsont.t ->
  'b
(** [procedure client ~nsid ~params ~input ~input_data ~decoder] executes an
    XRPC procedure (POST).

    @param nsid Namespace identifier
    @param params Query parameters
    @param input Optional jsont codec for encoding request body
    @param input_data Optional request body data
    @param decoder jsont codec for decoding the response

    @raise Eio.Io with [Xrpc_error.E] on failure *)

val procedure_blob :
  t ->
  nsid:string ->
  params:(string * string) list ->
  blob:string ->
  content_type:string ->
  decoder:'a Jsont.t ->
  'a
(** [procedure_blob client ~nsid ~params ~blob ~content_type ~decoder] executes
    a procedure with raw binary upload.

    Used for [com.atproto.repo.uploadBlob] and similar endpoints.

    @param blob Raw binary data to upload
    @param content_type MIME type of the blob (e.g., ["image/jpeg"])

    @raise Eio.Io with [Xrpc_error.E] on failure *)

(** {1 Raw Binary Operations} *)

val query_bytes :
  t -> nsid:string -> params:(string * string) list -> string * string
(** [query_bytes client ~nsid ~params] executes a query returning raw bytes.

    Returns [(body, content_type)]. Used for endpoints that return binary data
    like [com.atproto.sync.getBlob].

    @raise Eio.Io with [Xrpc_error.E] on failure *)

val procedure_bytes :
  t ->
  nsid:string ->
  params:(string * string) list ->
  body:string option ->
  content_type:string ->
  (string * string) option
(** [procedure_bytes client ~nsid ~params ~body ~content_type] executes a
    procedure with optional binary body.

    Returns [Some (body, content_type)] or [None] for 204 No Content.

    @raise Eio.Io with [Xrpc_error.E] on failure *)

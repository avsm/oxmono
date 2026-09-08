(** media — the media repository.

    Media is addressed by an [mxc://] URI, which names a server and an
    identifier on it. Downloads, thumbnails and the config use the authenticated
    endpoints under [/_matrix/client/v1/media] introduced in Matrix 1.11 when
    advertised by the homeserver, and otherwise fall back to the legacy
    unauthenticated endpoints. Uploads go to [/_matrix/media/v3/upload], which
    the specification has not moved. The [upload] convenience function holds its
    string body in memory; use [upload_stream] for one-shot sources. *)

(** {1 Media URIs} *)

module Mxc : sig
  @@ portable
  (** An [mxc://] URI, the only way the Matrix API names stored content. *)

  type t
  (** The type for [mxc://] URIs. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] is the URI [s] denotes. [s] must have a well-formed server
      name and a non-empty media identifier containing only ASCII letters,
      digits, [-] and [_], matching ruma's Matrix identifier validation. *)

  val to_string : t -> string
  (** [to_string t] is [t] in the [mxc://server_name/media_id] form. *)

  val server_name : t -> Matrix_proto.Id.Server_name.t
  (** [server_name t] is the homeserver that holds the content. *)

  val media_id : t -> string
  (** [media_id t] is the identifier the holding server files the content under.
  *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] name the same content. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. Decoding fails on a string that is not
      an [mxc://] URI. *)
end

val mxc_option_jsont : Mxc.t option Jsont.t
(** [mxc_option_jsont] reads an [mxc://] URI that a server may leave unset. JSON
    [null] and the empty string, which is what a server sends for an avatar the
    user cleared, both read as [None]. Any other string that is not an [mxc://]
    URI is a decode error. Encoding [None] writes [null]. *)

(** {1 Upload} *)

val upload :
  Client.t ->
  content_type:string ->
  data:string ->
  ?filename:string ->
  unit ->
  (Mxc.t, Error.t) result
(** [upload t ~content_type ~data ?filename ()] is
    [POST /_matrix/media/v3/upload] (Matrix 1.0) and is the URI of the stored
    content.

    [content_type] is sent verbatim, parameters and all, and the server hands it
    back on download. A body above the server's limit, which {!get_config}
    reports, is [M_TOO_LARGE]. [filename] is suggested to whoever downloads the
    content, and defaults to absent. *)

val upload_stream :
  Client.t ->
  content_type:string ->
  source:Eio.Flow.source_ty Eio.Resource.t ->
  ?length:int64 ->
  ?filename:string ->
  unit ->
  (Mxc.t, Error.t) result
(** [upload_stream t ~source ?length ?filename ()] uploads a one-shot source to
    the media repository without buffering it in a string. [length], when
    supplied, is the exact byte count and is passed to the transport. *)

type preallocated = {
  uri : Mxc.t;
      (** The URI reserved by the server. It has no content until
          {!upload_preallocated} succeeds. *)
  unused_expires_at : Matrix_proto.Event.Timestamp.t option;
      (** The server's optional deadline for beginning/completing the upload. *)
}
(** A content URI reserved independently of its bytes, as introduced in Matrix
    1.7 (MSC2246). *)

val create_content_uri : Client.t -> (preallocated, Error.t) result
(** [create_content_uri t] makes an authenticated, empty
    [POST /_matrix/media/v1/create] request. *)

type preallocated_upload_error =
  | Preallocated_expired
      (** The advertised local deadline has passed, or a legacy server
          explicitly reported expiry. *)
  | Cannot_overwrite  (** The server returned [M_CANNOT_OVERWRITE_MEDIA]. *)
  | Upload_error of Error.t  (** Any other request failure. *)

val pp_preallocated_upload_error :
  Format.formatter -> preallocated_upload_error -> unit

val upload_preallocated :
  ?now:Matrix_proto.Event.Timestamp.t ->
  Client.t ->
  preallocated ->
  content_type:string ->
  data:string ->
  ?filename:string ->
  unit ->
  (unit, preallocated_upload_error) result
(** [upload_preallocated t reservation ~content_type ~data ()] fills a URI
    returned by {!create_content_uri} with authenticated
    [PUT /_matrix/media/v3/upload/{serverName}/{mediaId}]. [filename] is the
    optional download name. When the reservation carries a deadline, the
    function refuses an already expired upload before making a request; [now]
    injects the comparison time for deterministic callers and tests.

    The server cannot distinguish every expired identifier from an invalid one,
    so an ordinary [M_NOT_FOUND] remains [Upload_error]. *)

(** {1 Encrypted attachments} *)

type encrypted_file = Matrix_proto.Event.Media_message_content.encrypted_file
(** The encrypted-file metadata carried by a media message. *)

type source =
  | Plain of Mxc.t
  | Encrypted of encrypted_file
      (** A plain or encrypted source for high-level media retrieval. *)

type format =
  | File
  | Thumbnail of {
      width : int;
      height : int;
      resize : [ `Crop | `Scale ] option;
    }  (** The requested representation of a media source. *)

type request = { source : source; format : format }
(** A media request, also used to select its cache identity. *)

type encrypted_error =
  | Media_error of Error.t
  | Attachment_error of Encrypted_attachment.error
      (** A transport failure or a failure validating encrypted attachment
          metadata or ciphertext. *)

val pp_encrypted_error : Format.formatter -> encrypted_error -> unit

val upload_encrypted :
  Client.t ->
  data:string ->
  ?filename:string ->
  unit ->
  (Mxc.t * encrypted_file, encrypted_error) result
(** Encrypt [data] before uploading it and return both its [mxc://] URI and the
    validated encrypted-file metadata suitable for a message event. The
    ciphertext is always sent as [application/octet-stream], as required by the
    encrypted-media upload path. Use {!upload_encrypted_stream} when the
    plaintext should not be buffered in one string. *)

val upload_encrypted_stream :
  Client.t ->
  source:Eio.Flow.source_ty Eio.Resource.t ->
  ?length:int64 ->
  ?filename:string ->
  unit ->
  (Mxc.t * encrypted_file, encrypted_error) result
(** Stream-encrypt [source] and upload it as [application/octet-stream]. When
    [length] is supplied it is the exact plaintext byte length and is passed to
    Fetch as the request length (ciphertext and plaintext have equal length).
    The source is one-shot: redirects or retries that need to replay it fail
    according to Fetch's [Body_not_replayable] semantics. The upload transport
    is incremental, but its small JSON response is still buffered. *)

(** {1 Download} *)

type content = {
  body : string;  (** The bytes the server sent. *)
  content_type : string option;
      (** The [Content-Type] header, absent when the server sent none. *)
}
(** A downloaded body. *)

val download :
  Client.t ->
  server_name:Matrix_proto.Id.Server_name.t ->
  media_id:string ->
  (content, Error.t) result
(** [download t ~server_name ~media_id] is
    [GET /_matrix/client/v1/media/download/{serverName}/{mediaId}] (Matrix
    1.11), or the deprecated [/_matrix/media/v3/download] endpoint when the
    homeserver does not advertise Matrix 1.11 or stable MSC3916. Both arguments
    come from an {!Mxc.t}. The function first queries
    [/_matrix/client/versions]; legacy media requests omit the bearer token.

    Media the server has not yet fetched from a remote homeserver can take a
    while. Media it never had is [M_NOT_FOUND]. *)

val download_encrypted :
  Client.t -> encrypted_file -> (string, encrypted_error) result
(** Download ciphertext named by [encrypted_file], verify its SHA-256 digest,
    and return plaintext only after verification succeeds. As with
    {!val-download}, the current transport buffers at most the client's 64 MiB
    response limit; use {!Encrypted_attachment.Decryptor} for caller-managed
    chunking. *)

val download_encrypted_stream :
  Client.t ->
  encrypted_file ->
  spool:_ Eio.File.rw ->
  output:_ Eio.Flow.sink ->
  (unit, encrypted_error) result
(** [download_encrypted_stream t file ~spool ~output] downloads and
    authenticates [file] without buffering the response or plaintext in memory.
    [spool] must be a caller-owned seekable read/write file; it is exclusively
    owned by this call (do not read or mutate it concurrently), truncated before
    the request, contains ciphertext only, and is left positioned at offset zero
    on every result after metadata/MXC validation. [output] receives plaintext
    only after the ciphertext digest has been checked (twice, including the
    spooled copy).

    The caller also owns filesystem policy: create a fresh spool with exclusive
    mode [0600] in an appropriate cache directory, enforce any download quota,
    and close and unlink it in a [finally] handler on success, error or
    cancellation. The function cannot infer those choices from an already-open
    capability and does not close either [spool] or [output]. It validates the
    encrypted metadata and MXC URI, queries server capabilities, and uses the
    authenticated Matrix 1.11 or unauthenticated legacy route accordingly. On
    any failure, [output] receives no bytes, except that an [Eio.Io] failure
    from [output] may occur after it has accepted a prefix; such a failure is
    returned as a [Media_error (Network_error _)]. Cancellation propagates. *)

val thumbnail :
  Client.t ->
  server_name:Matrix_proto.Id.Server_name.t ->
  media_id:string ->
  width:int ->
  height:int ->
  ?resize:[ `Crop | `Scale ] ->
  unit ->
  (content, Error.t) result
(** [thumbnail t ~server_name ~media_id ~width ~height ?resize ()] is
    [GET /_matrix/client/v1/media/thumbnail/{serverName}/{mediaId}] (Matrix
    1.11), or the deprecated [/_matrix/media/v3/thumbnail] endpoint on older
    homeservers. The capability query and authentication are as for
    {!val-download}. The server picks the nearest size it holds, so the result
    need not be exactly [width] by [height].

    [resize] is [`Crop] to fill the box and cut what does not fit, or [`Scale]
    to fit the whole image inside it. It defaults to absent, leaving the choice
    to the server. *)

val mxc_to_http :
  Client.t -> mxc:Mxc.t -> ?width:int -> ?height:int -> unit -> Uriz.t
(** [mxc_to_http t ~mxc ?width ?height ()] is the URL on the client's homeserver
    that serves [mxc].

    The URL addresses the authenticated media endpoint, so a request for it must
    carry the client's bearer token. It cannot be handed to an unauthenticated
    fetcher or embedded in a page.

    [width] and [height] together give a thumbnail URL. Both default to absent,
    and one without the other gives the download URL. *)

val mxc_to_http_unauthenticated :
  Client.t -> mxc:Mxc.t -> ?width:int -> ?height:int -> unit -> Uriz.t
(** [mxc_to_http_unauthenticated t ~mxc ?width ?height ()] is the pure URL for
    the deprecated unauthenticated [/_matrix/media/v3] media endpoints. It
    performs no capability query and is suitable for a browser or other fetcher
    that cannot attach the client's bearer token. Use {!mxc_to_http} for the
    authenticated Matrix 1.11 endpoint. *)

val mxc_to_http_resolved :
  Client.t ->
  mxc:Mxc.t ->
  ?width:int ->
  ?height:int ->
  unit ->
  (Uriz.t, Error.t) result
(** [mxc_to_http_resolved t ~mxc ?width ?height ()] queries the homeserver's
    versions and returns a URL using the same capability decision as
    {!val-download}. Matrix 1.11 and stable MSC3916 use the authenticated
    [/_matrix/client/v1/media] endpoint; older servers use the deprecated
    unauthenticated [/_matrix/media/v3] endpoint. The versions query is made
    with the client bearer token, but the returned legacy URL is for an
    unauthenticated fetcher. *)

(** {1 URL previews} *)

type preview = (string * Jsont.json) list
(** OpenGraph-like preview fields, sorted by name. The field set is deliberately
    open: homeservers may return standard [og:*] values and Matrix extensions
    such as [matrix:image:size]. An empty list is a successful empty object. *)

val get_url_preview :
  Client.t ->
  url:string ->
  ?ts:Matrix_proto.Event.Timestamp.t ->
  unit ->
  (preview, Error.t) result
(** [get_url_preview t ~url ()] asks the homeserver to fetch [url] and return
    its OpenGraph-like metadata. [ts] asks for the representation at a
    particular millisecond timestamp when supported.

    The function first reads [/_matrix/client/versions]. Matrix 1.11 or
    [org.matrix.msc3916.stable] selects the authenticated
    [/_matrix/client/v1/media/preview_url] endpoint; older servers use the
    deprecated [/_matrix/media/v3/preview_url] endpoint without sending the
    bearer token. A server may disable URL previews and return an ordinary
    Matrix error. In an encrypted room, calling this function still reveals the
    URL to the homeserver. *)

(** {1 Configuration} *)

type config = {
  upload_size : int option;
      (** Largest upload in bytes. [None] means the server declined to say,
          rather than that there is no limit. *)
  custom : (string * Jsont.json) list;
      (** Other configuration members, sorted by name and preserved verbatim. *)
}
(** The limits the homeserver puts on uploads. *)

val get_config : Client.t -> (config, Error.t) result
(** [get_config t] queries the homeserver capabilities, then uses authenticated
    [GET /_matrix/client/v1/media/config] for Matrix 1.11 or stable MSC3916, and
    unauthenticated deprecated [GET /_matrix/media/v3/config] otherwise. *)

(** media — the media repository, raising instead of returning for the ordinary
    endpoint wrappers.

    Every function that performs a request raises [Eio.Io] carrying [Error.E e]
    where {!Matrix_client.Media} returns [Error e]. That module documents what
    each call does, which endpoint it uses and which errors it produces.

    Downloads, thumbnails and the configuration query server capabilities and
    use authenticated Matrix 1.11 endpoints when available, falling back to
    unauthenticated legacy media endpoints; the authenticated route requires a
    logged-in client. Bodies are held whole in memory. The high-level
    {!get_content} wrapper preserves typed encrypted-attachment errors as a
    result because they are not all representable as [Eio.Error]. *)

(** {1 Media URIs} *)

module Mxc = Matrix_client.Media.Mxc
(** An [mxc://] URI, the only way the Matrix API names stored content. *)

type encrypted_file = Matrix_client.Media.encrypted_file
(** Encrypted attachment metadata carried by a media event. *)

type source = Matrix_client.Media.source =
  | Plain of Mxc.t
  | Encrypted of encrypted_file  (** A plain or encrypted media source. *)

type format = Matrix_client.Media.format =
  | File
  | Thumbnail of {
      width : int;
      height : int;
      resize : [ `Crop | `Scale ] option;
    }  (** The requested file or thumbnail representation. *)

type request = Matrix_client.Media.request = {
  source : source;
  format : format;
}
(** A high-level media retrieval request. *)

type encrypted_error = Matrix_client.Media.encrypted_error =
  | Media_error of Matrix_client.Error.t
  | Attachment_error of Matrix_client.Encrypted_attachment.error
      (** Typed transport, attachment metadata, and decryption failures. *)

val get_content :
  ?use_cache:bool ->
  ?store:Matrix_client.Media_store.t ->
  Client.t ->
  request ->
  (string, encrypted_error) result
(** [get_content c request] uses [c]'s current replaceable fetcher. Cache hits
    and local send-queue MXCs avoid the fetcher. Attachment and authentication
    failures remain typed {!encrypted_error} results rather than being flattened
    into [Eio.Io]. *)

val mxc_to_http :
  Client.t -> mxc:Mxc.t -> ?width:int -> ?height:int -> unit -> Uriz.t
(** [mxc_to_http c ~mxc ()] is {!Matrix_client.Media.mxc_to_http}. It performs
    no request and raises nothing. *)

val mxc_to_http_unauthenticated :
  Client.t -> mxc:Mxc.t -> ?width:int -> ?height:int -> unit -> Uriz.t
(** [mxc_to_http_unauthenticated c ~mxc ()] is
    {!Matrix_client.Media.mxc_to_http_unauthenticated}. It performs no request
    and raises nothing, and is suitable for embedding in a browser. *)

val mxc_to_http_resolved :
  Client.t -> mxc:Mxc.t -> ?width:int -> ?height:int -> unit -> Uriz.t
(** [mxc_to_http_resolved c ~mxc ?width ?height ()] probes server capabilities
    and returns the route-aware URL, or raises [Eio.Io] for the capability
    request failure. *)

(** {1 Upload} *)

val upload :
  Client.t ->
  content_type:string ->
  data:string ->
  ?filename:string ->
  unit ->
  Mxc.t
(** [upload c ~content_type ~data ()] is {!Matrix_client.Media.val-upload} with
    the result unwrapped. *)

type preallocated = Matrix_client.Media.preallocated = {
  uri : Mxc.t;
  unused_expires_at : Matrix_proto.Event.Timestamp.t option;
}

val create_content_uri : Client.t -> preallocated
(** [create_content_uri c] is {!Matrix_client.Media.create_content_uri} with the
    request error unwrapped. *)

type preallocated_upload_error = Matrix_client.Media.preallocated_upload_error =
  | Preallocated_expired
  | Cannot_overwrite
  | Upload_error of Matrix_client.Error.t

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
(** [upload_preallocated] is {!Matrix_client.Media.upload_preallocated}. Local
    expiry and overwrite failures remain explicit results; an [Upload_error]
    retains its ordinary client error for the caller to unwrap or inspect. *)

(** {1 Download} *)

type content = Matrix_client.Media.content = {
  body : string;
  content_type : string option;
}
(** A downloaded body. *)

val download :
  Client.t ->
  server_name:Matrix_proto.Id.Server_name.t ->
  media_id:string ->
  content
(** [download c ~server_name ~media_id] is {!Matrix_client.Media.val-download}
    with the result unwrapped. *)

val thumbnail :
  Client.t ->
  server_name:Matrix_proto.Id.Server_name.t ->
  media_id:string ->
  width:int ->
  height:int ->
  ?resize:[ `Crop | `Scale ] ->
  unit ->
  content
(** [thumbnail c ~server_name ~media_id ~width ~height ()] is
    {!Matrix_client.Media.thumbnail} with the result unwrapped. *)

(** {1 URL previews} *)

type preview = Matrix_client.Media.preview
(** OpenGraph-like preview fields. *)

val get_url_preview :
  Client.t ->
  url:string ->
  ?ts:Matrix_proto.Event.Timestamp.t ->
  unit ->
  preview
(** [get_url_preview c ~url ()] is {!Matrix_client.Media.get_url_preview} with
    the result unwrapped. *)

(** {1 Configuration} *)

type config = Matrix_client.Media.config = {
  upload_size : int option;
  custom : (string * Jsont.json) list;
}
(** The limits the homeserver puts on uploads. *)

val get_config : Client.t -> config
(** [get_config c] is {!Matrix_client.Media.get_config} with the result
    unwrapped. *)

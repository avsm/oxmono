(** Uploaded files and upload metadata.

    Attachments are files owned by the user authenticated by the client. *)

module Id = Zulip.Id.Attachment
(** Attachment identifiers. *)

type t = {
  id : Id.t;  (** The attachment identifier. *)
  name : string;  (** The uploaded file name. *)
  path_id : string;  (** The path below the server's user-upload root. *)
  size : int;  (** The file size in bytes. *)
  create_time : int;  (** The upload time as UTC Unix seconds. *)
  message_ids : Zulip.Id.Message.t list;
      (** The messages in the organization that reference the file. *)
  raw : Jsont.json;
      (** The complete attachment object, including unrecognized fields. *)
}
(** The type for attachment metadata. *)

type page = {
  attachments : t list;
  upload_space_used : int;  (** The user's occupied upload space in bytes. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for attachment-list responses. *)

val jsont : t Jsont.t
(** [jsont] is the JSON codec for attachment metadata. *)

val page_jsont : page Jsont.t
(** [page_jsont] is the JSON codec for attachment-list responses. *)

val list : Client.t -> (page, Error.t) result
(** [list client] is the metadata for files uploaded by the user authenticated
    by [client]. *)

val delete : Client.t -> attachment_id:Id.t -> (unit, Error.t) result
(** [delete client ~attachment_id] deletes the upload identified by
    [attachment_id]. Zulip rejects an invalid identifier or an upload that the
    authenticated user does not own. *)

type upload_result = {
  uri : string;  (** The relative URL for referencing the uploaded file. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed upload responses. *)

val upload_file_detailed :
  Client.t ->
  filename:string ->
  content_type:string ->
  string ->
  (upload_result, Error.t) result
(** [upload_file_detailed client ~filename ~content_type content] is the
    detailed response after uploading [content] as [filename] with media type
    [content_type]. The resulting URL is initially accessible only to the
    authenticated user. *)

val upload_file :
  Client.t ->
  filename:string ->
  content_type:string ->
  string ->
  (string, Error.t) result
(** [upload_file client ~filename ~content_type content] is the relative URL
    after uploading [content] as [filename] with media type [content_type]. *)

val upload_stream :
  Client.t ->
  filename:string ->
  content_type:string ->
  ?length:int64 ->
  _ Eio.Flow.source ->
  (string, Error.t) result
(** [upload_stream client ~filename ~content_type ~length source] is the
    relative URL after uploading bytes from [source]. [length] is the byte
    length when known and is omitted by default. The caller retains ownership of
    [source]. A failed upload may consume a prefix of it. A negative [length]
    returns {!Error.t.constructor-Invalid_request} without consuming [source].
*)

val upload_stream_detailed :
  Client.t ->
  filename:string ->
  content_type:string ->
  ?length:int64 ->
  _ Eio.Flow.source ->
  (upload_result, Error.t) result
(** [upload_stream_detailed client ~filename ~content_type ~length source] is
    the detailed response after uploading bytes from [source]. [length] is the
    byte length when known and is omitted by default. The caller retains
    ownership of [source]. A failed upload may consume a prefix of it. A
    negative [length] returns {!Error.t.constructor-Invalid_request} without
    consuming [source]. *)

type temporary_url_result = {
  url : string;  (** The temporary unauthenticated URL. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for detailed temporary-URL responses. *)

val temporary_url_detailed :
  Client.t ->
  realm_id:int ->
  filename:string ->
  (temporary_url_result, Error.t) result
(** [temporary_url_detailed client ~realm_id ~filename] is the detailed response
    containing an unauthenticated, short-lived URL for the upload path formed
    from [realm_id] and [filename]. Zulip's default validity is usually 60
    seconds and is server-configurable. *)

val temporary_url :
  Client.t -> realm_id:int -> filename:string -> (string, Error.t) result
(** [temporary_url client ~realm_id ~filename] is an unauthenticated,
    short-lived URL for the upload path formed from [realm_id] and [filename].
    It is intended for immediate use. *)

type thumbnail_status = {
  has_thumbnail : bool;
      (** Whether thumbnail generation has completed successfully. *)
  raw : Jsont.json;
      (** The complete response object, including unrecognized fields. *)
}
(** The type for thumbnail-generation responses. Zulip rejects uploads that it
    does not recognize as images. A false status can mean generation is still
    pending. *)

val thumbnail_status :
  Client.t ->
  realm_id:int ->
  filename:string ->
  (thumbnail_status, Error.t) result
(** [thumbnail_status client ~realm_id ~filename] is the detailed thumbnail
    status for the upload path formed from [realm_id] and [filename]. *)

val has_thumbnail :
  Client.t -> realm_id:int -> filename:string -> (bool, Error.t) result
(** [has_thumbnail client ~realm_id ~filename] is the thumbnail status for the
    upload path formed from [realm_id] and [filename]. *)

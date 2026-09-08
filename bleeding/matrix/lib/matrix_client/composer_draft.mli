(** Composer drafts persisted in the client's local state store.

    Drafts are kept separately for each room and optional thread root. Setters
    update the store in memory; callers use {!Store.flush} when they want an
    on-disk snapshot, just like the other store slots. Binary attachment data is
    encoded as unpadded base64 in the store. *)

type draft_type =
  | New_message
  | Reply of Matrix_proto.Id.Event_id.t
  | Edit of Matrix_proto.Id.Event_id.t
      (** What message the draft will create or replace. *)

type thumbnail = {
  filename : string;
  data : string;
  mimetype : string option;
  width : int64 option;
  height : int64 option;
  size : int64 option;
}
(** A thumbnail. [data] contains the raw bytes. *)

type attachment_content =
  | Image of {
      data : string;
      mimetype : string option;
      size : int64 option;
      width : int64 option;
      height : int64 option;
      blurhash : string option;
      thumbnail : thumbnail option;
    }
  | Video of {
      data : string;
      mimetype : string option;
      size : int64 option;
      width : int64 option;
      height : int64 option;
      duration_ms : int64 option;
      blurhash : string option;
      thumbnail : thumbnail option;
    }
  | Audio of {
      data : string;
      mimetype : string option;
      size : int64 option;
      duration_ms : int64 option;
    }
  | File of { data : string; mimetype : string option; size : int64 option }
      (** Attachment data and metadata. Durations are integer milliseconds. *)

type attachment = { filename : string; content : attachment_content }
(** An attachment in a draft. *)

type t = {
  plain_text : string;
  html_text : string option;
  draft_type : draft_type;
  attachments : attachment list;
}
(** The current draft for one room/thread. *)

val save :
  Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?thread_root:Matrix_proto.Id.Event_id.t ->
  t ->
  (unit, Error.t) result
(** [save store ~room_id ?thread_root draft] replaces that draft and marks
    [store] dirty. It does not flush the store. *)

val load :
  Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?thread_root:Matrix_proto.Id.Event_id.t ->
  unit ->
  (t option, Error.t) result
(** [load store ~room_id ?thread_root] returns the saved draft, if any. *)

val clear :
  Store.t ->
  room_id:Matrix_proto.Id.Room_id.t ->
  ?thread_root:Matrix_proto.Id.Event_id.t ->
  unit ->
  (unit, Error.t) result
(** [clear store ~room_id ?thread_root] removes only that room/thread draft. It
    does not flush the store. *)

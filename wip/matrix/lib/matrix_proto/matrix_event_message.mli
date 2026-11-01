@@ portable

(** The content of the message events.

    An [m.room.message] event's [msgtype] fixes what the rest of its content
    means, so one module covers each family of message types. [m.sticker] is
    here too, since it is a media message under another event type.

    @see <https://spec.matrix.org/v1.11/client-server-api/#mroommessage>
      m.room.message *)

module Msgtype : sig
  (** The [msgtype] of an [m.room.message], which fixes what the rest of the
      content means.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mroommessage-msgtypes>
        Message types *)

  type t =
    | Text
    | Emote
    | Notice
    | Image
    | File
    | Audio
    | Video
    | Location
    | Custom of string  (** A message type this type does not name. *)

  val to_string : t -> string
  (** [to_string t] is the wire form of [t]. *)

  val of_string : string -> t
  (** [of_string s] is the message type [s] names, or [Custom s]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] when [a] and [b] are the same message type. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints [to_string t] on [ppf]. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Media_info : sig
  (** What a media message claims about its attachment.

      All of it is the sender's word. A client must not size a layout or
      allocate a buffer from it without checking what actually arrives. *)

  type t = {
    mimetype : string option;
    size : int option;  (** Bytes. *)
    duration : int option;  (** Milliseconds, for audio and video. *)
    h : int option;  (** Height in pixels. *)
    w : int option;  (** Width in pixels. *)
    thumbnail_url : string option;  (** An [mxc://] URI. *)
    thumbnail_info : Matrix_event_core.Image_info.t option;
  }

  val v :
    ?mimetype:string ->
    ?size:int ->
    ?duration:int ->
    ?h:int ->
    ?w:int ->
    ?thumbnail_url:string ->
    ?thumbnail_info:Matrix_event_core.Image_info.t ->
    unit ->
    t
  (** [v ()] is media metadata. Every argument defaults to absent. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Text_message_content : sig
  (** The content of an [m.room.message] of msgtype [m.text], [m.emote] or
      [m.notice].

      @see <https://spec.matrix.org/v1.11/client-server-api/#mtext> m.text *)

  type t = {
    body : string;
    msgtype : Msgtype.t;
        (** One of {!Msgtype.Text}, {!Msgtype.Emote} and {!Msgtype.Notice}. *)
    format : string option;
    formatted_body : string option;
  }

  val make :
    body:string ->
    ?msgtype:Msgtype.t ->
    ?format:string ->
    ?formatted_body:string ->
    unit ->
    t
  (** [make ~body ()] is a text message content. [msgtype] defaults to
      {!Msgtype.Text}. [format] defaults to ["org.matrix.custom.html"] and is
      written only when [formatted_body] is given. [formatted_body] defaults to
      absent. *)

  val body : t -> string
  (** [body t] is the plain-text form, which every client can show. *)

  val msgtype : t -> Msgtype.t
  (** [msgtype t] is which of the three text types this is. *)

  val format : t -> string option
  (** [format t] is ["org.matrix.custom.html"] when {!val-formatted_body} is
      HTML. *)

  val formatted_body : t -> string option
  (** [formatted_body t] is the rich form. Sanitise it before rendering. The
      specification's list of permitted tags is advice to clients, not a
      guarantee about what arrives. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] prints the message type and the body. *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Media_message_content : sig
  (** The content of an [m.room.message] of msgtype [m.image], [m.file],
      [m.audio] or [m.video].

      Exactly one of {!field-url} and {!field-file} is present. The first is for
      an unencrypted room, the second for an encrypted one. Nothing here
      enforces that.

      @see <https://spec.matrix.org/v1.11/client-server-api/#mimage> m.image *)

  type t = {
    body : string;  (** A file name, or a description of the media. *)
    msgtype : Msgtype.t;
    url : string option;  (** An [mxc://] URI, in an unencrypted room. *)
    info : Media_info.t option;
    file : encrypted_file option;  (** In an encrypted room. *)
  }

  and encrypted_file = {
    url : string;  (** The [mxc://] URI of the ciphertext. *)
    key : Jsont.json;  (** A JSON Web Key for AES-CTR. *)
    iv : string;  (** The counter block, unpadded base64. *)
    hashes : (string * string) list;
        (** Algorithm to unpadded base64 digest of the ciphertext. Check it
            before decrypting. *)
    v : string;  (** ["v2"]. *)
  }
  (** The type for an attachment encrypted before upload, so the homeserver
      holds only ciphertext.

      @see <https://spec.matrix.org/v1.11/client-server-api/#sending-encrypted-attachments>
        Sending encrypted attachments *)

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Sticker_content : sig
  (** The content of [m.sticker], an image sent as a sticker rather than as a
      message. Its event type is [m.sticker], so it carries no [msgtype].

      @see <https://spec.matrix.org/v1.11/client-server-api/#msticker> m.sticker
  *)

  type t = {
    body : string;  (** A description of the sticker, for accessibility. *)
    info : Media_info.t option;
    url : string;  (** An [mxc://] URI. *)
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

module Location_message_content : sig
  (** The content of an [m.room.message] of msgtype [m.location].

      @see <https://spec.matrix.org/v1.11/client-server-api/#mlocation>
        m.location *)

  type location_info = {
    uri : string;  (** A [geo:] URI. *)
    description : string option;
  }
  (** The type for the extensible-events form of a location. *)

  type t = {
    body : string;  (** A description of the place, for display. *)
    msgtype : Msgtype.t;
    geo_uri : string;  (** A [geo:] URI. *)
    info : location_info option;
  }

  val jsont : t Jsont.t
  (** [jsont] is the JSON codec for {!t}. *)
end

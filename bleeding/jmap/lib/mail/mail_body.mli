@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Email body parts.

    {{:https://datatracker.ietf.org/doc/html/rfc8621#section-4.1.4} RFC 8621
     Section 4.1.4} defines the EmailBodyPart object, which describes one MIME
    part of a message, and the EmailBodyValue object, which carries the content
    of a part the client asked to have fetched.

    @canonical Jmap.Proto.Email_body *)

(** {1 Body values} *)

(** The content of a body part. *)
module Value : sig
  type t = {
    value : string;  (** The content, decoded and in Unicode. *)
    is_encoding_problem : bool;
        (** [true] if the content transfer encoding or the character set could
            not be decoded and the server had to substitute characters. *)
    is_truncated : bool;
        (** [true] if the content was cut short at the [maxBodyValueBytes] the
            client gave. *)
  }
  (** The type for EmailBodyValue objects. *)

  val v : ?is_encoding_problem:bool -> ?is_truncated:bool -> string -> t
  (** [v s] is the body value [s]. [is_encoding_problem] and [is_truncated] both
      default to [false]. The [bodyValues] of an Email being created are built
      this way (RFC 8621 Section 4.6). *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an EmailBodyValue. An absent [isEncodingProblem]
      or [isTruncated] decodes to [false], and [false] is omitted on encode. *)
end

(** {1 Body parts} *)

(** The structure of a body part. *)
module Part : sig
  type t = {
    part_id : string option;
        (** The key of this part in the [bodyValues] of the Email, or [None] for
            a part whose content cannot be fetched as text. *)
    blob_id : Proto_id.t option;
        (** The blob holding the raw content of this part, or [None] for a
            multipart part. *)
    size : int64 option;  (** The size of the decoded content in octets. *)
    headers : Mail_header.t list option;
        (** The header fields of this part, in the order they appear. *)
    name : string option;
        (** The filename the part suggests, from its Content-Disposition or
            Content-Type field. *)
    type_ : string option;
        (** The media type. It is always present in a response unless the client
            restricted [bodyProperties] to exclude it (RFC 8621 Section 4.2). *)
    charset : string option;
        (** The [charset] parameter of the Content-Type field. *)
    disposition : string option;
        (** The disposition of the Content-Disposition field. *)
    cid : string option;  (** The value of the Content-ID field. *)
    language : string list option;
        (** The values of the Content-Language field. *)
    location : string option;  (** The value of the Content-Location field. *)
    sub_parts : t list option;
        (** The parts of a multipart part, [None] for any other part. *)
    unknown : Proto_unknown.t;
        (** The members not defined above, kept verbatim. A client may name
            [header:*] properties in [bodyProperties] (RFC 8621 Section 4.1.4)
            and the server returns them on each part, where they land here. See
            {!header_property}. *)
  }
  (** The type for EmailBodyPart objects. *)

  val v :
    ?part_id:string ->
    ?blob_id:Proto_id.t ->
    ?size:int64 ->
    ?headers:Mail_header.t list ->
    ?name:string ->
    ?type_:string ->
    ?charset:string ->
    ?disposition:string ->
    ?cid:string ->
    ?language:string list ->
    ?location:string ->
    ?sub_parts:t list ->
    ?unknown:Proto_unknown.t ->
    unit ->
    t
  (** [v ()] is a body part with only the properties given set, every other one
      being [None] and left out of the JSON. [unknown] defaults to
      {!Jmap.Proto.Unknown.empty}. A part of an Email being created needs no
      more than [part_id], naming an entry of [bodyValues], and [type_], as in
      [v ~part_id:"1" ~type_:"text/plain" ()]. *)

  val unknown_member : t -> string -> Jsont.json option
  (** [unknown_member p name] is the value of the member [name] of [p] that is
      not one of the properties above, or [None] if [p] has no such member. *)

  val header_property : t -> string -> string option
  (** [header_property p name] is the string value of the [header:*] property
      [name] of [p], as in [header_property p "header:Content-Type"]. [name]
      must be given exactly as it was asked for, form suffix included. A form
      whose value is not a string, such as [asAddresses], is [None] here and is
      reached with {!unknown_member} instead. *)

  val jsont : t Jsont.t
  (** [jsont] is the codec for an EmailBodyPart. Members it does not define are
      kept in {!field-unknown}. *)
end

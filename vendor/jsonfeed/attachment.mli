(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Attachments for JSON Feed items.

    An attachment represents an external resource related to a feed item, such
    as audio files for podcasts, video files, or other downloadable content.
    Attachments with identical titles indicate alternate formats of the same
    resource.

    @see <https://www.jsonfeed.org/version/1.1/> JSON Feed Specification *)

type t
(** The type representing an attachment. *)

(** {1 Unknown Fields} *)

module Unknown : sig
  type t = Jsont.json
  (** Unknown/unrecognized JSON object members as a generic JSON object. Useful
      for preserving fields from custom extensions or future spec versions. *)

  val empty : t
  (** [empty] is the empty list of unknown fields. *)

  val is_empty : t -> bool
  (** [is_empty u] returns [true] if there are no unknown fields. *)
end

(** {1 Jsont Type} *)

val jsont : t Jsont.t
(** Declarative JSON type for attachments.

    Maps JSON objects with "url" (required), "mime_type" (required), and
    optional "title", "size_in_bytes", "duration_in_seconds" fields. *)

(** {1 Construction} *)

val create :
  url:string ->
  mime_type:string ->
  ?title:string ->
  ?size_in_bytes:int64 ->
  ?duration_in_seconds:int ->
  ?unknown:Unknown.t ->
  unit ->
  t
(** [create ~url ~mime_type ?title ?size_in_bytes ?duration_in_seconds ?unknown
     ()] creates an attachment object.

    @param url The location of the attachment (required)
    @param mime_type
      The MIME type of the attachment, e.g. ["audio/mpeg"] (required)
    @param title
      The name of the attachment; identical titles indicate alternate formats of
      the same resource
    @param size_in_bytes The size of the attachment file in bytes
    @param duration_in_seconds
      The duration of the attachment in seconds (for audio/video)
    @param unknown Unknown/custom fields for extensions (default: empty)

    {b Examples:}
    {[
      (* Simple attachment *)
      let att =
        Attachment.create ~url:"https://example.com/episode.mp3"
          ~mime_type:"audio/mpeg" ()

      (* Podcast episode with metadata *)
      let att =
        Attachment.create ~url:"https://example.com/episode.mp3"
          ~mime_type:"audio/mpeg" ~title:"Episode 42" ~size_in_bytes:15_728_640L
          ~duration_in_seconds:1800 ()
    ]} *)

(** {1 Accessors} *)

val url : t -> string
(** [url t] returns the attachment's URL. *)

val mime_type : t -> string
(** [mime_type t] returns the attachment's MIME type. *)

val title : t -> string option
(** [title t] returns the attachment's title, if set. *)

val size_in_bytes : t -> int64 option
(** [size_in_bytes t] returns the attachment's size in bytes, if set. *)

val duration_in_seconds : t -> int option
(** [duration_in_seconds t] returns the attachment's duration, if set. *)

val unknown : t -> Unknown.t
(** [unknown t] returns unrecognized fields from the JSON. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests equality between two attachments. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] pretty prints an attachment to the formatter.

    The output is human-readable and suitable for debugging.

    {b Example output:}
    {v episode.mp3 (audio/mpeg, 15.0 MB, 30m0s) v} *)

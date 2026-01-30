(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** JSON Feed format parser and serializer.

    @see <https://www.jsonfeed.org/version/1.1/> JSON Feed Specification *)

type t
(** The type representing a complete JSON Feed. *)

val jsont : t Jsont.t
(** Declarative type that describes the structure of JSON Feeds.

    Maps the complete JSON Feed 1.1 specification including all required and
    optional fields. *)

module Unknown : sig
  type t = Jsont.json
  (** Unknown or unrecognized JSON object members as a generic JSON object.
      Useful for preserving fields from custom extensions or future spec
      versions. *)

  val empty : t
  (** [empty] is the empty list of unknown fields. *)

  val is_empty : t -> bool
  (** [is_empty u] returns [true] if there are no unknown fields. *)
end

(** {1 Construction} *)

val create :
  title:string ->
  ?home_page_url:string ->
  ?feed_url:string ->
  ?description:string ->
  ?user_comment:string ->
  ?next_url:string ->
  ?icon:string ->
  ?favicon:string ->
  ?authors:Author.t list ->
  ?language:string ->
  ?expired:bool ->
  ?hubs:Hub.t list ->
  items:Item.t list ->
  ?unknown:Unknown.t ->
  unit ->
  t
(** [create ~title ~items ()] creates a new JSON Feed.

    @param title
      The name of the feed. Required field that should be plain text, not HTML.
    @param home_page_url
      The URL of the resource that the feed describes. This resource may or may
      not actually be a "home" page, but it should be an HTML page. If a feed is
      for a podcast, for instance, the home_page_url would be the URL for the
      podcast's website.
    @param feed_url
      The URL of the feed itself. This is the URL that was requested to get this
      JSON Feed response. Helps feed readers to determine when they're being
      redirected. Strongly recommended for feeds.
    @param description
      A plain text description of the feed, for human consumption. May contain
      some formatting (like newlines).
    @param user_comment
      A description of the purpose of the feed, for a person looking at the raw
      JSON. This is for the publisher's use only, not intended to be displayed
      to the user.
    @param next_url
      The URL of a feed that provides the next n items, where n is determined by
      the publisher. Used for pagination. A feed reader may continue to request
      the URLs in next_url until it reaches a feed without a next_url.
    @param icon
      The URL of an image for the feed suitable to be used in a timeline, much
      the way an avatar might be used. Should be square and relatively large -
      such as 512 x 512 pixels - and may be cropped to a circle or rounded
      corners. Should not be transparent.
    @param favicon
      The URL of an image for the feed suitable to be used in a source list.
      Should be square and relatively small - such as 64 x 64 pixels. Should not
      be transparent.
    @param authors
      Specifies one or more feed authors. The author object has several members
      (name, url, avatar) which are all optional, but at least one must be
      present for the object to be valid.
    @param language
      The primary language for the feed in RFC 5646 format. The value can be a
      language tag such as "en" or "en-US", or a language-region combination.
    @param expired
      Whether or not the feed is finished - that is, whether or not it will ever
      update again. A feed for a temporary event, like an instance of a
      conference, may expire. If the value is [true], feed readers may stop
      checking for updates.
    @param hubs
      Endpoints that can be used to subscribe to real-time notifications from
      the publisher of this feed. Each hub object has a type (such as "rssCloud"
      or "WebSub") and url.
    @param items
      The items in the feed. Required field, though it may be an empty array.
    @param unknown
      Unknown JSON object members preserved from parsing. Useful for custom
      extensions. *)

(** {1 Accessors} *)

val version : t -> string
(** [version feed] returns the URL of the version of the format the feed uses.
    This will always be "https://jsonfeed.org/version/1.1" for feeds created
    with this library. This is a required field in the JSON Feed spec. *)

val title : t -> string
(** [title feed] returns the name of the feed. This is plain text and should not
    contain HTML. This is a required field. *)

val home_page_url : t -> string option
(** [home_page_url feed] returns the URL of the resource that the feed
    describes. This resource may or may not actually be a "home" page, but it
    should be an HTML page. For instance, if a feed is for a podcast, the
    home_page_url would be the URL for the podcast's website. *)

val feed_url : t -> string option
(** [feed_url feed] returns the URL of the feed itself. This should be the URL
    that was requested to get this JSON Feed response. It helps feed readers
    determine when they're being redirected. This is strongly recommended for
    feeds. *)

val description : t -> string option
(** [description feed] returns a plain text description of the feed, for human
    consumption. This field may contain some formatting such as newlines. *)

val user_comment : t -> string option
(** [user_comment feed] returns a description of the purpose of the feed, for a
    person looking at the raw JSON. This is for the publisher's use only and is
    not intended to be displayed to end users. *)

val next_url : t -> string option
(** [next_url feed] returns the URL of a feed that provides the next n items,
    where n is determined by the publisher. This is used for pagination. A feed
    reader may continue to request the URLs in next_url until it reaches a feed
    without a next_url. *)

val icon : t -> string option
(** [icon feed] returns the URL of an image for the feed suitable to be used in
    a timeline, much the way an avatar might be used. It should be square and
    relatively large (such as 512 x 512 pixels) and may be cropped to a circle
    or rounded corners by feed readers. It should not be transparent. *)

val favicon : t -> string option
(** [favicon feed] returns the URL of an image for the feed suitable to be used
    in a source list. It should be square and relatively small (such as 64 x 64
    pixels) and should not be transparent. *)

val authors : t -> Author.t list option
(** [authors feed] returns the feed authors. Each author object has several
    members (name, url, avatar) which are all optional, but at least one must be
    present for the object to be valid. If a feed has multiple authors, they
    should all be listed here. *)

val language : t -> string option
(** [language feed] returns the primary language for the feed in RFC 5646
    format. The value can be a language tag such as "en" or "en-US", or a
    language-region combination. This field helps feed readers present the feed
    in the appropriate language. *)

val expired : t -> bool option
(** [expired feed] returns whether the feed is finished - that is, whether it
    will ever update again. A feed for a temporary event, like an instance of a
    conference, may expire. If the value is [Some true], feed readers may stop
    checking for updates. *)

val hubs : t -> Hub.t list option
(** [hubs feed] returns endpoints that can be used to subscribe to real-time
    notifications from the publisher of this feed. Each hub object has a type
    (such as "rssCloud" or "WebSub") and a url. Feed readers can use these to
    get immediate updates when new items are published. *)

val items : t -> Item.t list
(** [items feed] returns the array of items in the feed. This is a required
    field, though it may be an empty list. Items represent the individual
    entries in the feed - blog posts, podcast episodes, microblog posts, etc. *)

val unknown : t -> Unknown.t
(** [unknown feed] returns any unknown JSON object members that were preserved
    during parsing. This is useful for custom extensions or fields from future
    versions of the spec. *)

(** {1 Encoding and Decoding} *)

val decode :
  ?layout:bool ->
  ?locs:bool ->
  ?file:string ->
  Bytesrw.Bytes.Reader.t ->
  (t, Jsont.Error.t) result
(** [decode r] decodes a JSON Feed from bytesrw reader [r].

    @param layout Preserve whitespace for round-tripping (default: false)
    @param locs Track locations for better error messages (default: false)
    @param file Source file name for error reporting *)

val decode_string :
  ?layout:bool ->
  ?locs:bool ->
  ?file:string ->
  string ->
  (t, Jsont.Error.t) result
(** [decode_string s] decodes a JSON Feed from string [s]. *)

val encode :
  ?format:Jsont.format ->
  ?number_format:Jsont.number_format ->
  t ->
  eod:bool ->
  Bytesrw.Bytes.Writer.t ->
  (unit, Jsont.Error.t) result
(** [encode feed w] encodes [feed] to bytesrw writer [w].

    @param format
      Output formatting: [Jsont.Minify] or [Jsont.Indent] (default: Minify)
    @param number_format Printf format for numbers (default: "%.16g")
    @param eod Write end-of-data marker *)

val encode_string :
  ?format:Jsont.format ->
  ?number_format:Jsont.number_format ->
  t ->
  (string, Jsont.Error.t) result
(** [encode_string feed] encodes [feed] to a string. *)

val of_string : string -> (t, Jsont.Error.t) result
(** Alias for [decode_string] with default options. *)

val to_string : ?minify:bool -> t -> (string, Jsont.Error.t) result
(** [to_string feed] encodes [feed] to string.
    @param minify Use compact format (true) or indented (false, default) *)

(** {1 Validation} *)

val validate : t -> (unit, string list) result
(** [validate feed] validates the feed structure. Checks for unique item IDs,
    valid content, etc. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests equality between two feeds. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
val pp_summary : Format.formatter -> t -> unit

(** {1 Submodules} *)

module Rfc3339 = Rfc3339
module Cito = Cito
module Author = Author
module Attachment = Attachment
module Hub = Hub
module Reference = Reference
module Item = Item

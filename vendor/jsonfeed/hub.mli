(*---------------------------------------------------------------------------
   Copyright (c) 2024 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hub endpoints for real-time notifications.

    Hubs describe endpoints that can be used to subscribe to real-time
    notifications of changes to the feed. This is an optional and rarely-used
    feature of JSON Feed, primarily for feeds that update frequently.

    @see <https://www.jsonfeed.org/version/1.1/> JSON Feed Specification *)

type t
(** The type representing a hub endpoint. *)

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
(** Declarative JSON type for hubs.

    Maps JSON objects with "type" and "url" fields (both required). *)

(** {1 Construction} *)

val create : type_:string -> url:string -> ?unknown:Unknown.t -> unit -> t
(** [create ~type_ ~url ?unknown ()] creates a hub object.

    @param type_ The type of hub protocol (e.g., ["rssCloud"], ["WebSub"])
    @param url The URL endpoint for the hub
    @param unknown Unknown/custom fields for extensions (default: empty)

    {b Example:}
    {[
      let hub =
        Hub.create ~type_:"WebSub" ~url:"https://pubsubhubbub.appspot.com/" ()
    ]} *)

(** {1 Accessors} *)

val type_ : t -> string
(** [type_ t] returns the hub's protocol type. *)

val url : t -> string
(** [url t] returns the hub's endpoint URL. *)

val unknown : t -> Unknown.t
(** [unknown t] returns unrecognized fields from the JSON. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests equality between two hubs. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] pretty prints a hub to the formatter.

    The output is human-readable and suitable for debugging.

    {b Example output:}
    {v WebSub: https://pubsubhubbub.appspot.com/ v} *)

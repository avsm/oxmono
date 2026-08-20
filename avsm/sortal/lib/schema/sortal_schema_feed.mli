(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Feed subscription with type and URL.

    A feed represents a subscription to a content source (Atom, RSS, JSONFeed,
    or Manual discovery via Claude). *)

type t : immutable_data
(** A feed subscription. Every field is a string, an option over one, a bool or
    a {!feed_type}, so the kind is honest and a feed may be read by a portable
    function. *)

(** Feed type identifier. *)
type feed_type =
  | Atom    (** Atom feed format *)
  | Rss     (** RSS feed format *)
  | Json    (** JSON Feed format *)
  | Manual  (** Manual feed discovery via Claude *)

(** [make ~feed_type ~url ?name ?hint ?paused ()] creates a new feed.

    @param feed_type The type of feed (Atom, RSS, JSON, or Manual)
    @param url The feed URL (for Manual feeds, the page to scrape)
    @param name Optional human-readable name/label for the feed
    @param hint Optional hint text to guide Manual feed discovery
    @param paused Whether sync should skip this feed. Defaults to [false]. *)
val make : feed_type:feed_type -> url:string -> ?name:string -> ?hint:string -> ?paused:bool -> unit -> t

(** [feed_type t] returns the feed type. *)
val feed_type : t -> feed_type

(** [url t] returns the feed URL. *)
val url : t -> string

(** [name t] returns the feed name if set. *)
val name : t -> string option

(** [hint t] returns the discovery hint if set (used for Manual feeds). *)
val hint : t -> string option

(** [paused t] is [true] if sync should skip [t] without fetching. *)
val paused : t -> bool

(** [set_name t name] returns a new feed with the name updated. *)
val set_name : t -> string -> t

(** [set_feed_type t feed_type] is [t] with its feed type replaced. Used
    internally by sync to work with a feed's detected format without
    rewriting the contact's recorded type. *)
val set_feed_type : t -> feed_type -> t

(** [set_paused t paused] is [t] with its [paused] flag replaced. *)
val set_paused : t -> bool -> t

(** [feed_type_to_string ft] converts a feed type to a string. *)
val feed_type_to_string : feed_type -> string

(** [feed_type_of_string s] parses a feed type from a string.
    Returns [None] if the string is not recognized. *)
val feed_type_of_string : string -> feed_type option

(** [json_t] is the jsont encoder/decoder for feeds. The [paused] member
    is written only when [true], so an unpaused feed round-trips to
    exactly the bytes it was read from. *)
val json_t : t Jsont.t

(** [pp ppf t] pretty prints a feed. *)
val pp : Format.formatter -> t -> unit

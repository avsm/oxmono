(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Feed subscription with type and URL.

    A feed represents a subscription to a content source (Atom, RSS, JSONFeed,
    or Manual discovery via Claude). *)

type t

(** Feed type identifier. *)
type feed_type =
  | Atom    (** Atom feed format *)
  | Rss     (** RSS feed format *)
  | Json    (** JSON Feed format *)
  | Manual  (** Manual feed discovery via Claude *)

(** [make ~feed_type ~url ?name ?hint ()] creates a new feed.

    @param feed_type The type of feed (Atom, RSS, JSON, or Manual)
    @param url The feed URL (for Manual feeds, the page to scrape)
    @param name Optional human-readable name/label for the feed
    @param hint Optional hint text to guide Manual feed discovery *)
val make : feed_type:feed_type -> url:string -> ?name:string -> ?hint:string -> unit -> t

(** [feed_type t] returns the feed type. *)
val feed_type : t -> feed_type

(** [url t] returns the feed URL. *)
val url : t -> string

(** [name t] returns the feed name if set. *)
val name : t -> string option

(** [hint t] returns the discovery hint if set (used for Manual feeds). *)
val hint : t -> string option

(** [set_name t name] returns a new feed with the name updated. *)
val set_name : t -> string -> t

(** [feed_type_to_string ft] converts a feed type to a string. *)
val feed_type_to_string : feed_type -> string

(** [feed_type_of_string s] parses a feed type from a string.
    Returns [None] if the string is not recognized. *)
val feed_type_of_string : string -> feed_type option

(** [json_t] is the jsont encoder/decoder for feeds. *)
val json_t : t Jsont.t

(** [pp ppf t] pretty prints a feed. *)
val pp : Format.formatter -> t -> unit

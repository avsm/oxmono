(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Feed subscription with type and URL.

    A feed represents a subscription to a content source (Atom, RSS, or JSONFeed). *)

type t

(** Feed type identifier. *)
type feed_type =
  | Atom  (** Atom feed format *)
  | Rss   (** RSS feed format *)
  | Json  (** JSON Feed format *)

(** [make ~feed_type ~url ?name ()] creates a new feed.

    @param feed_type The type of feed (Atom, RSS, or JSON)
    @param url The feed URL
    @param name Optional human-readable name/label for the feed *)
val make : feed_type:feed_type -> url:string -> ?name:string -> unit -> t

(** [feed_type t] returns the feed type. *)
val feed_type : t -> feed_type

(** [url t] returns the feed URL. *)
val url : t -> string

(** [name t] returns the feed name if set. *)
val name : t -> string option

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

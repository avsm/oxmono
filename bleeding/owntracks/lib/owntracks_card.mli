(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Card message type for OwnTracks.

    @canonical Owntracks.Card

    Provides user information for display. Cards allow users to share their name
    and photo with others tracking their location. The tracker ID must match the
    location message's tid to associate the card with the correct user. *)

type t : immutable_data
(** The type for card messages. *)

(** {1 Constructors} *)

val v : ?name:string -> ?face:string -> ?tid:string -> unit -> t
(** [v ()] creates a card message with optional fields. *)

(** {1 Accessors} *)

val name : t -> string option
(** [name card] returns the full name of the user, if present. *)

val face : t -> string option
(** [face card] returns the Base64-encoded image (typically JPEG or PNG), if
    present. *)

val tid : t -> string option
(** [tid card] returns the tracker ID that this card belongs to. Must match the
    tid in location messages to be associated correctly. *)

(** {1 JSON Codec} *)

val jsont : t Jsont.t
(** [jsont] is a JSON codec for card messages. Expects the ["_type"] field to be
    ["card"]. *)

val jsont_bare : t Jsont.t
(** [jsont_bare] is a JSON codec that doesn't require the ["_type"] field. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf card] pretty-prints a card message. *)

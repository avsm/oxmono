@@ portable

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Zulip message flags.

    Flags record per-message state and server annotations. Unknown wire
    spellings remain representable in {!type-t}. *)

(** {1 Flag Types} *)

type modifiable =
  [ `Read  (** The message has been read. *)
  | `Starred  (** The message is starred. *)
  | `Collapsed  (** The message content is collapsed. *) ]
(** The type for flags that a user can modify directly. *)

type t =
  [ modifiable
  | `Mentioned  (** The user was \@-mentioned in the message. *)
  | `Wildcard_mentioned
    (** The user was mentioned through \@all or \@everyone. *)
  | `Has_alert_word  (** The message contains one of the user's alert words. *)
  | `Historical  (** Message predates user joining the stream. *)
  | `Other of string  (** The server supplied an unknown flag spelling. *) ]
(** The type for all message flags. *)

(** {1 Conversion} *)

val to_string : [< t ] -> string
(** [to_string flag] is the wire spelling of [flag]. *)

val of_string : string -> t
(** [of_string value] is the flag encoded by [value]. Unknown values produce
    [`Other value]. *)

val modifiable_of_string : string -> modifiable option
(** [modifiable_of_string value] is the modifiable flag encoded by [value], or
    [None] if [value] is not modifiable. *)

(** {1 Flag Update Operations} *)

(** The type for message flag update operations. *)
type op =
  | Add  (** The flag is added to messages. *)
  | Remove  (** The flag is removed from messages. *)

val op_to_string : op -> string
(** [op_to_string op] is the lowercase wire spelling of [op]. *)

(** {1 Pretty Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf flag] writes the wire spelling of [flag] to [ppf]. *)

(** {1 JSON Codec} *)

val jsont : t Jsont.t
(** [jsont] is a string codec for all message flags. Unknown spellings decode as
    [`Other value] and encode unchanged. *)

val modifiable_jsont : modifiable Jsont.t
(** [modifiable_jsont] is a string codec for modifiable flags. Unknown and
    nonmodifiable spellings are decoding errors. *)

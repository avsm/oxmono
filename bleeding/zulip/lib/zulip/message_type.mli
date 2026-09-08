@@ portable

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
 SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Message destination kinds.

    Direct messages and channel messages have distinct accepted wire spellings.
*)

type t = [ `Direct | `Channel ]
(** The type for message destination kinds. *)

val to_string : t -> string
(** [to_string kind] is ["direct"] for [`Direct] and ["stream"] for [`Channel].
*)

val of_string : string -> t option
(** [of_string value] is the message kind encoded by [value]. ["direct"] and
    ["private"] decode as [`Direct]. ["stream"] and ["channel"] decode as
    [`Channel]. Other values produce [None]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf kind] writes the canonical wire spelling of [kind] to [ppf]. *)

val jsont : t Jsont.t
(** [jsont] is a codec for message destination kinds. It accepts the spellings
    recognized by {!of_string} and emits the canonical spelling from
    {!to_string}. Other strings are decoding errors. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Zulip notification subjects. *)

val parse : string -> (string * string * string) option
(** [parse subject] is the channel, topic and server of a subject of the form
    [#channel > topic [server]]. The [#] must be the first character of
    [subject] and the [\]] the last, so a subject with anything after the
    bracketed suffix, a trailing space included, is not one of these. The first
    [>] separates the channel and topic; the last [\[] opens the server, so a
    topic can contain brackets but a server cannot contain [\]]. Whitespace
    around each of the three fields is trimmed, and each must be nonempty once
    trimmed. Subjects containing CR or LF are rejected. Parsing takes linear
    time in the length of [subject]. *)

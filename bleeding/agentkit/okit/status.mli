(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Why okit's tools are there or absent.

    okitd works this out when it starts and sends the note in its greeting, and
    the interface shows that same note. The wording lives here so that the line
    a person reads is written once rather than at each end of the protocol. *)

(** What was made of the workspace. *)
type t =
  | No_dune_project  (** No session was attempted. *)
  | Refused of string  (** The session would not start, for this reason. *)
  | Active of { merlin : bool }
      (** The session is up, with or without a merlin to answer beside it. *)

val note : ?flatten:(string -> string) -> t -> string
(** [note t] is the one line that says what okit is doing for this workspace,
    such as ["okit: dune tools active, ocamlmerlin found"].

    [flatten] is applied to the reason of a {!Refused}, and defaults to
    {!Fun.id}. A session's refusal runs to several lines, which is more than a
    note has room for, so a caller showing one passes a function that folds it
    onto a single line. okitd sends the reason whole, since the end that
    displays it is the end that knows how much room it has. *)

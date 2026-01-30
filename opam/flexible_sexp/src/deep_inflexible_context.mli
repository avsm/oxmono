@@ portable

open! Core
open! Import

(** This module supplies some helpers for implementing "deep inflexible" sexp parsing. See
    [Record.Deep_inflexible] or [Variant.Deep_inflexible] for details. *)

(** Run [f] within a "deep inflexible" context.

    - Within [f], we are guaranteed that [am_i_within] will return [true].
    - After [f] returns, we are guaranteed that [am_i_within] will return the same as it
      had returned the moment before [run_within] was called. *)
val run_within : f:(unit -> 'a) @ local unyielding -> 'a

(** Query whether we are inside a "deep inflexible" context. See [run_within].

    If true, flexible_sexp [t_of_sexp] implementations should be inflexible. *)
val am_i_within : unit -> bool

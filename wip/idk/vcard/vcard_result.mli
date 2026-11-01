@@ portable

(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Result combinators.

    The combinators the parsers and validators of this library are written with.
*)

val error : ('a, Format.formatter, unit, ('b, string) result) format4 -> 'a
(** [error fmt ...] is an [Error] whose message is formatted by [fmt]. *)

val check :
  bool -> ('a, Format.formatter, unit, (unit, string) result) format4 -> 'a
(** [check cond fmt ...] is [Ok ()] if [cond] holds and otherwise the [Error] of
    {!error}. The message is formatted only when [cond] is [false]. *)

val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
(** [let*] sequences results, stopping at the first error. *)

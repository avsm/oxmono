(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML parser - converts tokens to semantic events via state machine *)

type t

(** {2 Constructors} *)

val of_string : string -> t
(** Create parser from a string *)

val of_scanner : Scanner.t -> t
(** Create parser from a scanner *)

val of_input : Input.t -> t
(** Create parser from an input source *)

val of_reader : Bytesrw.Bytes.Reader.t -> t
(** Create parser from a Bytes.Reader *)

(** {2 Event Access} *)

val next : t -> Event.spanned option
(** Get next event *)

(** {2 Iteration} *)

val iter : (Event.spanned -> unit) -> t -> unit
(** Iterate over all events *)

val fold : ('a -> Event.spanned -> 'a) -> 'a -> t -> 'a
(** Fold over all events *)

val to_list : t -> Event.spanned list
(** Convert to list of events *)

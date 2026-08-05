(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML tokenizer/scanner with lookahead for ambiguity resolution *)

type t

(** {2 Constructors} *)

val of_string : string -> t
(** Create scanner from a string *)

val of_input : Input.t -> t
(** Create scanner from an input source *)

val of_reader : Bytesrw.Bytes.Reader.t -> t
(** Create scanner from a Bytes.Reader *)

(** {2 Position} *)

val position : t -> Position.t
(** Get current position in input *)

(** {2 Token Access} *)

val next : t -> Token.spanned option
(** Get next token *)

val peek : t -> Token.spanned option
(** Peek at next token without consuming *)

(** {2 Iteration} *)

val iter : (Token.spanned -> unit) -> t -> unit
(** Iterate over all tokens *)

val fold : ('a -> Token.spanned -> 'a) -> 'a -> t -> 'a
(** Fold over all tokens *)

val to_list : t -> Token.spanned list
(** Convert to list of tokens *)

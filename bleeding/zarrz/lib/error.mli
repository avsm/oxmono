(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Errors raised by the library.

    Parsing helpers that a caller may reasonably retry return
    [(_, string) result] instead. The message is wrapped in one of these
    constructors at the point where it becomes fatal. *)

type t =
  | Metadata of string  (** Malformed or unsupported [zarr.json]. *)
  | Store of string  (** A store operation failed. *)
  | Codec of string  (** A codec chain could not be built or run. *)
  | Checksum_mismatch of { expected : int32; got : int32 }
      (** A checksum codec found [got] where the stream declared
          [expected]. *)

exception E of t
(** The exception carrying a {!t}. Every failure of the library is
    reported through it. *)

val raise_ : t -> 'a
(** [raise_ e] raises {!E}[ e]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf e] prints a one line human readable rendering of [e]. *)

val to_string : t -> string
(** [to_string e] is [e] rendered by {!pp}. *)

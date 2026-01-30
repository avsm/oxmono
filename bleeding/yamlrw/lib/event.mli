(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML parser events *)

type t =
  | Stream_start of { encoding : Encoding.t }
  | Stream_end
  | Document_start of { version : (int * int) option; implicit : bool }
  | Document_end of { implicit : bool }
  | Alias of { anchor : string }
  | Scalar of {
      anchor : string option;
      tag : string option;
      value : string;
      plain_implicit : bool;
      quoted_implicit : bool;
      style : Scalar_style.t;
    }
  | Sequence_start of {
      anchor : string option;
      tag : string option;
      implicit : bool;
      style : Layout_style.t;
    }
  | Sequence_end
  | Mapping_start of {
      anchor : string option;
      tag : string option;
      implicit : bool;
      style : Layout_style.t;
    }
  | Mapping_end

type spanned = { event : t; span : Span.t }

val pp : Format.formatter -> t -> unit
(** Pretty-print an event *)

val pp_spanned : Format.formatter -> spanned -> unit
(** Pretty-print a spanned event *)

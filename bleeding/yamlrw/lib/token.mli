(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** YAML token types produced by the scanner *)

type t =
  | Stream_start of Encoding.t
  | Stream_end
  | Version_directive of { major : int; minor : int }
  | Tag_directive of { handle : string; prefix : string }
  | Document_start  (** --- *)
  | Document_end  (** ... *)
  | Block_sequence_start
  | Block_mapping_start
  | Block_entry  (** [-] *)
  | Block_end  (** implicit, from dedent *)
  | Flow_sequence_start  (** \[ *)
  | Flow_sequence_end  (** \] *)
  | Flow_mapping_start  (** \{ *)
  | Flow_mapping_end  (** \} *)
  | Flow_entry  (** , *)
  | Key  (** ? or implicit key *)
  | Value  (** : *)
  | Anchor of string  (** &name *)
  | Alias of string  (** *name *)
  | Tag of { handle : string; suffix : string }
  | Scalar of { style : Scalar_style.t; value : string }

type spanned = { token : t; span : Span.t }

val pp_token : Format.formatter -> t -> unit
(** Pretty-print a token *)

val pp : Format.formatter -> t -> unit
(** Pretty-print a token (alias for pp_token) *)

val pp_spanned : Format.formatter -> spanned -> unit
(** Pretty-print a spanned token *)

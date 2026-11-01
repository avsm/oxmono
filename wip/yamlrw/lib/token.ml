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
  | Flow_entry  (** [,] *)
  | Key  (** ? or implicit key *)
  | Value  (** : *)
  | Anchor of string  (** &name *)
  | Alias of string  (** *name *)
  | Tag of { handle : string; suffix : string }
  | Scalar of { style : Scalar_style.t; value : string }

type spanned = { token : t; span : Span.t }

let pp_token fmt = function
  | Stream_start enc -> Format.fprintf fmt "STREAM-START(%a)" Encoding.pp enc
  | Stream_end -> Format.fprintf fmt "STREAM-END"
  | Version_directive { major; minor } ->
      Format.fprintf fmt "VERSION-DIRECTIVE(%d.%d)" major minor
  | Tag_directive { handle; prefix } ->
      Format.fprintf fmt "TAG-DIRECTIVE(%s, %s)" handle prefix
  | Document_start -> Format.fprintf fmt "DOCUMENT-START"
  | Document_end -> Format.fprintf fmt "DOCUMENT-END"
  | Block_sequence_start -> Format.fprintf fmt "BLOCK-SEQUENCE-START"
  | Block_mapping_start -> Format.fprintf fmt "BLOCK-MAPPING-START"
  | Block_entry -> Format.fprintf fmt "BLOCK-ENTRY"
  | Block_end -> Format.fprintf fmt "BLOCK-END"
  | Flow_sequence_start -> Format.fprintf fmt "FLOW-SEQUENCE-START"
  | Flow_sequence_end -> Format.fprintf fmt "FLOW-SEQUENCE-END"
  | Flow_mapping_start -> Format.fprintf fmt "FLOW-MAPPING-START"
  | Flow_mapping_end -> Format.fprintf fmt "FLOW-MAPPING-END"
  | Flow_entry -> Format.fprintf fmt "FLOW-ENTRY"
  | Key -> Format.fprintf fmt "KEY"
  | Value -> Format.fprintf fmt "VALUE"
  | Anchor name -> Format.fprintf fmt "ANCHOR(%s)" name
  | Alias name -> Format.fprintf fmt "ALIAS(%s)" name
  | Tag { handle; suffix } -> Format.fprintf fmt "TAG(%s, %s)" handle suffix
  | Scalar { style; value } ->
      Format.fprintf fmt "SCALAR(%a, %S)" Scalar_style.pp style value

let pp fmt t = pp_token fmt t

let pp_spanned fmt { token; span } =
  Format.fprintf fmt "%a at %a" pp_token token Span.pp span

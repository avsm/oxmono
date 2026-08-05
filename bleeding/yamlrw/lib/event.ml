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

let pp_opt_str = Option.value ~default:"none"

let pp fmt = function
  | Stream_start { encoding } ->
      Format.fprintf fmt "stream-start(%a)" Encoding.pp encoding
  | Stream_end -> Format.fprintf fmt "stream-end"
  | Document_start { version; implicit } ->
      let version_str =
        match version with
        | None -> "none"
        | Some (maj, min) -> Printf.sprintf "%d.%d" maj min
      in
      Format.fprintf fmt "document-start(version=%s, implicit=%b)" version_str
        implicit
  | Document_end { implicit } ->
      Format.fprintf fmt "document-end(implicit=%b)" implicit
  | Alias { anchor } -> Format.fprintf fmt "alias(%s)" anchor
  | Scalar { anchor; tag; value; style; _ } ->
      Format.fprintf fmt "scalar(anchor=%s, tag=%s, style=%a, value=%S)"
        (pp_opt_str anchor) (pp_opt_str tag) Scalar_style.pp style value
  | Sequence_start { anchor; tag; implicit; style } ->
      Format.fprintf fmt
        "sequence-start(anchor=%s, tag=%s, implicit=%b, style=%a)"
        (pp_opt_str anchor) (pp_opt_str tag) implicit Layout_style.pp style
  | Sequence_end -> Format.fprintf fmt "sequence-end"
  | Mapping_start { anchor; tag; implicit; style } ->
      Format.fprintf fmt
        "mapping-start(anchor=%s, tag=%s, implicit=%b, style=%a)"
        (pp_opt_str anchor) (pp_opt_str tag) implicit Layout_style.pp style
  | Mapping_end -> Format.fprintf fmt "mapping-end"

let pp_spanned fmt { event; span } =
  Format.fprintf fmt "%a at %a" pp event Span.pp span

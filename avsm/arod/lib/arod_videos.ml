(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Video rendering for Arod webserver *)

open Htmlit
open Printf

module MV = Arod_model.Video

let video_for_feed v =
  let md = sprintf "![%%c](:%s)\n\n" v.MV.slug in
  (El.unsafe_raw (Arod_view.md_to_html md), None)

let one_video v =
  let md = sprintf "![%%c](:%s)\n\n%s" v.MV.slug v.MV.description in
  (El.splice [
    Arod_view.entry_href (`Video v);
    El.unsafe_raw (Arod_view.md_to_html md)
  ], None)

let one_video_full v =
  let (html, _word_count_info) = one_video v in
  html

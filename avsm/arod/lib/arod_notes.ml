(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Note rendering for Arod webserver *)

open Htmlit

let note_for_feed n =
  let (body_html, word_count_info) = Arod_view.truncated_body (`Note n) in
  (body_html, word_count_info)

let one_note_brief n =
  let (body_html, word_count_info) = Arod_view.truncated_body (`Note n) in
  (El.splice [
    Arod_view.entry_href (`Note n);
    body_html
  ], word_count_info)

let one_note_full n =
  let body = Arod_model.Note.body n in
  let body_with_ref = match Arod_model.Note.slug_ent n with
    | None -> body
    | Some slug_ent ->
      let parent_ent = Arod_model.lookup_exn slug_ent in
      let parent_title = Arod_model.Entry.title parent_ent in
      body ^ "\n\nRead more about [" ^ parent_title ^ "](:" ^ slug_ent ^ ")."
  in
  El.div ~at:[At.class' "note"] [
    El.unsafe_raw (Arod_view.md_to_html body_with_ref)
  ]

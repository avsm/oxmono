(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

(* A spinner printed once shows its first frame, as the text it prints, and is
   as wide as its widest frame; an empty spinner is refused. *)
let test_frame frames =
  (match Spinner.view ~frames:[] () with
  | _ -> fail "empty spinner accepted"
  | exception Invalid_argument _ -> ());
  match frames with
  | [] -> ()
  | first :: _ ->
      let v = Spinner.view ~frames () in
      let clean =
        List.map (Console_text.sanitize ~keep_newlines:false) frames
      in
      let widest =
        List.fold_left (fun n f -> max n (View.width (View.text f))) 0 clean
      in
      check (View.width v = widest);
      let first = Console_text.sanitize ~keep_newlines:false first in
      check (String.starts_with ~prefix:first (View.to_string v))

let test_spinner_room n frames =
  match frames with
  | [] -> ()
  | _ ->
      let width = 1 + (n mod 200) in
      let v = View.sized ~width (Spinner.view ~frames ()) in
      check (View.width v <= width)

let suite =
  ( "spinner",
    [
      test_case "custom spinners keep to their room"
        [ uint8; list bytes ]
        test_spinner_room;
      test_case "frame" [ list bytes ] test_frame;
    ] )

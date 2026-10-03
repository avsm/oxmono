(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains sub s = Re.execp Re.(compile (str sub)) s

(* The DOS preset is ASCII through and through: a blocky bar, the ASCII spinner
   and tree guide, a [+] done marker and an [OK]/[FAIL] result banner. *)
let test_dos () =
  let t = Theme.dos in
  Alcotest.(check bool) "blocky bar" true (Theme.bar t = `Blocky);
  Alcotest.(check string) "done marker" "[+]" (Theme.done_marker t);
  Alcotest.(check string) "ok marker" "[OK]" (Theme.ok_marker t);
  Alcotest.(check string) "fail marker" "[FAIL]" (Theme.fail_marker t);
  Alcotest.(check int)
    "ascii spinner has four frames" 4
    (Array.length (Spinner.frames (Theme.spinner t)));
  Alcotest.(check string)
    "ascii guide branch" "+-- "
    (Guide.branch (Theme.guide t))

(* The unicode preset uses a smooth bar, the braille spinner and check-mark
   verdict markers. *)
let test_unicode () =
  let t = Theme.unicode in
  Alcotest.(check bool) "smooth bar" true (Theme.bar t = `Smooth);
  Alcotest.(check string) "check-mark done" "✓" (Theme.done_marker t);
  Alcotest.(check string) "tick ok" "✓" (Theme.ok_marker t);
  Alcotest.(check string) "cross fail" "✗" (Theme.fail_marker t);
  Alcotest.(check int)
    "braille spinner has ten frames" 10
    (Array.length (Spinner.frames (Theme.spinner t)))

(* The rainbow preset uses the gradient bar and cycles the spectrum. *)
let test_rainbow () =
  let t = Theme.rainbow in
  Alcotest.(check bool) "rainbow bar" true (Theme.bar t = `Rainbow);
  Alcotest.(check int)
    "palette of six colours" 6
    (List.length (Theme.palette t))

(* Only the matrix preset asks for an animated (rain) border. *)
let test_animated_border () =
  Alcotest.(check bool)
    "matrix animates" true
    (Theme.animated_border Theme.matrix);
  Alcotest.(check bool) "dos still" false (Theme.animated_border Theme.dos);
  Alcotest.(check bool)
    "unicode still" false
    (Theme.animated_border Theme.unicode);
  Alcotest.(check bool)
    "rainbow still" false
    (Theme.animated_border Theme.rainbow)

(* [default] is the restrained Unicode preset. *)
let test_default () =
  Alcotest.(check bool)
    "default is unicode" true
    (Theme.equal Theme.default Theme.unicode)

(* [v] keeps the given fields and fills the rest with modern defaults: a cyan
   accent, a smooth bar, the unicode guide and [OK]/[FAIL] markers. *)
let test_v_defaults () =
  let t = Theme.v ~spinner:Spinner.ascii ~done_marker:"x" () in
  Alcotest.(check string) "given done marker" "x" (Theme.done_marker t);
  Alcotest.(check bool)
    "default cyan accent" true
    (Color.equal (Theme.accent t) Color.cyan);
  Alcotest.(check bool) "default smooth bar" true (Theme.bar t = `Smooth);
  Alcotest.(check string)
    "default unicode guide" "├── "
    (Guide.branch (Theme.guide t));
  Alcotest.(check bool)
    "row separators are opt-in" false
    (Theme.table_row_separators t);
  let separated =
    Theme.v ~table_row_separators:true ~spinner:Spinner.ascii ~done_marker:"x"
      ()
  in
  Alcotest.(check bool)
    "row separators participate in theme identity" false
    (Theme.equal t separated);
  Alcotest.(check string) "default ok marker" "[OK]" (Theme.ok_marker t);
  Alcotest.(check string) "default fail marker" "[FAIL]" (Theme.fail_marker t)

(* [with_verdict] replaces only the verdict markers, leaving the rest intact. *)
let test_with_verdict () =
  let t = Theme.with_verdict ~ok:"PASS" ~fail:"NOPE" Theme.dos in
  Alcotest.(check string) "ok replaced" "PASS" (Theme.ok_marker t);
  Alcotest.(check string) "fail replaced" "NOPE" (Theme.fail_marker t);
  Alcotest.(check string) "done marker untouched" "[+]" (Theme.done_marker t);
  (* an omitted field keeps the original marker *)
  let t2 = Theme.with_verdict ~ok:"Y" Theme.dos in
  Alcotest.(check string)
    "fail kept when omitted" "[FAIL]" (Theme.fail_marker t2)

(* The display draws six markers besides the verdict pair, and every preset
   draws the same six: they were hard-coded in the display until the theme took
   them, so a preset that moved one would silently move what a shipped command
   prints. *)
let test_every_preset_draws_the_same_markers () =
  let markers name t =
    let check = Alcotest.(check string) in
    check (name ^ ": note") "#" (Theme.note_marker t);
    check (name ^ ": warn") "!" (Theme.warn_marker t);
    check (name ^ ": error") "x" (Theme.error_marker t);
    check (name ^ ": succeeded") "=>" (Theme.committed_marker t `Succeeded);
    check (name ^ ": failed") "!!" (Theme.committed_marker t `Failed);
    check (name ^ ": cancelled") "--" (Theme.committed_marker t `Cancelled)
  in
  markers "dos" Theme.dos;
  markers "unicode" Theme.unicode;
  markers "matrix" Theme.matrix;
  markers "rainbow" Theme.rainbow;
  markers "v" (Theme.v ~spinner:Spinner.ascii ~done_marker:"x" ())

(* A marker is part of what a theme is: two themes that draw a warning or a
   committed failure differently are two themes, whatever else they share. *)
let test_markers_participate_in_identity () =
  let t = Theme.v ~spinner:Spinner.ascii ~done_marker:"x" () in
  Alcotest.(check bool)
    "a log marker distinguishes" false
    (Theme.equal t
       (Theme.v ~warn_marker:"W" ~spinner:Spinner.ascii ~done_marker:"x" ()));
  Alcotest.(check bool)
    "a committed marker distinguishes" false
    (Theme.equal t (Theme.with_verdict ~committed_failed:"F" t));
  Alcotest.(check string)
    "and the replacement is what it says" "F"
    (Theme.committed_marker
       (Theme.with_verdict ~committed_failed:"F" t)
       `Failed);
  Alcotest.(check string)
    "while its siblings are left alone" "--"
    (Theme.committed_marker
       (Theme.with_verdict ~committed_failed:"F" t)
       `Cancelled)

(* [pp] summarises the markers; the rendered text mentions each. *)
let test_pp () =
  let s = Fmt.str "%a" Theme.pp Theme.dos in
  Alcotest.(check bool) "mentions the done marker" true (contains "[+]" s);
  Alcotest.(check bool) "mentions the ok marker" true (contains "[OK]" s);
  Alcotest.(check bool) "mentions the warn marker" true (contains "warn = !" s);
  Alcotest.(check bool)
    "mentions the committed cancel marker" true
    (contains "cancelled = --" s)

let suite =
  ( "theme",
    [
      Alcotest.test_case "dos preset" `Quick test_dos;
      Alcotest.test_case "unicode preset" `Quick test_unicode;
      Alcotest.test_case "rainbow preset" `Quick test_rainbow;
      Alcotest.test_case "animated border" `Quick test_animated_border;
      Alcotest.test_case "default" `Quick test_default;
      Alcotest.test_case "v defaults" `Quick test_v_defaults;
      Alcotest.test_case "with_verdict" `Quick test_with_verdict;
      Alcotest.test_case "every preset draws the same markers" `Quick
        test_every_preset_draws_the_same_markers;
      Alcotest.test_case "markers participate in identity" `Quick
        test_markers_participate_in_identity;
      Alcotest.test_case "pp" `Quick test_pp;
    ] )

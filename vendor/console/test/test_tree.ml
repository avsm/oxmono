(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let str = Alcotest.(check string)

(* Each strict ancestor is a vertical line if siblings follow it and spaces
   if not, and the node is a branch or the last one. *)
let test_guides () =
  let leaf s = Tree.v (View.text s) [] in
  let tree =
    Tree.v (View.text "r")
      [
        Tree.v (View.text "a")
          [ leaf "a1"; Tree.v (View.text "a2") [ leaf "x" ] ];
        Tree.v (View.text "b") [ leaf "b1" ];
      ]
  in
  str "unicode"
    "r\n├── a\n│   ├── a1\n│   └── a2\n│       └── x\n└── b\n    └── b1"
    (View.to_string (Tree.view tree));
  let tree =
    Tree.v (View.text "r") [ Tree.v (View.text "a") [ leaf "a1" ]; leaf "b" ]
  in
  str "ascii" "r\n+-- a\n|   +-- a1\n+-- b"
    (View.to_string (Tree.view ~guide:Guide.ascii tree))

(* A multi-level tree renders one node per line, each ancestor's branch drawn
   once. *)
let test_render_exact () =
  let n label children = Tree.v (View.text label) children in
  let tree = n "root" [ n "a" [ n "a1" []; n "a2" [] ]; n "b" [ n "b1" [] ] ] in
  let expected =
    String.concat "\n"
      [ "root"; "├── a"; "│   ├── a1"; "│   └── a2"; "└── b"; "    └── b1" ]
  in
  str "exact tree" expected (View.to_string (Tree.view tree))

let test_render_single () =
  let tree = Tree.v (View.text "root") [ Tree.v (View.text "child") [] ] in
  str "single child" "root\n└── child" (View.to_string (Tree.view tree))

(* The formatter's margin is a ceiling on every row: a label wider than the
   room left by its guide wraps, hanging under the text after its first word,
   and the guide of its level runs on beside the continuation. *)
let test_margin_wraps () =
  let n label children = Tree.v (View.text label) children in
  let tree = n "root" [ n "a" [ n "leaf label that is long" [] ]; n "b" [] ] in
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf 20;
  View.pp ppf (Tree.view tree);
  Format.pp_print_flush ppf ();
  str "wrapped tree"
    (String.concat "\n"
       [
         "root";
         "├── a";
         "│   └── leaf label";
         "│            that is";
         "│            long";
         "└── b";
       ])
    (Buffer.contents buf);
  str "to_string is natural"
    "root\n├── a\n│   └── leaf label that is long\n└── b"
    (View.to_string (Tree.view tree))

(* A styled guide is drawn in its style. *)
let test_guide_style () =
  let blue = Color.rgb 0 0 255 in
  let guide = Guide.with_style (Style.fg blue) Guide.single in
  let tree =
    Tree.v (View.text "r")
      [ Tree.v (View.text "a") []; Tree.v (View.text "b") [] ]
  in
  Alcotest.(check string)
    "rows"
    "r\n\
     \027[38;2;0;0;255m\xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80 \027[0ma\n\
     \027[38;2;0;0;255m\xe2\x94\x94\xe2\x94\x80\xe2\x94\x80 \027[0mb"
    (View.to_string ~styled:true (Tree.view ~guide tree));
  Alcotest.(check bool)
    "with_style" true
    (Style.equal (Style.fg blue) (Guide.style guide));
  Alcotest.(check string)
    "plain"
    "r\n\
     \xe2\x94\x9c\xe2\x94\x80\xe2\x94\x80 a\n\
     \xe2\x94\x94\xe2\x94\x80\xe2\x94\x80 b"
    (View.to_string (Tree.view ~guide tree))

let at_margin margin t =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin ppf margin;
  Fmt.pf ppf "%a%!" View.pp (Tree.view t);
  Buffer.contents buf

(* A label wider than the margin wraps, each line after the first under the
   text after its first word; a hang of more than half the width is clamped to
   half. *)
let test_wrap_hang () =
  Alcotest.(check string)
    "hanging" "abcd efgh\n     ijkl\n     mnop"
    (at_margin 10 (Tree.v (View.text "abcd efgh ijkl mnop") []));
  Alcotest.(check string)
    "clamped" "abcdef\n   gh\n   ij"
    (at_margin 7 (Tree.v (View.text "abcdef gh ij") []))

let test_to_string_width () =
  List.iter
    (fun line ->
      Alcotest.(check bool)
        ("within 12 cells: " ^ line)
        true
        (View.width (View.text line) <= 12))
    (String.split_on_char '\n'
       (View.to_string
          (View.sized ~width:12
             (Tree.view
                (Tree.v (View.text "root")
                   [ Tree.v (View.text "a label longer than twelve") [] ])))))

let suite =
  ( "tree",
    [
      Alcotest.test_case "to_string within a width" `Quick test_to_string_width;
      Alcotest.test_case "wrapped label hangs" `Quick test_wrap_hang;
      Alcotest.test_case "guide style" `Quick test_guide_style;
      Alcotest.test_case "guides" `Quick test_guides;
      Alcotest.test_case "render exact" `Quick test_render_exact;
      Alcotest.test_case "render single" `Quick test_render_single;
      Alcotest.test_case "margin wraps" `Quick test_margin_wraps;
    ] )

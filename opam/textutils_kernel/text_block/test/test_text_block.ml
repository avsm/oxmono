open! Core
open! Async
open! Import
open Text_block
module Expect_test_config = Core.Expect_test_config

let yoyoma : t list = [ text "yo"; text "yo"; text "ma" ]

let test t =
  invariant t;
  print_endline (render t)
;;

let example =
  let return_address =
    vcat
      [ text "Kel Varnsen"
      ; text "Vandelay Industries"
      ; text "67 Lantern Dr."
      ; text "Brooklyn, NY 11224"
      ; vsep
      ; text "August 3, 1998"
      ]
  in
  let salutation =
    vcat
      [ text "Sincerely,"
      ; vstrut 4
      ; text "Kel Varnsen"
      ; text "Chief Procurement Officer"
      ]
  in
  let [ return_address; salutation ] =
    With_static_lengths.halign `Left [ return_address; salutation ]
  in
  vcat
    ~align:`Right
    [ return_address
    ; vstrut 4
    ; vcat
        [ text "H.E. Pennypacker"
        ; text "Kramerica Industries"
        ; text "129 W 81st St, Apt 5B"
        ; text "Manhattan, NY 10024"
        ; vsep
        ; text "Dear Mr. Pennypacker:"
        ; vsep
        ; text
            "It has come to my attention that your revolutionary oil tanker\n\
             bladder system makes extensive use of latex and latex products."
        ; vsep
        ; text
            "We at Vandelay Industries are happy to supply you these materials\n\
             at a discounted rate. If you would like to pursue this matter,\n\
             please contact our head of sales, George Costanza at 555-6893."
        ]
    ; vsep
    ; salutation
    ]
;;

let%expect_test "example" =
  print_endline (render example);
  [%expect
    {|
                                            Kel Varnsen
                                            Vandelay Industries
                                            67 Lantern Dr.
                                            Brooklyn, NY 11224

                                            August 3, 1998




    H.E. Pennypacker
    Kramerica Industries
    129 W 81st St, Apt 5B
    Manhattan, NY 10024

    Dear Mr. Pennypacker:

    It has come to my attention that your revolutionary oil tanker
    bladder system makes extensive use of latex and latex products.

    We at Vandelay Industries are happy to supply you these materials
    at a discounted rate. If you would like to pursue this matter,
    please contact our head of sales, George Costanza at 555-6893.

                                            Sincerely,




                                            Kel Varnsen
                                            Chief Procurement Officer
    |}]
;;

let%expect_test _ =
  test (hcat yoyoma);
  [%expect {| yoyoma |}]
;;

let%expect_test _ =
  test (hcat ~sep:(hstrut 1) yoyoma);
  [%expect {| yo yo ma |}]
;;

let%expect_test _ =
  test (hcat ~sep:(hstrut 2) yoyoma);
  [%expect {| yo  yo  ma |}]
;;

let%expect_test _ =
  test (vcat yoyoma);
  [%expect
    {|
    yo
    yo
    ma
    |}]
;;

let%expect_test _ =
  test (vcat ~sep:(vstrut 1) yoyoma);
  [%expect
    {|
    yo

    yo

    ma
    |}]
;;

let%expect_test _ =
  test (vcat ~sep:(vstrut 2) yoyoma);
  [%expect
    {|
    yo


    yo


    ma
    |}]
;;

let sep = text "."

let%expect_test _ =
  test (hcat ~sep [ vcat yoyoma; hcat yoyoma ]);
  [%expect
    {|
    yo.yoyoma
    yo
    ma
    |}]
;;

let%expect_test _ =
  test (hcat ~sep [ hcat yoyoma; vcat yoyoma ]);
  [%expect
    {|
    yoyoma.yo
           yo
           ma
    |}]
;;

let%expect_test _ =
  test (vcat ~sep [ vcat yoyoma; hcat yoyoma ]);
  [%expect
    {|
    yo
    yo
    ma
    .
    yoyoma
    |}]
;;

let%expect_test _ =
  test (vcat ~sep [ hcat yoyoma; vcat yoyoma ]);
  [%expect
    {|
    yoyoma
    .
    yo
    yo
    ma
    |}]
;;

let%expect_test "word wrap" =
  let test ~width str =
    let t = text ~wrap:{ max_width = width; preserve_leading_spaces = false } str in
    let vline = fill '|' ~width:1 ~height:(height t) in
    let hline = hcat [ text "+"; fill '-' ~width ~height:1; text "+" ] in
    test (vcat [ hline; hcat [ vline; vcat [ hstrut width; t ]; vline ]; hline ])
  in
  test
    ~width:30
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
     incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud \
     exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute \
     irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla \
     pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia \
     deserunt mollit anim id est laborum.";
  [%expect
    {|
    +------------------------------+
    |Lorem ipsum dolor sit amet,   |
    |consectetur adipiscing elit,  |
    |sed do eiusmod tempor         |
    |incididunt ut labore et dolore|
    |magna aliqua. Ut enim ad minim|
    |veniam, quis nostrud          |
    |exercitation ullamco laboris  |
    |nisi ut aliquip ex ea commodo |
    |consequat. Duis aute irure    |
    |dolor in reprehenderit in     |
    |voluptate velit esse cillum   |
    |dolore eu fugiat nulla        |
    |pariatur. Excepteur sint      |
    |occaecat cupidatat non        |
    |proident, sunt in culpa qui   |
    |officia deserunt mollit anim  |
    |id est laborum.               |
    +------------------------------+
    |}];
  test
    ~width:33
    "(Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
     incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud \
     exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute \
     irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla \
     pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia \
     deserunt mollit anim id est laborum.)";
  [%expect
    {|
    +---------------------------------+
    |(Lorem ipsum dolor sit amet,     |
    |consectetur adipiscing elit, sed |
    |do eiusmod tempor incididunt ut  |
    |labore et dolore magna aliqua. Ut|
    |enim ad minim veniam, quis       |
    |nostrud exercitation ullamco     |
    |laboris nisi ut aliquip ex ea    |
    |commodo consequat. Duis aute     |
    |irure dolor in reprehenderit in  |
    |voluptate velit esse cillum      |
    |dolore eu fugiat nulla pariatur. |
    |Excepteur sint occaecat cupidatat|
    |non proident, sunt in culpa qui  |
    |officia deserunt mollit anim id  |
    |est laborum.)                    |
    +---------------------------------+
    |}];
  test
    ~width:70
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor \
     incididunt ut labore et dolore magna aliqua.\n\n\
     Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip \
     ex ea commodo consequat.";
  [%expect
    {|
    +----------------------------------------------------------------------+
    |Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do       |
    |eiusmod tempor incididunt ut labore et dolore magna aliqua.           |
    |                                                                      |
    |Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris    |
    |nisi ut aliquip ex ea commodo consequat.                              |
    +----------------------------------------------------------------------+
    |}]
;;

let%expect_test "compress_table_header" =
  let test cols =
    let `Header header, `Rows rows = compress_table_header (`Cols cols) in
    test (vcat (header :: rows))
  in
  test
    [ text "abcdefg", [ text "1"; text "23"; text "456" ], `Right
    ; text "hijk", [ text "123"; text "4567"; text "89" ], `Left
    ; text "lmnop", [ text "12345"; text "678"; text "9" ], `Center
    ];
  [%expect
    {|
    abcdefg
    |    hijk  lmnop
    |    |     |
      1  123   12345
     23  4567   678
    456  89      9
    |}]
;;

(* Here's a trick for labeling an x-axis in an ascii graph by calling
   [compress_table_header] on columns whose only "data" is a single [hstrut] indicating
   how spaced out you'd like your x-axis labels to be. Note that one must pass
   ~sep_width:0 to compress_table_header to get this effect. *)
let%expect_test "number markers" =
  let label_width_pairs =
    [ 100, 4
    ; 251, 2
    ; 398, 2
    ; 631, 2
    ; 1, 3
    ; 2, 4
    ; 5, 3
    ; 10, 3
    ; 20, 3
    ; 40, 2
    ; 63, 2
    ; 100, 4
    ; 251, 2
    ; 398, 3
    ; 794, 1
    ]
  in
  let cols =
    List.map label_width_pairs ~f:(fun (label, width) ->
      sexp [%sexp_of: int] label, [ hstrut width ], `Left)
  in
  let `Header text, _ = compress_table_header ~sep_width:0 (`Cols cols) in
  test text;
  [%expect
    {|
        251
        | 398                 40      251
        | | 631               | 63    | 398
    100 | | | 1  2   5  10 20 | | 100 | |  794
    |   | | | |  |   |  |  |  | | |   | |  |
    |}];
  ()
;;

(* lines with trailing whitespace used to tickle a bug *)

let%expect_test _ =
  test (vcat [ hcat [ text "a"; text " " ]; hcat [ text "b" ] ]);
  [%expect
    {|
    a
    b
    |}]
;;

let%expect_test _ =
  test (vcat [ hcat [ text "a"; text "    " ]; hcat [ text "b" ] ]);
  [%expect
    {|
    a
    b
    |}]
;;

let yellow = ansi_escape ~prefix:"[33m" ~suffix:"[39m"

let%expect_test _ =
  test (yellow (vcat yoyoma));
  [%expect
    {|
    [33myo[39m
    [33myo[39m
    [33mma[39m
    |}]
;;

let%expect_test "unicode" =
  test
    (let contents = vcat [ text "✓ yes"; text "x no" ] in
     let height = height contents in
     hcat
       [ fill '|' ~height ~width:1
       ; space ~height ~width:1
       ; contents
       ; space ~height ~width:1
       ; fill '|' ~height ~width:1
       ]);
  [%expect
    {|
    | ✓ yes |
    | x no  |
    |}]
;;

let%expect_test "fill_uchar" =
  test (fill_uchar (Uchar.of_scalar_exn 0x1f600) ~width:2 ~height:4);
  [%expect
    {|
    😀😀
    😀😀
    😀😀
    😀😀
    |}]
;;

(* [Test_boxed] tests *)
module _ = struct
  let dump x = boxed x |> render |> print_string
  let a = text "A"
  let b = text "B"
  let c = text "C"
  let d = text "D"
  let e = text "E"

  let%expect_test "Basics" =
    dump Boxed.(hcat [ cell a; cell b ]);
    [%expect
      {|
      ┌───┬───┐
      │ A │ B │
      └───┴───┘
      |}];
    dump Boxed.(hcat ~align:`Center [ vcat [ cell a; cell b ]; cell c ]);
    dump
      Boxed.(
        hcat ~align:`Center [ vcat [ cell a; cell b ]; cell c; vcat [ cell d; cell e ] ]);
    dump Boxed.(hcat ~align:`Center [ vcat [ cell a; cell b ]; vcat [ cell d; cell e ] ]);
    [%expect
      {|
      ┌───┬───┐
      │ A │   │
      ├───┤ C │
      │ B │   │
      └───┴───┘
      ┌───┬───┬───┐
      │ A │   │ D │
      ├───┤ C ├───┤
      │ B │   │ E │
      └───┴───┴───┘
      ┌───┬───┐
      │ A │ D │
      ├───┼───┤
      │ B │ E │
      └───┴───┘
      |}];
    dump Boxed.(vcat ~align:`Center [ hcat [ cell a; cell b ]; cell c ]);
    dump
      Boxed.(
        vcat ~align:`Center [ hcat [ cell a; cell b ]; cell c; hcat [ cell d; cell e ] ]);
    dump Boxed.(vcat ~align:`Center [ hcat [ cell a; cell b ]; hcat [ cell d; cell e ] ]);
    [%expect
      {|
      ┌───┬───┐
      │ A │ B │
      ├───┴───┤
      │   C   │
      └───────┘
      ┌───┬───┐
      │ A │ B │
      ├───┴───┤
      │   C   │
      ├───┬───┤
      │ D │ E │
      └───┴───┘
      ┌───┬───┐
      │ A │ B │
      ├───┼───┤
      │ D │ E │
      └───┴───┘
      |}]
  ;;

  let%expect_test "frills are correctly offset for padding" =
    dump
      Boxed.(
        hcat
          ~align:`Center
          [ vcat
              ~align:`Center
              [ hcat [ cell a; cell b ]; cell c; hcat [ cell d; cell e ] ]
          ; vcat ~align:`Center [ cell (text "Top right"); cell (text "Bottom right") ]
          ]);
    [%expect
      {|
      ┌───┬───┬──────────────┐
      │ A │ B │              │
      ├───┴───┤  Top right   │
      │   C   ├──────────────┤
      ├───┬───┤ Bottom right │
      │ D │ E │              │
      └───┴───┴──────────────┘
      |}]
  ;;

  let%expect_test "align" =
    let addr1 = vcat [ text "2½ Devonshire Square"; text "London"; text "EC2M 4UJ" ] in
    let addr2 = vcat [ text "Windsor Castle"; text "Windsor"; text "SL4 1NJ" ] in
    let addr3 =
      vcat
        [ text "The White House"
        ; text "1600 Pennsylvania Av NW"
        ; text "Washington, DC"
        ; text "20500"
        ]
    in
    let test ~dir =
      vcat
        Boxed.(
          let cat1, cat2, cat3 =
            match dir with
            | `Horizontal -> hcat ~align:`Top, hcat ~align:`Center, hcat ~align:`Bottom
            | `Vertical -> vcat ~align:`Left, vcat ~align:`Center, vcat ~align:`Right
          in
          [ boxed (cat1 [ cell (text "A Streeter"); cell addr1 ])
          ; boxed (cat2 [ cell (text "Henry VIII"); cell addr2 ])
          ; boxed (cat3 [ cell (text "A Lincoln"); cell addr3 ])
          ])
      |> render
      |> print_string
    in
    test ~dir:`Horizontal;
    [%expect
      {|
      ┌────────────┬──────────────────────┐
      │ A Streeter │ 2½ Devonshire Square │
      │            │ London               │
      │            │ EC2M 4UJ             │
      └────────────┴──────────────────────┘
      ┌────────────┬────────────────┐
      │            │ Windsor Castle │
      │ Henry VIII │ Windsor        │
      │            │ SL4 1NJ        │
      └────────────┴────────────────┘
      ┌───────────┬─────────────────────────┐
      │           │ The White House         │
      │           │ 1600 Pennsylvania Av NW │
      │           │ Washington, DC          │
      │ A Lincoln │ 20500                   │
      └───────────┴─────────────────────────┘
      |}];
    test ~dir:`Vertical;
    [%expect
      {|
      ┌──────────────────────┐
      │ A Streeter           │
      ├──────────────────────┤
      │ 2½ Devonshire Square │
      │ London               │
      │ EC2M 4UJ             │
      └──────────────────────┘
      ┌────────────────┐
      │   Henry VIII   │
      ├────────────────┤
      │ Windsor Castle │
      │ Windsor        │
      │ SL4 1NJ        │
      └────────────────┘
      ┌─────────────────────────┐
      │               A Lincoln │
      ├─────────────────────────┤
      │ The White House         │
      │ 1600 Pennsylvania Av NW │
      │ Washington, DC          │
      │ 20500                   │
      └─────────────────────────┘
      |}]
  ;;

  let%expect_test "fib" =
    let square n =
      fill ' ' ~width:((4 * n) - 1) ~height:((2 * n) - 1) |> Boxed.cell ~hpadding:0
    in
    let rec nested_boxes ?(horizontal = true) = function
      | [] -> square 1
      | hd :: tl ->
        let cat =
          if horizontal
          then fun a b -> Boxed.hcat [ a; b ]
          else fun a b -> Boxed.vcat [ a; b ]
        in
        cat (square hd) (nested_boxes ~horizontal:(not horizontal) tl)
    in
    dump (nested_boxes [ 2; 1 ]);
    [%expect
      {|
      ┌───────┬───┐
      │       │   │
      │       ├───┤
      │       │   │
      └───────┴───┘
      |}];
    dump (nested_boxes [ 5; 3; 2; 1 ]);
    [%expect
      {|
      ┌───────────────────┬───────────┐
      │                   │           │
      │                   │           │
      │                   │           │
      │                   │           │
      │                   │           │
      │                   ├───────┬───┤
      │                   │       │   │
      │                   │       ├───┤
      │                   │       │   │
      └───────────────────┴───────┴───┘
      |}];
    dump (nested_boxes [ 8; 5; 3; 2; 1 ]);
    [%expect
      {|
      ┌───────────────────────────────┬───────────────────┐
      │                               │                   │
      │                               │                   │
      │                               │                   │
      │                               │                   │
      │                               │                   │
      │                               │                   │
      │                               │                   │
      │                               │                   │
      │                               │                   │
      │                               ├───────────┬───────┤
      │                               │           │       │
      │                               │           │       │
      │                               │           │       │
      │                               │           ├───┬───┤
      │                               │           │   │   │
      └───────────────────────────────┴───────────┴───┴───┘
      |}]
  ;;

  let%expect_test "padding" =
    vcat
      [ hcat
          (List.map [ `Left; `Center; `Right ] ~f:(fun align ->
             boxed
               Boxed.(
                 vcat
                   ~align
                   [ cell (sexp [%sexp_of: [ `Left | `Right | `Center ]] align)
                   ; vcat [ cell (text "A"); cell (text "B") ]
                   ])))
      ; hcat
          (List.map [ `Top; `Center; `Bottom ] ~f:(fun align ->
             boxed
               Boxed.(
                 hcat
                   ~align
                   [ cell
                       ~vpadding:1
                       (sexp [%sexp_of: [ `Top | `Bottom | `Center ]] align)
                   ; hcat [ cell (text "A"); cell (text "B") ]
                   ])))
      ]
    |> render
    |> print_string;
    [%expect
      {|
      ┌──────┐┌────────┐┌───────┐
      │ Left ││ Center ││ Right │
      ├──────┤├────────┤├───────┤
      │ A    ││   A    ││     A │
      ├──────┤├────────┤├───────┤
      │ B    ││   B    ││     B │
      └──────┘└────────┘└───────┘
      ┌─────┬───┬───┐┌────────┬───┬───┐┌────────┬───┬───┐
      │     │ A │ B ││        │   │   ││        │   │   │
      │ Top │   │   ││ Center │ A │ B ││ Bottom │   │   │
      │     │   │   ││        │   │   ││        │ A │ B │
      └─────┴───┴───┘└────────┴───┴───┘└────────┴───┴───┘
      |}]
  ;;
end

let%expect_test "span_banner" =
  let rows =
    let open List.Let_syntax in
    let%bind points = Up_or_down.[ Up; Down ] in
    let%bind extend_left = [ false; true ] in
    let%bind extend_right = [ false; true ] in
    [ extend_left, extend_right, points ]
  in
  let length = 10 in
  let label = text "label" in
  let `Header header, `Rows rows =
    compress_table_header
      (`Cols
        [ ( text "extend_left"
          , List.map rows ~f:(fun (extend_left, _, _) ->
              sexp [%sexp_of: bool] extend_left)
          , `Left )
        ; ( text "extend_right"
          , List.map rows ~f:(fun (_, extend_right, _) ->
              sexp [%sexp_of: bool] extend_right)
          , `Left )
        ; ( text "points"
          , List.map rows ~f:(fun (_, _, points) -> sexp [%sexp_of: Up_or_down.t] points)
          , `Left )
        ; ( text "span_banner"
          , List.map rows ~f:(fun (extend_left, extend_right, points) ->
              let align =
                match points with
                | Up -> `Top
                | Down -> `Bottom
              in
              hcat
                ~align
                [ vstrut 2; span_banner ~extend_left ~extend_right ~points ~length () ])
          , `Left )
        ; ( text "span_banner ~label"
          , List.map rows ~f:(fun (extend_left, extend_right, points) ->
              span_banner ~extend_left ~extend_right ~points ~length ~label ())
          , `Left )
        ])
  in
  test (vcat ~sep:vsep (header :: rows));
  [%expect
    {|
    extend_left
    |      extend_right
    |      |      points
    |      |      |     span_banner
    |      |      |     |           span_banner ~label
    |      |      |     |           |

    false  false  Up    └────────┘  └┬───────┘
                                     label

    false  true   Up    └─────────  └┬────────
                                     label

    true   false  Up    ─────────┘  ─┬───────┘
                                     label

    true   true   Up    ──────────  ─┬────────
                                     label

    false  false  Down               label
                        ┌────────┐  ┌┴───────┐

    false  true   Down               label
                        ┌─────────  ┌┴────────

    true   false  Down               label
                        ─────────┐  ─┴───────┐

    true   true   Down               label
                        ──────────  ─┴────────
    |}];
  ()
;;

let example_html =
  String.strip
    {|
<!DOCTYPE html>
<html>
<head>
    <title>Example HTML</title>
</head>
<body>
    <h1>Welcome to Example HTML</h1>
    <p>This is a paragraph of filler text. It is meant to serve as a placeholder for your actual content. It was generated by a helpful AI assistant.</p>
    <ul>
        <li>Item 1</li>
        <li>Item 2</li>
        <li>Item 3</li>
    </ul>
    <p>Another paragraph of filler text. You can replace this with your own content.</p>
</body>
</html>
|}
;;

let%expect_test "don't preserve leading spaces when wrapping" =
  test
    (Boxed.cell
       (text ~wrap:{ max_width = 80; preserve_leading_spaces = false } example_html)
     |> boxed);
  [%expect
    {|
    ┌──────────────────────────────────────────────────────────────────────────────┐
    │ <!DOCTYPE html>                                                              │
    │ <html>                                                                       │
    │ <head>                                                                       │
    │ <title>Example HTML</title>                                                  │
    │ </head>                                                                      │
    │ <body>                                                                       │
    │ <h1>Welcome to Example HTML</h1>                                             │
    │ <p>This is a paragraph of filler text. It is meant to serve as a placeholder │
    │ for your actual content. It was generated by a helpful AI assistant.</p>     │
    │ <ul>                                                                         │
    │ <li>Item 1</li>                                                              │
    │ <li>Item 2</li>                                                              │
    │ <li>Item 3</li>                                                              │
    │ </ul>                                                                        │
    │ <p>Another paragraph of filler text. You can replace this with your own      │
    │ content.</p>                                                                 │
    │ </body>                                                                      │
    │ </html>                                                                      │
    └──────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "do preserve leading spaces when wrapping" =
  test
    (Boxed.cell
       (text ~wrap:{ max_width = 80; preserve_leading_spaces = true } example_html)
     |> boxed);
  [%expect
    {|
    ┌─────────────────────────────────────────────────────────────────────────────┐
    │ <!DOCTYPE html>                                                             │
    │ <html>                                                                      │
    │ <head>                                                                      │
    │     <title>Example HTML</title>                                             │
    │ </head>                                                                     │
    │ <body>                                                                      │
    │     <h1>Welcome to Example HTML</h1>                                        │
    │     <p>This is a paragraph of filler text. It is meant to serve as a        │
    │ placeholder for your actual content. It was generated by a helpful AI       │
    │ assistant.</p>                                                              │
    │     <ul>                                                                    │
    │         <li>Item 1</li>                                                     │
    │         <li>Item 2</li>                                                     │
    │         <li>Item 3</li>                                                     │
    │     </ul>                                                                   │
    │     <p>Another paragraph of filler text. You can replace this with your own │
    │ content.</p>                                                                │
    │ </body>                                                                     │
    │ </html>                                                                     │
    └─────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "preserve_leading_spaces squashes non-leading spaces" =
  test
    (Boxed.cell
       (text
          ~wrap:{ max_width = 80; preserve_leading_spaces = true }
          "    Lorem ipsum dolor sit amet,         consectetur adipiscing elit, sed do \
           eiusmod  tempor incididunt ut labore et ...")
     |> boxed);
  [%expect
    {|
    ┌─────────────────────────────────────────────────────────────────────────────┐
    │     Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod │
    │ tempor incididunt ut labore et ...                                          │
    └─────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let example_prefix_text = "this is some random prefix text foo bar baz... "

(* taken from the wikipedia page for [supercalifragilisticexpialidocious] *)
let example_with_a_long_word_at_beginning_of_text =
  "supercalifragilisticexpialidocious is a song and single from the 1964 Disney musical \
   film Mary Poppins. It was written by the Sherman Brothers, and sung by Julie Andrews \
   and Dick Van Dyke. It also appears in the 2004 stage show version. Because Mary \
   Poppins was a period piece set in 1910, songs that sounded similar to songs of the \
   period were wanted. The movie version finished at #36 in AFI's 100 Years...100 Songs \
   survey of top tunes in American cinema."
;;

let example_with_a_long_word_NOT_at_beginning_of_text =
  example_prefix_text ^ example_with_a_long_word_at_beginning_of_text
;;

let example_with_back_to_back_long_words_at_beginning_of_text =
  "supercalifragilisticexpialidocious " ^ example_with_a_long_word_at_beginning_of_text
;;

let example_with_back_to_back_long_words_NOT_at_beginning_of_text =
  example_prefix_text ^ example_with_back_to_back_long_words_at_beginning_of_text
;;

let%expect_test "word that is longer than the max width at beginning of text" =
  test
    (Boxed.cell
       (text
          ~wrap:{ max_width = 25; preserve_leading_spaces = true }
          example_with_a_long_word_at_beginning_of_text)
     |> boxed);
  [%expect
    {|
    ┌────────────────────────────────────┐
    │ supercalifragilisticexpialidocious │
    │ is a song and single               │
    │ from the 1964 Disney               │
    │ musical film Mary                  │
    │ Poppins. It was written            │
    │ by the Sherman Brothers,           │
    │ and sung by Julie Andrews          │
    │ and Dick Van Dyke. It              │
    │ also appears in the 2004           │
    │ stage show version.                │
    │ Because Mary Poppins was           │
    │ a period piece set in              │
    │ 1910, songs that sounded           │
    │ similar to songs of the            │
    │ period were wanted. The            │
    │ movie version finished at          │
    │ #36 in AFI's 100                   │
    │ Years...100 Songs survey           │
    │ of top tunes in American           │
    │ cinema.                            │
    └────────────────────────────────────┘
    |}]
;;

let%expect_test "word that is longer than the max width NOT at beginning of text" =
  test
    (Boxed.cell
       (text
          ~wrap:{ max_width = 25; preserve_leading_spaces = true }
          example_with_a_long_word_NOT_at_beginning_of_text)
     |> boxed);
  [%expect
    {|
    ┌────────────────────────────────────┐
    │ this is some random                │
    │ prefix text foo bar                │
    │ baz...                             │
    │ supercalifragilisticexpialidocious │
    │ is a song and single from          │
    │ the 1964 Disney musical            │
    │ film Mary Poppins. It was          │
    │ written by the Sherman             │
    │ Brothers, and sung by              │
    │ Julie Andrews and Dick             │
    │ Van Dyke. It also appears          │
    │ in the 2004 stage show             │
    │ version. Because Mary              │
    │ Poppins was a period               │
    │ piece set in 1910, songs           │
    │ that sounded similar to            │
    │ songs of the period were           │
    │ wanted. The movie version          │
    │ finished at #36 in AFI's           │
    │ 100 Years...100 Songs              │
    │ survey of top tunes in             │
    │ American cinema.                   │
    └────────────────────────────────────┘
    |}]
;;

let%expect_test "two words that are longer than the max width at the beginning of text" =
  test
    (Boxed.cell
       (text
          ~wrap:{ max_width = 25; preserve_leading_spaces = true }
          example_with_back_to_back_long_words_at_beginning_of_text)
     |> boxed);
  [%expect
    {|
    ┌────────────────────────────────────┐
    │ supercalifragilisticexpialidocious │
    │ supercalifragilisticexpialidocious │
    │ is a song and single               │
    │ from the 1964 Disney               │
    │ musical film Mary                  │
    │ Poppins. It was written            │
    │ by the Sherman Brothers,           │
    │ and sung by Julie Andrews          │
    │ and Dick Van Dyke. It              │
    │ also appears in the 2004           │
    │ stage show version.                │
    │ Because Mary Poppins was           │
    │ a period piece set in              │
    │ 1910, songs that sounded           │
    │ similar to songs of the            │
    │ period were wanted. The            │
    │ movie version finished at          │
    │ #36 in AFI's 100                   │
    │ Years...100 Songs survey           │
    │ of top tunes in American           │
    │ cinema.                            │
    └────────────────────────────────────┘
    |}]
;;

let%expect_test "two words that are longer than the max width NOT at beginning of text" =
  test
    (Boxed.cell
       (text
          ~wrap:{ max_width = 25; preserve_leading_spaces = true }
          example_with_back_to_back_long_words_NOT_at_beginning_of_text)
     |> boxed);
  [%expect
    {|
    ┌────────────────────────────────────┐
    │ this is some random                │
    │ prefix text foo bar                │
    │ baz...                             │
    │ supercalifragilisticexpialidocious │
    │ supercalifragilisticexpialidocious │
    │ is a song and single from          │
    │ the 1964 Disney musical            │
    │ film Mary Poppins. It was          │
    │ written by the Sherman             │
    │ Brothers, and sung by              │
    │ Julie Andrews and Dick             │
    │ Van Dyke. It also appears          │
    │ in the 2004 stage show             │
    │ version. Because Mary              │
    │ Poppins was a period               │
    │ piece set in 1910, songs           │
    │ that sounded similar to            │
    │ songs of the period were           │
    │ wanted. The movie version          │
    │ finished at #36 in AFI's           │
    │ 100 Years...100 Songs              │
    │ survey of top tunes in             │
    │ American cinema.                   │
    └────────────────────────────────────┘
    |}]
;;

let%expect_test "Regression test against stack overflows" =
  let textblock = Text_block.vcat (List.init 1_000_000 ~f:(Fn.const (text ""))) in
  Expect_test_helpers_core.require_does_not_raise (fun () ->
    Text_block.render textblock |> (ignore : string -> unit))
;;

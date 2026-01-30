open! Core
open! Async
open! Import

let make_input_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner"; "price" ]
    ; [ "apple"; "4"; "Abraham"; "1.1" ]
    ; [ "apple"; "6"; "Bathsheba"; "-1.5" ]
    ; [ "orange"; "2"; "Cyrus"; "1.2" ]
    ]
;;

(* Exercise -count, -list, -sum-neg, -sum-pos, and -sum. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      system
        {| \
              csv sum-group input.csv \
              -count fruit \
              -list fruit \
              -sum-neg quantity \
              -sum-neg price \
              -sum-pos quantity \
              -sum-pos price \
              -sum quantity \
              -sum price  \
              | csv transpose  \
           | csv to-ascii-table |}
    in
    [%expect
      {|
      ┌──────────────────┬──────────────┐
      │     quantity_sum │           12 │
      ├──────────────────┼──────────────┤
      │        price_sum │          0.8 │
      │      fruit_count │            2 │
      │       fruit_list │ apple;orange │
      │ quantity_sum-pos │           12 │
      │    price_sum-pos │          2.3 │
      │ quantity_sum-neg │            0 │
      │    price_sum-neg │         -1.5 │
      └──────────────────┴──────────────┘
      |}];
    return ())
;;

(* Group by keys. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      system
        {| \
              csv sum-group input.csv \
              -key fruit \
              -sum-neg quantity \
              -sum-neg price \
              -sum-pos quantity \
              -sum-pos price \
              -sum quantity \
              -sum price \
              | csv transpose  \
           | csv to-ascii-table |}
    in
    [%expect
      {|
      ┌──────────────────┬───────┬────────┐
      │            fruit │ apple │ orange │
      ├──────────────────┼───────┼────────┤
      │     quantity_sum │    10 │      2 │
      │        price_sum │  -0.4 │    1.2 │
      │ quantity_sum-pos │    10 │      2 │
      │    price_sum-pos │   1.1 │    1.2 │
      │ quantity_sum-neg │     0 │      0 │
      │    price_sum-neg │  -1.5 │      0 │
      └──────────────────┴───────┴────────┘
      |}];
    return ())
;;

(* Exercise -count, -list, -sum-neg, -sum-pos, and -sum. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      run
        "csv"
        [ "sum-group"
        ; "input.csv"
        ; "-count"
        ; "fruit"
        ; "-list"
        ; "fruit"
        ; "-sum-neg"
        ; "quantity"
        ; "-sum-neg"
        ; "price"
        ; "-sum-pos"
        ; "quantity"
        ; "-sum-pos"
        ; "price"
        ; "-sum"
        ; "quantity"
        ; "-sum"
        ; "price"
        ; "-sep"
        ; "."
        ]
    in
    [%expect
      " \n\
      \ \
       quantity_sum.price_sum.fruit_count.fruit_list.quantity_sum-pos.price_sum-pos.quantity_sum-neg.price_sum-neg\n\
      \ 12.\"0.8\".2.apple;orange.12.\"2.3\".0.\"-1.5\"\n\
      \ "];
    return ())
;;

let%expect_test "non-existent field" =
  do_test (fun () ->
    let stdin = "x,y" in
    let%bind () = run "csv" [ "sum-group"; "-"; "-sum"; "not-a-real-field" ] ~stdin in
    [%expect {| not-a-real-field_sum |}];
    return ())
;;

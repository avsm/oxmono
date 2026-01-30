open! Core
open! Async
open! Import

let make_input_csv ?sep ?(gen_header = true) filename =
  let header = if gen_header then [ [ "fruit"; "quantity"; "owner" ] ] else [] in
  let rows =
    [ [ "apple"; "4"; "Abraham" ]
    ; [ "apple"; "6"; "Bathsheba" ]
    ; [ "orange"; "2"; "Cyrus" ]
    ]
  in
  make_input_csv ?sep filename (header @ rows)
;;

let%expect_test "happy case" =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "to-pipe-table"; "input.csv" ] in
    [%expect
      {|
      ||fruit||quantity||owner||
      |apple|4|Abraham|
      |apple|6|Bathsheba|
      |orange|2|Cyrus|
      |}];
    return ())
;;

let%expect_test "-sep" =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "to-pipe-table"; "-sep"; "."; "input.csv" ] in
    [%expect
      {|
      ||fruit||quantity||owner||
      |apple|4|Abraham|
      |apple|6|Bathsheba|
      |orange|2|Cyrus|
      |}];
    return ())
;;

let%expect_test "-suppress-header" =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "to-pipe-table"; "-suppress-header"; "input.csv" ] in
    [%expect
      {|
      |apple|4|Abraham|
      |apple|6|Bathsheba|
      |orange|2|Cyrus|
      |}];
    return ())
;;

let%expect_test "-no-header" =
  do_test (fun () ->
    let%bind () = make_input_csv ~gen_header:false "input.csv" in
    let%bind () = run "csv" [ "to-pipe-table"; "-no-header"; "input.csv" ] in
    [%expect
      {|
      |apple|4|Abraham|
      |apple|6|Bathsheba|
      |orange|2|Cyrus|
      |}];
    return ())
;;

let%expect_test "-no-header -suppress-header" =
  do_test (fun () ->
    let%bind () = make_input_csv ~gen_header:false "input.csv" in
    let%bind () =
      run "csv" [ "to-pipe-table"; "-no-header"; "-suppress-header"; "input.csv" ]
    in
    [%expect
      {|
      |apple|4|Abraham|
      |apple|6|Bathsheba|
      |orange|2|Cyrus|
      |}];
    return ())
;;

let%expect_test "ragged rows" =
  (* I don't know how the wiki handles ragged rows. I'll document the current (not
     carefully considered) behavior and we can revisit when someone has a use case. *)
  do_test (fun () ->
    let%bind () =
      Import.make_input_csv
        "input.csv"
        [ [ "fruit"; "quantity" ]; []; [ "apple"; "4"; "Abraham" ] ]
    in
    let%bind () = run "cat" [ "input.csv" ] in
    [%expect
      {|
      fruit,quantity

      apple,4,Abraham
      |}];
    let%bind () = run "csv" [ "to-pipe-table"; "input.csv" ] in
    [%expect
      {|
      ||fruit||quantity||
      | |
      |apple|4|Abraham|
      |}];
    return ())
;;

open! Core
open! Async
open! Import

let%expect_test "good input" =
  let input =
    {|
((F1 X1) (F2 Y1) (F3 Z1))
((F1 X2) (F2 Y2) (F3 Z2))
|}
  in
  do_test (fun () ->
    let%bind () = run ~stdin:input "csv" [ "of-sexp" ] in
    [%expect
      {|
      F1,F2,F3
      X1,Y1,Z1
      X2,Y2,Z2
      |}];
    return ())
;;

let%expect_test "sep" =
  let input =
    {|
((F1 X1) (F2 Y1) (F3 Z1))
((F1 X2) (F2 Y2) (F3 Z2))
|}
  in
  do_test (fun () ->
    let%bind () = run ~stdin:input "csv" [ "of-sexp"; "-sep"; "|" ] in
    [%expect
      {|
      F1|F2|F3
      X1|Y1|Z1
      X2|Y2|Z2
      |}];
    return ())
;;

let%expect_test "no header" =
  let input =
    {|
((F1 X1) (F2 Y1) (F3 Z1))
((F1 X2) (F2 Y2) (F3 Z2))
|}
  in
  do_test (fun () ->
    let%bind () = run ~stdin:input "csv" [ "of-sexp"; "-nh" ] in
    [%expect
      {|
      X1,Y1,Z1
      X2,Y2,Z2
      |}];
    return ())
;;

let%expect_test "fields not in header" =
  let input =
    {|
((F1 X1) (F2 Y1) (F3 Z1))
((F1 X2) (G2 Y2) (F3 Z2))
|}
  in
  do_test (fun () ->
    let%bind () = run ~stdin:input "csv" [ "of-sexp" ] in
    [%expect
      {|
      F1,F2,F3
      X1,Y1,Z1
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error
       ("Missing field in a row." (field F2) (row 2) ((F1 X2) (F3 Z2) (G2 Y2))))
      |}];
    return ())
;;

let%expect_test "detect fields not in header even if not printing header" =
  let input =
    {|
((F1 X1) (F2 Y1) (F3 Z1))
((F1 X2) (G2 Y2) (F3 Z2))
|}
  in
  do_test (fun () ->
    let%bind () = run ~stdin:input "csv" [ "of-sexp"; "-nh" ] in
    [%expect
      {|
      X1,Y1,Z1
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error
       ("Missing field in a row." (field F2) (row 2) ((F1 X2) (F3 Z2) (G2 Y2))))
      |}];
    return ())
;;

let%expect_test "fields reordered" =
  let input =
    {|
((F1 X1) (F2 Y1) (F3 Z1))
((F1 X2) (F3 Y2) (F2 Z2))
|}
  in
  do_test (fun () ->
    let%bind () = run ~stdin:input "csv" [ "of-sexp" ] in
    [%expect
      {|
      F1,F2,F3
      X1,Y1,Z1
      X2,Z2,Y2
      |}];
    return ())
;;

let%expect_test "field omitted" =
  let input =
    {|
((F1 X1) (F2 Y1) (F3 Z1))
((F1 X2) (F2 Y2))
|}
  in
  do_test (fun () ->
    let%bind () = run ~stdin:input "csv" [ "of-sexp" ] in
    [%expect
      {|
      F1,F2,F3
      X1,Y1,Z1
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error
       ("Row has wrong number of fields." (header (F1 F2 F3)) (row 2)
        ((F1 X2) (F2 Y2))))
      |}];
    return ())
;;

let%expect_test "empty input" =
  do_test (fun () ->
    let%bind () = run ~stdin:"" "csv" [ "of-sexp" ] in
    [%expect {| |}];
    return ())
;;

let%expect_test "duplicate fields" =
  let input =
    {|
((F1 X1) (F2 Y1) (F3 Z1))
((F1 X2) (F2 Y2) (F2 Y3))
|}
  in
  do_test (fun () ->
    let%bind () = run ~stdin:input "csv" [ "of-sexp" ] in
    [%expect
      {|
      F1,F2,F3
      X1,Y1,Z1
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error
       ("Malformed row sexp."
        (exn
         (Of_sexp_error "Map.t_of_sexp_direct: duplicate key" (invalid_sexp F2)))
        (sexp ((F1 X2) (F2 Y2) (F2 Y3)))))
      |}];
    return ())
;;

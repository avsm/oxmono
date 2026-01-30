open! Core
open! Async
open! Import

let example_csv =
  { Csv_common.header = [ "fruit"; "quantity"; "owner" ]
  ; lines =
      [ [ "apple"; "4"; "Abraham" ]
      ; [ "apple"; "6"; "Bathsheba" ]
      ; [ "orange"; "2"; "Cyrus" ]
      ]
  }
;;

let make_input_csv ?sep filename =
  let csv_lines = example_csv.header :: example_csv.lines in
  make_input_csv ?sep filename csv_lines
;;

(* Cut one column. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-fields"; "fruit"; "input.csv" ] in
    [%expect
      {|
      fruit
      apple
      apple
      orange
      |}];
    return ())
;;

(* Cut two columns. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-fields"; "fruit,quantity"; "input.csv" ] in
    [%expect
      {|
      fruit,quantity
      apple,4
      apple,6
      orange,2
      |}];
    return ())
;;

(* Cut all but two columns. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-v"; "-fields"; "fruit,quantity"; "input.csv" ] in
    [%expect
      {|
      owner
      Abraham
      Bathsheba
      Cyrus
      |}];
    return ())
;;

(* Cut out owner column. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-v"; "-fields"; "owner"; "input.csv" ] in
    [%expect
      {|
      fruit,quantity
      apple,4
      apple,6
      orange,2
      |}];
    return ())
;;

(* Pass -sh. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-sh"; "-fields"; "fruit"; "input.csv" ] in
    [%expect
      {|
      apple
      apple
      orange
      |}];
    return ())
;;

(* Use the wrong delimiter. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "cut"; "-d"; "."; "-fields"; "fruit"; "input.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Exception raised in Delimited.Read" (line_number 1) (header_map ())
         (buffer (fruit,quantity,owner)) (exn ("Unknown header" fruit)))
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "cut"; "-d"; "."; "-fields"; "fruit,quantity,owner"; "input.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Exception raised in Delimited.Read" (line_number 1) (header_map ())
         (buffer (fruit,quantity,owner)) (exn ("Unknown header" fruit)))
      |}];
    return ())
;;

(* Use the correct, unusual delimiter. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = run "csv" [ "cut"; "-d"; "."; "-fields"; "fruit"; "input.csv" ] in
    [%expect
      {|
      fruit
      apple
      apple
      orange
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      run "csv" [ "cut"; "-d"; "."; "-fields"; "fruit,quantity"; "input.csv" ]
    in
    [%expect
      {|
      fruit,quantity
      apple,4
      apple,6
      orange,2
      |}];
    return ())
;;

let%expect_test "incorrect duplication of header" =
  (* We used to have a bug where the header would be printed once per file. *)
  do_test (fun () ->
    let%bind () = make_input_csv "1.csv" in
    let%bind () = make_input_csv "2.csv" in
    let%bind () = run "csv" [ "cut"; "-fields"; "quantity"; "1.csv"; "2.csv" ] in
    [%expect
      {|
      quantity
      4
      6
      2
      4
      6
      2
      |}];
    return ())
;;

let%expect_test "properly cut a csv that's loaded into memory" =
  let f row = Csvlib.Csv.print [ Array.to_list row ] in
  let () =
    Cut.cut_by_field_names
      (Csv example_csv)
      ~sep:','
      ~skip_header:false
      ~f
      (`All_but [ "quantity" ])
  in
  return
    [%expect
      {|
      fruit,owner
      apple,Abraham
      apple,Bathsheba
      orange,Cyrus
      |}]
;;

let%expect_test "cut a CSV with no headers" =
  let lines = [ [ "a"; "c"; "c" ]; [ "c"; "e"; "f" ]; [ "a"; "a"; "a" ] ] in
  do_test (fun () ->
    let%bind () = Import.make_input_csv "input.csv" lines in
    let%bind () = run "csv" [ "cut"; "-no-headers"; "-fields"; "1"; "input.csv" ] in
    [%expect
      {|
      c
      e
      a
      |}];
    let%bind () = run "csv" [ "cut"; "-no-headers"; "-fields"; "0,2"; "input.csv" ] in
    [%expect
      {|
      a,c
      c,f
      a,a
      |}];
    (* fields come out in the order they are specified *)
    let%bind () = run "csv" [ "cut"; "-no-headers"; "-fields"; "2,0"; "input.csv" ] in
    [%expect
      {|
      c,a
      f,c
      a,a
      |}];
    (* Attempting to use named fields with -no-headers fails *)
    let%bind () = run "csv" [ "cut"; "-no-headers"; "-fields"; "a"; "input.csv" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Exception raised in Delimited.Read" (line_number 1) (header_map ())
         (buffer (a c c)) (exn (Failure "Int.of_string: \"a\"")))
      |}];
    (* Attempting to use a first row with duplicate named fields fails *)
    let%bind () = run "csv" [ "cut"; "-fields"; "1"; "input.csv" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Exception raised in Delimited.Read" (line_number 1) (header_map ())
         (buffer (a c c)) (exn ("Map.of_iteri_exn: duplicate key" c)))
      |}];
    (* It's not allowed to specify -no-headers -sh *)
    let%bind () =
      run "csv" [ "cut"; "-no-headers"; "-sh"; "-fields"; "1"; "input.csv" ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        (Failure "-suppress-header implies headers, but provided -no-headers too")
      |}];
    (* cut by indices even though a header row exists *)
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "cut"; "-fields"; "0,2"; "input.csv" ] in
    [%expect
      {|
      fruit,owner
      apple,Abraham
      apple,Bathsheba
      orange,Cyrus
      |}];
    (* cut by fields even though no header row exists *)
    let%bind () = run "csv" [ "cut"; "-fields"; "fruit"; "-no-header"; "input.csv" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Uncaught exception:

        ("Exception raised in Delimited.Read" (line_number 1) (header_map ())
         (buffer (fruit quantity owner))
         (exn (Failure "Int.of_string: \"fruit\"")))
      |}];
    return ())
;;

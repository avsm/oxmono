open! Core
open! Async
open! Import

let make_input_csv0 = make_input_csv

let make_input_csv ?sep filename =
  make_input_csv0
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner" ]
    ; [ "apple"; "4"; "Abraham" ]
    ; [ "apple"; "6"; "Bathsheba" ]
    ; [ "orange"; "2"; "Cyrus" ]
    ]
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run ~enable_ocaml_backtraces:false "csv" [ "to-sexp"; "input.csv" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      Error parsing command line:

        too many anonymous arguments

      For usage information, run

        csv to-sexp -help
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = system "csv to-sexp < input.csv" in
    [%expect
      {|
      ((fruit apple)(quantity 4)(owner Abraham))
      ((fruit apple)(quantity 6)(owner Bathsheba))
      ((fruit orange)(quantity 2)(owner Cyrus))
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = system "csv to-sexp -no-header < input.csv" in
    [%expect
      {|
      (fruit quantity owner)
      (apple 4 Abraham)
      (apple 6 Bathsheba)
      (orange 2 Cyrus)
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () = run "csv" [ "to-sexp"; "-nh" ] in
    [%expect {| |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () = system "csv to-sexp -d . < input.csv" in
    [%expect
      {|
      ((fruit apple)(quantity 4)(owner Abraham))
      ((fruit apple)(quantity 6)(owner Bathsheba))
      ((fruit orange)(quantity 2)(owner Cyrus))
      |}];
    return ())
;;

let%expect_test "utf8" =
  do_test (fun () ->
    let%bind () =
      make_input_csv0
        "input.csv"
        [ [ "raw_exchange_symbol"; "jane_romanization" ]; [ "币安人生"; "BIANRENSHENG" ] ]
    in
    let%bind () = system "csv to-sexp < input.csv" in
    let without_utf8 = expect_test_output () in
    let%bind () = system "csv to-sexp -utf8 < input.csv" in
    let with_utf8 = expect_test_output () in
    let get_field name s =
      let re = Re.(seq [ str name; group (rep1 (compl [ char ')' ])); str ")" ]) in
      Re.Group.get (Re.exec (Re.compile re) s) 1
    in
    let symbol_without_utf8 = get_field "raw_exchange_symbol" without_utf8 in
    let symbol_with_utf8 = get_field "raw_exchange_symbol" with_utf8 in
    require_not_equal (module String) symbol_without_utf8 symbol_with_utf8;
    [%expect {| |}];
    Expect_test_patdiff.print_patdiff symbol_without_utf8 symbol_with_utf8;
    [%expect
      {|
      === DIFF HUNK ===
      -|"\229\184\129\229\174\137\228\186\186\231\148\159"
      +| 币安人生
      |}];
    let remove src symbol = String.substr_replace_all src ~pattern:symbol ~with_:"" in
    require_equal
      (module String)
      (remove without_utf8 symbol_without_utf8)
      (remove with_utf8 symbol_with_utf8);
    [%expect {| |}];
    return ())
;;

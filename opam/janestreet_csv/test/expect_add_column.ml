open! Core
open! Async
open! Import

let%expect_test "add column at end" =
  do_test (fun () ->
    let csv = [ [ "name"; "age" ]; [ "Alice"; "30" ]; [ "Bob"; "25" ] ] in
    let%bind () = make_input_csv "input.csv" csv in
    let%bind () =
      run "csv" [ "add-column"; "-column"; "city"; "-value"; "NYC"; "input.csv" ]
    in
    [%expect
      {|
      name,age,city
      Alice,30,NYC
      Bob,25,NYC
      |}];
    return ())
;;

let%expect_test "add column with empty value" =
  do_test (fun () ->
    let csv = [ [ "name"; "age" ]; [ "Alice"; "30" ]; [ "Bob"; "25" ] ] in
    let%bind () = make_input_csv "input.csv" csv in
    let%bind () = run "csv" [ "add-column"; "-column"; "notes"; "input.csv" ] in
    [%expect
      {|
      name,age,notes
      Alice,30,
      Bob,25,
      |}];
    return ())
;;

let%expect_test "add column after specific column" =
  do_test (fun () ->
    let csv =
      [ [ "name"; "age"; "department" ]
      ; [ "Alice"; "30"; "Engineering" ]
      ; [ "Bob"; "25"; "Sales" ]
      ]
    in
    let%bind () = make_input_csv "input.csv" csv in
    let%bind () =
      run
        "csv"
        [ "add-column"; "-column"; "city"; "-value"; "NYC"; "-after"; "age"; "input.csv" ]
    in
    [%expect
      {|
      name,age,city,department
      Alice,30,NYC,Engineering
      Bob,25,NYC,Sales
      |}];
    return ())
;;

let%expect_test "add column after first column" =
  do_test (fun () ->
    let csv =
      [ [ "name"; "age"; "department" ]
      ; [ "Alice"; "30"; "Engineering" ]
      ; [ "Bob"; "25"; "Sales" ]
      ]
    in
    let%bind () = make_input_csv "input.csv" csv in
    let%bind () =
      run
        "csv"
        [ "add-column"; "-column"; "id"; "-value"; "001"; "-after"; "name"; "input.csv" ]
    in
    [%expect
      {|
      name,id,age,department
      Alice,001,30,Engineering
      Bob,001,25,Sales
      |}];
    return ())
;;

let%expect_test "add column after non-existent column fails" =
  do_test (fun () ->
    let csv = [ [ "name"; "age" ]; [ "Alice"; "30" ]; [ "Bob"; "25" ] ] in
    let%bind () = make_input_csv "input.csv" csv in
    let%bind () =
      run
        ~enable_ocaml_backtraces:false
        "csv"
        [ "add-column"
        ; "-column"
        ; "city"
        ; "-value"
        ; "NYC"
        ; "-after"
        ; "nonexistent"
        ; "input.csv"
        ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error (Failure "Column 'nonexistent' not found in header"))
      |}];
    return ())
;;

let%expect_test "add column with custom separator" =
  do_test (fun () ->
    let sep = ';' in
    let csv = [ [ "name"; "age" ]; [ "Alice"; "30" ]; [ "Bob"; "25" ] ] in
    let%bind () = make_input_csv ~sep "input.csv" csv in
    let%bind () =
      run
        "csv"
        [ "add-column"
        ; "-sep"
        ; String.of_char sep
        ; "-column"
        ; "city"
        ; "-value"
        ; "NYC"
        ; "input.csv"
        ]
    in
    [%expect
      {|
      name;age;city
      Alice;30;NYC
      Bob;25;NYC
      |}];
    return ())
;;

let%expect_test "add column with name that already exists" =
  do_test (fun () ->
    let sep = ';' in
    let csv = [ [ "name"; "age" ]; [ "Alice"; "30" ]; [ "Bob"; "25" ] ] in
    let%bind () = make_input_csv ~sep "input.csv" csv in
    (* fails without -allow-duplicate-column *)
    let%bind () =
      run
        "csv"
        [ "add-column"
        ; "-sep"
        ; String.of_char sep
        ; "-column"
        ; "age"
        ; "-value"
        ; "31"
        ; "input.csv"
        ]
    in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error
       (Failure
        "Column 'age' already exists in header (pass -allow-duplicate-column to add anyway)"))
      |}];
    (* succeeds with -allow-duplicate-column *)
    let%bind () =
      run
        "csv"
        [ "add-column"
        ; "-sep"
        ; String.of_char sep
        ; "-column"
        ; "age"
        ; "-value"
        ; "31"
        ; "input.csv"
        ; "-allow-duplicate-column"
        ]
    in
    [%expect
      {|
      name;age;age
      Alice;30;31
      Bob;25;31
      |}];
    return ())
;;

let%expect_test "pass through existing duplication" =
  (* Postel's law: be liberal in what you accept and strict in what you emit. *)
  do_test (fun () ->
    let sep = ';' in
    let csv = [ [ "name"; "name" ]; [ "Alice"; "Alice" ]; [ "Bob"; "Bob" ] ] in
    let%bind () = make_input_csv ~sep "input.csv" csv in
    (* fails without -allow-duplicate-column *)
    let%bind () =
      run
        "csv"
        [ "add-column"
        ; "-sep"
        ; String.of_char sep
        ; "-column"
        ; "age"
        ; "-value"
        ; "31"
        ; "input.csv"
        ]
    in
    [%expect
      {|
      name;name;age
      Alice;Alice;31
      Bob;Bob;31
      |}];
    return ())
;;

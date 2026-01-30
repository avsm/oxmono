open! Core
open! Async
open! Import

let time_str hr =
  Time.to_string
    (Time.of_date_ofday
       (Date.create_exn ~y:2016 ~m:Jan ~d:01)
       (Time.Ofday.create ~hr ~min:0 ~sec:0 ())
       ~zone:Time.Zone.utc)
;;

let make_input_csv ?sep filename =
  make_input_csv
    ?sep
    filename
    [ [ "fruit"; "quantity"; "owner"; "time" ]
    ; [ "apple"; "4"; "Abraham"; time_str 1 ]
    ; [ "apple"; "6"; "Bathsheba"; time_str 2 ]
    ; [ "orange"; "2"; "Cyrus"; time_str 3 ]
    ]
;;

(* The grid is [start + step * n] for all [n] in positive integers, until [stop]. Weirdly,
   [start] is not included, but [stop] is included.

   Or, in pseudo-C to be unambiguous: for (n = 1; start + step * n <= stop; ++n)
*)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        "csv"
        [ "grid"
        ; "-start"
        ; time_str 1
        ; "-stop"
        ; time_str 3
        ; "-step"
        ; "30m"
        ; "-time-field"
        ; "time"
        ; "input.csv"
        ]
    in
    [%expect
      {|
      fruit,quantity,owner,time
      apple,4,Abraham,2015-12-31 20:30:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:00:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:30:00.000000-05:00
      orange,2,Cyrus,2015-12-31 22:00:00.000000-05:00
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        "csv"
        [ "grid"
        ; "-start"
        ; time_str 1
        ; "-stop"
        ; time_str 3
        ; "-step"
        ; "19m"
        ; "-time-field"
        ; "time"
        ; "input.csv"
        ]
    in
    [%expect
      {|
      fruit,quantity,owner,time
      apple,4,Abraham,2015-12-31 20:19:00.000000-05:00
      apple,4,Abraham,2015-12-31 20:38:00.000000-05:00
      apple,4,Abraham,2015-12-31 20:57:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:16:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:35:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:54:00.000000-05:00
      |}];
    return ())
;;

let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        "csv"
        [ "grid"
        ; "-start"
        ; time_str 1
        ; "-stop"
        ; time_str 3
        ; "-step"
        ; "20m"
        ; "-time-field"
        ; "time"
        ; "input.csv"
        ]
    in
    [%expect
      {|
      fruit,quantity,owner,time
      apple,4,Abraham,2015-12-31 20:20:00.000000-05:00
      apple,4,Abraham,2015-12-31 20:40:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:00:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:20:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:40:00.000000-05:00
      orange,2,Cyrus,2015-12-31 22:00:00.000000-05:00
      |}];
    return ())
;;

(* For times before the first data point, assume all fields are blank. For times after the
   last data point, carry forward the last data point. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv "input.csv" in
    let%bind () =
      run
        "csv"
        [ "grid"
        ; "-start"
        ; time_str 0
        ; "-stop"
        ; time_str 4
        ; "-step"
        ; "30m"
        ; "-time-field"
        ; "time"
        ; "input.csv"
        ]
    in
    [%expect
      {|
      fruit,quantity,owner,time
      ,,,2015-12-31 19:30:00.000000-05:00
      apple,4,Abraham,2015-12-31 20:00:00.000000-05:00
      apple,4,Abraham,2015-12-31 20:30:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:00:00.000000-05:00
      apple,6,Bathsheba,2015-12-31 21:30:00.000000-05:00
      orange,2,Cyrus,2015-12-31 22:00:00.000000-05:00
      orange,2,Cyrus,2015-12-31 22:30:00.000000-05:00
      orange,2,Cyrus,2015-12-31 23:00:00.000000-05:00
      |}];
    return ())
;;

(* Unusual delimiter. *)
let%expect_test _ =
  do_test (fun () ->
    let%bind () = make_input_csv ~sep:'.' "input.csv" in
    let%bind () =
      run
        "csv"
        [ "grid"
        ; "-start"
        ; time_str 1
        ; "-stop"
        ; time_str 3
        ; "-step"
        ; "30m"
        ; "-time-field"
        ; "time"
        ; "-sep"
        ; "."
        ; "input.csv"
        ]
    in
    [%expect
      {|
      fruit.quantity.owner.time
      apple.4.Abraham."2015-12-31 20:30:00.000000-05:00"
      apple.6.Bathsheba."2015-12-31 21:00:00.000000-05:00"
      apple.6.Bathsheba."2015-12-31 21:30:00.000000-05:00"
      orange.2.Cyrus."2015-12-31 22:00:00.000000-05:00"
      |}];
    return ())
;;

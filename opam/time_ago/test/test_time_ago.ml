open! Core

let time_init = Time_ns.now ()

let print_string_after_span span =
  let now = Time_ns.add time_init span in
  Time_ago.to_string ~now time_init |> printf !"%s\n"
;;

let%expect_test "Just now elapsed" =
  Time_ns.Span.[ of_sec 1.; of_sec 6.; of_sec 29. ]
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    just now
    just now
    just now
    |}]
;;

let%expect_test "Just now future" =
  Time_ns.Span.[ of_sec 0.; of_sec 1.; of_sec 29.; of_sec 30. ]
  |> List.map ~f:Time_ns.Span.neg
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    just now
    in a moment
    in a moment
    in a moment
    |}]
;;

let%expect_test "minutes elapsed" =
  Time_ns.Span.[ of_sec 30.; of_sec 89.; of_sec 90.; of_sec 91.; of_min 2.; of_min 44. ]
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about a minute ago
    about a minute ago
    1 minute ago
    1 minute ago
    2 minutes ago
    44 minutes ago
    |}]
;;

let%expect_test "minutes future" =
  Time_ns.Span.[ of_sec 31.; of_sec 89.; of_sec 90.; of_sec 91.; of_min 44.; of_min 45. ]
  |> List.map ~f:Time_ns.Span.neg
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about a minute from now
    about a minute from now
    about a minute from now
    1 minute from now
    44 minutes from now
    45 minutes from now
    |}]
;;

let%expect_test "hours elapsed" =
  Time_ns.Span.[ of_min 45.; of_min 89.; of_min 90.; of_hr 2.; of_hr 17. ]
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about an hour ago
    about an hour ago
    1 hour ago
    2 hours ago
    17 hours ago
    |}]
;;

let%expect_test "hours future" =
  Time_ns.Span.[ of_min 46.; of_min 89.; of_min 90.; of_min 91.; of_hr 17.; of_hr 18. ]
  |> List.map ~f:Time_ns.Span.neg
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about an hour from now
    about an hour from now
    about an hour from now
    1 hour from now
    17 hours from now
    18 hours from now
    |}]
;;

let%expect_test "days elapsed" =
  Time_ns.Span.[ of_hr 18.; of_hr 27.; of_hr 28.; of_hr 43.; of_hr 44.; of_day 3. ]
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about a day ago
    about a day ago
    more than a day ago
    more than a day ago
    1 day ago
    3 days ago
    |}]
;;

let%expect_test "days future" =
  Time_ns.Span.[ of_hr 19.; of_hr 28.; of_hr 29.; of_hr 44.; of_hr 45.; of_day 3. ]
  |> List.map ~f:Time_ns.Span.neg
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about a day from now
    about a day from now
    more than a day from now
    more than a day from now
    1 day from now
    3 days from now
    |}]
;;

let%expect_test "months elapsed" =
  Time_ns.Span.[ of_day 29.; of_day 32.; of_day 36.; of_day 61.; of_day 345. ]
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about a month ago
    about a month ago
    about a month ago
    2 months ago
    about a year ago
    |}]
;;

let%expect_test "months future" =
  Time_ns.Span.[ of_day 25.; of_day 32.; of_day 36.; of_day 61.; of_day 345. ]
  |> List.map ~f:Time_ns.Span.neg
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    25 days from now
    about a month from now
    about a month from now
    2 months from now
    about a year from now
    |}]
;;

let%expect_test "years elapsed" =
  Time_ns.Span.[ of_day 330.; of_day 364.; of_day 375.; of_day 800.; of_day 20_000. ]
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about a year ago
    1 year ago
    1 year ago
    2 years ago
    55 years ago
    |}]
;;

let%expect_test "years future" =
  Time_ns.Span.[ of_day 330.; of_day 364.; of_day 375.; of_day 800.; of_day 20_000. ]
  |> List.map ~f:Time_ns.Span.neg
  |> List.iter ~f:print_string_after_span;
  [%expect
    {|
    about a year from now
    1 year from now
    1 year from now
    2 years from now
    55 years from now
    |}]
;;

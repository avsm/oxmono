open! Core
open! Async
open! Import

let assert_unique_columns contents =
  match Csvlib.Csv.load_string contents with
  | [] -> raise_s [%message "Empty csv file"]
  | header :: _ ->
    List.find_a_dup header ~compare:[%compare: string]
    |> Option.iter ~f:(fun duplicate -> raise_s [%message "Duplicate column" duplicate])
;;

let make_csv_contents contents =
  let contents = Dedent.string contents in
  let contents =
    match String.is_suffix contents ~suffix:"\n" with
    | true -> contents
    | false -> [%string "%{contents}\n"]
  in
  assert_unique_columns contents;
  contents
;;

let diff_output ~(here : [%call_pos]) ?cr_if_different prev next ~f =
  let%bind prev = f prev in
  let%bind next = f next in
  match String.( = ) prev next with
  | true -> return ()
  | false ->
    Option.iter cr_if_different ~f:(print_cr ~here);
    Expect_test_patdiff.print_patdiff prev next ~location_style:None;
    return ()
;;

let csv_output stdin args = Process.run_exn () ~prog:"csv" ~args:(List.concat args) ~stdin

let csv_cut stdin ~fields ~invert_match =
  csv_output
    stdin
    [ [ "cut" ]
    ; (if invert_match then [ "-v" ] else [])
    ; [ "-fields"; String.concat fields ~sep:"," ]
    ]
;;

let csv_destructure stdin args = csv_output stdin ([ "destructure" ] :: args)

let diff_input_with_destructured input args =
  match%bind Deferred.Or_error.try_with (fun () -> csv_destructure input args) with
  | Ok output ->
    Expect_test_patdiff.print_patdiff input output ~location_style:None;
    return ()
  | Error _ ->
    (* re-run to show the error nicely *)
    run "csv" ("destructure" :: List.concat args) ~stdin:input
;;

let%expect_test "destructure columns" =
  let input =
    make_csv_contents
      {|
      unit,percent,span,time
      foo,2.3x,1d3h5m623.123s,(2022-01-01 12:30:45.123456789-05:00)
      (),25%,100ns,1970-01-01 00:05:00Z
      bar,30bp,-100us,1970-01-01T00:05:00.12345Z
    |}
  in
  let go ?(args = []) kind =
    let%bind all_fields_output = csv_destructure input [ [ kind ]; args ] in
    let%bind one_field_output =
      let on_error = if String.equal kind "unit" then [] else [ "-on-error"; "fail" ] in
      csv_destructure input [ [ kind ]; [ "-field"; kind ]; on_error; args ]
    in
    let%bind () =
      diff_output
        all_fields_output
        one_field_output
        ~cr_if_different:[%message "Selecting one field should not change output."]
        ~f:return
    in
    let%bind () =
      diff_output
        input
        one_field_output
        ~cr_if_different:[%message "Only the column of matching kind should change."]
        ~f:(csv_cut ~fields:[ kind ] ~invert_match:true)
    in
    let%bind () =
      diff_output
        input
        all_fields_output
        ~f:(csv_cut ~fields:[ kind ] ~invert_match:false)
    in
    return ()
  in
  do_test (fun () ->
    let%bind () = go "unit" in
    [%expect
      {|
        unit
        foo
      -|()
        bar
      |}];
    let%bind () = go "percent" in
    [%expect
      {|
        percent
      -|2.3x
      -|25%
      -|30bp
      +|2.3
      +|0.25
      +|0.003
      |}];
    let%bind () = go "span" in
    [%expect
      {|
        span
      -|1d3h5m623.123s
      -|100ns
      -|-100us
      +|98123.123000000
      +|0.000000100
      +|-0.000100000
      |}];
    let%bind () = go "span" ~args:[ "-unit"; "d" ] in
    [%expect
      {|
        span
      -|1d3h5m623.123s
      -|100ns
      -|-100us
      +|1.1356842939814815
      +|1.1574074074074074e-12
      +|-1.1574074074074074e-09
      |}];
    let%bind () = go "span" ~args:[ "-unit"; "ns" ] in
    [%expect
      {|
        span
      -|1d3h5m623.123s
      -|100ns
      -|-100us
      +|98123123000000
      +|100
      +|-100000
      |}];
    let%bind () = go "time" in
    [%expect
      {|
        time
      -|(2022-01-01 12:30:45.123456789-05:00)
      +|2022-01-01T17:30:45.123456789Z
      -|1970-01-01 00:05:00Z
      +|1970-01-01T00:05:00.000000000Z
      -|1970-01-01T00:05:00.12345Z
      +|1970-01-01T00:05:00.123450000Z
      |}];
    let%bind () = go "time" ~args:[ "-to-utc" ] in
    [%expect
      {|
        time
      -|(2022-01-01 12:30:45.123456789-05:00)
      +|2022-01-01 17:30:45.123456789Z
      -|1970-01-01 00:05:00Z
      +|1970-01-01 00:05:00.000000000Z
      -|1970-01-01T00:05:00.12345Z
      +|1970-01-01 00:05:00.123450000Z
      |}];
    let%bind () = go "time" ~args:[ "-to-unix-fractional" ] in
    [%expect
      {|
        time
      -|(2022-01-01 12:30:45.123456789-05:00)
      -|1970-01-01 00:05:00Z
      -|1970-01-01T00:05:00.12345Z
      +|1641058245.123456789
      +|300.000000000
      +|300.123450000
      |}];
    let%bind () = go "time" ~args:[ "-to-unix-seconds" ] in
    [%expect
      {|
        time
      -|(2022-01-01 12:30:45.123456789-05:00)
      -|1970-01-01 00:05:00Z
      -|1970-01-01T00:05:00.12345Z
      +|1641058245
      +|300
      +|300
      |}];
    return ())
;;

let%expect_test "[-on-error skip] vs [-on-error fail]" =
  let input =
    make_csv_contents
      {|
        unit,percent,span,time
        foo,2.3x,1d3h5m623.123s,(2022-01-01 12:30:45.123456789-05:00)
        (),25%,100ns,1970-01-01 00:05:00Z
        bar,30bp,-100us,1970-01-01T00:05:00.12345Z
      |}
  in
  do_test (fun () ->
    let destructure_percent_as_time args =
      diff_input_with_destructured input [ [ "time" ]; [ "-field"; "percent" ]; args ]
    in
    let%bind () = destructure_percent_as_time [ "-on-error"; "skip" ] in
    [%expect {| |}];
    let%bind () = destructure_percent_as_time [ "-on-error"; "fail" ] in
    [%expect
      {|
      unit,percent,span,time
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error
       ("Failed to convert value" (value 2.3x)
        (parse_failure
         (time_ns.ml.To_and_of_string.Time_ns_of_string 2.3x
          (Failure "no spaces or T found")))))
      |}];
    return ())
;;

let%expect_test "[-on-error fail] plus all-fields" =
  do_test (fun () ->
    let%bind () = run "csv" [ "destructure"; "time"; "-on-error"; "fail" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error
       (Failure "-on-error is only allowed if -fields are specified."))
      |}];
    return ())
;;

let%expect_test "[-fail] vs [-only-fail-if-nonempty]" =
  let input =
    make_csv_contents
      {|
        a,duration,b
        1,100ns,1
        2,1us,2
        3,,3
      |}
  in
  let destructure_duration_as_span args =
    diff_input_with_destructured input [ [ "span" ]; [ "-field"; "duration" ]; args ]
  in
  do_test (fun () ->
    let%bind () = destructure_duration_as_span [ "-on-error"; "fail" ] in
    [%expect
      {|
      a,duration,b
      1,0.000000100,1
      2,0.000001000,2
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      (monitor.ml.Error
       ("Failed to convert value" (value "")
        (parse_failure
         ("Time_ns.Span.of_string: invalid string" (string "")
          (reason "empty string")))))
      |}];
    let%bind () = destructure_duration_as_span [ "-on-error"; "only-fail-if-nonempty" ] in
    [%expect
      {|
        a,duration,b
      -|1,100ns,1
      +|1,0.000000100,1
      -|2,1us,2
      +|2,0.000001000,2
        3,,3
      |}];
    return ())
;;

let%expect_test "On a non-csv" =
  do_test (fun () ->
    let input =
      make_csv_contents
        {|
          1x
          2x
          50%
          20bp
        |}
    in
    let%bind () = run "csv" [ "destructure"; "percent" ] ~stdin:input in
    [%expect
      {|
      1.
      2.
      0.5
      0.002
      |}];
    return ())
;;

let%expect_test "special values" =
  let min_max_zero =
    make_csv_contents
      {|
        min,max,zero
        1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
        -53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s
      |}
  in
  let near_1e9 =
    make_csv_contents
      {|
        near-1e9-a,near-1e9-b,near-1e9-c
        1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
        1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
        999ms999us999ns,1s,1s1ns
        -999ms999us999ns,-1s,-1s1ns
      |}
  in
  let run args =
    let%bind () = diff_input_with_destructured min_max_zero [ args ] in
    let%bind () = diff_input_with_destructured near_1e9 [ args ] in
    return ()
  in
  do_test (fun () ->
    let%bind () = run [ "time"; "-to-iso8601" ] in
    [%expect
      {|
        min,max,zero
      -|1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      +|1823-11-12T00:06:21.572612096Z,2116-02-20T23:53:38.427387903Z,1970-01-01T00:00:00.000000000Z
        -53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s

        near-1e9-a,near-1e9-b,near-1e9-c
      -|1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
      +|1970-01-01T00:00:00.999999999Z,1970-01-01T00:00:01.000000000Z,1970-01-01T00:00:01.000000001Z
      -|1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      +|1969-12-31T23:59:59.000000001Z,1969-12-31T23:59:59.000000000Z,1969-12-31T23:59:58.999999999Z
        999ms999us999ns,1s,1s1ns
        -999ms999us999ns,-1s,-1s1ns
      |}];
    let%bind () = run [ "time"; "-to-unix-fractional" ] in
    [%expect
      {|
        min,max,zero
      -|1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      +|-4611686018.427387904,4611686018.427387903,0.000000000
        -53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s

        near-1e9-a,near-1e9-b,near-1e9-c
      -|1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
      -|1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      +|0.999999999,1.000000000,1.000000001
      +|-0.999999999,-1.000000000,-1.000000001
        999ms999us999ns,1s,1s1ns
        -999ms999us999ns,-1s,-1s1ns
      |}];
    let%bind () = run [ "time"; "-to-unix-nanos" ] in
    [%expect
      {|
        min,max,zero
      -|1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      +|-4611686018427387904,4611686018427387903,0
        -53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s

        near-1e9-a,near-1e9-b,near-1e9-c
      -|1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
      -|1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      +|999999999,1000000000,1000000001
      +|-999999999,-1000000000,-1000000001
        999ms999us999ns,1s,1s1ns
        -999ms999us999ns,-1s,-1s1ns
      |}];
    let%bind () = run [ "time"; "-to-unix-seconds" ] in
    [%expect
      {|
        min,max,zero
      -|1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      +|-4611686018,4611686018,0
        -53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s

        near-1e9-a,near-1e9-b,near-1e9-c
      -|1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
      -|1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      +|0,1,1
      +|0,-1,-1
        999ms999us999ns,1s,1s1ns
        -999ms999us999ns,-1s,-1s1ns
      |}];
    let%bind () = run [ "time"; "-to-utc" ] in
    [%expect
      {|
        min,max,zero
      -|1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      +|1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00.000000000Z
        -53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s

        near-1e9-a,near-1e9-b,near-1e9-c
      -|1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
      +|1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01.000000000Z,1970-01-01 00:00:01.000000001Z
      -|1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      +|1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59.000000000Z,1969-12-31 23:59:58.999999999Z
        999ms999us999ns,1s,1s1ns
        -999ms999us999ns,-1s,-1s1ns
      |}];
    let%bind () = run [ "span"; "-unit"; "day" ] in
    [%expect
      {|
        min,max,zero
        1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      -|-53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s
      +|-53375.995583650321,53375.995583650321,0.

        near-1e9-a,near-1e9-b,near-1e9-c
        1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
        1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      -|999ms999us999ns,1s,1s1ns
      -|-999ms999us999ns,-1s,-1s1ns
      +|1.15740740625e-05,1.1574074074074073e-05,1.1574074085648149e-05
      +|-1.15740740625e-05,-1.1574074074074073e-05,-1.1574074085648149e-05
      |}];
    let%bind () = run [ "span"; "-unit"; "hour" ] in
    [%expect
      {|
        min,max,zero
        1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      -|-53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s
      +|-1281023.8940076078,1281023.8940076078,0.

        near-1e9-a,near-1e9-b,near-1e9-c
        1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
        1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      -|999ms999us999ns,1s,1s1ns
      -|-999ms999us999ns,-1s,-1s1ns
      +|0.0002777777775,0.00027777777777777778,0.00027777777805555558
      +|-0.0002777777775,-0.00027777777777777778,-0.00027777777805555558
      |}];
    let%bind () = run [ "span"; "-unit"; "min" ] in
    [%expect
      {|
        min,max,zero
        1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      -|-53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s
      +|-76861433.640456468,76861433.640456468,0.

        near-1e9-a,near-1e9-b,near-1e9-c
        1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
        1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      -|999ms999us999ns,1s,1s1ns
      -|-999ms999us999ns,-1s,-1s1ns
      +|0.01666666665,0.016666666666666666,0.016666666683333334
      +|-0.01666666665,-0.016666666666666666,-0.016666666683333334
      |}];
    let%bind () = run [ "span"; "-unit"; "s" ] in
    [%expect
      {|
        min,max,zero
        1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      -|-53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s
      +|-4611686018.427387904,4611686018.427387903,0.000000000

        near-1e9-a,near-1e9-b,near-1e9-c
        1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
        1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      -|999ms999us999ns,1s,1s1ns
      -|-999ms999us999ns,-1s,-1s1ns
      +|0.999999999,1.000000000,1.000000001
      +|-0.999999999,-1.000000000,-1.000000001
      |}];
    let%bind () = run [ "span"; "-unit"; "ms" ] in
    [%expect
      {|
        min,max,zero
        1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      -|-53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s
      +|-4611686018427.387904,4611686018427.387903,0.000000

        near-1e9-a,near-1e9-b,near-1e9-c
        1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
        1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      -|999ms999us999ns,1s,1s1ns
      -|-999ms999us999ns,-1s,-1s1ns
      +|999.999999,1000.000000,1000.000001
      +|-999.999999,-1000.000000,-1000.000001
      |}];
    let%bind () = run [ "span"; "-unit"; "us" ] in
    [%expect
      {|
        min,max,zero
        1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      -|-53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s
      +|-4611686018427387.904,4611686018427387.903,0.000

        near-1e9-a,near-1e9-b,near-1e9-c
        1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
        1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      -|999ms999us999ns,1s,1s1ns
      -|-999ms999us999ns,-1s,-1s1ns
      +|999999.999,1000000.000,1000000.001
      +|-999999.999,-1000000.000,-1000000.001
      |}];
    let%bind () = run [ "span"; "-unit"; "ns" ] in
    [%expect
      {|
        min,max,zero
        1823-11-12 00:06:21.572612096Z,2116-02-20 23:53:38.427387903Z,1970-01-01 00:00:00Z
      -|-53375d23h53m38.427387904s,53375d23h53m38.427387903s,0s
      +|-4611686018427387904,4611686018427387903,0

        near-1e9-a,near-1e9-b,near-1e9-c
        1970-01-01 00:00:00.999999999Z,1970-01-01 00:00:01Z,1970-01-01 00:00:01.000000001Z
        1969-12-31 23:59:59.000000001Z,1969-12-31 23:59:59Z,1969-12-31 23:59:58.999999999Z
      -|999ms999us999ns,1s,1s1ns
      -|-999ms999us999ns,-1s,-1s1ns
      +|999999999,1000000000,1000000001
      +|-999999999,-1000000000,-1000000001
      |}];
    return ())
;;

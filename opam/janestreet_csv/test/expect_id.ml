open! Core
open! Async
open! Import

let%expect_test "simple" =
  do_test (fun () ->
    let%bind () =
      Writer.with_file "input.csv" ~f:(fun writer ->
        Writer.write
          writer
          {|"foo","bar","baz"
"1","2","3"
"x","y","z"
"","foo""bar",xyz
|};
        Writer.flushed writer)
    in
    let%bind () = run "csv" [ "id"; "input.csv" ] in
    [%expect
      {|
      foo,bar,baz
      1,2,3
      x,y,z
      ,"foo""bar",xyz
      |}];
    return ())
;;

let%expect_test "ragged" =
  do_test (fun () ->
    let%bind () =
      Writer.with_file "input.csv" ~f:(fun writer ->
        Writer.write
          writer
          {|"foo","bar","baz"
"1","2"
"x"
"","foo""bar",xyz,pqr
|};
        Writer.flushed writer)
    in
    let%bind () = run "csv" [ "id"; "input.csv" ] in
    [%expect
      {|
      foo,bar,baz
      1,2
      x
      ,"foo""bar",xyz,pqr
      |}];
    return ())
;;

let%expect_test "suppress header" =
  do_test (fun () ->
    let%bind () =
      Writer.with_file "input.csv" ~f:(fun writer ->
        Writer.write
          writer
          {|"foo","bar","baz"
"1","2","3"
"x","y","z"
"","foo""bar",xyz
|};
        Writer.flushed writer)
    in
    let%bind () = run "csv" [ "id"; "input.csv"; "-sh" ] in
    [%expect
      {|
      1,2,3
      x,y,z
      ,"foo""bar",xyz
      |}];
    return ())
;;

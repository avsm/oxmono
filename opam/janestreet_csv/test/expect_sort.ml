open! Core
open! Async
open! Import

let default_data =
  [ [ "string"; "int"; "float"; "time" ]
  ; [ "Abraham"; "1"; "1.1"; "0001-01-01T0000-00:00" ]
  ; [ "Bathsheba"; "2"; "2.2"; "0001-01-01T0500+10:00" ]
  ; [ "Cyrus"; "2"; "3.3"; "0002-01-01T0000-00:00" ]
  ; [ "Deborah"; "04"; "4.4"; "0003-01-01T0000-00:00" ]
  ; [ "Elijah"; "5"; "6.5"; "0004-01-01T0000-00:00" ]
  ]
;;

module Test_mode = struct
  type t =
    | Normal
    | Strip_header_and_treat_as_anonymous
  [@@deriving enumerate]
end

let make_input_csv ?sep ~data filename = make_input_csv ?sep filename data

let sort_test ?sep ~data ?field ?field_type flags ~test_mode =
  let flags =
    match field_type with
    | None -> flags
    | Some t -> "-field-type" :: t :: flags
  in
  let data, field, no_headers_flag =
    match (test_mode : Test_mode.t) with
    | Normal -> data, field, []
    | Strip_header_and_treat_as_anonymous ->
      let header, data = List.hd_exn data, List.tl_exn data in
      let field =
        let%map.Option field in
        let header_idx =
          List.mapi header ~f:(fun i h -> h, i) |> Map.of_alist_exn (module String)
        in
        String.split field ~on:','
        |> List.map ~f:(Map.find_exn header_idx)
        |> List.map ~f:Int.to_string
        |> String.concat ~sep:","
      in
      data, field, [ "-no-headers" ]
  in
  let flags =
    match field with
    | None -> flags
    | Some t -> "-field" :: t :: flags
  in
  let flags = flags @ no_headers_flag in
  do_test (fun () ->
    let%bind () = make_input_csv ?sep ~data "input.csv" in
    run "csv" ("sort" :: "input.csv" :: flags))
;;

(** Allows us to reuse all the existing tests for sorts on CSVs with a header to also test
    sorts on CSVs without a header. We get the output for both test modes, assert that the
    output for the anonymous CSV is the same as the output for the named CSV (sans the
    header), then print the output for the named CSV. *)
let sort_test_with_named_and_anonymous_of_named
  ?sep
  ?(data = default_data)
  ?field
  ?field_type
  flags
  =
  let%map outputs =
    Deferred.List.map Test_mode.all ~how:`Sequential ~f:(fun test_mode ->
      let%map () = sort_test ?sep ~data ?field ?field_type flags ~test_mode in
      Expect_test_helpers_core.expect_test_output ())
  in
  let output_for_named, output_for_anonymous =
    match outputs with
    | [ n; a ] -> n, a
    | _ -> failwith "too many test modes!"
  in
  let filter_out_header =
    let header_line =
      List.hd_exn data
      |> String.concat ~sep:(Option.value_map sep ~default:"," ~f:Char.to_string)
    in
    List.filter ~f:(Fn.non (String.equal header_line))
  in
  if String.( <> )
       (output_for_named
        |> String.split_lines
        |> filter_out_header
        |> String.concat_lines
        |> String.strip)
       (output_for_anonymous |> String.strip)
  then (
    print_cr [%message "Different output with and without header"];
    Expect_test_patdiff.print_patdiff output_for_named output_for_anonymous);
  print_endline output_for_named
;;

let sort_test_with_named_and_anonymous_of_named_multi
  (type a)
  (cases : a list)
  ~header_lines
  ~f
  =
  let%map outputs =
    Deferred.List.map Test_mode.all ~how:`Sequential ~f:(fun test_mode ->
      let%map () =
        Deferred.List.iter cases ~how:`Sequential ~f:(fun case ->
          let data, field, flags = f case ~test_mode in
          sort_test ~data ~field flags ~test_mode)
      in
      Expect_test_helpers_core.expect_test_output ())
  in
  let output_for_named, output_for_anonymous =
    match outputs with
    | [ n; a ] -> n, a
    | _ -> failwith "too many test modes!"
  in
  let filter_out_headers =
    List.filter ~f:(Fn.non (List.mem header_lines ~equal:String.equal))
  in
  (fun with_ without ->
    if String.( <> ) with_ without
    then (
      print_cr [%message "Different output with and without header" with_ without];
      Expect_test_patdiff.print_patdiff with_ without))
    (output_for_named
     |> String.split_lines
     |> filter_out_headers
     |> String.concat_lines
     |> String.strip)
    (output_for_anonymous |> String.split_lines |> String.concat_lines |> String.strip);
  print_endline output_for_named
;;

(* Sort on a field as a string. *)
let%expect_test _ =
  let%map () =
    sort_test_with_named_and_anonymous_of_named ~field:"string" ~field_type:"string" []
  in
  [%expect
    {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}]
;;

(* Reversed string sort. *)
let%expect_test _ =
  let%map () =
    sort_test_with_named_and_anonymous_of_named
      ~field:"string"
      ~field_type:"string"
      [ "-reverse" ]
  in
  [%expect
    {|
    string,int,float,time
    Elijah,5,6.5,0004-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Abraham,1,1.1,0001-01-01T0000-00:00
    |}]
;;

(* Sort on an int field as an int. *)
let%expect_test _ =
  let%map () =
    sort_test_with_named_and_anonymous_of_named ~field:"int" ~field_type:"int" []
  in
  [%expect
    {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}]
;;

(* Sort on an int field as a string. *)
let%expect_test _ =
  let%map () =
    sort_test_with_named_and_anonymous_of_named ~field:"int" ~field_type:"string" []
  in
  [%expect
    {|
    string,int,float,time
    Deborah,04,4.4,0003-01-01T0000-00:00
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}]
;;

(* Reverse int sort. Note the stability. *)
let%expect_test _ =
  let%map () =
    sort_test_with_named_and_anonymous_of_named
      ~field:"int"
      ~field_type:"int"
      [ "-reverse" ]
  in
  [%expect
    {|
    string,int,float,time
    Elijah,5,6.5,0004-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Abraham,1,1.1,0001-01-01T0000-00:00
    |}]
;;

(* Sort on a float field as a float. *)
let%expect_test _ =
  let%map () =
    sort_test_with_named_and_anonymous_of_named ~field:"float" ~field_type:"float" []
  in
  [%expect
    {|
    string,int,float,time
    Abraham,1,1.1,0001-01-01T0000-00:00
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}]
;;

(* Sort on a time field as a time. Note the time zone of Bathsheba, which breaks the usual
   equivalence between lexicographic and chronological ordering of ISO 8601 timestamps. *)
let%expect_test _ =
  let%map () =
    sort_test_with_named_and_anonymous_of_named ~field:"time" ~field_type:"time" []
  in
  [%expect
    {|
    string,int,float,time
    Bathsheba,2,2.2,0001-01-01T0500+10:00
    Abraham,1,1.1,0001-01-01T0000-00:00
    Cyrus,2,3.3,0002-01-01T0000-00:00
    Deborah,04,4.4,0003-01-01T0000-00:00
    Elijah,5,6.5,0004-01-01T0000-00:00
    |}]
;;

(* Delimiter. *)
let%expect_test _ =
  let%map () =
    sort_test_with_named_and_anonymous_of_named
      ~field:"string"
      ~field_type:"string"
      ~sep:'.'
      [ "-sep"; "." ]
  in
  [%expect
    {|
    string.int.float.time
    Abraham.1."1.1".0001-01-01T0000-00:00
    Bathsheba.2."2.2".0001-01-01T0500+10:00
    Cyrus.2."3.3".0002-01-01T0000-00:00
    Deborah.04."4.4".0003-01-01T0000-00:00
    Elijah.5."6.5".0004-01-01T0000-00:00
    |}]
;;

(* natural sort requires some different data: *)

let%expect_test _ =
  let data =
    [ [ "name_with_ints"; "value" ]
    ; [ "sys1"; "foo" ]
    ; [ "sys2"; "bar" ]
    ; [ "sys10"; "baz" ]
    ; [ "sys200"; "qwux" ]
    ]
  in
  let sort how =
    sort_test_with_named_and_anonymous_of_named
      ~data
      ~field:"name_with_ints"
      ~field_type:how
      []
  in
  let%bind () = sort "string" in
  [%expect
    {|
    name_with_ints,value
    sys1,foo
    sys10,baz
    sys2,bar
    sys200,qwux
    |}];
  let%bind () = sort "natsort" in
  [%expect
    {|
    name_with_ints,value
    sys1,foo
    sys2,bar
    sys10,baz
    sys200,qwux
    |}];
  return ()
;;

let%expect_test "bytes, spans" =
  let data =
    [ [ "latency"; "size" ]; [ "2h"; "17b" ]; [ "1ns"; "1K" ]; [ "3us"; "1.2G" ] ]
  in
  let sort field field_type =
    sort_test_with_named_and_anonymous_of_named ~field ~field_type ~data []
  in
  let%bind () = sort "latency" "span" in
  [%expect
    {|
    latency,size
    1ns,1K
    3us,1.2G
    2h,17b
    |}];
  let%bind () = sort "size" "bytes" in
  [%expect
    {|
    latency,size
    2h,17b
    1ns,1K
    3us,1.2G
    |}];
  return ()
;;

let%expect_test "inference" =
  let headers = [ "string"; "int"; "bytes"; "float" ] in
  let data =
    (* bytes column chosen to detect if we're just ignoring the suffix. *)
    [ headers
    ; [ "a"; "4"; "1g"; "3" ]
    ; [ "d"; "5"; "3k"; "2.5" ]
    ; [ "c"; "2"; "2m"; "0.17e4" ]
    ]
  in
  let%map () =
    sort_test_with_named_and_anonymous_of_named_multi
      headers
      ~header_lines:[ String.concat headers ~sep:"," ]
      ~f:(fun field ~test_mode:_ ->
        print_newline ();
        print_endline [%string "sorted by %{field}\n================="];
        data, field, [])
  in
  [%expect
    {|
    sorted by string
    =================
    string,int,bytes,float
    a,4,1g,3
    c,2,2m,0.17e4
    d,5,3k,2.5

    sorted by int
    =================
    string,int,bytes,float
    c,2,2m,0.17e4
    a,4,1g,3
    d,5,3k,2.5

    sorted by bytes
    =================
    string,int,bytes,float
    d,5,3k,2.5
    c,2,2m,0.17e4
    a,4,1g,3

    sorted by float
    =================
    string,int,bytes,float
    d,5,3k,2.5
    a,4,1g,3
    c,2,2m,0.17e4
    |}]
;;

let multi_column_headers = [ "string"; "int"; "bytes"; "float" ]

let multi_column_data =
  [ multi_column_headers
    (* 1g vs 3k in bytes column chosen to detect if we're just ignoring the suffix. *)
  ; [ "b"; "2"; "1g"; "0.17e4" ]
  ; [ "b"; "1"; "1g"; "0.17e4" ]
  ; [ "b"; "1"; "1g"; "   2.5" ]
  ; [ "a"; "1"; "1g"; "   2.5" ]
  ; [ "a"; "2"; "3k"; "0.17e4" ]
  ; [ "a"; "1"; "3k"; "0.17e4" ]
  ; [ "b"; "1"; "3k"; "   2.5" ]
  ; [ "a"; "1"; "1g"; "0.17e4" ]
  ; [ "a"; "1"; "3k"; "   2.5" ]
  ; [ "b"; "2"; "3k"; "0.17e4" ]
  ; [ "b"; "1"; "3k"; "0.17e4" ]
  ; [ "a"; "2"; "1g"; "0.17e4" ]
  ; [ "a"; "2"; "1g"; "   2.5" ]
  ; [ "b"; "2"; "1g"; "   2.5" ]
  ; [ "a"; "2"; "3k"; "   2.5" ]
  ; [ "b"; "2"; "3k"; "   2.5" ]
  ]
;;

let multi_column_sort_test ~extra_args =
  let headers = multi_column_headers in
  let headers_i = List.mapi headers ~f:(fun i header -> i, header) in
  let data = multi_column_data in
  let field_pairs =
    List.cartesian_product headers_i headers_i
    |> List.filter ~f:(fun ((_, x), (_, y)) -> String.(x <> y))
  in
  sort_test_with_named_and_anonymous_of_named_multi
    field_pairs
    ~header_lines:[ String.concat headers ~sep:"," ]
    ~f:(fun ((i1, field1), (i2, field2)) ~test_mode ->
      let comma_separated_fields = [%string "%{field1},%{field2}"] in
      print_newline ();
      print_endline [%string "sorted by %{comma_separated_fields}\n================="];
      let extra_args =
        match (test_mode : Test_mode.t) with
        | Normal -> extra_args ~fields:(field1, field2)
        | Strip_header_and_treat_as_anonymous ->
          extra_args ~fields:(Int.to_string i1, Int.to_string i2)
      in
      data, comma_separated_fields, extra_args)
;;

let%expect_test "multi-column" =
  let%bind () = multi_column_sort_test ~extra_args:(fun ~fields:_ -> []) in
  [%expect
    {|
    sorted by string,int
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by string,bytes
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5

    sorted by string,float
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4

    sorted by int,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by int,bytes
    =================
    string,int,bytes,float
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5

    sorted by int,float
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4

    sorted by bytes,string
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5

    sorted by bytes,int
    =================
    string,int,bytes,float
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5

    sorted by bytes,float
    =================
    string,int,bytes,float
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4

    sorted by float,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4

    sorted by float,int
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4

    sorted by float,bytes
    =================
    string,int,bytes,float
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    |}];
  return ()
;;

let%expect_test "multi-column-all-rev" =
  let%bind () = multi_column_sort_test ~extra_args:(fun ~fields:_ -> [ "-reverse" ]) in
  [%expect
    {|
    sorted by string,int
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5

    sorted by string,bytes
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5

    sorted by string,float
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5

    sorted by int,string
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5

    sorted by int,bytes
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4

    sorted by int,float
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5

    sorted by bytes,string
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5

    sorted by bytes,int
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4

    sorted by bytes,float
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by float,string
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5

    sorted by float,int
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5

    sorted by float,bytes
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    |}];
  return ()
;;

let%expect_test "multi-column-one-rev" =
  let%bind () =
    multi_column_sort_test ~extra_args:(fun ~fields:(x, _) -> [ "-reverse-fields"; x ])
  in
  [%expect
    {|
    sorted by string,int
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5

    sorted by string,bytes
    =================
    string,int,bytes,float
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5

    sorted by string,float
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4

    sorted by int,string
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4

    sorted by int,bytes
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4

    sorted by int,float
    =================
    string,int,bytes,float
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4

    sorted by bytes,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5

    sorted by bytes,int
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by bytes,float
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4

    sorted by float,string
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by float,int
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by float,bytes
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    |}];
  return ()
;;

let%expect_test "multi-column-sort-types" =
  let%bind () =
    multi_column_sort_test ~extra_args:(fun ~fields:_ ->
      [ "-field-types"; "string,natsort" ])
  in
  [%expect
    {|
    sorted by string,int
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by string,bytes
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5

    sorted by string,float
    =================
    string,int,bytes,float
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by int,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,1,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,2,1g,0.17e4
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,3k,   2.5

    sorted by int,bytes
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by int,float
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by bytes,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,1,3k,   2.5
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,2,3k,   2.5

    sorted by bytes,int
    =================
    string,int,bytes,float
    b,1,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,1,1g,0.17e4
    b,2,1g,0.17e4
    a,2,1g,0.17e4
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    b,1,3k,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by bytes,float
    =================
    string,int,bytes,float
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5

    sorted by float,string
    =================
    string,int,bytes,float
    a,1,1g,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    a,2,3k,   2.5
    b,1,1g,   2.5
    b,1,3k,   2.5
    b,2,1g,   2.5
    b,2,3k,   2.5
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4

    sorted by float,int
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,1,1g,0.17e4
    a,1,3k,0.17e4
    a,1,1g,0.17e4
    b,1,3k,0.17e4
    b,2,1g,0.17e4
    a,2,3k,0.17e4
    b,2,3k,0.17e4
    a,2,1g,0.17e4

    sorted by float,bytes
    =================
    string,int,bytes,float
    b,1,1g,   2.5
    a,1,1g,   2.5
    a,2,1g,   2.5
    b,2,1g,   2.5
    b,1,3k,   2.5
    a,1,3k,   2.5
    a,2,3k,   2.5
    b,2,3k,   2.5
    b,2,1g,0.17e4
    b,1,1g,0.17e4
    a,1,1g,0.17e4
    a,2,1g,0.17e4
    a,2,3k,0.17e4
    a,1,3k,0.17e4
    b,2,3k,0.17e4
    b,1,3k,0.17e4
    |}];
  return ()
;;

let%expect_test "no fields" =
  let%bind () = sort_test_with_named_and_anonymous_of_named ~data:multi_column_data [] in
  [%expect
    {|
    string,int,bytes,float
    a,1,3k,   2.5
    a,1,3k,0.17e4
    a,1,1g,   2.5
    a,1,1g,0.17e4
    a,2,3k,   2.5
    a,2,3k,0.17e4
    a,2,1g,   2.5
    a,2,1g,0.17e4
    b,1,3k,   2.5
    b,1,3k,0.17e4
    b,1,1g,   2.5
    b,1,1g,0.17e4
    b,2,3k,   2.5
    b,2,3k,0.17e4
    b,2,1g,   2.5
    b,2,1g,0.17e4
    |}];
  return ()
;;

let%expect_test "no fields reversed" =
  let%bind () =
    sort_test_with_named_and_anonymous_of_named ~data:multi_column_data [ "-reverse" ]
  in
  [%expect
    {|
    string,int,bytes,float
    b,2,1g,0.17e4
    b,2,1g,   2.5
    b,2,3k,0.17e4
    b,2,3k,   2.5
    b,1,1g,0.17e4
    b,1,1g,   2.5
    b,1,3k,0.17e4
    b,1,3k,   2.5
    a,2,1g,0.17e4
    a,2,1g,   2.5
    a,2,3k,0.17e4
    a,2,3k,   2.5
    a,1,1g,0.17e4
    a,1,1g,   2.5
    a,1,3k,0.17e4
    a,1,3k,   2.5
    |}];
  return ()
;;

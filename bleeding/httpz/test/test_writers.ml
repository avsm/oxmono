(* test_writers.ml - checks for the table-driven response writers.

   {!Httpz.Buf_write.int} emits two digits per division from a lookup table,
   and {!Httpz.Res.write_status_line} copies a single static string per status.
   Both replaced straightforward code with tables that are easy to get subtly
   wrong, and both are checked here against an independent reference. *)

open Base

module I16 = Stdlib_stable.Int16_u
module I64 = Stdlib_upstream_compatible.Int64_u

let[@inline always] i16 x = I16.of_int x
let[@inline always] to_int x = I16.to_int x
let failures = ref 0

let check name cond detail =
  if not cond
  then begin
    Int.incr failures;
    if !failures <= 20 then Stdio.printf "FAIL [%s] %s\n" name (detail ())
  end
;;

let buf = Bytes.make 256 '\000'

let written f =
  Bytes.fill buf ~pos:0 ~len:256 '\000';
  let off = f buf ~off:(i16 0) in
  Bytes.To_string.sub buf ~pos:0 ~len:(to_int off)
;;

(* Every value where a digit-count boundary or a two-digit-table index could
   go wrong, plus a dense sweep of the small values that Content-Length and
   header integers actually take. *)
let test_int () =
  let boundaries =
    [ 0; 1; 9; 10; 11; 99; 100; 101; 999; 1_000; 9_999; 10_000; 99_999; 100_000
    ; 999_999; 1_000_000; 9_999_999; 10_000_000; 99_999_999; 100_000_000
    ; 999_999_999; 1_000_000_000; 4_294_967_295; 1_000_000_000_000
    ; Int.max_value
    ]
  in
  List.iter boundaries ~f:(fun n ->
    let got = written (fun buf ~off -> Httpz.Buf_write.int buf ~off n) in
    check "int/boundary" (String.equal got (Int.to_string n)) (fun () ->
      Printf.sprintf "n=%d got=%S" n got));
  for n = 0 to 200_000 do
    let got = written (fun buf ~off -> Httpz.Buf_write.int buf ~off n) in
    check "int/sweep" (String.equal got (Int.to_string n)) (fun () ->
      Printf.sprintf "n=%d got=%S" n got)
  done
;;

let test_int64 () =
  let cases =
    [ 0L; 1L; 9L; 10L; 99L; 100L; 12_345L; 1_000_000L; 4_294_967_296L
    ; 9_223_372_036_854_775_807L
    ]
  in
  List.iter cases ~f:(fun n ->
    let got =
      written (fun buf ~off -> Httpz.Buf_write.int64 buf ~off (I64.of_int64 n))
    in
    check "int64" (String.equal got (Int64.to_string n)) (fun () ->
      Printf.sprintf "n=%Ld got=%S" n got))
;;

(* The status line is now one static string per status; it must still be
   assembled exactly as "<version> <code> <reason>\r\n". *)
let test_status_line () =
  let statuses =
    [ 100; 101; 200; 201; 202; 204; 205; 206; 207; 301; 302; 303; 304; 307; 308; 400
    ; 401; 403; 404; 405; 406; 408; 409; 410; 411; 412; 413; 414; 415; 416; 417
    ; 422; 423; 424; 426; 428; 429; 500; 501; 502; 503; 504; 505; 507
    ]
  in
  List.iter statuses ~f:(fun code ->
    match Httpz.Res.status_of_int code with
    | None -> check "status/known" false (fun () -> Printf.sprintf "code=%d" code)
    | Some st ->
      let reason = Httpz.Res.status_reason st in
      check "status/code" (Httpz.Res.status_code st = code) (fun () ->
        Printf.sprintf "code=%d" code);
      check
        "status/to_string"
        (String.equal (Httpz.Res.status_to_string st) (Printf.sprintf "%d %s" code reason))
        (fun () -> Printf.sprintf "code=%d got=%S" code (Httpz.Res.status_to_string st));
      List.iter
        [ Httpz.Version.Http_1_1, "HTTP/1.1"; Httpz.Version.Http_1_0, "HTTP/1.0" ]
        ~f:(fun (v, vs) ->
          let got =
            written (fun buf ~off -> Httpz.Res.write_status_line buf ~off st v)
          in
          let expect = Printf.sprintf "%s %d %s\r\n" vs code reason in
          check "status/line" (String.equal got expect) (fun () ->
            Printf.sprintf "code=%d got=%S expect=%S" code got expect)))
;;

(* These two became static strings rather than name/value assembly. *)
let test_fixed_headers () =
  let got = written (fun buf ~off -> Httpz.Res.write_connection buf ~off ~keep_alive:true) in
  check "connection/keep_alive" (String.equal got "Connection: keep-alive\r\n") (fun () ->
    Printf.sprintf "got=%S" got);
  let got =
    written (fun buf ~off -> Httpz.Res.write_connection buf ~off ~keep_alive:false)
  in
  check "connection/close" (String.equal got "Connection: close\r\n") (fun () ->
    Printf.sprintf "got=%S" got);
  let got = written (fun buf ~off -> Httpz.Res.write_transfer_encoding_chunked buf ~off) in
  check "te/chunked" (String.equal got "Transfer-Encoding: chunked\r\n") (fun () ->
    Printf.sprintf "got=%S" got)
;;

let () =
  test_int ();
  test_int64 ();
  test_status_line ();
  test_fixed_headers ();
  if !failures > 0
  then begin
    Stdio.printf "%d writer failures\n" !failures;
    Stdlib.exit 1
  end;
  Stdio.printf "test_writers: integer and status-line writers match reference\n"
;;

(* Every spelling inet_aton(3) accepts, and the near misses it does not. *)

open Base
module Ip = Httpz.Ip

let failures = ref 0

let check name cond detail =
  if not cond
  then (
    Int.incr failures;
    if !failures <= 20 then Stdio.printf "FAIL [%s] %s\n" name (detail ()))
;;

let canonical =
  [ "127.0.0.1", "127.0.0.1"
  ; "127.1", "127.0.0.1"
  ; "127.0.1", "127.0.0.1"
  ; "2130706433", "127.0.0.1"
  ; "0x7f000001", "127.0.0.1"
  ; "0X7F000001", "127.0.0.1"
  ; "0177.0.0.1", "127.0.0.1"
  ; "0x7f.1", "127.0.0.1"
  ; "0x7f.0.0.1", "127.0.0.1"
  ; "0xc0.0.0.1", "192.0.0.1"
  ; "1.2.3", "1.2.0.3"
  ; "0", "0.0.0.0"
  ; "0.0.0.0", "0.0.0.0"
  ; "00", "0.0.0.0"
  ; "4294967295", "255.255.255.255"
  ; "0xffffffff", "255.255.255.255"
  ; "255.255.255.255", "255.255.255.255"
  ; "192.168.000.001", "192.168.0.1"
  ; "010.010.010.010", "8.8.8.8"
  ; "127.0.0.1.", "127.0.0.1"
  ]
;;

let not_ipv4 =
  [ ""
  ; "."
  ; "1.2.3.4.5"
  ; "1..2"
  ; ".1.2.3"
  ; "0x"
  ; "0X"
  ; "08"
  ; "0.09"
  ; "256.1.1.1"
  ; "1.2.3.256"
  ; "1.2.256.3"
  ; "4294967296"
  ; "0x100000000"
  ; "example.com"
  ; "12abc"
  ; "127.0.0.1a"
  ; "-1.2.3.4"
  ; "1.2.3.4 "
  ; " 1.2.3.4"
  ; "0x7f.0x1.0x1.0x1.0x1"
  ; "::1"
  ; "127.0.0.1.."
  ]
;;

let test_ipv4 () =
  List.iter canonical ~f:(fun (input, expect) ->
    check
      "ipv4_canonical"
      (match Ip.ipv4_canonical input with
       | Some got -> String.equal got expect
       | None -> false)
      (fun () ->
         Printf.sprintf
           "%S -> %s, want %S"
           input
           (Option.value ~default:"None" (Ip.ipv4_canonical input))
           expect);
    check "is_ipv4_literal" (Ip.is_ipv4_literal input) (fun () ->
      Printf.sprintf "%S not recognized" input);
    check "is_literal" (Ip.is_literal input) (fun () ->
      Printf.sprintf "%S not a literal" input));
  List.iter not_ipv4 ~f:(fun input ->
    check "not ipv4" (not (Ip.is_ipv4_literal input)) (fun () ->
      Printf.sprintf "%S accepted as IPv4" input))
;;

(* The 32-bit value, not just its rendering: a policy that stores addresses
   needs the two spellings to compare equal. *)
let test_ipv4_value () =
  let loopback = Ip.ipv4_of_string "127.0.0.1" in
  List.iter [ "127.1"; "127.0.1"; "2130706433"; "0x7f000001"; "0177.0.0.1" ] ~f:(fun s ->
    check
      "same value"
      (match loopback, Ip.ipv4_of_string s with
       | Some a, Some b -> a = b
       | _ -> false)
      (fun () -> Printf.sprintf "%S differs from 127.0.0.1" s));
  check
    "host byte order"
    (match Ip.ipv4_of_string "1.2.3.4" with
     | Some v -> v = 0x01020304
     | None -> false)
    (fun () -> "1.2.3.4 is not 0x01020304")
;;

let test_ipv6 () =
  let yes =
    [ "::1"
    ; "[::1]"
    ; "2001:db8::1"
    ; "[2001:db8::1]"
    ; "::ffff:127.0.0.1"
    ; "fe80::1%lo"
    ; "fe80::1%25"
    ; "[fe80::1%lo]"
    ; "[fe80::1%25]"
    ; "[fe80::1%25eth0]"
    ]
  in
  let no =
    [ ""
    ; "[]"
    ; ":::"
    ; "[:::]"
    ; "99999::1"
    ; "[99999::1]"
    ; "gg::1"
    ; "127.0.0.1"
    ; "fe80::1%"
    ]
  in
  List.iter yes ~f:(fun s ->
    check "is_ipv6_literal" (Ip.is_ipv6_literal s) (fun () ->
      Printf.sprintf "%S not recognized" s);
    check "is_literal/v6" (Ip.is_literal s) (fun () ->
      Printf.sprintf "%S not a literal" s));
  List.iter no ~f:(fun s ->
    check "not ipv6" (not (Ip.is_ipv6_literal s)) (fun () ->
      Printf.sprintf "%S accepted as IPv6" s));
  (* An IPv4-mapped tail stays IPv6: folding it here would let a policy
     believe it had checked the address the socket will use. *)
  check
    "mapped tail is not IPv4"
    (not (Ip.is_ipv4_literal "::ffff:127.0.0.1"))
    (fun () -> "::ffff:127.0.0.1 recognized as IPv4")
;;

let () =
  test_ipv4 ();
  test_ipv4_value ();
  test_ipv6 ();
  if !failures > 0
  then (
    Stdio.printf "%d Ip failures\n" !failures;
    Stdlib.exit 1);
  Stdio.printf "test_ip: inet_aton spellings and IPv6 literals recognized\n"
;;

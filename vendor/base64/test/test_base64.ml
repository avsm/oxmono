(* This copy of base64 is patched for OxCaml portability, so the test pins both
   halves of that patch: the RFC 4648 vectors still decode and encode as they
   did with the upstream [int array] tables, and the interface is callable from
   a portable context. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* RFC 4648 section 10. *)
let vectors =
  [
    ("", "");
    ("f", "Zg==");
    ("fo", "Zm8=");
    ("foo", "Zm9v");
    ("foob", "Zm9vYg==");
    ("fooba", "Zm9vYmE=");
    ("foobar", "Zm9vYmFy");
  ]

let decodes ?pad ?alphabet s = Base64.decode ?pad ?alphabet s

let () =
  List.iter
    (fun (plain, encoded) ->
      check ("encode " ^ plain) (Base64.encode_string plain = encoded);
      check ("decode " ^ encoded) (decodes encoded = Ok plain))
    vectors;
  check "the unpadded encoding drops the padding"
    (Base64.encode_string ~pad:false "foob" = "Zm9vYg");
  check "an unpadded input decodes with [pad:false]"
    (decodes ~pad:false "Zm9vYg" = Ok "foob");
  check "an unpadded input is refused with [pad:true]"
    (Result.is_error (decodes "Zm9vYg"));
  check "a character outside the alphabet is refused"
    (Result.is_error (decodes "Zm9*Yg=="));
  check "padding in the middle is refused"
    (Result.is_error (decodes "Zm=vYmFy"));
  check "an out of range window is refused"
    (Result.is_error (Base64.decode ~off:4 ~len:8 "Zm9vYmFy"));
  check "a window of the input decodes"
    (Base64.decode ~off:4 ~len:4 "....Zm9v" = Ok "foo");
  check "the URI-safe alphabet is used where asked"
    (Base64.encode_string ~alphabet:Base64.uri_safe_alphabet "\xff\xef\xfe"
   = "_-_-");
  check "the URI-safe alphabet round-trips"
    (decodes ~alphabet:Base64.uri_safe_alphabet "_-_-" = Ok "\xff\xef\xfe");
  check "the default alphabet reads back"
    (Base64.alphabet Base64.default_alphabet
   = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/");
  check "an alphabet is 64 characters long"
    (Base64.length_alphabet Base64.uri_safe_alphabet = 64);
  check "a short alphabet is refused"
    (match Base64.make_alphabet "abc" with
    | exception Invalid_argument _ -> true
    | _ -> false);
  check "an alphabet holding the padding character is refused"
    (match
       Base64.make_alphabet
         "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
     with
    | exception Invalid_argument _ -> true
    | _ -> false);
  Printf.printf "test_base64: %d checks ok\n" !checks

(* The point of the patch: a closure over the interface is portable, which it
   is not if the tables the alphabets hold are mutable. *)
let _portable =
  ((fun () ->
     ignore (Base64.decode "Zm9v" : (string, [ `Msg of string ]) result);
     ignore (Base64.encode_string ~alphabet:Base64.uri_safe_alphabet "foo"))
    : _ @ portable)

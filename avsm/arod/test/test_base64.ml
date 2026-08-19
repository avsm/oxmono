(* [base64] is vendored under vendor/ and patched so that its interface is
   callable from the portable check {!Arod_handlers.stats_auth} is used as.
   Dune skips aliases under a vendored directory, so the vendored copy's own
   test never runs. This one does, and it fails if a re-vendor drops the
   patch or changes what the auth path decodes. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* The ascription is the point: it does not compile unless [base64.mli] still
   carries its [@@ portable] annotations. *)
let decode = (Base64.decode : (string -> _) @ portable)

let () =
  (* RFC 4648 section 10. *)
  check "the empty string decodes" (decode "" = Ok "");
  check "one padded byte decodes" (decode "Zg==" = Ok "f");
  check "two padded bytes decode" (decode "Zm8=" = Ok "fo");
  check "a whole group decodes" (decode "Zm9v" = Ok "foo");
  check "several groups decode" (decode "Zm9vYmFy" = Ok "foobar");
  check "an unpadded length is refused" (Result.is_error (decode "Zm9vYg"));
  check "a character outside the alphabet is refused"
    (Result.is_error (decode "Zm9*Yg=="));
  Printf.printf "test_base64: %d checks ok\n" !checks

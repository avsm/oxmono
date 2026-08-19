(* The stats dashboard is gated on [stats_auth] alone, and it decodes the
   credentials itself rather than through the base64 library, which a portable
   check cannot call. Each way the decode can be fed bad input is pinned. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let accepts ~password auth =
  Arod_handlers.stats_auth ~password (Some ("Basic " ^ auth))

let () =
  check "the right password is accepted"
    (accepts ~password:"s3cret" "dXNlcjpzM2NyZXQ=");
  check "the user name is ignored"
    (accepts ~password:"hunter2" "YW5pbDpodW50ZXIy");
  check "a password holding a colon keeps it"
    (accepts ~password:"b:c" "YTpiOmM=");
  check "an empty user name is accepted" (accepts ~password:"pw" "OnB3");
  check "a wrong password is refused"
    (not (accepts ~password:"other" "dXNlcjpzM2NyZXQ="));
  check "credentials with no colon are refused"
    (not (accepts ~password:"nocolon" "bm9jb2xvbg=="));
  check "no Authorization field is refused"
    (not (Arod_handlers.stats_auth ~password:"s3cret" None));
  check "another scheme is refused"
    (not (Arod_handlers.stats_auth ~password:"s3cret" (Some "Bearer abc")));
  check "an unpadded length is refused"
    (not (accepts ~password:"s3cret" "dXNlcjpzM2NyZXQ"));
  check "a character outside the alphabet is refused"
    (not (accepts ~password:"s3cret" "dXNlcjpzM2N*ZXQ="));
  check "padding in the middle is refused"
    (not (accepts ~password:"s3cret" "dXNl=jpzM2NyZXQ="));
  check "empty credentials are refused" (not (accepts ~password:"" ""));
  Printf.printf "test_auth: %d checks ok\n" !checks

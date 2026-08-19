(* The stats dashboard is gated on [stats_auth] alone. Each way the credentials
   can be malformed is pinned, since a decode that answered anything other than
   a refusal there would open the dashboard. *)

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
  (* [Base64] accepts whole "====" groups after a group that needed no padding,
     and that is the contract here. Such a field is another spelling of
     credentials that already authenticate, so the only plaintext the wider
     accept set adds is "", which carries no colon and so authenticates
     nothing. Any future base64 must keep that property. *)
  check "padding groups after the credentials are accepted"
    (accepts ~password:"s3cr" "dXNlcjpzM2Ny====");
  check "padding after a padded group is refused"
    (not (accepts ~password:"s3cret" "dXNlcjpzM2NyZXQ====="));
  (* The trim is deliberately more lenient than RFC 7235, which allows optional
     whitespace after the scheme token but not after the credentials. *)
  check "whitespace around the payload is ignored"
    (accepts ~password:"s3cret" "  dXNlcjpzM2NyZXQ=  ");
  Printf.printf "test_auth: %d checks ok\n" !checks

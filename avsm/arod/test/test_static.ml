(* The tail of a static route is client input. [confined_path] is what keeps a
   request under the serving directory, so each way out of it is checked. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let refused name segs =
  check name (Arod_handlers.confined_path segs = None)

let () =
  check "a plain path is allowed"
    (Arod_handlers.confined_path [ "a"; "b.png" ] = Some "a/b.png");
  check "a single segment is allowed"
    (Arod_handlers.confined_path [ "paper.pdf" ] = Some "paper.pdf");
  check "no segments is the directory itself"
    (Arod_handlers.confined_path [] = Some "");
  refused "a parent segment is refused" [ ".."; "etc"; "passwd" ];
  refused "a parent segment in the middle is refused" [ "a"; ".."; ".."; "b" ];
  refused "a trailing parent segment is refused" [ "a"; ".." ];
  refused "a current-directory segment is refused" [ "."; "a" ];
  refused "an empty segment is refused" [ "a"; ""; "b" ];
  refused "an embedded slash is refused" [ "a/../b" ];
  refused "an absolute-looking segment is refused" [ "/etc/passwd" ];
  refused "a NUL is refused" [ "a\000b" ];
  (* Not decoded by the router, so this is a filename and not an escape. It is
     still refused for holding no separator only by luck, so pin the shape. *)
  check "a percent-encoded parent is left as a name"
    (Arod_handlers.confined_path [ "%2e%2e" ] = Some "%2e%2e");
  Printf.printf "test_static: %d checks ok\n" !checks

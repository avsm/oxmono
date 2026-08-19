(* The tail of a static route is client input. [Proffer.Static.confine] is what
   keeps a request under the serving directory, so each way out of it is
   checked here as well as in proffer's own suite: arod is what a change to it
   would break. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let refused name segs =
  check name (Proffer.Static.confine segs = None)

let () =
  check "a plain path is allowed"
    (Proffer.Static.confine [ "a"; "b.png" ] = Some "a/b.png");
  check "a single segment is allowed"
    (Proffer.Static.confine [ "paper.pdf" ] = Some "paper.pdf");
  check "no segments is the directory itself"
    (Proffer.Static.confine [] = Some "");
  refused "a parent segment is refused" [ ".."; "etc"; "passwd" ];
  refused "a parent segment in the middle is refused" [ "a"; ".."; ".."; "b" ];
  refused "a trailing parent segment is refused" [ "a"; ".." ];
  refused "a current-directory segment is refused" [ "."; "a" ];
  refused "an empty segment is refused" [ "a"; ""; "b" ];
  refused "an embedded slash is refused" [ "a/../b" ];
  refused "an absolute-looking segment is refused" [ "/etc/passwd" ];
  refused "a NUL is refused" [ "a\000b" ];
  (* The router percent-decodes a segment before matching, so a request for
     "%2e%2e" arrives as ".." and is refused by the clause above. What reaches
     confine spelled "%2e%2e" is therefore a file literally named that, which
     is a name and not an escape. Pin that it is allowed through. *)
  check "a literal percent-encoded parent is a name"
    (Proffer.Static.confine [ "%2e%2e" ] = Some "%2e%2e");
  Printf.printf "test_static: %d checks ok\n" !checks

open Proffer

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let refused name segs = check name (Static.confine segs = None)

let () =
  check "a plain path joins" (Static.confine [ "a"; "b.png" ] = Some "a/b.png");
  check "one segment" (Static.confine [ "x.pdf" ] = Some "x.pdf");
  check "empty list is the root" (Static.confine [] = Some "");
  refused "parent segment" [ ".."; "etc"; "passwd" ];
  refused "parent in the middle" [ "a"; ".."; "b" ];
  refused "current-dir segment" [ "."; "a" ];
  refused "empty segment" [ "a"; ""; "b" ];
  refused "embedded slash" [ "a/b" ];
  refused "embedded NUL" [ "a\000b" ];
  Printf.printf "test_static: %d checks ok\n" !checks

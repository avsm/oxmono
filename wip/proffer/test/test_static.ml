open Proffer

let check = Test_check.check
;;

let refused name segs = check name (Static.confine segs = None)

let () =
  check "a plain path joins" (Static.confine [ "a"; "b.png" ] = Some "a/b.png");
  check "one segment" (Static.confine [ "x.pdf" ] = Some "x.pdf");
  (* An empty capture names the root directory rather than something under
     it, and [Route.rest] produces one for a request to the mount point. *)
  refused "empty list" [];
  refused "parent segment" [ ".."; "etc"; "passwd" ];
  refused "parent in the middle" [ "a"; ".."; "b" ];
  refused "current-dir segment" [ "."; "a" ];
  refused "empty segment" [ "a"; ""; "b" ];
  refused "embedded slash" [ "a/b" ];
  refused "embedded backslash" [ "a\\b" ];
  refused "embedded NUL" [ "a\000b" ];
  refused "embedded newline" [ "a\nb" ];
  refused "embedded tab" [ "a\tb" ];
  refused "embedded DEL" [ "a\127b" ];
  Test_check.report "test_static"
;;

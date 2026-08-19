(* [htmlit] is vendored under vendor/ and patched so that its interface is
   callable from the portable context arod renders pages in. Dune skips
   aliases under a vendored directory, so the vendored copy's own test never
   runs. This one does, and it fails if a re-vendor drops the patch or changes
   which elements render without a closing tag. *)

open Htmlit

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* The ascriptions are the point. [render] and [link] do not compile unless
   [htmlit.mli] still carries its [@@ portable] annotations. *)
let render : (El.html -> string) @ portable =
 fun h -> El.to_string ~doctype:false h

let link : (string -> string -> El.html) @ portable =
 fun href text -> El.a ~at:[At.href href; At.class' "u"] [El.txt text]

(* [footer] and [cls] do not compile unless [El.html] and [At.t] still have
   the [immutable_data] kind. A portable closure reads a module-level value
   only if its type crosses portability and contention, and a page arod
   renders holds fragments and attributes built once, as these are. *)
let footer = El.footer [El.txt "\194\169 arod"]
let cls = At.class' "u"

let with_footer : (string -> string) @ portable =
 fun body -> render (El.div ~at:[cls] [El.txt body; footer])

let () =
  check "an anchor renders with its attributes"
    (String.equal
       (render (link "/notes/x" "a & b"))
       "<a href=\"/notes/x\" class=\"u\">a &amp; b</a>");
  check "a void element renders without a closing tag"
    (String.equal (render (El.img ~at:[At.src "i.png"] ())) "<img src=\"i.png\">");
  check "a non-void element renders with one"
    (String.equal (render (El.div [El.txt "x"])) "<div>x</div>");
  check "classes merge into one attribute"
    (String.equal
       (render (El.div ~at:[At.class' "a"; At.class' "b"] []))
       "<div class=\"a b\"></div>");
  check "a fragment and an attribute held at module level render"
    (String.equal
       (with_footer "b")
       "<div class=\"u\">b<footer>\194\169 arod</footer></div>");
  check "a doctype is prepended when asked for"
    (String.equal (El.to_string ~doctype:true (El.p [])) "<!DOCTYPE html>\n<p></p>");
  Printf.printf "test_htmlit: %d checks ok\n" !checks

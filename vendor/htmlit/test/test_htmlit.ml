(* Pins what the OxCaml patch touches: the void element table, which is a
   match here and a set upstream, and the portability of the interface. *)

open Htmlit

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* These three ascriptions pin the [@@ portable] annotations in [htmlit.mli].
   They say nothing about the kinds: a type that appears only as a parameter
   or a result does not have to cross anything. *)
let render : (El.html -> string) @ portable =
 fun h -> El.to_string ~doctype:false h
let attr : (string -> At.t) @ portable = At.class'

let page : (string -> El.html) @ portable =
 fun t -> El.page ~title:t (El.body [El.p [El.txt t]])

(* These two pin the kinds. [frag] and [at] are module-level, so a portable
   closure may read them only if their types cross portability and contention,
   which is what [immutable_data] on [El.html] and [At.t] says. Drop either
   kind from the interface and only these two stop compiling. *)
let frag = El.p [El.txt "held"]
let at = At.class' "held"

let held_html : (unit -> string) @ portable =
 fun () -> El.to_string ~doctype:false frag

let held_at : (unit -> string) @ portable =
 fun () -> El.to_string ~doctype:false (El.div ~at:[at] [])

let void_els =
  [ "area"; "base"; "br"; "col"; "embed"; "hr"; "img"; "input"; "link";
    "meta"; "param"; "source"; "track"; "wbr" ]

let () =
  List.iter
    (fun n ->
      check
        (n ^ " renders without a closing tag")
        (String.equal (render (El.v n [El.txt "x"])) ("<" ^ n ^ ">")))
    void_els;
  List.iter
    (fun n ->
      check
        (n ^ " renders with a closing tag")
        (String.equal
           (render (El.v n [El.txt "x"]))
           ("<" ^ n ^ ">x</" ^ n ^ ">")))
    ["div"; "p"; "span"; "area1"; "are"; "wb"; "AREA"; "table"];
  check "the empty name is not void"
    (String.equal (render (El.v "" [])) "<></>");
  check "text is escaped"
    (String.equal
       (render (El.txt "a & b < c > d ' e \" f"))
       "a &amp; b &lt; c &gt; d &apos; e &quot; f");
  check "raw data is not escaped"
    (String.equal (render (El.unsafe_raw "<b>&")) "<b>&");
  check "classes merge and styles merge"
    (String.equal
       (render
          (El.div ~at:[attr "a"; At.style "x:1"; attr "b"; At.style "y:2"] []))
       "<div class=\"a b\" style=\"x:1;y:2\"></div>");
  check "a void attribute renders nothing"
    (String.equal (render (El.div ~at:[At.void] [])) "<div></div>");
  check "a splice separator is inserted between children"
    (String.equal
       (render (El.splice ~sep:(El.txt ",") [El.txt "1"; El.txt "2"]))
       "1,2");
  check "a fragment held at module level renders"
    (String.equal (held_html ()) "<p>held</p>");
  check "an attribute held at module level renders"
    (String.equal (held_at ()) "<div class=\"held\"></div>");
  check "a page carries the doctype and the title"
    (String.equal
       (El.to_string ~doctype:true (page "T"))
       "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"><meta \
        name=\"viewport\" content=\"width=device-width, \
        initial-scale=1.0\"><title>T</title></head><body><p>T</p></body></html>");
  Printf.printf "test_htmlit: %d checks ok\n" !checks

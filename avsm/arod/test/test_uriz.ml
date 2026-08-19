(* [uriz] lives under vendor/ and its interface is annotated so that it is
   callable from the portable context arod routes and renders in. Dune skips
   aliases under a vendored directory, so the vendored copy's own test never
   runs. This one does, and it fails if a change to [uriz.mli] drops the
   annotations or the parser stops normalizing what the router relies on. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* [get] is called from the portable closures below, so it must not close over
   anything nonportable. [exit] is nonportable, hence the exception. *)
let get = function Null -> invalid_arg "not a URI reference" | This v -> v

(* The ascriptions are the point. None of these compile unless [uriz.mli]
   still carries its [@@ portable] annotations. *)

let parse : (string -> Uriz.t or_null) @ portable = Uriz.of_string
let text : (Uriz.t -> string) @ portable = Uriz.to_string
let param : (Uriz.t -> string -> string or_null) @ portable =
 fun u k -> Uriz.find_query u k

let against_base : (base:Uriz.t -> Uriz.t -> Uriz.t) @ portable =
 fun ~base r -> Uriz.resolve ~base r

(* [origin] and [feed] do not compile unless [Uriz.t] still has the
   [immutable_data] kind. A portable closure reads a module-level value only
   if its type crosses portability and contention, and arod holds its site
   root parsed once, as [origin] is. Passing a URI as an argument would prove
   nothing, since a type used only as a parameter or a result need not cross
   anything. [feed] is a [t or_null], which is what {!Uriz.of_string} answers
   with, so it pins that the [or_null] wrapper crosses too. *)

let origin = Uriz.of_string_exn "https://anil.recoil.org/notes/"
let feed = Uriz.of_string "https://anil.recoil.org/feed.xml"

let absolute : (string -> string) @ portable =
 fun href -> text (against_base ~base:origin (get (parse href)))

let feed_path : (unit -> string) @ portable = fun () -> Uriz.path (get feed)

let () =
  check "a relative reference resolves against a module-level base"
    (String.equal (absolute "../about") "https://anil.recoil.org/about");
  check "a module-level or_null holds a parsed URI"
    (String.equal (feed_path ()) "/feed.xml");
  check "the scheme and host are lowercased"
    (String.equal (text (get (parse "HTTP://Example.ORG/A"))) "http://example.org/A");
  check "a triplet encoding an unreserved character is decoded"
    (String.equal (text (get (parse "http://a/x%7Ey"))) "http://a/x~y");
  check "percent hex is uppercased"
    (String.equal (text (get (parse "http://a/x%2fy"))) "http://a/x%2Fy");
  check "a query parameter is found and decoded"
    (param (get (parse "http://a/?q=a%20b&r=c")) "q" = This "a b");
  check "an absent component is Null"
    (Uriz.query (get (parse "http://a/")) = Null);
  check "a present but empty component is not"
    (Uriz.query (get (parse "http://a/?")) = This "");
  check "input that is not a URI reference is rejected"
    (parse "http://a b/" = Null);
  check "dot segments survive of_string and go under normalize"
    (String.equal
       (text (Uriz.normalize (get (parse "http://a/b/../c"))))
       "http://a/c");
  Printf.printf "test_uriz: %d checks ok\n" !checks

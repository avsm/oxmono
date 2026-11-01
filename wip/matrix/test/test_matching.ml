(** Hermetic tests for {!Matrix_ui.Matching} and {!Matrix_ui.Presentation.Html}.

    Two things live here: the HTML sanitiser, which decides what of a
    [formatted_body] a toolkit is allowed to render, and the fuzzy matcher
    behind {!Matrix_ui.Room_list.Filter.Fuzzy}.

    The normalization tests live next door in [test/test_room_list.ml], beside
    the filters that use them. *)

module Html = Matrix_ui.Presentation.Html
module Matching = Matrix_ui.Matching

let check_bool = Alcotest.(check bool)
let check_string = Alcotest.(check string)

(* The spec lists the tags and attributes a client may render. Anything else
   is dropped, and the elements that can carry script are dropped with their
   contents.

   https://spec.matrix.org/v1.11/client-server-api/#mroommessage-msgtypes *)

let test_sanitizer () =
  let sanitize = Html.sanitize in
  check_string "a script element goes, contents and all" "<p>ab</p>"
    (sanitize {|<p>a<script>evil()</script>b</p>|});
  check_string "an event handler is not an allowed attribute" "<p>hi</p>"
    (sanitize {|<p onclick="evil()">hi</p>|});
  check_string "a javascript: href is dropped, the link is kept" "<a>x</a>"
    (sanitize {|<a href="javascript:alert(1)">x</a>|});
  check_string "an https href and a title survive; target does not"
    {|<a href="https://e/x" title="t">x</a>|}
    (sanitize {|<a href="https://e/x" title="t" target="_blank">x</a>|});
  check_string "the mx-reply fallback goes with its contents" "new"
    (sanitize {|<mx-reply><blockquote>old</blockquote></mx-reply>new|});
  check_string "an unlisted element goes, its contents stay" "b"
    (sanitize {|<blink>b</blink>|});
  check_string "the two data-mx colour attributes are listed"
    {|<span data-mx-color="#f00">c</span>|}
    (sanitize {|<span data-mx-color="#f00" style="x">c</span>|});
  check_string "and a language class on code"
    {|<code class="language-ocaml">let</code>|}
    (sanitize {|<code class="language-ocaml" id="x">let</code>|})

(* [<img>] is in the spec's subset with [src], [alt], [title], [width] and
   [height], and its [src] must be an [mxc://] URI. An image whose [src] does
   not survive is dropped whole rather than left as an empty box. *)

let test_images () =
  let sanitize = Html.sanitize in
  check_string "an mxc src is kept, with the listed attributes"
    {|<img src="mxc://s/a" alt="A" title="T" width="1" height="2">|}
    (sanitize
       {|<img src="mxc://s/a" alt="A" title="T" width="1" height="2" srcset="x" onerror="e">|});
  check_string "an https src is not a Matrix image" ""
    (sanitize {|<img src="https://e/x.png" alt="A">|});
  check_string "nor is a javascript: one" ""
    (sanitize {|<img src="javascript:alert(1)">|});
  check_string "nor a data: one" ""
    (sanitize {|<img src="data:image/png;base64,AAA">|});
  check_string "an img with no src at all is dropped" ""
    (sanitize {|<img alt="A">|});
  check_string "the scheme is matched caselessly" {|<img src="MXC://s/a">|}
    (sanitize {|<img src="MXC://s/a">|});
  check_string "a dropped image leaves the surrounding text alone"
    "<p>beforeafter</p>"
    (sanitize {|<p>before<img src="https://e/x">after</p>|});
  check_string "and it contributes nothing to the plain text" "hi  there"
    (Html.to_plain (sanitize {|<p>hi <img src="mxc://s/a" alt="A"> there</p>|}));

  (* The resolver a toolkit passes to turn an mxc URI into something a
     renderer can load. Returning [None] drops the image. *)
  let resolve =
    Html.sanitize ~resolve_mxc:(fun uri -> Some ("https://h/" ^ uri))
  in
  check_string "the resolver rewrites the src"
    {|<p><img src="https://h/mxc://s/a" alt="A"></p>|}
    (resolve {|<p><img src="mxc://s/a" alt="A"></p>|});
  check_string "and is not consulted for a src that is not an mxc URI" ""
    (resolve {|<img src="https://e/x.png">|});
  check_string "a resolver that declines drops the image" "<p>beforeafter</p>"
    (Html.sanitize
       ~resolve_mxc:(fun _ -> None)
       {|<p>before<img src="mxc://s/a">after</p>|})

let test_sanitizer_attribute_values () =
  let sanitize = Html.sanitize in
  check_string "image dimensions are canonicalized and bounded"
    {|<img src="mxc://s/a" width="1" height="16384">|}
    (sanitize {|<img src="mxc://s/a" width="0001" height="16384">|});
  check_string "zero and oversized dimensions are dropped"
    {|<img src="mxc://s/a">|}
    (sanitize {|<img src="mxc://s/a" width="0" height="16385">|});
  check_string "CSS-like and whitespace dimensions are dropped"
    {|<img src="mxc://s/a">|}
    (sanitize {|<img src="mxc://s/a" width="1px" height=" 2">|});
  check_string "only strict Matrix hex colours survive"
    {|<span data-mx-color="#f00" data-mx-bg-color="#11223344">c</span>|}
    (sanitize
       {|<span data-mx-color="#f00" data-mx-bg-color="#11223344">c</span>|});
  check_string "named, functional and malformed colours are dropped"
    "<span>c</span>"
    (sanitize
       {|<span data-mx-color="red" data-mx-bg-color="rgb(1,2,3)" data-mx-color2="#12">c</span>|})

let test_void_elements () =
  let sanitize = Html.sanitize in
  check_string "br is emitted without a closing tag" "<p>before<br>after</p>"
    (sanitize {|<p>before<br>after</p>|});
  check_string "hr is emitted without a closing tag"
    "<div>before<hr>after</div>"
    (sanitize {|<div>before<hr>after</div>|});
  check_string "img is emitted without a closing tag"
    {|<p><img src="mxc://s/a"></p>|}
    (sanitize {|<p><img src="mxc://s/a"></p>|})

(* [Matching.fuzzy_score] returns a score rather than a boolean, so that a
   room list can rank on it. *)

let matches needle haystack =
  Option.is_some (Matching.fuzzy_score ~haystack ~needle)

let score needle haystack =
  match Matching.fuzzy_score ~haystack ~needle with
  | Some score -> score
  | None -> Alcotest.failf "%S does not match %S" needle haystack

let test_fuzzy_matching () =
  check_bool "characters in order need not be adjacent" true
    (matches "mtx" "matrix");
  check_bool "out of order is not a match" false (matches "mxt" "matrix");
  check_bool "a needle longer than the haystack cannot match" false
    (matches "matrixxx" "matrix");
  check_bool "a character that is not there at all" false
    (matches "mtz" "matrix");
  (* The needle and the subject both go through [search_key] first. *)
  check_bool "case is folded on both sides" true (matches "Mtx" "MaTrIX");
  check_bool "and diacritics are dropped" true (matches "stf" "\u{0218}tefan");
  check_bool "and compatibility spellings normalize" true
    (matches "ubete" "un bel \u{00E9}t\u{00E9}");
  (* An absent pattern matches everything. *)
  check_bool "the empty needle matches anything" true (matches "" "hello");
  check_bool "even the empty haystack" true (matches "" "");
  check_bool "which the non-empty needle does not" false (matches "a" "")

let check_int_gt what left right =
  if left <= right then Alcotest.failf "%s: expected %d > %d" what left right

let test_fuzzy_ranking () =
  check_int_gt "a consecutive run beats a scattered one" (score "mat" "matrix")
    (score "mat" "mxaxtrix");
  check_int_gt "a word start beats the middle of a word"
    (score "ab" "alpha beta") (score "ab" "alphabet");
  check_int_gt "an unbroken match beats one with a gap in it"
    (score "abc" "abc") (score "abc" "abxc");
  check_int_gt "a match at the start of a word beats one inside it"
    (score "x" "xzzz") (score "x" "zzzx");
  Alcotest.(check int) "the empty needle scores zero" 0 (score "" "anything");
  (* The bonuses are close enough that a word start can pay for the gap in
     front of it, so ["a-b"] ranks above ["ab"] for the needle ["ab"]. That
     is deliberate. *)
  check_int_gt "a word start can outweigh the gap it costs to reach"
    (score "ab" "a-b") (score "ab" "ab")

let () =
  Alcotest.run "matrix.ui matching"
    [
      ( "sanitizer",
        [
          Alcotest.test_case "the Matrix HTML subset" `Quick test_sanitizer;
          Alcotest.test_case "img and mxc resolution" `Quick test_images;
          Alcotest.test_case "safe attribute values" `Quick
            test_sanitizer_attribute_values;
          Alcotest.test_case "HTML void elements" `Quick test_void_elements;
        ] );
      ( "fuzzy",
        [
          Alcotest.test_case "subsequence matching" `Quick test_fuzzy_matching;
          Alcotest.test_case "the score's ordering" `Quick test_fuzzy_ranking;
        ] );
    ]

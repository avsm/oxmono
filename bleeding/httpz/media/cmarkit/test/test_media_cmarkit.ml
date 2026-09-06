(* Regression for R40: [sanitize_url_attributes] must fail closed when an
   href/src attribute has no closing quote, rather than emit the unchecked
   tail verbatim. Unreachable through {!Fetch.Markdown.html}, whose renderer
   always closes the attributes it writes, so this drives the sanitiser
   directly on a copy of the module compiled for this test only (mirroring
   how [fetch/curl/dune] shares [gzip_stream.ml]). *)

let contains ~needle hay =
  let n = String.length needle and m = String.length hay in
  let rec same j k = k = n || (hay.[j + k] = needle.[k] && same j (k + 1)) in
  let rec go i =
    i + n <= m && (same i 0 || go (i + 1))
  in
  n = 0 || go 0

let test_fails_closed_on_unterminated_attribute () =
  let input = "<p>ok</p><a href=\"javascript:alert(1)//no-closing-quote" in
  let output = Httpz_media_cmarkit.sanitize_url_attributes input in
  Alcotest.(check bool) "prefix retained" true (contains ~needle:"<p>ok</p>" output);
  Alcotest.(check bool) "attribute prefix retained" true
    (contains ~needle:" href=\"" output);
  Alcotest.(check bool) "unterminated value dropped" false
    (contains ~needle:"javascript:alert" output);
  Alcotest.(check bool) "trailing text dropped" false
    (contains ~needle:"no-closing-quote" output)

let test_fails_closed_when_value_start_is_end_of_string () =
  let input = "<img src=\"" in
  let output = Httpz_media_cmarkit.sanitize_url_attributes input in
  Alcotest.(check string) "nothing spurious is appended" input output

let test_passes_through_when_no_url_attribute () =
  let input = "<p>no links here</p>" in
  Alcotest.(check bool) "returned unchanged"
    true
    (Httpz_media_cmarkit.sanitize_url_attributes input == input)

let () =
  Alcotest.run "fetch-media-cmarkit"
    [
      ( "sanitize_url_attributes",
        [
          Alcotest.test_case "fails closed on unterminated attribute" `Quick
            test_fails_closed_on_unterminated_attribute;
          Alcotest.test_case "fails closed at end of string" `Quick
            test_fails_closed_when_value_start_is_end_of_string;
          Alcotest.test_case "unchanged with no url attribute" `Quick
            test_passes_through_when_no_url_attribute;
        ] );
    ]

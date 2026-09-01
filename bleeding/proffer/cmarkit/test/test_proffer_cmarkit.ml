let source = "# Hello\n\nSome *markdown*.\n"

module Media = Httpz.Media

let test_markdown () =
  let md = Proffer.Markdown.markdown () in
  Alcotest.(check string) "type" "text/markdown; charset=utf-8" (Media.content_type md);
  Alcotest.(check bool) "x-markdown" true (Media.accepts md (Some "text/x-markdown"));
  match Media.decode md source with
  | Ok doc -> Alcotest.(check string) "round trip" source (Media.encode md doc)
  | Error e -> Alcotest.fail (Media.error_to_string e)
;;

let test_parser_regressions () =
  let codec = Proffer.Markdown.markdown () in
  let decode source = ignore (Media.decode codec source) in
  let hidden =
    String.concat "" (List.init 22 (fun _ -> "[`]`")) ^ "x"
    ^ String.make 22 ']'
  in
  decode (String.make 22 '[' ^ "x" ^ String.make 22 ']');
  decode hidden;
  decode (String.concat "" (List.init 200 (fun _ -> "\\[")) ^ "x");
  decode (String.concat "" (List.init 200 (fun _ -> "[x](")));
  decode (String.concat "" (List.init 100_000 (fun _ -> "word ")))

let test_html () =
  let html = Proffer.Markdown.html () in
  Alcotest.(check bool) "encode only" false (Media.can_decode html);
  let doc = Cmarkit.Doc.of_string source in
  Alcotest.(check string)
    "render"
    "<h1>Hello</h1>\n<p>Some <em>markdown</em>.</p>\n"
    (Media.encode html doc);
  let unsafe = Cmarkit.Doc.of_string "<script>x</script>\n" in
  Alcotest.(check bool)
    "safe drops raw html"
    false
    (let s = Media.encode html unsafe in
     String.length s >= 7 && String.sub s 0 7 = "<script");
  let contains ~needle haystack =
    let needle_len = String.length needle in
    let haystack_len = String.length haystack in
    let rec loop i =
      i <= haystack_len - needle_len
      && (String.sub haystack i needle_len = needle || loop (i + 1))
    in
    loop 0
  in
  let render ?safe source =
    Media.encode
      (Proffer.Markdown.html ?safe ())
      (Cmarkit.Doc.of_string source)
  in
  List.iter
    (fun source ->
      Alcotest.(check bool)
        ("safe URL: " ^ source)
        true
        (contains ~needle:"href=\"\"" (render source)))
    [ "[x](java&Tab;script:alert(1))"
    ; "[x](java&NewLine;script:alert(1))"
    ; "[x](java&#13;script:alert(1))"
    ; "[x](&#1;javascript:alert(1))"
    ; "[x](java%09script:alert(1))"
    ];
  Alcotest.(check bool)
    "safe image URL"
    true
    (contains ~needle:"src=\"\"" (render "![x](java&Tab;script:alert(1))"));
  Alcotest.(check bool)
    "safe https URL is retained"
    true
    (contains
       ~needle:"href=\"https://example.test/a\""
       (render "[x](https://example.test/a)"));
  Alcotest.(check bool)
    "unsafe mode is unchanged"
    true
    (contains
       ~needle:"href=\"java%09script:alert(1)\""
       (render ~safe:false "[x](java&Tab;script:alert(1))"))
;;

let () =
  Alcotest.run "proffer-cmarkit"
    [ ( "codecs",
        [ Alcotest.test_case "markdown" `Quick test_markdown;
          Alcotest.test_case "bounded parser regressions" `Quick
            test_parser_regressions;
          Alcotest.test_case "html" `Quick test_html ] ) ]

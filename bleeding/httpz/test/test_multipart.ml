module M = Httpz.Multipart

let crlf lines = String.concat "\r\n" lines

let check_str = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_opt_str = Alcotest.(check (option string))

let parts_of ?max_parts ~boundary body =
  match M.parse ?max_parts ~boundary body with
  | Ok parts -> parts
  | Error e -> Alcotest.failf "parse: %s" e

let error_of ?max_parts ~boundary body =
  match M.parse ?max_parts ~boundary body with
  | Ok _ -> Alcotest.fail "expected an error"
  | Error e -> e

(* RFC 7578, Section 4.4. *)
let rfc7578 =
  crlf
    [ "--AaB03x";
      "Content-Disposition: form-data; name=\"submit-name\"";
      "";
      "Larry";
      "--AaB03x";
      "Content-Disposition: form-data; name=\"files\"; filename=\"file1.txt\"";
      "Content-Type: text/plain";
      "";
      "... contents of file1.txt ...";
      "--AaB03x--";
      "" ]

let test_rfc7578 () =
  let ps = parts_of ~boundary:"AaB03x" rfc7578 in
  Alcotest.(check int) "two parts" 2 (List.length ps);
  let a = List.nth ps 0 and b = List.nth ps 1 in
  check_str "first name" "submit-name" a.M.name;
  check_opt_str "first has no filename" None a.M.filename;
  check_opt_str "first has no content type" None a.M.content_type;
  check_str "first content" "Larry" (M.content rfc7578 a);
  check_str "second name" "files" b.M.name;
  check_opt_str "second filename" (Some "file1.txt") b.M.filename;
  check_opt_str "second content type" (Some "text/plain") b.M.content_type;
  check_str "second content" "... contents of file1.txt ..."
    (M.content rfc7578 b);
  Alcotest.(check (list (pair string string)))
    "headers kept"
    [ ("content-disposition",
       "form-data; name=\"files\"; filename=\"file1.txt\"");
      ("content-type", "text/plain") ]
    b.M.headers

let test_preamble_and_epilogue () =
  let body =
    crlf
      [ "This is a MIME multipart message.";
        "Readers that do not understand it see this.";
        "--b";
        "Content-Disposition: form-data; name=\"a\"";
        "";
        "1";
        "--b--";
        "Trailing epilogue, ignored.";
        "" ]
  in
  let ps = parts_of ~boundary:"b" body in
  Alcotest.(check int) "one part" 1 (List.length ps);
  check_str "content" "1" (M.content body (List.hd ps))

(* A preamble that happens to begin with the boundary bytes is still a
   preamble. *)
let test_preamble_looks_like_boundary () =
  let body =
    "--bogus text\r\n--b\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n1\r\n--b--\r\n"
  in
  let ps = parts_of ~boundary:"b" body in
  Alcotest.(check int) "one part" 1 (List.length ps);
  check_str "content" "1" (M.content body (List.hd ps))

let test_quoted_filename () =
  let body =
    crlf
      [ "--b";
        "Content-Disposition: form-data; name=\"f\"; filename=\"a\\\"b;c.txt\"";
        "";
        "x";
        "--b--";
        "" ]
  in
  let p = List.hd (parts_of ~boundary:"b" body) in
  check_opt_str "quoted pair unescaped" (Some "a\"b;c.txt") p.M.filename

let test_ext_filename () =
  let body =
    crlf
      [ "--b";
        "Content-Disposition: form-data; name=\"f\"; filename=\"plain.txt\"; \
         filename*=UTF-8''%C3%A9.txt";
        "";
        "x";
        "--b--";
        "" ]
  in
  let p = List.hd (parts_of ~boundary:"b" body) in
  check_opt_str "filename* wins" (Some "\xc3\xa9.txt") p.M.filename;
  let latin =
    crlf
      [ "--b";
        "Content-Disposition: form-data; name=\"f\"; filename=\"plain.txt\"; \
         filename*=ISO-8859-1''%E9.txt";
        "";
        "x";
        "--b--";
        "" ]
  in
  let q = List.hd (parts_of ~boundary:"b" latin) in
  check_opt_str "unknown charset falls back" (Some "plain.txt") q.M.filename

let test_transport_padding () =
  let body =
    "--b \t\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n1\r\n--b--  \r\n"
  in
  let ps = parts_of ~boundary:"b" body in
  Alcotest.(check int) "one part" 1 (List.length ps);
  check_str "content" "1" (M.content body (List.hd ps))

let test_binary_content () =
  let content = "\x00\x01ab\r\n--xy\xff-" in
  let body =
    "--b\r\nContent-Disposition: form-data; name=\"bin\"\r\n\
     Content-Type: application/octet-stream\r\n\r\n" ^ content
    ^ "\r\n--b--\r\n"
  in
  let p = List.hd (parts_of ~boundary:"b" body) in
  check_str "binary preserved" content (M.content body p);
  check_opt_str "content type" (Some "application/octet-stream")
    p.M.content_type

(* [--bX] is a different boundary line, not [--b] followed by content. *)
let test_boundary_prefix () =
  let content = "before\r\n--bX\r\nafter" in
  let body =
    "--b\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n" ^ content
    ^ "\r\n--b--\r\n"
  in
  let p = List.hd (parts_of ~boundary:"b" body) in
  check_str "prefix does not delimit" content (M.content body p)

let test_empty_part () =
  let body = "--b\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\n\r\n--b--\r\n" in
  let p = List.hd (parts_of ~boundary:"b" body) in
  check_str "empty content" "" (M.content body p)

let test_missing_close () =
  let body =
    crlf
      [ "--b"; "Content-Disposition: form-data; name=\"a\""; ""; "1"; "" ]
  in
  check_str "truncated" "missing closing delimiter" (error_of ~boundary:"b" body)

let test_max_parts () =
  let part n =
    Printf.sprintf "--b\r\nContent-Disposition: form-data; name=\"%d\"\r\n\r\n%d\r\n" n n
  in
  let body = part 1 ^ part 2 ^ part 3 ^ "--b--\r\n" in
  Alcotest.(check int) "three by default" 3
    (List.length (parts_of ~boundary:"b" body));
  check_str "bound" "too many parts" (error_of ~max_parts:2 ~boundary:"b" body);
  Alcotest.(check int) "at the bound" 3
    (List.length (parts_of ~max_parts:3 ~boundary:"b" body));
  check_str "zero rejects a part" "too many parts"
    (error_of ~max_parts:0 ~boundary:"b" body);
  Alcotest.(check int) "zero accepts an empty multipart" 0
    (List.length (parts_of ~max_parts:0 ~boundary:"b" "--b--\r\n"));
  Alcotest.check_raises "negative bound"
    (Invalid_argument "Httpz.Multipart.parse: max_parts is negative")
    (fun () -> ignore (M.parse ~max_parts:(-1) ~boundary:"b" body))

let test_bare_lf () =
  let lf =
    "--b\nContent-Disposition: form-data; name=\"a\"\n\n1\n--b--\n"
  in
  check_str "framing" "bare LF in the multipart framing"
    (error_of ~boundary:"b" lf);
  let header_lf =
    "--b\r\nContent-Disposition: form-data; name=\"a\"\nContent-Type: text/plain\r\n\r\n1\r\n--b--\r\n"
  in
  check_str "header" "bare LF in a part header"
    (error_of ~boundary:"b" header_lf)

let test_header_injection () =
  let cr =
    "--b\r\nContent-Disposition: form-data; name=\"a\rb\"\r\n\r\n1\r\n--b--\r\n"
  in
  check_str "bare CR" "bare CR in a part header" (error_of ~boundary:"b" cr);
  let nul =
    "--b\r\nContent-Disposition: form-data; name=\"a\000b\"\r\n\r\n1\r\n--b--\r\n"
  in
  check_str "NUL" "NUL in a part header" (error_of ~boundary:"b" nul);
  let control =
    "--b\r\nContent-Disposition: form-data; name=\"a\001b\"\r\n\r\n1\r\n--b--\r\n"
  in
  check_str "C0" "control byte in a part header"
    (error_of ~boundary:"b" control);
  let del =
    "--b\r\nContent-Disposition: form-data; name=\"a\127b\"\r\n\r\n1\r\n--b--\r\n"
  in
  check_str "DEL" "control byte in a part header"
    (error_of ~boundary:"b" del);
  let fold =
    "--b\r\nContent-Disposition: form-data;\r\n name=\"a\"\r\n\r\n1\r\n--b--\r\n"
  in
  check_str "obs-fold" "obsolete line folding in a part header"
    (error_of ~boundary:"b" fold)

let test_bad_disposition () =
  let none = "--b\r\nContent-Type: text/plain\r\n\r\n1\r\n--b--\r\n" in
  check_str "absent" "a part has no Content-Disposition"
    (error_of ~boundary:"b" none);
  let attachment =
    "--b\r\nContent-Disposition: attachment; name=\"a\"\r\n\r\n1\r\n--b--\r\n"
  in
  check_str "not form-data" "a part is not Content-Disposition: form-data"
    (error_of ~boundary:"b" attachment);
  let unnamed = "--b\r\nContent-Disposition: form-data\r\n\r\n1\r\n--b--\r\n" in
  check_str "unnamed" "a part has no name parameter"
    (error_of ~boundary:"b" unnamed)

let test_no_delimiter () =
  check_str "empty body" "no multipart delimiter" (error_of ~boundary:"b" "");
  check_str "nothing to frame" "no multipart delimiter"
    (error_of ~boundary:"b" "just some text\r\n");
  check_str "invalid boundary" "invalid boundary" (error_of ~boundary:"" "--\r\n");
  check_str "boundary too long" "invalid boundary"
    (error_of ~boundary:(String.make 71 'a') "x")

let test_too_many_headers () =
  let extra =
    String.concat ""
      (List.init 32 (fun i -> Printf.sprintf "X-N%d: v\r\n" i))
  in
  let body =
    "--b\r\nContent-Disposition: form-data; name=\"a\"\r\n" ^ extra
    ^ "\r\n1\r\n--b--\r\n"
  in
  check_str "cap" "too many part headers" (error_of ~boundary:"b" body);
  let long =
    "--b\r\nContent-Disposition: form-data; name=\"a\"\r\nX-Long: "
    ^ String.make 9000 'v' ^ "\r\n\r\n1\r\n--b--\r\n"
  in
  check_str "line length" "part header line is too long"
    (error_of ~boundary:"b" long)

let test_boundary_of_content_type () =
  check_opt_str "token" (Some "AaB03x")
    (M.boundary_of_content_type "multipart/form-data; boundary=AaB03x");
  check_opt_str "quoted" (Some "a b-c")
    (M.boundary_of_content_type "multipart/form-data; boundary=\"a b-c\"");
  check_opt_str "case folded" (Some "x")
    (M.boundary_of_content_type "Multipart/Form-Data; BOUNDARY=x");
  check_opt_str "other parameters first" (Some "x")
    (M.boundary_of_content_type
       "multipart/form-data; charset=utf-8; boundary=x");
  check_opt_str "absent" None (M.boundary_of_content_type "multipart/form-data");
  check_opt_str "wrong media type" None
    (M.boundary_of_content_type "multipart/mixed; boundary=x");
  check_opt_str "not multipart" None
    (M.boundary_of_content_type "text/plain; boundary=x");
  check_opt_str "too long" None
    (M.boundary_of_content_type
       ("multipart/form-data; boundary=" ^ String.make 71 'a'));
  check_opt_str "seventy is allowed" (Some (String.make 70 'a'))
    (M.boundary_of_content_type
       ("multipart/form-data; boundary=" ^ String.make 70 'a'));
  check_opt_str "empty" None
    (M.boundary_of_content_type "multipart/form-data; boundary=\"\"");
  check_opt_str "trailing space" None
    (M.boundary_of_content_type "multipart/form-data; boundary=\"a \"");
  check_opt_str "non-bchar" None
    (M.boundary_of_content_type "multipart/form-data; boundary=\"a\tb\"");
  check_opt_str "parameter suffix is not truncated" None
    (M.boundary_of_content_type "multipart/form-data; boundary=a:b");
  check_opt_str "duplicate boundary is ambiguous" None
    (M.boundary_of_content_type
       "multipart/form-data; boundary=first; boundary=second");
  check_opt_str "control in another quoted parameter" None
    (M.boundary_of_content_type
       "multipart/form-data; note=\"bad\001value\"; boundary=x");
  check_opt_str "byteranges type" (Some "range")
    (M.boundary_of_content_type ~media_type:"multipart/byteranges"
       "multipart/byteranges; boundary=range");
  check_bool "allocation-free boundary check" true
    (M.has_boundary ~media_type:"multipart/byteranges"
       "multipart/byteranges; note=ok; boundary=\"range\" \t");
  check_bool "allocation-free strict suffix" false
    (M.has_boundary "multipart/form-data; boundary=a:b");
  check_bool "allocation-free duplicate boundary" false
    (M.has_boundary
       "multipart/form-data; boundary=first; boundary=second");
  check_bool "round trip with parse" true
    (match M.boundary_of_content_type "multipart/form-data; boundary=AaB03x" with
     | Some b -> ( match M.parse ~boundary:b rfc7578 with Ok _ -> true | Error _ -> false)
     | None -> false)

let test_strict_closing_delimiter () =
  let prefix =
    "--b\r\nContent-Disposition: form-data; name=\"a\"\r\n\r\nx\r\n--b--"
  in
  check_str "junk after close" "missing closing delimiter"
    (error_of ~boundary:"b" (prefix ^ "junk\r\n"));
  check_str "close needs CRLF" "missing closing delimiter"
    (error_of ~boundary:"b" prefix);
  check_str "bare LF after close" "bare LF in the multipart framing"
    (error_of ~boundary:"b" (prefix ^ "\n"))

let test_duplicate_disposition_parameter () =
  let body =
    "--b\r\nContent-Disposition: form-data; name=first; name=second\r\n\r\nx\r\n--b--\r\n"
  in
  check_str "duplicate name" "malformed Content-Disposition parameters"
    (error_of ~boundary:"b" body)

let () =
  Alcotest.run "multipart"
    [ ( "parse",
        [ Alcotest.test_case "rfc 7578 example" `Quick test_rfc7578;
          Alcotest.test_case "preamble and epilogue" `Quick
            test_preamble_and_epilogue;
          Alcotest.test_case "preamble like a boundary" `Quick
            test_preamble_looks_like_boundary;
          Alcotest.test_case "quoted filename" `Quick test_quoted_filename;
          Alcotest.test_case "extended filename" `Quick test_ext_filename;
          Alcotest.test_case "transport padding" `Quick test_transport_padding;
          Alcotest.test_case "binary content" `Quick test_binary_content;
          Alcotest.test_case "boundary prefix" `Quick test_boundary_prefix;
          Alcotest.test_case "empty part" `Quick test_empty_part ] );
      ( "errors",
        [ Alcotest.test_case "missing close" `Quick test_missing_close;
          Alcotest.test_case "max parts" `Quick test_max_parts;
          Alcotest.test_case "bare lf" `Quick test_bare_lf;
          Alcotest.test_case "header injection" `Quick test_header_injection;
          Alcotest.test_case "duplicate disposition parameter" `Quick
            test_duplicate_disposition_parameter;
          Alcotest.test_case "bad disposition" `Quick test_bad_disposition;
          Alcotest.test_case "no delimiter" `Quick test_no_delimiter;
          Alcotest.test_case "header caps" `Quick test_too_many_headers ] );
      ( "content type",
        [ Alcotest.test_case "boundary" `Quick test_boundary_of_content_type;
          Alcotest.test_case "strict closing delimiter" `Quick
            test_strict_closing_delimiter ] )
    ]

open Base

module Template = Httpz.Uri_template

let checks = ref 0

let check name condition =
  Int.incr checks;
  if not condition then failwith ("test_uri_template: " ^ name)
;;

let template source =
  match Template.of_string source with
  | Ok template -> template
  | Error error ->
      failwith
        (Stdlib.Format.asprintf "test_uri_template: could not parse %S: %a" source
           Template.pp_error error)
;;

let bindings =
  [ "count", `List [ "one"; "two"; "three" ]
  ; "dom", `List [ "example"; "com" ]
  ; "dub", `String "me/too"
  ; "hello", `String "Hello World!"
  ; "half", `String "50%"
  ; "var", `String "value"
  ; "who", `String "fred"
  ; "base", `String "http://example.com/home/"
  ; "path", `String "/foo/bar"
  ; "list", `List [ "red"; "green"; "blue" ]
  ; "keys", `Assoc [ "semi", ";"; "dot", "."; "comma", "," ]
  ; "v", `String "6"
  ; "x", `String "1024"
  ; "y", `String "768"
  ; "empty", `String ""
  ; "empty_keys", `Assoc []
  ]
;;

let expand source =
  match Template.expand_assoc (template source) bindings with
  | Ok expansion -> expansion
  | Error error ->
      failwith
        (Stdlib.Format.asprintf "test_uri_template: could not expand %S: %a" source
           Template.pp_error error)
;;

let examples =
  [ "{var}", "value"
  ; "'{var}'", "'value'"
  ; "{hello}", "Hello%20World%21"
  ; "{half}", "50%25"
  ; "O{empty}X", "OX"
  ; "O{undef}X", "OX"
  ; "{x,y}", "1024,768"
  ; "{x,hello,y}", "1024,Hello%20World%21,768"
  ; "?{x,empty}", "?1024,"
  ; "?{x,undef}", "?1024"
  ; "{var:3}", "val"
  ; "{list}", "red,green,blue"
  ; "{list*}", "red,green,blue"
  ; "{keys}", "semi,%3B,dot,.,comma,%2C"
  ; "{keys*}", "semi=%3B,dot=.,comma=%2C"
  ; "{+path}/here", "/foo/bar/here"
  ; "here?ref={+path}", "here?ref=/foo/bar"
  ; "{+path:6}/here", "/foo/b/here"
  ; "{+keys}", "semi,;,dot,.,comma,,"
  ; "{+keys*}", "semi=;,dot=.,comma=,"
  ; "foo{#empty}", "foo#"
  ; "foo{#undef}", "foo"
  ; "{#path,x}/here", "#/foo/bar,1024/here"
  ; "www{.dom*}", "www.example.com"
  ; "X{.empty}", "X."
  ; "X{.list}", "X.red,green,blue"
  ; "X{.list*}", "X.red.green.blue"
  ; "{/who,dub}", "/fred/me%2Ftoo"
  ; "{/var,empty}", "/value/"
  ; "{/list}", "/red,green,blue"
  ; "{/list*}", "/red/green/blue"
  ; "{/keys*}", "/semi=%3B/dot=./comma=%2C"
  ; "{;who}", ";who=fred"
  ; "{;empty}", ";empty"
  ; "{;list}", ";list=red,green,blue"
  ; "{;list*}", ";list=red;list=green;list=blue"
  ; "{;keys*}", ";semi=%3B;dot=.;comma=%2C"
  ; "{?x,y}", "?x=1024&y=768"
  ; "{?x,y,empty}", "?x=1024&y=768&empty="
  ; "{?list}", "?list=red,green,blue"
  ; "{?list*}", "?list=red&list=green&list=blue"
  ; "{?keys*}", "?semi=%3B&dot=.&comma=%2C"
  ; "{&x,y,empty}", "&x=1024&y=768&empty="
  ; "{&list*}", "&list=red&list=green&list=blue"
  ]
;;

let test_examples () =
  List.iter examples ~f:(fun (source, expected) ->
    let actual = expand source in
    check
      (Stdlib.Printf.sprintf "%s: expected %S, got %S" source expected actual)
      (String.equal expected actual))
;;

let test_unicode_and_percent () =
  let t = template "/r\195\169sum\195\169/{name:1}/{+encoded}" in
  let expanded =
    Template.expand_assoc t
      [ "name", `String "\240\159\152\128tail"; "encoded", `String "%2f" ]
  in
  check "Unicode literals and prefix count characters"
    (match expanded with
     | Ok value -> String.equal value "/r%C3%A9sum%C3%A9/%F0%9F%98%80/%2f"
     | Error _ -> false);
  check "variable names retain percent spelling"
    (List.equal String.equal
       (Template.variables (template "{a,a,b%20c}"))
       [ "a"; "b%20c" ]);
  check "a prefix does not split percent-encoded UTF-8"
    (match
       Template.expand_assoc (template "{+x:1}")
         [ "x", `String "%F0%9F%98%80tail" ]
     with
     | Ok value -> String.equal value "%F0%9F%98%80"
     | Error _ -> false)
;;

let test_uri () =
  let t = template "https://example.test{/path*}{?accountId}" in
  match
    Template.expand_uri_assoc t
      [ "path", `List [ "jmap"; "download" ]; "accountId", `String "a/b" ]
  with
  | Error error ->
      failwith (Stdlib.Format.asprintf "%a" Template.pp_error error)
  | Ok uri ->
      check "direct Uriz expansion"
        (String.equal
           (Httpz.Uriz.to_string uri)
           "https://example.test/jmap/download?accountId=a%2Fb")
;;

let test_levels () =
  List.iter
    [ "literal", `Level_1
    ; "{var}", `Level_1
    ; "{var}/{who}", `Level_1
    ; "{+var}", `Level_2
    ; "{#var}", `Level_2
    ; "{x,y}", `Level_3
    ; "{?x}", `Level_3
    ; "{/x}", `Level_3
    ; "{var:3}", `Level_4
    ; "{list*}", `Level_4
    ]
    ~f:(fun (source, expected) ->
      check ("level of " ^ source) (Poly.equal expected (Template.level (template source))))
;;

let test_expand_resolve () =
  let base = Httpz.Uriz.of_string_exn "https://example.test/a/session?old=1#fragment" in
  let resolve source bindings =
    match Template.expand_resolve_assoc ~base (template source) bindings with
    | Ok resolved -> Httpz.Uriz.to_string resolved
    | Error error ->
        failwith
          (Stdlib.Format.asprintf "could not resolve %S: %a" source
             Template.pp_error error)
  in
  check "relative template"
    (String.equal
       (resolve "../download/{id}?literal=%7b" [ "id", `String "42" ])
       "https://example.test/download/42?literal=%7B");
  check "absolute template"
    (String.equal
       (resolve "https://blobs.test/{accountId}/{blobId}"
          [ "accountId", `String "a"; "blobId", `String "b" ])
       "https://blobs.test/a/b");
  let cases =
    [ "{x}", []
    ; "{x}", [ "x", `String "" ]
    ; "{x}", [ "x", `String "." ]
    ; "{x}", [ "x", `String ".." ]
    ; "next{x}", [ "x", `String "" ]
    ; "{?x}", []
    ; "{#x}", [ "x", `String "new" ]
    ; "{+x}", [ "x", `String "../other?fresh=1" ]
    ]
  in
  List.iter cases ~f:(fun (source, bindings) ->
    let expanded =
      match Template.expand_uri_assoc (template source) bindings with
      | Ok uri -> uri
      | Error error -> failwith (Stdlib.Format.asprintf "%a" Template.pp_error error)
    in
    let expected = Httpz.Uriz.resolve ~base expanded |> Httpz.Uriz.to_string in
    check ("expand then resolve: " ^ source)
      (String.equal expected (resolve source bindings)))
;;

let test_errors () =
  List.iter
    [ "{}"; "{=x}"; "{x:0}"; "{x:10000}"; "{x,}"; "{.x"; "a b";
      "{%xx}"; "{a..b}" ]
    ~f:(fun source ->
      check ("invalid " ^ source) (Result.is_error (Template.of_string source)));
  let composite_prefix = template "{list:2}" in
  check "prefix does not apply to a composite"
    (Result.is_error
       (Template.expand_assoc composite_prefix [ "list", `List [ "a" ] ]));
  let invalid_utf8 = template "{x}" in
  check "invalid UTF-8 binding is rejected"
    (Result.is_error
       (Template.expand_assoc invalid_utf8 [ "x", `String "\255" ]));
  let calls = ref 0 in
  let repeated = template "{x}/{x}" in
  let result =
    Template.expand repeated (fun _ ->
      Int.incr calls;
      Some (`String (Int.to_string !calls)))
  in
  check "lookup is stable within one expansion"
    (!calls = 1
     && match result with Ok value -> String.equal value "1/1" | Error _ -> false)
;;

let () =
  test_examples ();
  test_unicode_and_percent ();
  test_uri ();
  test_levels ();
  test_expand_resolve ();
  test_errors ();
  Stdio.printf "test_uri_template: %d checks passed\n" !checks
;;

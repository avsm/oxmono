let check name test = if not test then failwith name

let parse ?max_bytes ?max_depth ?max_feeds s =
  Sortal_feed.Opml.decode ?max_bytes ?max_depth ?max_feeds s

let valid body =
  "<opml version=\"2.0\"><head><title>Test</title></head><body>" ^ body
  ^ "</body></opml>"

let () =
  let xml =
    valid
      {|<outline text="Friends"><outline text="A &amp; B" xmlUrl="https://example.org/feed" htmlUrl="https://example.org/"/><outline text="Duplicate" xmlUrl="https://example.org/feed"/></outline>|}
  in
  (match parse xml with
  | Error message -> failwith message
  | Ok doc ->
      check "title" (doc.title = Some "Test");
      check "deduplicated" (List.length doc.feeds = 1);
      let feed = List.hd doc.feeds in
      check "entity decoding" (feed.title = "A & B");
      check "group preserved" (feed.groups = [ "Friends" ]));
  let rejected name source = check name (Result.is_error (parse source)) in
  rejected "DTD rejected"
    "<!DOCTYPE opml [<!ENTITY x 'hi'>]><opml version='2.0'><body/></opml>";
  rejected "external entity rejected"
    "<!DOCTYPE opml SYSTEM 'file:///etc/passwd'><opml \
     version='2.0'><body/></opml>";
  rejected "unknown entity" (valid "<outline text='&unknown;'/>");
  rejected "wrong root" "<rss/>";
  rejected "missing body" "<opml version='2.0'/>";
  rejected "duplicate body" "<opml version='2.0'><body/><body/></opml>";
  rejected "URL credentials"
    (valid "<outline xmlUrl='https://user:password@host/feed'/>");
  rejected "non-HTTP URL" (valid "<outline xmlUrl='file:///tmp/a'/>");
  rejected "relative URL" (valid "<outline xmlUrl='/feed'/>");
  rejected "truncated XML" "<opml version='2.0'><body>";
  rejected "trailing document" (valid "" ^ valid "");
  check "byte limit" (Result.is_error (parse ~max_bytes:10 xml));
  check "depth limit" (Result.is_error (parse ~max_depth:3 xml));
  check "duplicate counts toward limit"
    (Result.is_error (parse ~max_feeds:1 xml));
  check "no include fetching"
    (Result.is_ok
       (parse (valid "<outline type='include' url='file:///tmp/a'/>")));
  check "OPML 1.0" (Result.is_ok (parse "<opml version='1.0'><body/></opml>"));
  print_endline "sortal OPML: parsing, limits and untrusted XML checks passed"

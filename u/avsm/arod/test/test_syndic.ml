(* [syndic] is vendored under vendor/ and patched so that its interface is
   callable from the portable context arod and sortal parse and build feeds in.
   Dune skips aliases under a vendored directory, so the vendored copy's own
   tests never run. This one does. It fails if a re-vendor drops the
   annotations, and it pins the tables the patch turned into code. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let stamp s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error _ -> invalid_arg ("not a timestamp: " ^ s)

let contains ~needle s =
  let n = String.length needle and m = String.length s in
  let rec go i =
    i + n <= m && (String.equal (String.sub s i n) needle || go (i + 1))
  in
  n = 0 || go 0

(* A feed held at module level. It does not compile unless every type reachable
   from [Syndic.Atom.feed] crosses portability and contention, which is what
   makes [Uriz.t] and [Ptime.t] having the [immutable_data] kind matter here.
   Passing a feed in as an argument would prove nothing. *)
let feed =
  let updated = stamp "2026-08-19T09:41:07Z" in
  let entry =
    Syndic.Atom.entry
      ~id:(Uriz.of_string_exn "urn:uuid:guard-1")
      ~authors:(Syndic.Atom.author "A Name", [])
      ~title:(Syndic.Atom.Text "An entry")
      ~links:
        [ Syndic.Atom.link ~rel:Syndic.Atom.Alternate
            (Uriz.of_string_exn "https://example.com/posts/1") ]
      ~content:(Syndic.Atom.Text "Body text")
      ~published:(stamp "2026-08-18T00:00:00Z")
      ~updated ()
  in
  Syndic.Atom.feed
    ~id:(Uriz.of_string_exn "https://example.com/atom.xml")
    ~title:(Syndic.Atom.Text "A feed")
    ~links:
      [ Syndic.Atom.link ~rel:Syndic.Atom.Self
          (Uriz.of_string_exn "https://example.com/atom.xml") ]
    ~updated [ entry ]

(* The ascriptions are the point. None of these compile unless the seven
   syndic interfaces still carry their [@@ portable] annotations. *)

let render : (unit -> string) @ portable =
 fun () ->
  let b = Buffer.create 1024 in
  Syndic.Atom.output feed (`Buffer b);
  Buffer.contents b

let render_tree : (unit -> string) @ portable =
 fun () ->
  let ns_prefix s =
    if String.equal s "http://www.w3.org/2005/Atom" then Some "" else None
  in
  Syndic.XML.to_string ~ns_prefix (Syndic.Atom.to_xml feed)

let feed_title : (unit -> string) @ portable =
 fun () ->
  match feed.Syndic.Atom.title with Syndic.Atom.Text t -> t | _ -> "?"

let atom_doc =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
   <feed xmlns=\"http://www.w3.org/2005/Atom\">\
   <title>Parsed</title>\
   <id>urn:uuid:guard-2</id>\
   <updated>2026-08-19T09:41:07Z</updated>\
   <entry>\
   <title>An entry</title>\
   <id>urn:uuid:guard-3</id>\
   <link rel=\"alternate\" href=\"https://example.com/posts/2\"/>\
   <updated>2026-08-19T09:41:07Z</updated>\
   </entry>\
   </feed>"

let rss2_doc =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
   <rss version=\"2.0\"><channel>\
   <title>An RSS channel</title>\
   <link>https://example.com/</link>\
   <description>Described</description>\
   <item>\
   <title>An item</title>\
   <link>https://example.com/posts/3</link>\
   <guid>https://example.com/posts/3</guid>\
   <pubDate>Tue, 19 Aug 2026 09:41:07 +0000</pubDate>\
   </item>\
   </channel></rss>"

let parse_atom : (string -> string) @ portable =
 fun s ->
  let feed = Syndic.Atom.parse (Xmlm.make_input (`String (0, s))) in
  match feed.Syndic.Atom.title with Syndic.Atom.Text t -> t | _ -> "?"

let parse_rss2 : (string -> string) @ portable =
 fun s ->
  let channel = Syndic.Rss2.parse (Xmlm.make_input (`String (0, s))) in
  let feed = Syndic.Rss2.to_atom channel in
  match feed.Syndic.Atom.entries with
  | e :: _ -> Uriz.to_string e.Syndic.Atom.id
  | [] -> "no entries"

let render_opml : (unit -> string) @ portable =
 fun () ->
  let head =
    Syndic.Opml1.head
      ~date_created:(stamp "2026-08-19T09:41:07Z")
      ~date_modified:(stamp "2026-08-19T09:41:07Z")
      ~owner_name:"A Name" ~owner_email:"a@example.com" "A list"
  in
  let outline =
    Syndic.Opml1.outline
      ~xml_url:(Uriz.of_string_exn "https://example.com/atom.xml")
      "A feed"
  in
  let o = { Syndic.Opml1.version = "1.0"; head; body = [ outline ] } in
  let b = Buffer.create 512 in
  Syndic.Opml1.output o (`Buffer b);
  Buffer.contents b

(* [rfc822] pins both arrays [syndic_date.ml] used to close over. [to_rfc822]
   names the weekday and the month, so the two matches that replaced them are
   both on this path. *)
let rfc822 : (string -> string) @ portable =
 fun s -> Syndic.Date.to_rfc822 (Syndic.Date.of_rfc822 s)

let month_name : (string -> string) @ portable =
 fun s ->
  Syndic.Date.string_of_month (Syndic.Date.month (Syndic.Date.of_rfc822 s))

let lax_uri : (string -> string) @ portable =
 fun s -> Uriz.to_string (Syndic.XML.uri_of_string s)

let () =
  check "a feed held at module level renders from a portable closure"
    (String.starts_with
       ~prefix:"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<feed xmlns="
       (render ()));
  check "the rendered feed carries its entry"
    (contains ~needle:"<id>urn:uuid:guard-1</id>" (render ()));
  check "the rendered feed carries its self link"
    (contains ~needle:"href=\"https://example.com/atom.xml\"" (render ()));
  check "to_xml renders the same document body"
    (contains ~needle:"<title type=\"text\">A feed</title>" (render_tree ()));
  check "a field of the module-level feed is readable"
    (String.equal (feed_title ()) "A feed");
  check "an Atom document parses" (String.equal (parse_atom atom_doc) "Parsed");
  check "an RSS 2.0 document parses and converts"
    (String.equal (parse_rss2 rss2_doc) "https://example.com/posts/3");
  check "an OPML document renders"
    (contains
       ~needle:"xmlUrl=\"https://example.com/atom.xml\""
       (render_opml ()));
  (* The weekday and month tables were arrays closed over by the two functions
     below before the patch and are matches after it. *)
  (* [of_rfc822] ignores the weekday it is given and [to_rfc822] computes one,
     so this also shows the weekday match answering for a date whose input
     names a different day. 19 August 2026 was a Wednesday. *)
  check "an RFC 822 date round trips through both replaced tables"
    (String.equal (rfc822 "Tue, 19 Aug 2026 09:41:07 +0000")
       "Wed, 19 Aug 2026 09:41:07 0000");
  check "the twelve months name themselves"
    (String.equal
       (String.concat ""
          (List.map
             (fun m -> month_name ("Tue, 01 " ^ m ^ " 2026 00:00:00 +0000"))
             [ "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep";
               "Oct"; "Nov"; "Dec" ]))
       "JanFebMarAprMayJunJulAugSepOctNovDec");
  check "every weekday names itself"
    (String.equal
       (String.concat " "
          (List.map
             (fun d ->
               String.sub
                 (rfc822 ("Mon, " ^ d ^ " Aug 2026 00:00:00 +0000"))
                 0 3)
             [ "17"; "18"; "19"; "20"; "21"; "22"; "23" ]))
       "Mon Tue Wed Thu Fri Sat Sun");
  check "the lax URI helper is reachable from a portable closure"
    (String.equal
       (lax_uri "https://example.com/a b")
       "https://example.com/a%20b");
  Printf.printf "test_syndic: %d checks ok\n" !checks

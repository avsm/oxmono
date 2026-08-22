(* [sitemap] is vendored under vendor/ and annotated so that its interface is
   callable from the portable context arod renders /sitemap.xml in. Dune skips
   aliases under a vendored directory, so the vendored copy's own tests never
   run. This one does, and it fails to compile if a re-vendor drops the
   annotation. It also pins the document the library writes, since arod serves
   those bytes. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let eq name got want =
  incr checks;
  if not (String.equal got want) then (
    prerr_endline ("FAIL: " ^ name);
    prerr_endline ("  got : " ^ got);
    prerr_endline ("  want: " ^ want);
    exit 1)

let decl = {|<?xml version="1.0" encoding="UTF-8"?>|} ^ "\n"
let urlset body =
  decl ^ {|<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">|} ^ body
  ^ "</urlset>\n"

(* [urls] is bound at module level and read inside a portable closure, which
   is what proves the annotation rather than assuming it. A parameter-shaped
   ascription would prove nothing: it is the capture that requires
   [Sitemap.url] to cross portability and contention, and [Sitemap.output] to
   be portable. *)
let urls =
  [ Sitemap.v ~lastmod:(2024, 2, 3) ~changefreq:Sitemap.Weekly
      ~priority:(Sitemap.priority 0.75) "https://example.com/a";
    Sitemap.v "https://example.com/b" ]

let captured : (unit -> string) @ portable = fun () -> Sitemap.output urls

(* The other three entry points take an [Xmlm.output] or a [Buffer.t]. Neither
   type carries a kind, so a portable closure reaches them only by making one
   itself, which is what arod does. *)
let through_xmlm : (unit -> string) @ portable =
 fun () ->
  let b = Buffer.create 256 in
  let o = Xmlm.make_output ~nl:true (`Buffer b) in
  Sitemap.output_urlset o urls;
  Buffer.contents b

let through_buffer : (unit -> string) @ portable =
 fun () ->
  let b = Buffer.create 256 in
  Sitemap.output_urlset_to_buffer b urls;
  Buffer.contents b

let one_url : (string -> string) @ portable =
 fun loc ->
  let b = Buffer.create 256 in
  let o = Xmlm.make_output ~decl:false ~nl:true (`Buffer b) in
  Xmlm.output o (`Dtd None);
  Sitemap.output_url o (Sitemap.v loc);
  Buffer.contents b

let freqs : (unit -> string) @ portable =
 fun () ->
  String.concat " "
    (List.map Sitemap.changefreq_to_string
       [ Sitemap.Always; Hourly; Daily; Weekly; Monthly; Yearly; Never ])

let clamped : (float -> string) @ portable =
 fun p ->
  Sitemap.output [ Sitemap.v ~priority:(Sitemap.priority p) "u" ]

let both =
  urlset
    ({|<url><loc>https://example.com/a</loc><lastmod>2024-02-03</lastmod>|}
    ^ {|<changefreq>weekly</changefreq><priority>0.8</priority></url>|}
    ^ {|<url><loc>https://example.com/b</loc></url>|})

let () =
  eq "a portable closure writes a sitemap from module-level urls" (captured ())
    both;
  eq "and through an Xmlm output it makes itself" (through_xmlm ()) both;
  eq "and through a buffer it makes itself" (through_buffer ()) both;
  eq "a single url is a url element" (one_url "https://example.com/c")
    ({|<url><loc>https://example.com/c</loc></url>|} ^ "\n");
  eq "an optional member is absent rather than empty"
    (Sitemap.output [ Sitemap.v "https://example.com/b" ])
    (urlset {|<url><loc>https://example.com/b</loc></url>|});
  eq "the change frequencies are the schema's seven" (freqs ())
    "always hourly daily weekly monthly yearly never"

(* [priority] clamps to [0.0, 1.0] and the document carries one decimal, so a
   priority is rounded on the way out and two that differ in the second place
   are one value to a crawler. *)
let () =
  eq "a priority above one clamps" (clamped 9.)
    (urlset {|<url><loc>u</loc><priority>1.0</priority></url>|});
  eq "a priority below zero clamps" (clamped (-9.))
    (urlset {|<url><loc>u</loc><priority>0.0</priority></url>|});
  eq "a priority is written to one decimal" (clamped 0.75)
    (urlset {|<url><loc>u</loc><priority>0.8</priority></url>|})

(* The protocol caps a location at 2048 characters and the library refuses one
   at the cap rather than above it. Arod builds locations from entry URLs, so
   nothing it serves comes near, but a caller that concatenates a query would.
*)
let () =
  check "a location at the cap is refused"
    (match Sitemap.v (String.make 2048 'x') with
    | (_ : Sitemap.url) -> false
    | exception Invalid_argument m ->
      String.equal m "location value must be less than 2048 characters");
  check "one below it is accepted"
    (match Sitemap.v (String.make 2047 'x') with
    | (_ : Sitemap.url) -> true
    | exception Invalid_argument _ -> false)

let () = Printf.printf "test_sitemap: %d checks ok\n" !checks

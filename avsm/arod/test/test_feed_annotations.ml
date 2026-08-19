(* Feed annotations are stored in a JSON file keyed by the rendered URL of the
   entry they describe. The rendering is not stable: keys written before Syndic
   moved from [uri] to [uriz] carry opam uri's spelling, which decoded
   percent-encoded reserved characters that uriz keeps. A key written then and
   looked up now would miss, and a backlink would vanish with no error. These
   tests pin that both spellings resolve, and that the write side stores the
   form the read side looks up. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* The spelling Syndic emits today, for a URL a feed really can carry. *)
let rendered s = Uriz.to_string (Syndic.XML.uri_of_string s)

let resolves ~name ~stored ~looked_up =
  let ann = Sortal_feed.Annotations.empty () in
  Sortal_feed.Annotations.add_slug ann ~url:stored ~slug:"a-note";
  let idx = Arod.Ctx.annotation_index ann in
  check name (Arod.Ctx.annotation_slugs idx looked_up = [ "a-note" ])

(* opam uri decoded [%2B] in a path, so an older key holds a literal [+]. *)
let test_plus_in_path () =
  let url = "https://example.com/wiki/C%2B%2B" in
  check "uriz keeps %2B" (rendered url = url);
  (* The bug this guards against. A raw lookup, which is what the reader did
     before the index went in, misses the older key. *)
  let raw = Sortal_feed.Annotations.empty () in
  Sortal_feed.Annotations.add_slug raw ~url:"https://example.com/wiki/C++"
    ~slug:"a-note";
  check "a raw lookup misses, which is why the index exists"
    (Sortal_feed.Annotations.slugs_for_url raw (rendered url) = []);
  resolves ~name:"old plus key resolves for the uriz rendering"
    ~stored:"https://example.com/wiki/C++" ~looked_up:(rendered url);
  resolves ~name:"new key resolves for the old spelling"
    ~stored:(rendered url) ~looked_up:"https://example.com/wiki/C++"

(* opam uri decoded [%3A%2F%2F] in a query, turning one parameter into
   something that reads as a nested URL. *)
let test_encoded_url_in_query () =
  let url = "https://example.com/go?to=https%3A%2F%2Fa.org%2Fb" in
  check "uriz keeps the encoded query triplets" (rendered url = url);
  resolves ~name:"old decoded-query key resolves for the uriz rendering"
    ~stored:"https://example.com/go?to=https://a.org/b"
    ~looked_up:(rendered url)

(* The normalisation the index applies is the one the rest of Arod.Ctx keys
   on, so a stored key that differs only by [www.] or a trailing slash also
   resolves. That behaviour predates the uriz swap and must survive it. *)
let test_host_and_slash () =
  resolves ~name:"www. prefix is ignored"
    ~stored:"https://www.example.com/post/1"
    ~looked_up:"https://example.com/post/1";
  resolves ~name:"trailing slash is ignored"
    ~stored:"https://example.com/post/1/"
    ~looked_up:"https://example.com/post/1"

let test_distinct_urls_stay_distinct () =
  let ann = Sortal_feed.Annotations.empty () in
  Sortal_feed.Annotations.add_slug ann ~url:"https://example.com/a" ~slug:"a";
  let idx = Arod.Ctx.annotation_index ann in
  check "an unrelated URL finds nothing"
    (Arod.Ctx.annotation_slugs idx "https://example.com/b" = [])

(* Two spellings of one URL can coexist in a file, because [arod feed
   associate] appends to whatever is already there. The index must union them
   rather than pick one. *)
let test_two_spellings_union () =
  let ann = Sortal_feed.Annotations.empty () in
  Sortal_feed.Annotations.add_slug ann ~url:"https://example.com/wiki/C++"
    ~slug:"old";
  Sortal_feed.Annotations.add_slug ann ~url:"https://example.com/wiki/C%2B%2B"
    ~slug:"new";
  let idx = Arod.Ctx.annotation_index ann in
  let slugs = List.sort compare (Arod.Ctx.annotation_slugs idx
                                   "https://example.com/wiki/C%2B%2B") in
  check "both spellings contribute their slugs" (slugs = [ "new"; "old" ])

(* [arod feed associate] writes [normalise_url] of the entry URL, which is
   what [annotation_slugs] looks up. This pins the two halves together. *)
let test_write_key_matches_lookup () =
  let url = rendered "https://example.com/wiki/C%2B%2B" in
  let ann = Sortal_feed.Annotations.empty () in
  Sortal_feed.Annotations.add_slug ann ~url:(Arod.Ctx.normalise_url url)
    ~slug:"a-note";
  let idx = Arod.Ctx.annotation_index ann in
  check "the key the writer stores is the key the reader finds"
    (Arod.Ctx.annotation_slugs idx url = [ "a-note" ])

let () =
  test_plus_in_path ();
  test_encoded_url_in_query ();
  test_host_and_slash ();
  test_distinct_urls_stay_distinct ();
  test_two_spellings_union ();
  test_write_key_matches_lookup ();
  Printf.printf "test_feed_annotations: %d checks ok\n" !checks

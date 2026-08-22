(* The JSON API responses are pinned here byte for byte. render_capture.sh
   covers the pagination API over a real corpus, but the search API is outside
   its route set, and the route tests drive the environment through stubs that
   never reach a codec. A member order, an omitted member or an escaping rule
   that changes is a change to what the search box reads, so it has to be a
   deliberate edit to a string below rather than a silent one.

   The stats views are not pinned here. Each takes a database handle, and the
   route that serves it needs an access log the test corpus does not have. *)

let checks = ref 0

let eq name got want =
  incr checks;
  if got <> want then (
    prerr_endline ("FAIL: " ^ name);
    prerr_endline ("  got : " ^ got);
    prerr_endline ("  want: " ^ want);
    exit 1)

let cfg : Arod.Config.t =
  {
    Arod.Config.default with
    site = { Arod.Config.default.site with base_url = "https://example.com" };
  }

let note : Bushel.Note.t =
  {
    Bushel.Note.title = "A Note";
    date = (2024, 2, 3);
    slug = "a-note";
    body = "Body.";
    tags = [];
    draft = false;
    updated = None;
    sidebar = None;
    index_page = false;
    perma = false;
    weeknote = false;
    featured = false;
    doi = None;
    synopsis = None;
    titleimage = None;
    via = None;
    slug_ent = None;
    source = None;
    url = None;
    author = None;
    category = None;
    standardsite = None;
    social = None;
    source_file = None;
  }

let ctx =
  Arod.Ctx.of_entries ~config:cfg
    (Bushel.Entry.v ~papers:[] ~notes:[ note ] ~projects:[] ~ideas:[]
       ~videos:[] ~contacts:[] ~data_dir:"." ())

let result ?(kind = "note") ?(snippet = "s") ?(tags = [])
    ?(parent_slugs = []) slug : Arod_search.result =
  {
    Arod_search.slug;
    kind;
    url = "/notes/" ^ slug;
    title = "T";
    snippet;
    date = "2024-02-03";
    rank = 0.0;
    parent_slugs;
    tags;
  }

let search results = Arod_handlers.Render.search ~ctx results

let () =
  eq "an empty result set is an empty array"
    (search [])
    {|{"results":[]}|};
  eq "a bare hit omits tags, thumbnail and parents"
    (search [ result "a-note" ])
    ({|{"results":[{"slug":"a-note","kind":"note","url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03"}]}|});
  eq "tags are emitted when the entry has them"
    (search [ result ~tags:[ "ocaml"; "mirage" ] "a-note" ])
    ({|{"results":[{"slug":"a-note","kind":"note","url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03",|}
     ^ {|"tags":["ocaml","mirage"]}]}|});
  eq "a parent the context knows is expanded, one it does not is dropped"
    (search [ result ~parent_slugs:[ "a-note"; "absent" ] "a-note" ])
    ({|{"results":[{"slug":"a-note","kind":"note","url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03",|}
     ^ {|"parents":[{"slug":"a-note","title":"A Note",|}
     ^ {|"url":"/notes/a-note","kind":"note"}]}]}|})

(* A snippet carries arbitrary document text into the response, so the
   escaping rule is pinned on the characters that have one. *)
let () =
  let snippet = "q\"b s\\b lt< amp& nl\n tab\t cr\r del\127 e\xc3\xa9" in
  eq "a snippet escapes what RFC 8259 requires and nothing else"
    (search [ result ~snippet "a-note" ])
    ({|{"results":[{"slug":"a-note","kind":"note","url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"q\"b s\\b lt< amp& nl\n tab\t cr\r |}
     ^ "del\\u007F e\xc3\xa9\"" ^ {|,"date":"2024-02-03"}]}|})

let () =
  eq "an absent collection is a JSON error object"
    (Arod_handlers.Render.pagination ~ctx ~collection:None ~offset:0 ~limit:10
       ~types:[])
    {|{"error":"Missing collection parameter"}|};
  eq "so is an unknown one"
    (Arod_handlers.Render.pagination ~ctx ~collection:(Some "nope") ~offset:0
       ~limit:10 ~types:[])
    {|{"error":"Invalid collection type"}|}

let () = Printf.printf "test_json: %d checks ok\n" !checks

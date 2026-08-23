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

let hit ?(kind = "note") ?(snippet = "s") ?(tags = []) ?(parent_slugs = [])
    slug : Arod_search.hit =
  {
    Arod_search.slug;
    kind;
    url = "/notes/" ^ slug;
    title = "T";
    snippet;
    date = "2024-02-03";
    tags;
    parent_slugs;
    score = 1.0;
  }

let results ?(goto = []) ?(links = []) ?(kinds = []) ?(years = [])
    ?(tags = []) work : Arod_search.results =
  {
    Arod_search.terms = [];
    goto;
    work;
    work_total = List.length work;
    links;
    links_total = List.length links;
    kinds;
    years;
    tags;
  }

(* The responses are written rather than returned, so the test plays backend:
   it lends a sink that accumulates and reads what came out. Going through
   [emit_sub] is deliberate, since that is the path a real encode takes. *)
let render write =
  let b = Buffer.create 4096 in
  write
    (Proffer.Backend.sink
       ~emit_sub:(fun s off len -> Buffer.add_subbytes b s off len)
       (fun s -> Buffer.add_string b s));
  Buffer.contents b

let search r = render (Arod_handlers.Render.search ~ctx r)

let empty_tail =
  {|,"work_total":0,"links":[],"links_total":0,"kinds":[],"years":[],|}
  ^ {|"tags":[]}|}

let () =
  eq "an empty result set is every member, empty"
    (search Arod_search.empty)
    ({|{"goto":[],"work":[]|} ^ empty_tail);
  eq "a bare hit omits tags, thumbnail and parents"
    (search (results [ hit "a-note" ]))
    ({|{"goto":[],"work":[{"slug":"a-note","kind":"note",|}
     ^ {|"url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03"}],"work_total":1,|}
     ^ {|"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}|});
  eq "tags are emitted when the entry has them"
    (search (results [ hit ~tags:[ "ocaml"; "mirage" ] "a-note" ]))
    ({|{"goto":[],"work":[{"slug":"a-note","kind":"note",|}
     ^ {|"url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03",|}
     ^ {|"tags":["ocaml","mirage"]}],"work_total":1,|}
     ^ {|"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}|});
  eq "a parent the context knows is expanded, one it does not is dropped"
    (search (results [ hit ~parent_slugs:[ "a-note"; "absent" ] "a-note" ]))
    ({|{"goto":[],"work":[{"slug":"a-note","kind":"note",|}
     ^ {|"url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"s","date":"2024-02-03",|}
     ^ {|"parents":[{"slug":"a-note","title":"A Note",|}
     ^ {|"url":"/notes/a-note","kind":"note"}]}],"work_total":1,|}
     ^ {|"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}|});
  eq "go-to hits and facets are objects"
    (search
       (results
          ~goto:[ { Arod_search.label = "Papers"; url = "/papers";
                    detail = "section"; goto_kind = `Section } ]
          ~kinds:[ ("note", 2) ] ~years:[ (2024, 2) ] ~tags:[ ("ocaml", 1) ]
          []))
    ({|{"goto":[{"label":"Papers","url":"/papers","detail":"section",|}
     ^ {|"kind":"section"}],"work":[],"work_total":0,"links":[],|}
     ^ {|"links_total":0,"kinds":[{"kind":"note","count":2}],|}
     ^ {|"years":[{"year":2024,"count":2}],|}
     ^ {|"tags":[{"tag":"ocaml","count":1}]}|})

(* A snippet carries arbitrary document text into the response, so the
   escaping rule is pinned on the characters that have one. *)
let () =
  let snippet = "q\"b s\\b lt< amp& nl\n tab\t cr\r del\127 e\xc3\xa9" in
  eq "a snippet escapes what RFC 8259 requires and nothing else"
    (search (results [ hit ~snippet "a-note" ]))
    ({|{"goto":[],"work":[{"slug":"a-note","kind":"note",|}
     ^ {|"url":"/notes/a-note",|}
     ^ {|"title":"T","snippet":"q\"b s\\b lt< amp& nl\n tab\t cr\r |}
     ^ "del\\u007F e\xc3\xa9\"" ^ {|,"date":"2024-02-03"}],"work_total":1,|}
     ^ {|"links":[],"links_total":0,"kinds":[],"years":[],"tags":[]}|})

let () =
  eq "an absent collection is a JSON error object"
    (render
       (Arod_handlers.Render.pagination ~ctx ~collection:None ~offset:0
          ~limit:10 ~types:[]))
    {|{"error":"Missing collection parameter"}|};
  eq "so is an unknown one"
    (render
       (Arod_handlers.Render.pagination ~ctx ~collection:(Some "nope")
          ~offset:0 ~limit:10 ~types:[]))
    {|{"error":"Invalid collection type"}|}

let () = Printf.printf "test_json: %d checks ok\n" !checks

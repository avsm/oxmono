(* The payload guard.

   [test_smap] pins what {!Bushel.Smap} computes. This test pins something the
   compiler decides rather than something the program computes: that a request
   handler marked [portable] can capture a loaded context and read the values
   it looks up.

   A portable function may only touch data whose type crosses contention, and a
   type only crosses if the compiler has been told a kind for it. An abstract
   type in an [.mli] with no kind annotation defaults to [value] and crosses
   nothing, however immutable its representation is. Nothing warns about that
   until a capture is attempted, so the kinds on {!Arod.Ctx.t},
   {!Bushel.Entry.t}, {!Sortal_schema.Contact.t}, {!Sortal_schema.Feed.t} and
   the map inside {!Srcsetter.t} are invisible from every other test.

   The three closures below are the guard. Deleting a kind annotation from any
   of those interfaces, or putting a [Hashtbl.t] or a stdlib [Map.S.t] back
   into one of their representations, stops this file compiling. The third
   closure guards modalities rather than kinds, and says so where it stands.
   Their reads are real and their answers are asserted, so a handler that
   crossed but found nothing would fail too. *)

module Contact = Sortal_schema.Contact

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* {1 Fixtures} *)

let note ?titleimage ~slug ~title ~date () =
  {
    Bushel.Note.title;
    date;
    slug;
    body = "";
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
    titleimage;
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

let idea ~slug ~title ~supervisors =
  {
    Bushel.Idea.slug;
    title;
    level = Bushel.Idea.MPhil;
    project = ":a-project";
    status = Bushel.Idea.Available;
    month = 3;
    year = 2024;
    supervisors;
    students = [];
    supervisor_handles = [];
    student_handles = [];
    reading = "";
    body = "";
    url = None;
    tags = [];
    social = None;
  }

(* [ada] carries a feed so that the closure reaches {!Sortal_schema.Feed.t},
   which is abstract in its own interface and crosses only because of the kind
   there. *)
let ada =
  Contact.make ~handle:"ada" ~names:[ "Ada Lovelace" ]
    ~feeds:
      [
        Sortal_schema.Feed.make ~feed_type:Sortal_schema.Feed.Atom
          ~url:"https://example.com/feed.xml" ();
      ]
    ()

(* The URL is the shape every video in the real collection has, a PeerTube
   watch path. {!Bushel.Md.to_markdown} rewrites the [watch] segment to
   [embed], which is the one place on that path that parses and reassembles a
   URL. *)
let a_video =
  {
    Bushel.Video.slug = "a-video";
    title = "A Sample Video";
    published_date = Option.get (Ptime.of_date (2022, 9, 1));
    uuid = "0b1c2d3e-4f56-4789-8abc-def012345678";
    description = "";
    url = "https://crank.example.org/videos/watch/0b1c2d3e-4f56-4789-8abc-def012345678";
    talk = true;
    vertical = false;
    paper = None;
    project = None;
    tags = [];
    social = None;
  }

let sample_image =
  let variants =
    Srcsetter.MS.of_list
      [ ("sample-480.webp", (480, 320)); ("sample-960.webp", (960, 640)) ]
  in
  Srcsetter.v "sample.webp" "sample-image" "src/sample.png" variants (1920, 1280)

(* Filed under the handle of [ada], which is the slug
   {!Bushel.Entry.contact_thumbnail} looks a contact's face up by. *)
let ada_image =
  let variants =
    Srcsetter.MS.of_list
      [ ("ada-320.webp", (320, 320)); ("ada-640.webp", (640, 640)) ]
  in
  Srcsetter.v "ada.webp" "ada" "src/ada.png" variants (1280, 1280)

let entries =
  Bushel.Entry.v ~papers:[]
    ~notes:
      [
        note ~slug:"hello-note" ~title:"Hello Note" ~date:(2025, 1, 5)
          ~titleimage:"sample-image" ();
      ]
    ~projects:[]
    ~ideas:[ idea ~slug:"an-idea" ~title:"A Sample Idea" ~supervisors:[ ada ] ]
    ~videos:[ a_video ] ~contacts:[ ada ]
    ~images:[ sample_image; ada_image ]
    ~data_dir:"." ()

let ctx = Arod.Ctx.of_entries ~config:Arod.Config.default entries

(* {1 The entry handler}

   Each read crosses one payload type into the closure. The entry variant
   carries {!Bushel.Note.t} and {!Bushel.Idea.t}; the idea carries a
   {!Sortal_schema.Contact.t}, which carries a {!Sortal_schema.Feed.t}; the
   image is a {!Srcsetter.t} and its variant map is walked, not just its scalar
   fields read. *)

let handler : (string -> string) @ portable =
 fun slug ->
  match Arod.Ctx.lookup ctx slug with
  | Some (`Note n) -> (
      let y, m, d = n.Bushel.Note.date in
      let stamp = Printf.sprintf "%s %04d-%02d-%02d" n.Bushel.Note.title y m d in
      match n.Bushel.Note.titleimage with
      | None -> stamp
      | Some img_slug -> (
          match Arod.Ctx.lookup_image ctx img_slug with
          | None -> stamp
          | Some img ->
              let widths =
                List.map
                  (fun (_, (w, _)) -> string_of_int w)
                  (Srcsetter.MS.bindings img.Srcsetter.variants)
              in
              Printf.sprintf "%s %s [%s]" stamp img.Srcsetter.name
                (String.concat " " widths)))
  | Some (`Idea i) ->
      let who =
        List.map
          (fun c ->
            Contact.name c ^ "/" ^ string_of_int (List.length (Contact.feeds c)))
          i.Bushel.Idea.supervisors
      in
      Printf.sprintf "%s <%s>" i.Bushel.Idea.title (String.concat "," who)
  | Some (`Paper p) -> p.Bushel.Paper.title
  | Some (`Project p) -> p.Bushel.Project.title
  | Some (`Video v) -> v.Bushel.Video.title
  | None -> ""

(* {1 The link handler}

   {!Arod.Ctx.of_entries} leaves the link table and the two backlink tables
   empty, and filling them needs a filesystem. Their payloads are covered by a
   second closure over a record with those fields and nothing else. *)

let link : Bushel.Link.t =
  {
    Bushel.Link.url = "https://example.com/post";
    date = (2025, 2, 3);
    description = "A Post";
    karakeep = None;
    bushel = None;
  }

let backlink : Arod.Ctx.feed_backlink =
  {
    Arod.Ctx.contact = ada;
    feed_entry =
      {
        Sortal_feed.Entry.id = "urn:1";
        title = Some "A Feed Post";
        date = Ptime.of_date (2025, 2, 3);
        summary = None;
        content = None;
        url =
          (match Uriz.of_string "https://example.com/post" with
          | Null -> None
          | This u -> Some u);
        source_feed = "https://example.com/feed.xml";
        source_type = Sortal_schema.Feed.Atom;
      };
  }

type tables = {
  links_by_url : Bushel.Link.t Bushel.Smap.t;
  feed_backlinks : Arod.Ctx.feed_backlink list Bushel.Smap.t;
}

let tables =
  {
    links_by_url = Bushel.Smap.of_list [ (link.Bushel.Link.url, link) ];
    feed_backlinks = Bushel.Smap.of_list [ ("hello-note", [ backlink ]) ];
  }

let link_handler : (string -> string) @ portable =
 fun key ->
  match Bushel.Smap.find_opt key tables.links_by_url with
  | Some l -> l.Bushel.Link.description
  | None -> (
      match Bushel.Smap.find_opt key tables.feed_backlinks with
      | Some (b :: _) ->
          Printf.sprintf "%s: %s"
            (Contact.handle b.Arod.Ctx.contact)
            (Option.value ~default:""
               b.Arod.Ctx.feed_entry.Sortal_feed.Entry.title)
      | Some [] | None -> "")

(* {1 The render handler}

   The two closures above guard kinds on payload types. This one guards
   modalities on the interfaces the render path reads through: the floating
   [@@ portable] on [srcsetter.mli], [bushel_entry.mli], [bushel_md.mli] and
   the four leaf entry interfaces. Every call below names a module-level
   function, which a portable closure may only read if that function is
   itself portable, so deleting any one of those floating annotations stops
   this file compiling.

   [Bushel.Md.with_bushel_links] is the resolver the goldens depend on, and
   it is reached the way a renderer reaches it, as an argument to
   [Cmarkit.Doc.of_string]. [Bushel.Md.make_link_only_mapper] is reached the
   way a renderer reaches it too, through [Cmarkit.Mapper.make], which is
   what a mapper written with [Cmarkit.Mapper.default] cannot be passed
   to. *)

let render_handler : (string -> string) @ portable =
 fun slug ->
  let target = ":" ^ slug in
  if not (Bushel.Md.is_bushel_slug target) then "not a slug"
  else
    let key = Bushel.Md.strip_handle target in
    let doc =
      Cmarkit.Doc.of_string ~strict:false
        ~resolver:Bushel.Md.with_bushel_links
        (Printf.sprintf "[text][%s]\n" target)
    in
    (* The resolver answers [:slug] with a tagged label that carries no
       destination, and the mapper turns that tagged label into a link to the
       entry's site path. Without the resolver the reference would have
       stayed literal text and the mapper would have had nothing to rewrite,
       so the rendered anchor is proof that both ran. *)
    let mapper =
      Cmarkit.Mapper.make ~inline:(Bushel.Md.make_link_only_mapper entries) ()
    in
    let doc = Cmarkit.Mapper.map_doc mapper doc in
    let html = String.trim (Cmarkit_html.of_doc ~safe:true doc) in
    match Bushel.Entry.lookup entries key with
    | None -> "no entry"
    | Some (`Note n as e) ->
        let iso_y, iso_w = Bushel.Note.week_number n in
        Printf.sprintf "%s %s %s %d-W%02d %s"
          (Bushel.Entry.to_type_string e)
          (Bushel.Entry.site_url e) (Bushel.Entry.title e) iso_y iso_w html
    | Some (`Idea i as e) ->
        let face =
          match Bushel.Entry.contact_thumbnail entries (List.hd i.Bushel.Idea.supervisors) with
          | Some path -> path
          | None -> "none"
        in
        Printf.sprintf "%s %s %s %s %s %s"
          (Bushel.Entry.to_type_string e)
          (Bushel.Entry.site_url e)
          (Bushel.Idea.level_to_string (Bushel.Idea.level i))
          (Bushel.Idea.status_to_string (Bushel.Idea.status i))
          face html
    | Some e -> Bushel.Entry.title e

(* {1 The context render}

   The three closures above stop at Bushel. This one captures a whole
   {!Arod.Ctx.t} and calls the real {!Arod.Md.to_html} through it, which is
   the render the site serves rather than a reconstruction of one. Two things
   hold it up and each was checked by removing it. Dropping the floating
   [@@ portable] from [arod_md.mli] fails here naming [Arod.Md.to_html], and
   dropping the [immutable_data] kind from [Arod.Ctx.t] fails here naming
   [ctx]. Dropping the floating [@@ portable] from [arod_ctx.mli] fails the
   build one step earlier, inside [arod] itself.

   The markdown is written here rather than read out of an entry body,
   because what is being pinned is that the renderer runs at all from inside
   a portable closure, not what it emits. [test_md_golden] owns the bytes. *)

(* {1 The markdown export}

   {!Bushel.Md.to_markdown} is the only render on this path that reassembles a
   URL, rewriting a video's [watch] segment to [embed] so the export can carry
   an [iframe]. It moved from opam [Uri] to [Uriz] with the rest of the path,
   and nothing else under [dune runtest] reaches it: [test_md_golden]'s three
   renderers are all HTML. So this pins the rewritten bytes as well as the
   modality. Breaking the rewrite, by dropping the [watch] case from the
   segment map, fails the check here. *)

let markdown_export : (string -> string) @ portable =
 fun md -> String.trim (Bushel.Md.to_markdown ~entries md)

let ctx_render : (string -> string) @ portable =
 fun md ->
  let html, sidenotes = Arod.Md.to_html ~ctx md in
  Printf.sprintf "%d %s" (List.length sidenotes) (String.trim html)

(* {1 The page renders}

   A proffer handler is portable and reaches {!Arod_handlers.Render} directly,
   with no closure in {!Arod_handlers.Env} between them. That is what the two
   closures below stand guard over: each captures the context and calls one of
   the real page renders, the collection listing and the single entry. Between
   them they run the whole render path, from the Bushel accessors through the
   markdown export.

   Dropping the [@@ portable] from {!Arod_handlers.Render.listing} or
   {!Arod_handlers.Render.entry}, or putting their closure fields back into
   {!Arod_handlers.Env} and calling through those, stops this file compiling.
   The markdown flavour is asked for because its bytes are short enough to
   assert whole, so a render that crossed but produced nothing would fail
   too. *)

let listing_render : (unit -> string) @ portable =
 fun () -> Arod_handlers.Render.listing ~ctx `Notes `Markdown

let entry_render : (string -> string) @ portable =
 fun slug -> Arod_handlers.Render.entry ~ctx `Note slug `Markdown

let () =
  check "note, its date and its image cross"
    (handler "hello-note" = "Hello Note 2025-01-05 sample.webp [480 960]");
  check "idea, its supervisor contact and that contact's feeds cross"
    (handler "an-idea" = "A Sample Idea <Ada Lovelace/1>");
  check "a miss is a miss" (handler "nothing" = "");
  check "a link in a Smap crosses"
    (link_handler "https://example.com/post" = "A Post");
  check "a feed backlink in a Smap crosses"
    (link_handler "hello-note" = "ada: A Feed Post");
  check "the Bushel markdown resolver, mapper and note interface are portable"
    (render_handler "hello-note"
    = "note /notes/hello-note Hello Note 2025-W01 \
       <p><a href=\"/notes/hello-note\">text</a></p>");
  check "the entry, idea and image interfaces are portable"
    (render_handler "an-idea"
    = "idea /ideas/an-idea MPhil Available /images/ada-640.webp \
       <p><a href=\"/ideas/an-idea\">text</a></p>");
  check "a captured context renders a contact sidenote portably"
    (ctx_render "See [x][@ada].\n"
    = "1 <p>See <span class=\"sidenote-anchor\"><a href=\"#\" \
       class=\"sidenote-ref\" data-sidenote=\"ada\">Ada Lovelace</a></span>.</p>");
  check "a captured context numbers a heading portably"
    (ctx_render "## Head\n\nBody.\n"
    = "0 <h2 id=\"head\" class=\"group relative text-lg font-semibold mt-5 \
       mb-2\"><a href=\"#head\" class=\"heading-number\" aria-label=\"Link to \
       this section\">1</a> Head</h2>\n<p>Body.</p>");
  check "a portable closure renders a listing page through a captured context"
    (listing_render ()
    = String.concat "\n"
        [
          "# Notes";
          "";
          "Notes and blog posts.";
          "";
          "- [Hello Note](http://localhost:8080/notes/hello-note) (2025-01-05)";
          "";
          "---";
          "Canonical: http://localhost:8080/notes";
          "Feeds: [Atom](http://localhost:8080/news.xml), \
           [JSON](http://localhost:8080/feed.json)";
          "License: CC BY 4.0 <https://creativecommons.org/licenses/by/4.0/>";
          "";
        ]);
  check "and an entry page through the same context"
    (entry_render "hello-note"
    = String.concat "\n"
        [
          "# Hello Note";
          "";
          "*2025-01-05 \xe2\x80\x94 note*";
          "";
          "";
          "---";
          "Canonical: http://localhost:8080/notes/hello-note";
          "Type: note";
          "License: CC BY 4.0 <https://creativecommons.org/licenses/by/4.0/>";
          "";
        ]);
  check "a video watch URL is rewritten to an embed URL portably"
    (markdown_export "![%c](:a-video \"A talk\")\n"
    = "<div class=\"video-center\"><iframe title=\"A talk\" width=\"100%\" \
       height=\"315px\" \
       src=\"https://crank.example.org/videos/embed/\
       0b1c2d3e-4f56-4789-8abc-def012345678\" frameborder=\"0\" \
       allowfullscreen sandbox=\"allow-same-origin allow-scripts \
       allow-popups allow-forms\"></iframe></div>");
  Printf.printf "test_payload_kinds: %d checks ok\n" !checks

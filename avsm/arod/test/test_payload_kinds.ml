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
    ~videos:[] ~contacts:[ ada ]
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
   [Cmarkit.Doc.of_string]. The mappers are absent on purpose: they read
   [Cmarkit.Mapper.default] and [bushel_md.mli] records them as
   nonportable. *)

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
       destination, so the HTML backend prints a comment naming an undefined
       label. Without the resolver the reference would have stayed literal
       text and no comment would appear, which is what makes the comment
       proof that the resolver ran. *)
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
  check "the Bushel markdown resolver and the note interface are portable"
    (render_handler "hello-note"
    = "note /notes/hello-note Hello Note 2025-W01 \
       <p>text<!-- Undefined label :hello-note --></p>");
  check "the entry, idea and image interfaces are portable"
    (render_handler "an-idea"
    = "idea /ideas/an-idea MPhil Available /images/ada-640.webp \
       <p>text<!-- Undefined label :an-idea --></p>");
  Printf.printf "test_payload_kinds: %d checks ok\n" !checks

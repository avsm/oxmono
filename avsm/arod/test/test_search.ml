(* The ranking in Arod_search is pinned here over an in-memory index built
   from synthetic entries and links, so each check names the one property
   of the model it holds. The real corpus is not in the repository. *)

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let note ?(tags = []) ?(date = (2024, 2, 3)) ~slug ~title body : Bushel.Note.t =
  {
    Bushel.Note.title;
    date;
    slug;
    body;
    tags;
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

let link ?(title = "") ?(slugs = []) ?(date = (2024, 1, 1)) url
    : Bushel.Link.t =
  {
    Bushel.Link.url;
    date;
    description = "";
    karakeep =
      (if title = "" then None
       else
         Some
           {
             Bushel.Link.remote_url = url;
             id = "k";
             tags = [];
             metadata = [ ("title", title) ];
           });
    bushel = Some { Bushel.Link.slugs; tags = [] };
  }

(* [Arod_search.create_memory] ties the database to [sw]: it closes when
   the switch that opened it finishes. [Arod_search.search] also runs
   inside the Eio scheduler that [Eio_main.run] installs. So [index] takes
   the caller's already-open [sw] rather than opening its own, and every
   check below that touches [t] runs inside one [Eio_main.run]. *)
let index ~sw ?(own_host = "") ~notes ~links () =
  let t = Arod_search.create_memory ~sw () in
  Arod_search.index t ~own_host
    ~contact_name:(fun _ -> None)
    ~entries:(List.map (fun n -> `Note n) notes)
    ~links;
  t

let today = (2026, 8, 23)

let slugs (hits : Arod_search.hit list) =
  List.map (fun (h : Arod_search.hit) -> h.slug) hits

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t =
    index ~sw
      ~notes:
        [
          note ~slug:"old" ~date:(2010, 1, 1) ~title:"Unikernels" "Body.";
          note ~slug:"new" ~date:(2026, 1, 1) ~title:"Unikernels" "Body.";
          note ~slug:"body-only" ~title:"Other" "About unikernels here.";
        ]
      ~links:
        [
          link ~title:"Unikernel blog" ~slugs:[ "old" ]
            "https://x.org/unikernel";
          link ~title:"Unikernel blog" ~slugs:[ "old"; "new" ]
            "https://www.x.org/unikernel/";
          link ~title:"Unikernel mirror" ~slugs:[ "old" ]
            "https://X.org/unikernel#";
          link ~title:"Unikernels local" ~slugs:[ "old" ]
            "https://example.com/papers/u.pdf";
          link ~title:"Unikernels twice" ~slugs:[ "old"; "new" ]
            "https://y.org/a";
          link ~title:"Unikernels once" ~slugs:[ "old" ] "https://z.org/a";
        ]
      ~own_host:"example.com" ()
  in
  let r = Arod_search.search t ~today "unikernel" in
  check "a title match outranks a body match"
    (List.mem "body-only" (slugs r.work)
    && List.nth (slugs r.work) 2 = "body-only");
  check "freshness breaks the tie between equal title matches"
    (slugs r.work |> List.filteri (fun i _ -> i < 2) = [ "new"; "old" ]);
  check "links never appear in the work tier"
    (List.for_all (fun (h : Arod_search.hit) -> h.kind <> "link") r.work);
  check "URLs differing by scheme, www, trailing slash or hash are one link"
    (List.length
       (List.filter
          (fun (h : Arod_search.hit) ->
            Arod_search.normalise_url h.url = "x.org/unikernel")
          r.links)
    = 1);
  check "a link on the site's own host is dropped"
    (not (List.exists (fun (h : Arod_search.hit) ->
              h.url = "https://example.com/papers/u.pdf") r.links));
  check "a link cited twice outranks the same title cited once"
    (let ys = List.filter (fun (h : Arod_search.hit) ->
         String.starts_with ~prefix:"https://y.org" h.url
         || String.starts_with ~prefix:"https://z.org" h.url) r.links in
     List.map (fun (h : Arod_search.hit) -> h.url) ys
     = [ "https://y.org/a"; "https://z.org/a" ]);
  check "totals count matches before the limit"
    (r.work_total = 3 && r.links_total = 3);
  check "the query words are returned lowercased without the prefix star"
    (r.terms = [ "unikernel" ])

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t =
    index ~sw
      ~notes:
        [
          note ~slug:"a" ~date:(2020, 1, 1) ~title:"A" "x";
          note ~slug:"b" ~date:(2021, 1, 1) ~title:"B" "x";
        ]
      ~links:[] ()
  in
  let r = Arod_search.search t ~today ~limit:1 "x" in
  check "limit caps the work list" (List.length r.work = 1);
  check "but not the total" (r.work_total = 2);
  let r = Arod_search.search t ~today "kind:note" in
  check "a filter-only query browses by date"
    (slugs r.work = [ "b"; "a" ]);
  check "an empty query is empty"
    (Arod_search.search t ~today "" = Arod_search.empty)

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t =
    index ~sw
      ~notes:[]
      ~links:
        [
          link ~title:"Duplicate" ~slugs:[ "p" ] "https://d.org/a";
          link ~title:"Duplicate" ~slugs:[ "p" ]
            "https://www.d.org/a/";
          link ~title:"Own" ~slugs:[ "p" ] "https://example.com/x";
        ]
      ~own_host:"example.com" ()
  in
  let r = Arod_search.search t ~today "kind:link" in
  check "a browse dedupes links and drops the site's own host"
    (List.length r.links = 1 && r.links_total = 1
    && Arod_search.normalise_url (List.hd r.links).url = "d.org/a")

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t =
    index ~sw
      ~notes:[]
      ~links:
        [
          link ~title:"Same" ~slugs:[ "p" ] "https://h.org/a";
          link ~title:"Same" ~slugs:[ "p"; "q" ] "https://h.org/b";
        ]
      ()
  in
  let r = Arod_search.search t ~today "same" in
  check "two URLs on one host sharing a title are one link, kept by score"
    (let hits =
       List.filter
         (fun (h : Arod_search.hit) -> Arod_search.host_of_url h.url = "h.org")
         r.links
     in
     List.map (fun (h : Arod_search.hit) -> h.url) hits
     = [ "https://h.org/b" ])

let () =
  check "kind priors favour projects over ideas"
    (Arod_search.kind_prior "project" > Arod_search.kind_prior "paper"
    && Arod_search.kind_prior "paper" > Arod_search.kind_prior "idea");
  check "freshness is 1.25 this month and 1.0 after eight years"
    (Arod_search.freshness ~today "2026-08-01" = 1.25
    && Arod_search.freshness ~today "2018-01-01" = 1.0);
  check "freshness does not exceed 1.25 for a future date"
    (Arod_search.freshness ~today "2030-01-01" = 1.25);
  check "the citation bonus is 1 for a single citation"
    (Arod_search.citation_bonus 1 = 1.0
    && Arod_search.citation_bonus 2 > 1.0);
  check "host_of_url drops scheme and www"
    (Arod_search.host_of_url "https://www.Example.com/a/b" = "example.com")

let project ~slug ~title ~start : Bushel.Project.t =
  { Bushel.Project.slug; title; start; finish = None; tags = []; ideas = "";
    body = "Body."; social = None }

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t = Arod_search.create_memory ~sw () in
  Arod_search.index t ~own_host:""
    ~contact_name:(fun _ -> None)
    ~entries:
      [
        `Project (project ~slug:"ocamllabs" ~title:"OCaml Labs" ~start:2012);
        `Note (note ~slug:"n1" ~date:(2020, 1, 1) ~tags:[ "ocaml"; "eio" ]
                 ~title:"OCaml one" "x");
        `Note (note ~slug:"n2" ~date:(2021, 1, 1) ~tags:[ "ocaml" ]
                 ~title:"OCaml two" "x");
        `Note (note ~slug:"n3" ~date:(2022, 6, 1) ~tags:[ "ocaml-labs" ]
                 ~title:"Other" "x");
      ]
    ~links:[ link ~title:"OCaml site" ~slugs:[ "n1" ] "https://ocaml.org" ];
  let r = Arod_search.search t ~today "ocaml" in
  let gotos = List.map (fun (g : Arod_search.goto) ->
      (g.goto_kind, g.label, g.url, g.detail)) r.goto in
  check "a project whose title starts with the word is a go-to hit"
    (List.mem (`Project, "OCaml Labs", "/projects/ocamllabs", "2012 project")
       gotos);
  check "a tag is a go-to hit with its entry count, most used first"
    (List.filter (fun (k, _, _, _) -> k = `Tag) gotos
     = [ (`Tag, "ocaml", "/#tag=ocaml", "2 entries");
         (`Tag, "ocaml-labs", "/#tag=ocaml-labs", "1 entry") ]);
  check "projects come before tags"
    (match gotos with (`Project, _, _, _) :: _ -> true | _ -> false);
  (* n3 matches through its tag, since the tags column tokenises
     "ocaml-labs" into two words. A project indexes at its start year. *)
  check "kinds count work matches per kind, by name"
    (r.kinds = [ ("note", 3); ("project", 1) ]);
  check "years count work matches ascending"
    (r.years = [ (2012, 1); (2020, 1); (2021, 1); (2022, 1) ]);
  check "tags are the most used among work matches, then by name"
    (r.tags = [ ("ocaml", 2); ("eio", 1); ("ocaml-labs", 1) ]);
  let r = Arod_search.search t ~today "pap" in
  check "a section matches on a prefix of its name"
    (List.exists (fun (g : Arod_search.goto) ->
         g.goto_kind = `Section && g.url = "/papers") r.goto);
  let r = Arod_search.search t ~today "lab" in
  check "a project matches on a prefix of any title word"
    (List.exists (fun (g : Arod_search.goto) ->
         g.goto_kind = `Project) r.goto)

let () = Printf.printf "test_search: %d checks ok\n" !checks

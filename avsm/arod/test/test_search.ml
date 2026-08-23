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

let index ?own_host ~notes ~links () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t = Arod_search.create_memory ~sw () in
  Arod_search.index t ?own_host
    ~contact_name:(fun _ -> None)
    ~entries:(List.map (fun n -> `Note n) notes)
    ~links;
  t

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let t = Arod_search.create_memory ~sw () in
  Arod_search.index t ~own_host:""
    ~contact_name:(fun _ -> None)
    ~entries:[ `Note (note ~slug:"a" ~title:"Unikernels" "A body.") ]
    ~links:[ link ~title:"Unikernel blog" ~slugs:[ "a" ] "https://x.org/u" ];
  let results = Arod_search.search t "unikernel" in
  check "an indexed note is found"
    (List.exists (fun (r : Arod_search.result) -> r.slug = "a") results);
  check "so is an indexed link"
    (List.exists
       (fun (r : Arod_search.result) -> r.slug = "https://x.org/u")
       results)

let () = Printf.printf "test_search: %d checks ok\n" !checks

(* The loader classifies each markdown link as a bushel slug, a contact, a
   tag or an external URL. The classification is one if-chain, so an inner
   [if] without an [else] silently swallows the branches after it and the
   external tier of the graph comes out empty. These checks pin every branch
   of the chain, including a contact handle that no contact matches. *)

module G = Bushel.Link_graph

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let note ~slug ~body =
  {
    Bushel.Note.title = slug;
    date = (2026, 1, 1);
    slug;
    body;
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

let body =
  "A [slug](:target) link, an [absent contact](@nobody) link, a \
   [tag](#tag/ocaml) link, an [external](https://example.com/a) link and \
   an [insecure](http://example.com/b) one.\n"

let graph =
  let notes = [ note ~slug:"source" ~body; note ~slug:"target" ~body:"" ] in
  let entries =
    Bushel.Entry.v ~papers:[] ~notes ~projects:[] ~ideas:[] ~videos:[]
      ~contacts:[] ~data_dir:"." ()
  in
  Bushel_eio.Bushel_loader.build_link_graph entries

let externals = G.all_external_links graph

let () =
  check "external links are collected"
    (List.length externals = 2);
  check "https link is present"
    (List.exists (fun (l : G.external_link) -> l.url = "https://example.com/a")
       externals);
  check "http link is present"
    (List.exists (fun (l : G.external_link) -> l.url = "http://example.com/b")
       externals);
  check "external link records its source"
    (List.for_all (fun (l : G.external_link) -> l.source = "source") externals);
  check "external link records its domain"
    (List.for_all
       (fun (l : G.external_link) -> l.domain = "example.com") externals);
  check "a slug link is internal" (G.outbound graph "source" = [ "target" ]);
  check "a tag link is not external"
    (not
       (List.exists
          (fun (l : G.external_link) -> l.url = "#tag/ocaml") externals));
  Printf.printf "test_link_graph: %d checks passed\n" !checks

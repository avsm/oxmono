module C = Sortal_schema.Contact
module I = Retrieval.Index
module E = Retrieval.Embedding
module J = Retrieval.Json

type edge = {
  source : string;
  target : string;
  relation : string;
  evidence : string;
}

type contact = { id : string; value : C.t }
type candidate = { contact : contact; matched_on : string; exact : bool }

type snapshot = {
  contacts : contact list;
  edges : edge list;
  adjacency : edge list Bushel.Smap.t;
  exact : candidate list Bushel.Smap.t;
  prefixes : (contact * string list) list;
  generation : string;
}

type t = { index : I.t; mutable snapshot : snapshot }

let create index =
  {
    index;
    snapshot =
      {
        contacts = [];
        edges = [];
        adjacency = Bushel.Smap.empty;
        exact = Bushel.Smap.empty;
        prefixes = [];
        generation = "";
      };
  }

(* Preserve the input order within each group, including duplicate evidence. *)
let group pairs =
  let table = Hashtbl.create 32 in
  List.iter
    (fun (key, value) ->
      let old = Option.value ~default:[] (Hashtbl.find_opt table key) in
      Hashtbl.replace table key (value :: old))
    pairs;
  Hashtbl.fold (fun key values acc -> (key, List.rev values) :: acc) table []
  |> Bushel.Smap.of_list

let lookup key index =
  match Bushel.Smap.find_or_null key index with Null -> [] | This xs -> xs

let key parts = J.encode (Jsont.list Jsont.string) parts
let entry_id slug = key [ "bushel"; slug ]

let contact_id c =
  match Sortal.Store.uid c with
  | Some uid -> key [ "sortal"; uid ]
  | None -> invalid_arg ("Contact has no durable UID: " ^ C.handle c)

let feed_id feed id = key [ "feed"; feed; id ]
let url_id url = key [ "url"; url ]
let hash = E.hash

let html_links html =
  let links = ref [] in
  Markup.string html |> Markup.parse_html |> Markup.signals
  |> Markup.iter (function
       | `Start_element ((_, "a"), attrs) ->
           Option.iter
             (fun url -> links := url :: !links)
             (List.assoc_opt ("", "href") attrs)
       | _ -> ());
  List.sort_uniq String.compare !links

let plain_html html =
  let b = Buffer.create 256 in
  Markup.string html |> Markup.parse_html |> Markup.signals
  |> Markup.iter (function
       | `Text texts -> List.iter (fun text -> Buffer.add_string b text) texts
       | `End_element -> Buffer.add_char b '\n'
       | _ -> ());
  Buffer.contents b

let edges t = t.snapshot.edges
let contacts t = t.snapshot.contacts
let generation t = t.snapshot.generation
let get t = I.get t.index
let find_url t = I.find_url t.index
let related t id = lookup id t.snapshot.adjacency
let normal s = String.lowercase_ascii (String.trim s)

let contact_indexes contacts =
  let exact =
    List.concat_map
      (fun contact ->
        let c = contact.value in
        let fields =
          (("handle", C.handle c) :: List.map (fun s -> ("name", s)) (C.names c))
          @ List.map (fun s -> ("email", s)) (C.emails c)
          @ List.concat_map
              (fun a ->
                [
                  ("account", C.Account.handle a);
                  ("account_url", C.Account.url a);
                ])
              (C.accounts c)
        in
        let seen = Hashtbl.create 8 in
        List.filter_map
          (fun (matched_on, value) ->
            let value = normal value in
            if Hashtbl.mem seen value then None
            else (
              Hashtbl.add seen value ();
              Some (value, { contact; matched_on; exact = true })))
          fields)
      contacts
    |> group
  in
  let prefixes =
    List.map
      (fun contact ->
        let names = List.map normal (C.names contact.value) in
        (contact, names @ List.concat_map (String.split_on_char ' ') names))
      contacts
  in
  (exact, prefixes)

let resolve t query =
  let q = normal query in
  if q = "" then invalid_arg "A contact query must not be empty";
  match lookup q t.snapshot.exact with
  | _ :: _ as exact -> exact
  | [] ->
      List.filter_map
        (fun (contact, prefixes) ->
          if List.exists (String.starts_with ~prefix:q) prefixes then
            Some { contact; matched_on = "name_prefix"; exact = false }
          else None)
        t.snapshot.prefixes

let refresh ?(base_url = "") ?(links = []) t ~entries ~contacts ~feeds =
  let base_url =
    if String.ends_with ~suffix:"/" base_url then
      String.sub base_url 0 (String.length base_url - 1)
    else base_url
  in
  let contacts =
    List.map (fun value -> { id = contact_id value; value }) contacts
  in
  let handles =
    List.rev_map (fun c -> (C.handle c.value, c)) contacts
    |> Bushel.Smap.of_list
  in
  let by_handle handle = Bushel.Smap.find_opt handle handles in
  let contact_name handle =
    Option.map (fun c -> C.name c.value) (by_handle handle)
  in
  let documents = ref [] and edges = ref [] in
  let add_edge source target relation evidence =
    edges := { source; target; relation; evidence } :: !edges
  in
  let add_document ~id ~kind ~title ~text ~url ~date ~facets =
    let revision =
      hash
        (key
           [
             kind;
             title;
             text;
             url;
             date;
             key
               (List.map
                  (fun (k, v) -> key [ k; v ])
                  (List.sort_uniq compare facets));
           ])
    in
    documents :=
      I.{ id; revision; kind; title; text; url; date; facets } :: !documents
  in
  List.iter
    (fun c ->
      let text = J.encode C.json_t c.value in
      add_document ~id:c.id ~kind:"contact" ~title:(C.name c.value) ~text
        ~url:(Option.value ~default:"" (C.best_url c.value))
        ~date:""
        ~facets:[ ("contact", C.handle c.value) ])
    contacts;
  let graph = Bushel.Entry.graph entries in
  let internal_links = Bushel.Link_graph.all_internal_links graph in
  let external_links = Bushel.Link_graph.all_external_links graph in
  let internal_by_source =
    List.map (fun (l : Bushel.Link_graph.internal_link) -> (l.source, l))
      internal_links
    |> group
  in
  let external_by_url =
    List.map (fun (l : Bushel.Link_graph.external_link) -> (l.url, l))
      external_links
    |> group
  in
  let all_entries = Bushel.Entry.all_entries entries in
  let entries_by_url =
    List.map (fun ent -> (base_url ^ Bushel.Entry.site_url ent, ent))
      all_entries
    |> group
  in
  List.iter
    (fun (l : Bushel.Link_graph.internal_link) ->
      let target =
        match l.target_type with
        | `Contact -> Option.map (fun c -> c.id) (by_handle l.target)
        | _ -> Some (entry_id l.target)
      in
      Option.iter
        (fun target ->
          add_edge (entry_id l.source) target
            (if l.target_type = `Contact then "mentions_contact"
             else "links_entry")
            "bushel_link_graph")
        target)
    internal_links;
  List.iter
    (fun (l : Bushel.Link_graph.external_link) ->
      add_edge (entry_id l.source) (url_id l.url) "links_url" l.url)
    external_links;
  List.iter
    (fun ent ->
      let slug = Bushel.Entry.slug ent in
      let id = entry_id slug in
      let markdown =
        String.concat "\n\n"
          (Bushel.Entry.body ent
          :: List.filter_map Fun.id
               [ Bushel.Entry.synopsis ent; Bushel.Entry.sidebar ent ])
      in
      let text = Bushel.Md.plain_text_of_markdown ~contact_name markdown in
      let y, m, d = Bushel.Entry.date ent in
      let tags = Bushel.Entry.tags_of_ent ent in
      let metadata_contacts =
        match ent with
        | `Paper paper ->
            List.filter_map
              (fun name ->
                let matches =
                  List.filter
                    (fun c ->
                      List.exists
                        (fun alias -> normal alias = normal name)
                        (C.names c.value))
                    contacts
                in
                match matches with
                | [ c ] -> Some (c, "authored_by", "paper_author:" ^ name)
                | _ -> None)
              (Bushel.Paper.authors paper)
        | `Idea idea ->
            List.filter_map
              (fun handle ->
                Option.map
                  (fun c -> (c, "supervised_by", "idea_supervisor"))
                  (by_handle handle))
              (Bushel.Idea.supervisor_handles idea)
        | _ -> []
      in
      List.iter
        (fun (c, relation, evidence) -> add_edge id c.id relation evidence)
        metadata_contacts;
      let metadata_facets =
        List.map
          (fun (c, _, _) -> ("contact", C.handle c.value))
          metadata_contacts
      in
      let facets =
        List.concat_map
          (function
            | `Contact handle ->
                Option.iter
                  (fun c -> add_edge id c.id "mentions_contact" "contact_tag")
                  (by_handle handle);
                [ ("contact", handle) ]
            | `Slug slug ->
                add_edge id (entry_id slug) "tagged_entry" "slug_tag";
                [ ("project", slug) ]
            | t -> [ ("tag", Bushel.Tags.to_raw_string t) ])
          tags
      in
      let linked =
        lookup slug internal_by_source
        |> List.filter_map (fun (l : Bushel.Link_graph.internal_link) ->
                 match l.target_type with
                 | `Contact -> Some ("contact", l.target)
                 | `Project -> Some ("project", l.target)
                 | _ -> None)
      in
      add_document ~id
        ~kind:(Bushel.Entry.to_type_string ent)
        ~title:(Bushel.Entry.title ent) ~text
        ~url:(base_url ^ Bushel.Entry.site_url ent)
        ~date:(Printf.sprintf "%04d-%02d-%02d" y m d)
        ~facets:(metadata_facets @ facets @ linked))
    all_entries;
  (* Feed ownership is an association, never an authorship assertion. *)
  List.iter
    (fun (handle, (entry : Sortal_feed.Entry.t), annotations) ->
      let id = feed_id entry.source_feed entry.id in
      Option.iter
        (fun c -> add_edge id c.id "associated_contact" entry.source_feed)
        (by_handle handle);
      let title = Option.value ~default:"Untitled feed item" entry.title in
      let text =
        plain_html
          (Option.value
             ~default:(Option.value ~default:"" entry.summary)
             entry.content)
      in
      let url = Option.fold ~none:"" ~some:Uriz.to_string entry.url in
      if url <> "" then
        add_edge id (url_id url) "published_at" entry.source_feed;
      List.iter
        (fun slug ->
          add_edge id (entry_id slug) "annotated_entry" "sortal_feed_annotation")
        annotations;
      List.iter
        (fun (l : Bushel.Link_graph.external_link) ->
          add_edge (entry_id l.source) id "links_feed_item" l.url)
        (if url = "" then [] else lookup url external_by_url);
      html_links (Option.value ~default:"" entry.content)
      |> List.iter (fun link ->
             List.iter
               (fun ent ->
                 add_edge id (entry_id (Bushel.Entry.slug ent))
                   "links_entry" link)
               (lookup link entries_by_url));
      let date =
        Option.fold ~none:""
          ~some:(fun d ->
            let y, m, d = Ptime.to_date d in
            Printf.sprintf "%04d-%02d-%02d" y m d)
          entry.date
      in
      add_document ~id ~kind:"feed" ~title ~text ~url ~date
        ~facets:
          ([ ("contact", handle); ("feed", entry.source_feed) ]
          @ List.map (fun slug -> ("project", slug)) annotations))
    feeds;
  List.iter
    (fun (link : Bushel.Link.t) ->
      let id = url_id link.url in
      let y, m, d = link.date in
      let tags =
        Option.fold ~none:[]
          ~some:(fun (k : Bushel.Link.karakeep_data) -> k.tags)
          link.karakeep
        @ Option.fold ~none:[]
            ~some:(fun (b : Bushel.Link.bushel_data) -> b.tags)
            link.bushel
      in
      Option.iter
        (fun (b : Bushel.Link.bushel_data) ->
          List.iter
            (fun slug -> add_edge (entry_id slug) id "links_url" "links.yml")
            b.slugs)
        link.bushel;
      add_document ~id ~kind:"link" ~title:link.description
        ~text:link.description ~url:link.url
        ~date:(Printf.sprintf "%04d-%02d-%02d" y m d)
        ~facets:(List.map (fun tag -> ("tag", tag)) tags))
    links;
  (* Shared subscriptions may appear under several contacts. Union their facets. *)
  let table = Hashtbl.create 128 in
  List.iter
    (fun (d : I.document) ->
      match Hashtbl.find_opt table d.id with
      | None -> Hashtbl.add table d.id d
      | Some old
        when old.text = d.text && old.title = d.title && old.kind = "feed" ->
          let facets = List.sort_uniq compare (old.facets @ d.facets) in
          Hashtbl.replace table d.id { d with facets }
      | Some _ -> invalid_arg ("Duplicate knowledge identity: " ^ d.id))
    !documents;
  let documents =
    Hashtbl.to_seq_values table
    |> List.of_seq
    |> List.sort (fun (a : I.document) b -> String.compare a.id b.id)
  in
  let documents =
    List.map
      (fun (d : I.document) ->
        {
          d with
          revision =
            hash
              (key
                 [
                   d.kind;
                   d.title;
                   d.text;
                   d.url;
                   d.date;
                   key
                     (List.map
                        (fun (k, v) -> key [ k; v ])
                        (List.sort_uniq compare d.facets));
                 ]);
        })
      documents
  in
  let edges = List.sort_uniq compare !edges in
  let adjacency =
    List.concat_map
      (fun e ->
        if e.source = e.target then [ (e.source, e) ]
        else [ (e.source, e); (e.target, e) ])
      edges
    |> group
  in
  let exact, prefixes = contact_indexes contacts in
  let generation =
    hash
      (key
         (List.map (fun (d : I.document) -> key [ d.id; d.revision ]) documents
         @ List.map
             (fun e -> key [ e.source; e.target; e.relation; e.evidence ])
             edges))
  in
  let snapshot = { contacts; edges; adjacency; exact; prefixes; generation } in
  I.replace t.index documents;
  t.snapshot <- snapshot

let load ?(base_url = "") t ~fs ~bushel_dir ~sortal_dir =
  List.iter
    (fun dir ->
      if (Eio.Path.stat ~follow:true Eio.Path.(fs / dir)).kind <> `Directory
      then invalid_arg ("Knowledge source is not a directory: " ^ dir))
    [ bushel_dir; sortal_dir ];
  let store = Sortal.Store.create_at fs sortal_dir in
  let contacts = Sortal.Store.list_readonly store in
  let entries =
    Bushel_eio.Bushel_loader.load ~strict:true ~contacts fs bushel_dir
  in
  let feed_store = Sortal_feed.Store.create (Sortal.Store.data_dir store) in
  let feeds =
    List.concat_map
      (fun c ->
        let handle = C.handle c in
        List.concat_map
          (fun feed ->
            let feed =
              Sortal_schema.Feed.set_feed_type feed
                (Sortal_feed.Store.effective_type feed_store handle feed)
            in
            let annotations =
              Sortal_feed.Annotations.load ~strict:true
                (Sortal_feed.Store.annotations_file feed_store handle feed)
            in
            Sortal_feed.Store.entries_of_feed ~strict:true feed_store ~handle
              feed
            |> List.map (fun entry ->
                   let slugs =
                     Option.fold ~none:[]
                       ~some:(fun url ->
                         Sortal_feed.Annotations.slugs_for_url annotations
                           (Uriz.to_string url))
                       entry.Sortal_feed.Entry.url
                   in
                   (handle, entry, slugs)))
          (C.feeds c))
      contacts
  in
  let links =
    Bushel.Link.load_links_file ~strict:true
      (Filename.concat bushel_dir "links.yml")
  in
  refresh ~base_url ~links t ~entries ~contacts ~feeds

let index t provider ~check ~max_batches =
  E.index (I.backend t.index I.all) provider ~check ~max_batches

let search t ?provider ~check ~mode ~filter query =
  let backend = I.backend t.index filter in
  match (mode, provider) with
  | "lexical", _ -> backend.lexical query
  | "semantic", Some p -> E.search backend p ~check query |> List.map fst
  | "hybrid", Some p -> E.hybrid backend p ~check ~lexical:query ~semantic:query
  | ("semantic" | "hybrid"), None ->
      invalid_arg "No embedding provider configured. Use lexical search."
  | _ -> invalid_arg "Search mode must be lexical, semantic or hybrid"

let coverage t provider =
  let backend = I.backend t.index I.all in
  let index = E.index_name backend provider in
  let documents = backend.documents () in
  let ready =
    List.filter
      (fun document ->
        backend.current ~index ~document
          ~input_hash:(E.hash (backend.passage document)))
      documents
  in
  (List.length ready, List.length documents)

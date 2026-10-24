module K = Bushel_knowledge
module I = Retrieval.Index
module C = Sortal_schema.Contact

let check s b = if not b then failwith s
let noop () = ()

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

let contact uid handle names =
  C.make ~handle ~names () |> fun c ->
  C.with_source c
    (Some
       ("BEGIN:VCARD\r\nVERSION:4.0\r\nUID:" ^ uid
      ^ "\r\nFN:Test\r\nEND:VCARD\r\n"))

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let a = contact "id-a" "same" [ "Alex Smith"; "Alexander Smith" ] in
  let b = contact "id-b" "other" [ "Alex Smith" ] in
  let contacts = [ a; b ] in
  let notes =
    [
      note ~slug:"same" ~body:"A note [self](:same)";
      note ~slug:"source"
        ~body:
          "[person](@same) [note](:same) [article](https://example.org/article)";
    ]
  in
  let entries =
    Bushel.Entry.v ~papers:[] ~notes ~projects:[] ~ideas:[] ~videos:[] ~contacts
      ~data_dir:"." ()
  in
  let entries =
    Bushel.Entry.with_graph entries
      (Bushel_eio.Bushel_loader.build_link_graph entries)
  in
  check "graph preserves contact and slug with same spelling"
    (List.length
       (Bushel.Link_graph.all_internal_links (Bushel.Entry.graph entries))
    = 3);
  let feed =
    Sortal_feed.Entry.
      {
        id = "one";
        title = Some "Feed story";
        date = None;
        summary = None;
        content =
          Some
            "<p>A story <a href='https://mysite/notes/same'>about my \
             note</a></p>";
        url =
          Some
            (match Uriz.of_string "https://example.org/article" with
            | This u -> u
            | Null -> assert false);
        source_feed = "https://example.org/feed";
        source_type = Sortal_schema.Feed.Atom;
      }
  in
  let db = I.memory ~sw () in
  let corpus = K.create db in
  K.refresh ~base_url:"https://mysite" corpus ~entries ~contacts
    ~feeds:[ ("same", feed, [ "same" ]) ];
  let check_adjacency () =
    List.iter
      (fun id ->
        check "adjacency preserves edge order and self-link multiplicity"
          (K.related corpus id
          = List.filter
              (fun (e : K.edge) -> e.source = id || e.target = id)
              (K.edges corpus)))
      [
        K.entry_id "same";
        K.entry_id "source";
        K.contact_id a;
        K.feed_id feed.source_feed feed.id;
        "missing";
      ]
  in
  check_adjacency ();
  check "contact exact fields retain precedence"
    (List.map
       (fun (c : K.candidate) -> c.matched_on)
       (K.resolve corpus " SAME ")
    = [ "handle" ]);
  check "contact prefix token lookup preserves ambiguity"
    (List.length (K.resolve corpus "smi") = 2);
  let candidates = K.resolve corpus "Alex Smith" in
  check "same-name people remain ambiguous" (List.length candidates = 2);
  check "alias resolves" (List.length (K.resolve corpus "Alexander Smith") = 1);
  let es = K.related corpus (K.entry_id "source") in
  check "typed contact identity"
    (List.exists (fun (e : K.edge) -> e.target = K.contact_id a) es);
  check "typed entry identity"
    (List.exists (fun (e : K.edge) -> e.target = K.entry_id "same") es);
  check "UID survives handle rename"
    (K.contact_id a = K.contact_id (contact "id-a" "renamed" [ "Alex Smith" ]));
  let feed_id = K.feed_id feed.source_feed feed.id in
  let es = K.related corpus feed_id in
  check "feed association is not authorship"
    (List.exists (fun (e : K.edge) -> e.relation = "associated_contact") es
    && not (List.exists (fun (e : K.edge) -> e.relation = "author") es));
  check "feed annotation has provenance"
    (List.exists
       (fun (e : K.edge) ->
         e.relation = "annotated_entry" && e.evidence = "sortal_feed_annotation")
       es);
  check "note links feed item"
    (List.exists (fun (e : K.edge) -> e.relation = "links_feed_item") es);
  let tools = Bushel_agent.create ~corpus ~max_bytes:2048 () in
  let invoke name args =
    match Bushel_agent.invoke tools ~check:noop name args with
    | Ok s -> s
    | Error e -> failwith e
  in
  check "fractional offsets rejected"
    (Result.is_error
       (Bushel_agent.invoke tools ~check:noop "contacts_resolve"
          {|{"query":"Alex","offset":0.5}|}));
  check "invalid dates rejected"
    (Result.is_error
       (Bushel_agent.invoke tools ~check:noop "knowledge_search"
          {|{"query":"story","since":"2026-02-30"}|}));
  let result = invoke "contacts_resolve" {|{"query":"Alex Smith"}|} in
  let status = Retrieval.Json.decode (Jsont.mem "status" Jsont.string) result in
  check "tool reports ambiguity" (status = "ambiguous");
  check "semantic mode requires provider"
    (match
       Bushel_agent.invoke tools ~check:noop "knowledge_search"
         {|{"query":"story","mode":"semantic"}|}
     with
    | Error _ -> true
    | _ -> false);
  check "strict arguments"
    (match
       Bushel_agent.invoke tools ~check:noop "contacts_get"
         {|{"id":"x","path":"/etc/passwd"}|}
     with
    | Error _ -> true
    | _ -> false);
  let result =
    invoke "knowledge_search"
      {|{"query":"story","kind":"feed","contact":"same"}|}
  in
  check "bounded valid JSON"
    (String.length result <= 2048
    && Result.is_ok (Jsont_bytesrw.decode_string Jsont.json result));
  let result =
    invoke "feeds_context" {|{"url":"https://example.org/article"}|}
  in
  check "article URL finds feed context"
    (Retrieval.Json.decode
       (Jsont.mem "source_ids" (Jsont.list Jsont.string))
       result
    <> []);
  let generation = K.generation corpus in
  K.refresh ~base_url:"https://mysite" corpus ~entries
    ~contacts:(List.rev contacts)
    ~feeds:[ ("same", feed, [ "same" ]) ];
  check "generation independent of contact order"
    (K.generation corpus = generation);
  K.refresh corpus ~entries ~contacts ~feeds:[];
  check "removed feeds disappear" (K.get corpus feed_id = None);
  check "removed feed adjacency disappears" (K.related corpus feed_id = []);
  check_adjacency ();
  let previous = K.generation corpus and previous_edges = K.edges corpus in
  check "failed refresh leaves graph snapshot intact"
    (try
       K.refresh corpus ~entries ~contacts:[ a; a ] ~feeds:[];
       false
     with Invalid_argument _ ->
       K.generation corpus = previous && K.edges corpus = previous_edges);
  check_adjacency ();
  let path = Filename.temp_file "knowledge-malformed" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  let notes_dir = Filename.concat path "notes" in
  Unix.mkdir notes_dir 0o700;
  let file = Filename.concat notes_dir "broken.md" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove file;
      Unix.rmdir notes_dir;
      Unix.rmdir path)
    (fun () ->
      Out_channel.with_open_bin file (fun oc ->
          output_string oc "---\ntitle: [broken\n---\nbody");
      let previous = K.generation corpus in
      check "malformed note aborts refresh without replacing snapshot"
        (try
           K.load corpus ~fs:(Eio.Stdenv.fs env) ~bushel_dir:path
             ~sortal_dir:path;
           false
         with Failure _ -> K.generation corpus = previous));
  let published =
    {
      (note ~slug:"published" ~body:"bushelneedle latest post") with
      date = (2022, 1, 1);
    }
  in
  let updated =
    {
      (note ~slug:"updated" ~body:"bushelneedle older post") with
      date = (2020, 1, 1);
      updated = Some (2025, 1, 1);
    }
  in
  let draft =
    {
      (note ~slug:"draft" ~body:"bushelneedle draft") with
      draft = true;
      date = (2024, 1, 1);
    }
  in
  let index_page =
    {
      (note ~slug:"index" ~body:"bushelneedle index") with
      index_page = true;
      date = (2024, 1, 1);
    }
  in
  let future =
    {
      (note ~slug:"future" ~body:"bushelneedle future") with
      date = (9999, 1, 1);
    }
  in
  let make_entries published =
    Bushel.Entry.v ~papers:[]
      ~notes:[ published; updated; draft; index_page; future ]
      ~projects:[] ~ideas:[] ~videos:[] ~contacts:[] ~data_dir:"." ()
  in
  let feeds =
    List.init 120 (fun i ->
        ( "same",
          {
            feed with
            id = string_of_int i;
            title = Some "bushelneedle syndicated story";
            content = Some "bushelneedle other person's post";
          },
          [] ))
  in
  let refresh_posts published =
    K.refresh ~base_url:"https://mysite" corpus
      ~entries:(make_entries published) ~contacts:[] ~feeds
  in
  refresh_posts published;
  let latest = K.latest_posts corpus ~until:"2026-01-01" in
  check "latest posts use original publication dates and exclude non-posts"
    (List.map (fun (d : I.document) -> (d.id, d.date)) latest
    = [
        (K.entry_id "published", "2022-01-01");
        (K.entry_id "updated", "2020-01-01");
      ]);
  let ids result =
    Retrieval.Json.decode
      (Jsont.mem "items" (Jsont.list (Jsont.mem "id" Jsont.string)))
      result
  in
  let result = invoke "bushel_latest_posts" "{}" in
  check "latest tool needs no model and excludes future and draft posts"
    (ids result = [ K.entry_id "published"; K.entry_id "updated" ]);
  check "latest post pagination retains publication order"
    (ids (invoke "bushel_latest_posts" {|{"offset":1}|})
    = [ K.entry_id "updated" ]);
  check "Bushel scope is applied before the 100-result lexical limit"
    (List.length (ids (invoke "bushel_search" {|{"query":"bushelneedle"}|})) = 5);
  check "Bushel tool cannot search feed kinds"
    (Result.is_error
       (Bushel_agent.invoke tools ~check:noop "bushel_search"
          {|{"query":"bushelneedle","kind":"feed"}|}));
  check "latest tool rejects unsupported search arguments"
    (Result.is_error
       (Bushel_agent.invoke tools ~check:noop "bushel_latest_posts"
          {|{"query":"latest"}|}));
  let embedded = ref [] in
  let provider =
    Retrieval.Embedding.
      {
        model = "bushel-scope";
        dimensions = 2;
        max_batch = 128;
        embed =
          (fun ~task ~check inputs ->
            check ();
            embedded := (task, inputs) :: !embedded;
            List.map (fun _ -> [ 1.; 0. ]) inputs);
      }
  in
  let semantic = Bushel_agent.create ~corpus ~provider ~max_bytes:65536 () in
  let result =
    Bushel_agent.invoke semantic ~check:noop "bushel_search"
      {|{"query":"bushelneedle","mode":"semantic"}|}
    |> Result.get_ok
  in
  check "semantic Bushel search excludes feeds before ranking"
    (List.length (ids result) = 5);
  check "semantic Bushel search indexes only matching local site entries"
    (K.coverage corpus provider = (5, 125));
  let before = List.length !embedded in
  let generation = K.generation corpus in
  refresh_posts { published with draft = true };
  check "publication visibility changes invalidate pagination generation"
    (K.generation corpus <> generation);
  check "publication metadata does not invalidate saved embedding vectors"
    (K.coverage corpus provider = (5, 125));
  check "changed draft status is reflected without inference"
    (ids (invoke "bushel_latest_posts" "{}") = [ K.entry_id "updated" ]
    && List.length !embedded = before);
  print_endline "knowledge tests passed"

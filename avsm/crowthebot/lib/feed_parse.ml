type document =
  | Opml of string * string list
  | Feed of string * string * Feed_store.entry list

let xml_root body =
  if String.length body > 2 * 1024 * 1024 then invalid_arg "Feed exceeds 2 MiB.";
  let input = Xmlm.make_input (`String (0, body)) in
  let depth = ref 0 and count = ref 0 and root = ref None in
  while not (Xmlm.eoi input) do
    incr count;
    if !count > 100000 then invalid_arg "Feed XML has too many nodes.";
    match Xmlm.input input with
    | `Dtd None -> ()
    | `Dtd (Some _) -> invalid_arg "Feed DTDs are forbidden."
    | `El_start (name, _) ->
        incr depth;
        if !depth > 64 then invalid_arg "Feed XML is too deeply nested.";
        if !root = None then root := Some name
    | `El_end -> decr depth
    | `Data _ -> ()
  done;
  Option.get !root

let atom_text (text : Syndic.Atom.text_construct) =
  match text with
  | Syndic.Atom.Text s | Html (_, s) -> s
  | Xhtml (_, nodes) -> String.concat "" (List.map Syndic.XML.to_string nodes)

let digest s = Digestif.SHA256.(to_hex (digest_string s))
let clip = Plugin.clip

let link url =
  Option.bind url (fun u ->
      try Some (Feed_http.normalize (Uriz.to_string u))
      with Invalid_argument _ -> None)

let entry ~fallback (e : Sortal_feed.Entry.t) =
  let title = Option.value ~default:"(untitled)" e.title in
  let summary =
    Option.value ~default:""
      (match e.summary with Some _ -> e.summary | None -> e.content)
  in
  let published = Option.map (Ptime.to_rfc3339 ~frac_s:0) e.date in
  let identity =
    if fallback || e.id = "" then
      String.concat "\000"
        [ title; Option.value ~default:"" published; summary ]
    else e.id
  in
  Feed_store.
    {
      entry_id = 0;
      source_id = 0;
      key = digest identity;
      title = clip ~bytes:512 title;
      summary = clip ~bytes:2048 summary;
      url = link e.url;
      published;
      observed_at = "";
    }

let decode ~url body =
  try
    let root = xml_root body in
    let input () = Xmlm.make_input (`String (0, body)) in
    let xmlbase = Uriz.of_string_exn url in
    let result =
      match root with
      | "", "opml" -> (
          match Sortal_feed.Opml.decode ~max_feeds:1000 body with
          | Error m -> invalid_arg m
          | Ok opml ->
              Opml
                ( clip ~bytes:512 (Option.value ~default:url opml.title),
                  List.map
                    (fun (f : Sortal_feed.Opml.feed) ->
                      Feed_http.normalize f.xml_url)
                    opml.feeds ))
      | "http://www.w3.org/2005/Atom", "feed" ->
          let feed = Syndic.Atom.parse ~xmlbase (input ()) in
          Feed
            ( "atom",
              clip ~bytes:512 (atom_text feed.title),
              List.map
                (fun e ->
                  entry ~fallback:false
                    (Sortal_feed.Entry.of_atom_entry ~source_feed:url e))
                feed.entries )
      | "", "rss" ->
          let feed = Syndic.Rss2.parse ~xmlbase (input ()) in
          Feed
            ( "rss",
              clip ~bytes:512 feed.title,
              List.map
                (fun (e : Syndic.Rss2.item) ->
                  entry
                    ~fallback:(e.guid = None && e.link = None)
                    (Sortal_feed.Entry.of_rss2_item ~source_feed:url e))
                feed.items )
      | "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "RDF" ->
          let feed = Syndic.Rss1.parse ~xmlbase (input ()) in
          Feed
            ( "rss1",
              clip ~bytes:512 feed.channel.title,
              List.map
                (fun (e : Syndic.Rss1.item) ->
                  Feed_store.
                    {
                      entry_id = 0;
                      source_id = 0;
                      key = digest (Uriz.to_string e.about);
                      title = clip ~bytes:512 e.title;
                      url = link (Some e.link);
                      published = None;
                      summary =
                        clip ~bytes:2048
                          (Option.value ~default:"" e.description);
                      observed_at = "";
                    })
                feed.item )
      | _ -> invalid_arg "Expected RSS, Atom or OPML, not a web page."
    in
    (match result with
    | Feed (_, _, entries) when List.length entries > 2000 ->
        invalid_arg "Feed contains more than 2000 entries."
    | _ -> ());
    Ok result
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Invalid_argument m -> Error m
  | _ -> Error "Could not parse this RSS, Atom or OPML document."

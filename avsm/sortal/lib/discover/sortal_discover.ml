(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let src = Logs.Src.create "sortal.discover" ~doc:"Manual feed discovery"
module Log = (val Logs.src_log src : Logs.LOG)

let system_prompt = {|You are a feed discovery agent. Your job is to inspect a person's website
and identify blog posts, articles, or other published content.

You have access to Read (for reading local files) and WebFetch (for
fetching web pages). Use WebFetch to inspect the URL provided and find
content entries.

IMPORTANT RULES:
- Find blog posts, articles, weeknotes, or other dated content
- For each entry provide: the URL, the title, the publication date (ISO 8601),
  and a concise factual summary (1-2 sentences, no editorialising)
- Summaries should state what the post covers, not evaluate or praise it
- Do NOT include entries that already exist in the existing feed
- Use the hint provided to guide your content discovery
- Return your findings as structured JSON matching the output schema|}

(* JSON schema for structured output *)
let m = Jsont.Meta.none

let entry_schema =
  Jsont.Object ([
    (("type", m), Jsont.String ("object", m));
    (("properties", m), Jsont.Object ([
      (("url", m), Jsont.Object ([
        (("type", m), Jsont.String ("string", m));
        (("description", m), Jsont.String ("Canonical URL of the content", m));
      ], m));
      (("title", m), Jsont.Object ([
        (("type", m), Jsont.String ("string", m));
        (("description", m), Jsont.String ("Title of the blog post or article", m));
      ], m));
      (("updated", m), Jsont.Object ([
        (("type", m), Jsont.String ("string", m));
        (("description", m), Jsont.String ("Publication date in ISO 8601 format (e.g. 2025-07-07T00:00:00Z)", m));
      ], m));
      (("summary", m), Jsont.Object ([
        (("type", m), Jsont.String ("string", m));
        (("description", m), Jsont.String ("Concise 1-2 sentence factual summary of the content", m));
      ], m));
    ], m));
    (("required", m), Jsont.Array ([
      Jsont.String ("url", m);
      Jsont.String ("title", m);
      Jsont.String ("updated", m);
      Jsont.String ("summary", m);
    ], m));
    (("additionalProperties", m), Jsont.Bool (false, m));
  ], m)

let discovery_schema =
  Jsont.Object ([
    (("type", m), Jsont.String ("object", m));
    (("properties", m), Jsont.Object ([
      (("entries", m), Jsont.Object ([
        (("type", m), Jsont.String ("array", m));
        (("items", m), entry_schema);
        (("description", m), Jsont.String ("List of discovered content entries", m));
      ], m));
    ], m));
    (("required", m), Jsont.Array ([
      Jsont.String ("entries", m);
    ], m));
    (("additionalProperties", m), Jsont.Bool (false, m));
  ], m)

let build_user_message ~contact_yaml ~hint ~url ~existing_entries =
  let hint_text = match hint with
    | Some h -> Printf.sprintf "\nDiscovery hint: %s\n" h
    | None -> ""
  in
  let existing_text = match existing_entries with
    | [] -> "\nNo existing entries yet (this is a fresh discovery).\n"
    | entries ->
      let lines = List.map (fun (id, title) ->
        Printf.sprintf "- %s  %s" id title
      ) entries in
      Printf.sprintf
        "\nAlready tracked entries (do NOT return these — only return NEW content):\n%s\n"
        (String.concat "\n" lines)
  in
  Printf.sprintf
    {|Discover feed entries for this contact.

Contact information:
```yaml
%s
```
%s
URL to inspect: %s
%s
Use WebFetch to inspect the URL and find NEW content entries only.
Return the results as structured JSON.|}
    contact_yaml hint_text url existing_text

(* Extract a string field from a Jsont.json object *)
let get_field json key =
  let codec =
    Jsont.Object.map ~kind:"field" (fun v -> v)
    |> Jsont.Object.opt_mem key Jsont.string ~enc:Fun.id
    |> Jsont.Object.finish
  in
  match Jsont.Json.decode codec json with Ok v -> v | Error _ -> None

let get_entries json =
  let codec =
    Jsont.Object.map ~kind:"entries" (fun v -> v)
    |> Jsont.Object.opt_mem "entries" (Jsont.list Jsont.json) ~enc:Fun.id
    |> Jsont.Object.finish
  in
  match Jsont.Json.decode codec json with Ok v -> v | Error _ -> None

(* Parse an ISO 8601 date string to Ptime.t *)
let parse_date s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Some t
  | Error _ ->
    (* Try date-only format YYYY-MM-DD *)
    match Ptime.of_rfc3339 (s ^ "T00:00:00Z") with
    | Ok (t, _, _) -> Some t
    | Error _ -> None

(* Build a Syndic.Atom.entry from structured JSON *)
let atom_entry_of_json json =
  let url = get_field json "url" in
  let title = get_field json "title" in
  let updated_str = get_field json "updated" in
  let summary = get_field json "summary" in
  match url, title, updated_str with
  | Some url, Some title, Some updated_str ->
    let updated = match parse_date updated_str with
      | Some t -> t
      | None -> Ptime_clock.now ()
    in
    let tc s : Syndic.Atom.text_construct = Text s in
    let summary_tc = Option.map tc summary in
    let entry = Syndic.Atom.entry
      ~id:(Uri.of_string url)
      ~authors:({ Syndic.Atom.name = ""; uri = None; email = None }, [])
      ~title:(tc title)
      ~updated
      ~links:[Syndic.Atom.link ~rel:Syndic.Atom.Alternate (Uri.of_string url)]
      ?summary:summary_tc
      ()
    in
    Some entry
  | _ ->
    Log.warn (fun f -> f "Skipping entry with missing url/title/updated");
    None

(* Build a Syndic.Atom.feed from a list of entries *)
let make_atom_feed ~url entries =
  let now = Ptime_clock.now () in
  Syndic.Atom.feed
    ~id:(Uri.of_string url)
    ~title:(Syndic.Atom.Text "Discovered feed")
    ~updated:now
    entries

let dedup_atom_entries entries =
  let tbl = Hashtbl.create (List.length entries) in
  List.filter (fun (e : Syndic.Atom.entry) ->
    let key = Uri.to_string e.id in
    if Hashtbl.mem tbl key then false
    else (Hashtbl.replace tbl key (); true)
  ) entries

let existing_entry_ids feed_path =
  match Sortal_feed.Store.load_atom feed_path with
  | Some atom_feed ->
    List.map (fun (e : Syndic.Atom.entry) ->
      let id = Uri.to_string e.id in
      let title = match e.title with
        | Text s | Html (_, s) -> s
        | Xhtml _ -> "(xhtml)"
      in
      (id, title)
    ) atom_feed.entries
  | None -> []

let discover ~sw ~process_mgr ~clock ~store ~handle ~contact_yaml feed =
  let url = Sortal_schema.Feed.url feed in
  let hint = Sortal_schema.Feed.hint feed in
  let feed_path = Sortal_feed.Store.feed_file store handle feed in
  let existing_entries = existing_entry_ids feed_path in
  let user_msg =
    build_user_message ~contact_yaml ~hint ~url ~existing_entries
  in
  Log.info (fun f -> f "Discovering entries for @%s from %s" handle url);
  (* Configure Claude with structured output *)
  let output_format =
    Claude.Proto.Structured_output.of_json_schema discovery_schema
  in
  let options =
    Claude.Options.default
    |> Claude.Options.with_system_prompt system_prompt
    |> Claude.Options.with_output_format output_format
    |> Claude.Options.with_allowed_tools ["Read"; "WebFetch"; "Glob"; "Grep"]
    |> Claude.Options.with_permission_mode Claude.Permissions.Mode.Bypass_permissions
    |> Claude.Options.with_max_budget_usd 0.50
    |> Claude.Options.with_model `Sonnet_4_5
    |> Claude.Options.with_no_settings
  in
  (* Run Claude and collect structured output *)
  let result_json = ref None in
  (try
     let client =
       Claude.Client.create ~options ~sw ~process_mgr ~clock ()
     in
     Claude.Client.query client user_msg;
     let handler = object
       inherit Claude.Handler.default
       method! on_complete c =
         result_json := Claude.Response.Complete.structured_output c
       method! on_error e =
         Log.warn (fun f -> f "Claude error: %s" (Claude.Response.Error.message e))
     end in
     Claude.Client.run client ~handler
   with exn ->
     Log.err (fun f -> f "Claude discovery failed: %s" (Printexc.to_string exn)));
  match !result_json with
  | None ->
    Error "Claude returned no structured output"
  | Some json ->
    match get_entries json with
    | None | Some [] ->
      Log.info (fun f -> f "No entries discovered for @%s" handle);
      Ok { Sortal_feed.Sync.new_entries = 0; total_entries = 0;
           feed_name = Sortal_schema.Feed.name feed }
    | Some entry_jsons ->
      let entries = List.filter_map atom_entry_of_json entry_jsons in
      let new_count = List.length entries in
      Log.info (fun f -> f "Discovered %d entries for @%s" new_count handle);
      if new_count = 0 then
        Ok { Sortal_feed.Sync.new_entries = 0; total_entries = 0;
             feed_name = Sortal_schema.Feed.name feed }
      else begin
        let new_feed = make_atom_feed ~url entries in
        (* Ensure feed directory exists *)
        Sortal_feed.Store.ensure_feed_dir store handle;
        (* Merge with existing *)
        let existing_count, merged =
          match Sortal_feed.Store.load_atom feed_path with
          | Some existing ->
            let count = List.length existing.entries in
            let combined =
              Syndic.Atom.aggregate ~sort:`Newest_first [existing; new_feed]
            in
            let deduped =
              { combined with entries = dedup_atom_entries combined.entries }
            in
            (count, deduped)
          | None -> (0, new_feed)
        in
        Sortal_feed.Store.save_atom feed_path merged;
        let total = List.length merged.entries in
        let new_entries = max 0 (total - existing_count) in
        (* Update metadata *)
        let meta_path = Sortal_feed.Store.meta_file store handle feed in
        let meta : Sortal_feed.Meta.t =
          match Sortal_feed.Meta.load meta_path with
          | Some mt -> { mt with last_sync = Some (Ptime_clock.now ()); entry_count = total }
          | None -> {
              feed_url = url;
              feed_type = Sortal_schema.Feed.feed_type feed;
              last_sync = Some (Ptime_clock.now ());
              etag = None;
              last_modified = None;
              entry_count = total;
            }
        in
        Sortal_feed.Meta.save meta_path meta;
        Ok { Sortal_feed.Sync.new_entries; total_entries = total;
             feed_name = Sortal_schema.Feed.name feed }
      end

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* The listing and entry render path reads URLs out of data files: a link
   URL, a paper URL, a contact URL, a video URL. It used to parse them with
   opam uri, which coerces anything into a URI, and now parses them with
   uriz, which refuses what is not one. Two consequences need pinning.

   A URL that uriz rejects must degrade, never raise: the data file it came
   from is written by hand and by a bookmarking tool, and neither is a
   parser. Each site here is a render, so a raise would take out a whole
   page over one bad link.

   A URL that both parsers accept must display the same, with one deliberate
   exception. uriz keeps a percent-triplet that encodes a reserved character
   where uri decoded it, so a path spelled [C%2B%2B] now shows as written
   rather than as [C++]. That is the same divergence the feed path took when
   Syndic moved, and it is pinned here rather than left to be discovered on
   the links page.

   These functions are not reachable from test_md_golden, whose corpus stops
   at the three markdown renderers and holds no video embed. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let eq name ~expected ~actual =
  incr checks;
  if expected <> actual then (
    prerr_endline ("FAIL: " ^ name);
    prerr_endline ("  expected: " ^ expected);
    prerr_endline ("  actual:   " ^ actual);
    exit 1)

module Links = Arod_component.Links

(* {1 The link listing} *)

let test_domain_and_path () =
  let dap u =
    let d, p = Links.domain_and_path u in
    d ^ " " ^ p
  in
  eq "a plain link splits into host and path"
    ~expected:"github.com /mirage/mirage/pull/1234"
    ~actual:(dap "https://github.com/mirage/mirage/pull/1234");
  eq "www. survives here, since only the classifier strips it"
    ~expected:"www.cl.cam.ac.uk /~avsm2/"
    ~actual:(dap "http://www.cl.cam.ac.uk/~avsm2/");
  eq "a root path prints as no path" ~expected:"example.com "
    ~actual:(dap "https://example.com/");
  eq "a mailto has no host, and the mailbox is its path"
    ~expected:" anil@recoil.org"
    ~actual:(dap "mailto:anil@recoil.org");
  eq "a percent triplet is kept, where uri decoded it"
    ~expected:"en.wikipedia.org /wiki/C%2B%2B"
    ~actual:(dap "https://en.wikipedia.org/wiki/C%2B%2B");
  (* A raw space is not legal in a URI reference. The row still renders, with
     the text the data file holds where the path goes. *)
  eq "a URL that does not parse renders as its own text"
    ~expected:" https://example.com/a b" ~actual:(dap "https://example.com/a b");
  eq "a long path is truncated"
    ~expected:
      "example.com /a/path/that/runs/on/well/past/fifty/characters/en\xe2\x80\xa6"
    ~actual:
      (dap
         "https://example.com/a/path/that/runs/on/well/past/fifty/characters/end")

let test_path_segments () =
  let segs u = String.concat "|" (Links.path_segments u) in
  eq "segments drop the empties" ~expected:"abs|2401.01234"
    ~actual:(segs "https://arxiv.org/abs/2401.01234");
  eq "a root path has no segments" ~expected:""
    ~actual:(segs "https://example.com/");
  eq "an unparseable URL has no segments" ~expected:""
    ~actual:(segs "https://example.com/a b")

let test_host_and_path () =
  let hap u =
    let h, p = Links.host_and_path u in
    h ^ " " ^ p
  in
  eq "an empty path reads as the root" ~expected:"example.com /"
    ~actual:(hap "https://example.com");
  eq "a doi path keeps its slash" ~expected:"doi.org /10.1145/3123456"
    ~actual:(hap "https://doi.org/10.1145/3123456");
  eq "an unparseable URL classifies as hostless" ~expected:" /"
    ~actual:(hap "not a url at all")

(* {1 Host badges}

   The paper sidebar and the contact sidebar both label a link with its host.
   A missing host must leave the badge off rather than stop the sidebar. *)

let test_host_badges () =
  eq "a paper URL badges its host without www."
    ~expected:"usenix.org"
    ~actual:(Arod_component.Paper.host_without_www
               "https://www.usenix.org/conference/atc23/presentation/x");
  eq "a hostless URL badges nothing" ~expected:""
    ~actual:(Arod_component.Paper.host_without_www "not a url at all");
  check "url_host answers Null for a URL with no host"
    (Arod_component.Common.url_host "mailto:anil@recoil.org" = Null);
  check "url_host answers Null for a URL that does not parse"
    (Arod_component.Common.url_host "https://example.com/a b" = Null)

(* {1 Degraded classification}

   Two cases where the parser change is visible through [classify_url].

   A contact URL that names no host, such as a [file:] one, used to be filed
   under the empty bare host. Every URL that fails to parse also reports the
   empty host, so one such contact would have claimed all of them. It is now
   skipped.

   A URL that is not a URI reference must still reach the listing. The one in
   the live data below carries a raw en dash. *)

let empty_ctx =
  let entries =
    Bushel.Entry.v ~papers:[] ~notes:[] ~projects:[] ~ideas:[] ~contacts:[]
      ~videos:[] ~data_dir:"." ()
  in
  Arod.Ctx.of_entries ~config:Arod.Config.default entries

let test_degraded_classification () =
  let hostless =
    Sortal_schema.Contact.make ~handle:"ada" ~names:[ "Ada Lovelace" ]
      ~links:[ { url = "file:///home/ada/notes.html"; label = None } ]
      ()
  in
  let tbl = Links.build_contact_by_domain [ hostless ] in
  check "a contact URL with no host claims no domain" (Hashtbl.length tbl = 0);
  let display =
    Links.classify_url ~contact_by_domain:tbl ~doi_entries:[] ~ctx:empty_ctx
      "http://en.wikipedia.org/wiki/Smith\xe2\x80\x93Waterman_algorithm"
  in
  eq "a URL that is not a URI reference still classifies"
    ~expected:"untitled"
    ~actual:(Links.string_of_kind display.kind);
  eq "and labels as its own text, with no host"
    ~expected:" http://en.wikipedia.org/wiki/Smith\xe2\x80\x93Waterman_algo\xe2\x80\xa6"
    ~actual:display.label

(* {1 The video embed}

   [Arod.Md] rewrites a [/watch] path segment to [/embed] before it builds
   the iframe. The rewrite re-serializes the URL, so it is the one place in
   the render path where a parser change can alter an attribute that a
   browser then fetches. *)

let video ~url =
  {
    Bushel.Video.slug = "a-video";
    title = "A Sample Video";
    published_date = Option.get (Ptime.of_date (2022, 9, 1));
    uuid = "video-uuid";
    description = "";
    url;
    talk = true;
    vertical = false;
    paper = None;
    project = None;
    tags = [];
    social = None;
  }

(* [attribute name html] is the value of the first [name="..."] in [html]. *)
let attribute name html =
  let marker = name ^ "=\"" in
  let m = String.length marker and n = String.length html in
  let rec start i =
    if i + m > n then None
    else if String.sub html i m = marker then Some (i + m)
    else start (i + 1)
  in
  match start 0 with
  | None -> "no " ^ name
  | Some i -> (
    match String.index_from_opt html i '"' with
    | None -> "unterminated " ^ name
    | Some j -> String.sub html i (j - i))

let embed_src url =
  let entries =
    Bushel.Entry.v ~papers:[] ~notes:[] ~projects:[] ~ideas:[] ~contacts:[]
      ~videos:[ video ~url ] ~data_dir:"." ()
  in
  let ctx = Arod.Ctx.of_entries ~config:Arod.Config.default entries in
  let html, _ = Arod.Md.to_html ~ctx "![](/videos/a-video)" in
  attribute "src" html

let test_video_embed () =
  eq "a youtube watch URL becomes an embed URL"
    ~expected:"https://www.youtube.com/embed?v=dQw4w9WgXcQ"
    ~actual:(embed_src "https://www.youtube.com/watch?v=dQw4w9WgXcQ");
  eq "a URL with no watch segment is unchanged"
    ~expected:"https://vimeo.com/123456789"
    ~actual:(embed_src "https://vimeo.com/123456789");
  eq "a URL that does not parse is embedded as it stands"
    ~expected:"https://example.com/a b"
    ~actual:(embed_src "https://example.com/a b")

let () =
  test_domain_and_path ();
  test_path_segments ();
  test_host_and_path ();
  test_host_badges ();
  test_degraded_classification ();
  test_video_embed ();
  Printf.printf "test_url_render: %d checks ok\n" !checks

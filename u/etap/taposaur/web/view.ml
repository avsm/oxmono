open Htmlit
open Taposaur
module C = Taposaur_catalog
module Ui = Taposaur_ui
module P = Taposaur_paper
let el name children = El.v name children
let txt = El.txt
let at = At.v
let encode = Uriz.pct_encode ~component:`Unreserved
let url path params = path ^ "?" ^ String.concat "&"
  (List.map (fun (k,v) -> encode k ^ "=" ^ encode v) params)
let link href label = El.v "a" ~at:[at "href" href] [txt label]
let p s = el "p" [txt s]
let code s = el "code" [txt s]
let empty = Option.value ~default:""
let classed name cls children = El.v name ~at:[at "class" cls] children
let author (a : Author.t) = String.trim (empty a.given ^ " " ^ a.family)
let authors xs = String.concat "; " (List.map author xs)
let joined xs = String.concat " · " (List.filter ((<>) "") xs)
let nonempty s = if s = "" then El.void else p s
let count n =
  let s = string_of_int n in
  let len = String.length s in
  let b = Buffer.create (len + len / 3) in
  String.iteri (fun i c ->
    if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char b ',';
    Buffer.add_char b c) s;
  Buffer.contents b
let size n =
  let f = Int64.to_float n in
  if f >= 1048576. then Printf.sprintf "%.1f MiB" (f /. 1048576.)
  else if f >= 1024. then Printf.sprintf "%.1f KiB" (f /. 1024.)
  else Int64.to_string n ^ " B"
let stamp f =
  let d = Unix.gmtime f in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d UTC"
    (d.tm_year + 1900) (d.tm_mon + 1) d.tm_mday d.tm_hour d.tm_min

let css = {|
:root{color-scheme:light;--ink:#23352e;--muted:#65736b;--line:#d8dfd8;
--paper:#fafaf5;--accent:#236149}*{box-sizing:border-box}
body{margin:0;background:var(--paper);color:var(--ink);
font:14px/1.45 system-ui,sans-serif}a{color:var(--accent);
text-underline-offset:3px}a:hover{color:#123d2b}
header{border-bottom:1px solid var(--line);
padding:18px max(24px,calc((100% - 1120px)/2));display:flex;
gap:32px;align-items:center}header a:first-child{font-weight:800;
font-size:22px;text-decoration:none;letter-spacing:-1px}
main{max-width:1120px;margin:24px auto;padding:0 24px 40px}
h1{font-size:24px;line-height:1.3;font-weight:600;margin:0 0 16px;
letter-spacing:-.4px;overflow-wrap:anywhere}
h2{font-size:16px;line-height:1.4;font-weight:600;margin:24px 0 8px}
p{margin:6px 0}.muted,small{color:var(--muted)}small{font-size:12px}
.summary{display:flex;flex-wrap:wrap;gap:8px 28px;padding:12px 0;
border-block:1px solid var(--line);margin:16px 0 6px}
.summary span{color:var(--muted)}.summary strong{color:var(--ink);
font-size:18px;font-weight:600;font-variant-numeric:tabular-nums}
.status{font-size:12px;color:var(--muted)}
form{margin:0 0 20px}.search-line{display:flex;gap:8px;align-items:end}
.search-line label{flex:1}.filters{display:grid;
grid-template-columns:1fr 1.2fr 1.3fr 1.6fr 1.6fr .8fr .8fr;gap:8px;
margin-top:8px}.filters button{align-self:end}
label{display:flex;flex-direction:column;font-size:12px;
color:var(--muted);gap:3px;min-width:0}input,select,button{font:inherit;
font-size:14px;height:32px;padding:4px 8px;border:1px solid #bccabe;
border-radius:3px;background:white;color:var(--ink);min-width:0;width:100%}
button{width:auto;background:var(--accent);color:white;padding:4px 18px;
cursor:pointer}button:hover{background:#184d38}
:focus-visible{outline:2px solid var(--accent);outline-offset:3px}
.result{padding:11px 0;border-bottom:1px solid var(--line)}
.result:first-child{border-top:1px solid var(--line)}
.result-heading{display:flex;align-items:baseline;gap:12px}
.result h2{font-size:15px;line-height:1.4;margin:0;flex:1}
.result h2 a{text-decoration:none}.result h2 a:hover{text-decoration:underline}
.result p{font-size:12px;color:var(--muted);margin:3px 0 0}
.result .abstract-snippet{display:-webkit-box;-webkit-line-clamp:2;
-webkit-box-orient:vertical;overflow:hidden;color:var(--ink);margin-top:5px}
.result .authors{color:var(--ink);overflow:hidden;text-overflow:ellipsis;
white-space:nowrap}.formats{display:flex;gap:6px;flex-shrink:0;
font-size:10px;font-weight:600;color:var(--muted);letter-spacing:.3px}
code,pre{font:12px/1.5 ui-monospace,monospace}code{overflow-wrap:anywhere}
pre{white-space:pre-wrap;overflow-wrap:anywhere;padding:10px;
background:#f0f2eb;border:1px solid var(--line);border-radius:3px}
.actions{display:flex;align-items:center;flex-wrap:wrap;gap:8px 18px;
padding:10px 0;border-block:1px solid var(--line);margin:12px 0 16px}
.actions .identity{flex:1;min-width:180px}.metadata{display:grid;
grid-template-columns:100px minmax(0,1fr);gap:5px 16px;margin:12px 0}
dt{color:var(--muted);font-size:12px}dd{margin:0;overflow-wrap:anywhere}
.abstract{max-width:90ch;white-space:pre-line}
.table-wrap{overflow-x:auto}table{border-collapse:collapse;width:100%}
td,th{text-align:left;padding:7px 8px;border-bottom:1px solid var(--line);
vertical-align:top;font-size:12px}th{font-weight:500;color:var(--muted)}
td:first-child,th:first-child{padding-left:0}td:last-child,th:last-child{
padding-right:0}.numeric{text-align:right;white-space:nowrap;
font-variant-numeric:tabular-nums}td{overflow-wrap:anywhere}
.file-name{display:block;margin-bottom:3px}.file-actions{white-space:nowrap}
details{margin-top:5px}summary{cursor:pointer;color:var(--muted);
font-size:12px}details .metadata{grid-template-columns:85px minmax(0,1fr);
font-size:12px;gap:4px 10px;margin:8px 0}.pagination{margin-top:16px}
.preview-heading{display:flex;align-items:baseline;gap:16px;margin-top:24px}
.preview-heading h2{margin:0 0 8px}.preview-heading a{font-size:12px}
iframe{display:block;width:100%;height:75vh;min-height:360px;
border:1px solid var(--line);background:white}
.tei{max-width:780px;font:17px/1.7 Georgia,serif;margin:24px auto}
.tei h2{font:600 21px/1.4 system-ui,sans-serif}.tei table{font-size:13px}
.tei figure{margin:20px 0}.tei p{margin:12px 0}
.tei-reference{margin:1.2em 0;padding:1em 1.2em;border:1px solid var(--line);
border-radius:8px;background:#fff;overflow-wrap:anywhere;
scroll-margin-top:1em;font:15px/1.65 system-ui,sans-serif}
.tei-reference-title{font:600 17px/1.5 Georgia,serif;margin:.25em 0}
.tei-reference-title a{text-decoration:none}.tei-reference-title a:hover{text-decoration:underline}
.tei-reference-authors,.tei-reference-publication{color:var(--muted)}
.tei-reference-label{font-weight:700}.tei-reference-source{display:block;color:var(--muted)}
.tei-reference details{font-size:12px;margin-top:.5em}
.tei-reference-raw{white-space:pre-wrap}.tei-backlinks{font:12px system-ui,sans-serif}
.tei h1{font:600 26px/1.3 system-ui,sans-serif}.tei h3,.tei h4,.tei h5,.tei h6{font-family:system-ui,sans-serif}
.tei table{display:block;overflow:auto;border-collapse:collapse;max-width:100%}
.tei td,.tei th{border:1px solid var(--line);padding:.5em;vertical-align:top}
.tei caption{font-weight:600;text-align:left;margin-bottom:.5em}
.tei figcaption{color:var(--muted);font:14px/1.6 system-ui,sans-serif}
.tei img{max-width:100%;height:auto}.tei-note{background:#fff;padding:.3em .5em;font-size:.9em}
.tei-page,.tei-unavailable{color:var(--muted);font:12px/1.5 system-ui,sans-serif}
.tei-page{display:block;border-top:1px solid var(--line);margin:1em 0}
.tei-equation{display:block;overflow:auto;padding:1em 0;text-align:center}
.tei-equation-number{margin-left:1em}.tei-formula{overflow-wrap:normal}
.tei math{font-size:1.1em}.tei [data-tex]{white-space:pre-wrap}
.tei-contents{font:14px/1.8 system-ui,sans-serif;padding:1em;border:1px solid var(--line)}
.tei :focus{outline:2px solid currentColor;outline-offset:3px}
.tei-reference:target{background:#fff4cc;color:#222}
.tei-identifier{display:inline-block;font:12px/1.5 system-ui,sans-serif;
margin-left:.4em;color:var(--muted)}
@media(max-width:700px){header{gap:20px;padding:14px 16px}
main{padding:0 16px 24px;margin-top:20px}h1{font-size:21px}
.filters{grid-template-columns:repeat(2,minmax(0,1fr))}
.result-heading{flex-wrap:wrap;gap:3px}.result h2{flex-basis:100%}
.metadata{grid-template-columns:80px minmax(0,1fr);gap:5px 10px}
.summary{gap:6px 20px}.actions{gap:8px 14px}}
|}

let interface_css = {|
[hidden]{display:none!important}.sr-only{position:absolute;width:1px;height:1px;
padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap}
.journal-field{position:relative;min-width:0}.journal-control{position:relative}
.journal-control input{padding-right:30px}.journal-clear{position:absolute;
right:1px;top:1px;height:30px;width:28px;padding:0;border:0;background:transparent;
color:var(--muted)}.journal-clear:hover{background:#eef2ec;color:var(--ink)}
.journal-menu{position:absolute;z-index:10;top:100%;right:0;margin-top:4px;
width:min(460px,calc(100vw - 32px));min-width:100%;background:white;
border:1px solid var(--line);border-radius:4px;box-shadow:0 6px 24px #23352e18}
.journal-menu ul{padding:4px;margin:0;list-style:none;max-height:300px;overflow:auto}
.journal-menu li{display:flex;justify-content:space-between;align-items:baseline;
gap:16px;padding:8px 10px;border-radius:2px;cursor:pointer;font-size:13px}
.journal-menu li span{overflow-wrap:anywhere}.journal-menu li small{white-space:nowrap}
.journal-menu li:hover,.journal-menu li[aria-selected=true]{background:#eaf2e9}
.journal-menu p{padding:8px 12px;margin:0;border-top:1px solid var(--line);
font-size:11px;color:var(--muted)}.selected-journal{display:flex;gap:10px;
align-items:baseline;padding:8px 0;margin-bottom:8px;border-bottom:1px solid var(--line)}
.selected-journal a{font-size:12px}.journal-link{font-weight:500}
.paper-layout{display:grid;grid-template-columns:minmax(0,1fr);gap:28px}
.paper-detail{min-width:0}.paper-page.panel-open{max-width:1600px}
.paper-layout.panel-open{grid-template-columns:minmax(320px,.85fr) minmax(0,1.15fr)}
.paper-panel{position:sticky;top:16px;align-self:start;background:white;
border:1px solid var(--line);border-radius:4px;display:flex;flex-direction:column;
height:calc(100vh - 100px);min-height:320px;overflow:hidden}
.panel-heading{display:flex;align-items:center;justify-content:space-between;
gap:12px;padding:10px 16px;border-bottom:1px solid var(--line)}
.panel-heading h2{font-size:13px;margin:0;font-weight:500;color:var(--muted)}
.panel-close{background:transparent;color:var(--muted);border:0;padding:2px 4px;
font-size:12px;height:28px}.panel-close:hover{background:#eef2ec;color:var(--ink)}
.panel-tabs{display:flex;gap:20px;padding:0 16px;border-bottom:1px solid var(--line)}
.panel-tabs a{padding:10px 0 8px;border-bottom:2px solid transparent;
text-decoration:none;font-size:13px;color:var(--muted)}
.panel-tabs a[aria-selected=true]{border-bottom-color:var(--accent);color:var(--accent);
font-weight:600}.panel-context{display:flex;justify-content:space-between;gap:12px;
padding:8px 16px;font-size:11px;color:var(--muted);border-bottom:1px solid var(--line)}
#panel-filename{overflow-wrap:anywhere}.panel-context a{white-space:nowrap}
.panel-body{padding:12px 20px 24px;overflow:auto;overscroll-behavior:contain;
min-height:0;flex:1}.panel-body .tei{margin:0;font-size:16px}
.panel-body pre{font-size:11px}.panel-body h2:first-child{margin-top:0}
.actions a[aria-expanded=true]{font-weight:600}
@media(max-width:900px){.paper-layout.panel-open{grid-template-columns:minmax(0,1fr)}
.paper-panel{position:fixed;top:auto;bottom:0;left:0;right:0;height:55dvh;
min-height:240px;z-index:20;border-radius:8px 8px 0 0;box-shadow:0 -4px 24px #23352e18}
.paper-page.panel-open{padding-bottom:calc(55dvh + 24px)}
.panel-body{padding:12px 16px 24px}.panel-heading{padding:6px 16px}}
@media(max-width:700px){.journal-menu{left:0;right:auto}}
|}

(* Taposaur's stylesheet and scripts are served at content-hashed paths, so
   pages need no inline style or script. Its colours are light only. *)
let stylesheet =
  Sitekit.Asset.css ~name:"taposaur" (css ^ interface_css ^ Ui.css)
let script = Sitekit.Asset.js ~name:"interface" Client.source
let katex = Sitekit.Asset.js ~name:"katex" Katex.source
let assets = [stylesheet; script; katex]

let nav label path = Sitekit.nav label (Sitekit.Url.path [path])

let site = Sitekit.site ~name:"taposaur" ~brand:"tp" ~theme:`Light
  ~nav:[nav "Browse & search" "search"; nav "Reviews" "reviews";
        nav "Jobs" "jobs"; nav "Activity" "activity"; nav "Docs" "docs"]
  ~stylesheets:[stylesheet] ~scripts:[katex; script]
  ~footer:[El.v "details" ~at:[at "id" "server-debug"]
    [el "summary" [txt "Debug · server paths & capabilities"];
     El.v "div" ~at:[at "id" "server-debug-content"]
       [p "Server configuration and available fetch strategies.";
        link "/debug" "Open server details"]]] ()

(* The current section is marked in the browser, since pages are built
   before the request path reaches them. *)
let page ?(kind = "") title body =
  Sitekit.page site ~current:""
    ?kind:(if kind = "" then None else Some kind) ~title body

let form params =
  let get k = Option.value (List.assoc_opt k params) ~default:"" in
  let input name label kind = Ui.input ~name ~label ~kind ~value:(get name) in
  let select name label options =
    Ui.select ~name ~label ~value:(get name) options in
  El.v "form" ~at:[at "method" "get"; at "action" "/search"] [
    classed "div" "search-line" [input "q" "Search papers" "search";
      el "button" [txt "Search"]];
    classed "div" "filters" [
      select "scheme" "Archive" ["","All";"doi","DOI";"pmc","PMC"];
      select "has" "Content" ["","Any";"pdf","PDF";"xml","Publisher XML";
        "tei","TEI";"s2orc","S2ORC";"unconverted","Unconverted"];
      select "metadata" "Metadata" ["","Any";
        "title","With title"; "abstract","With abstract";
        "publisher","With publisher"; "no-title","Without title";
        "no-abstract","Without abstract"; "no-publisher","Without publisher"];
      input "publisher" "Publisher (exact)" "text";
      classed "div" "journal-field" [
        El.v "label" ~at:[at "for" "journal-input"] [txt "Journal"];
        classed "div" "journal-control" [
          El.v "input" ~at:[at "id" "journal-input"; at "name" "journal";
            at "type" "text"; at "value" (get "journal");
            at "placeholder" "Find a journal…"; at "autocomplete" "off";
            at "maxlength" "256"; at "role" "combobox";
            at "aria-autocomplete" "list"; at "aria-expanded" "false";
            at "aria-controls" "journal-options";
            at "aria-describedby" "journal-hint"] [];
          El.v "button" ~at:[at "id" "journal-clear";
            at "class" "journal-clear"; at "type" "button";
            at "aria-label" "Clear journal"; at "hidden" ""] [txt "×"]];
        El.v "span" ~at:[at "id" "journal-hint"; at "class" "sr-only"]
          [txt "Type a journal name. Use arrow keys and Enter to select."];
        El.v "div" ~at:[at "id" "journal-menu"; at "class" "journal-menu";
          at "hidden" ""] [
          El.v "ul" ~at:[at "id" "journal-options"; at "role" "listbox";
            at "aria-label" "Journals"] [];
          El.v "p" ~at:[at "id" "journal-status"; at "role" "status"] []]];
      input "from" "From year" "number"; input "to" "To year" "number"]]

let home ~details_cached (s : C.stats) errors =
  let coverage_row label href n =
    let pct = if s.papers = 0 then 0.
      else 100. *. float n /. float s.papers in
    classed "li" "availability-row"
      [link href label;classed "span" "availability-count"
        [link href (count n)];
       Ui.coverage ~present:n ~total:s.papers;
       classed "small" "numeric" [txt (Printf.sprintf "%.1f%%" pct)]] in
  let panel title description rows =
    classed "section" "availability-panel"
      [el "h2" [txt title];p description;el "ul" rows] in
  let diagnostic label value = [el "dt" [txt label];el "dd" [txt value]] in
  let nonzero label n = if n = 0 then [] else diagnostic label (count n) in
  let shards = match s.discovered_shards with
    | None -> count s.loaded_shards ^ " loaded; total not yet known"
    | Some n -> count s.loaded_shards ^ " / " ^ count n ^ " loaded" in
  page "Catalogue" [
    p (count s.papers ^ " papers to search and explore.");
    form [];
    classed "div" "availability-grid"
      [panel "Available documents" "Recorded formats; a paper can have more than one."
         [coverage_row "PDF" "/search?has=pdf" s.pdf;
          coverage_row "Publisher XML" "/search?has=xml" s.xml;
          coverage_row "TEI text" "/search?has=tei" s.tei;
          coverage_row "S2ORC" "/search?has=s2orc" s.s2orc];
       panel "Searchable metadata" "Coverage across the catalogue, refreshed as indexing progresses."
         [coverage_row "Title" "/search?metadata=title" s.titles;
          coverage_row "Abstract" "/search?metadata=abstract" s.abstracts;
          coverage_row "Publisher" "/search?metadata=publisher" s.publishers]];
    classed "p" "home-reviews" [txt "Working with a research collection? ";
      link "/reviews" "Open reviews and snapshots →"];
    El.v "details" ~at:[at "class" "catalogue-diagnostics"]
      [el "summary" [txt "Indexing diagnostics";
        (if s.errors = 0 then El.void else
          Ui.badge ~tone:`Warning (count s.errors ^ " import issues"))];
       p "Operational information about the catalogue import and metadata cache.";
       classed "dl" "metadata"
         (diagnostic "Saved details" (count details_cached) @
          diagnostic "Fully resolved" (count s.enriched) @
          nonzero "Partial resolutions" s.partial @
          nonzero "Metadata errors" s.enrichment_errors @
          nonzero "Stale records" s.stale @
          diagnostic "Index shards" shards @
          Option.fold ~none:[] ~some:(fun t ->
            diagnostic "Last shard import" (stamp t)) s.last_import);
       (if s.errors = 0 then El.void else el "section"
         [el "h2" [txt "Import issues"];
          p "These shards need repair. Successfully imported papers remain searchable.";
          Ui.table ["Shard",false;"Issue",false]
            (List.map (fun (path,message) ->
              [Ui.cell path [code path];Ui.cell message [txt message]]) errors)])]]

let entry (summary : Summary.t) =
  let e = summary.bibliography in
  let uid = Uid.to_string summary.uid in
  let format yes s = if yes then Ui.badge ~tone:(if s = "TEI" then `Good else `Neutral) s else El.void in
  classed "article" "result" [
    classed "div" "result-heading" [
      el "h2" [link (url "/paper" ["uid",uid])
        (Option.value e.title ~default:"Untitled paper")];
      classed "div" "formats" [format summary.holdings.pdf "PDF";
        format summary.holdings.xml "XML"; format summary.holdings.tei "TEI";
        format summary.holdings.s2orc "S2ORC"]];
    (if e.authors = [] then El.void else
      classed "p" "authors" [txt (authors e.authors)]);
    classed "p" "citation" [
      Option.fold ~none:El.void ~some:(fun journal ->
        link (url "/search" ["journal",journal]) journal) e.journal;
      txt (let rest = joined [
        Option.fold ~none:"" ~some:string_of_int e.year; empty e.publisher] in
        if e.journal <> None && rest <> "" then " · " ^ rest else rest)];
    (match e.abstract with
     | None | Some "" -> El.void
     | Some text -> classed "p" "abstract-snippet" [txt text]);
    classed "p" "identity" [code uid]]

let search params (result : C.page) =
  let journal = Option.value ~default:"" (List.assoc_opt "journal" params) in
  let clear = List.filter (fun (key, _) -> key <> "journal" && key <> "after")
    params in
  page "Browse & search" ([form params;
    (if journal = "" then El.void else classed "div" "selected-journal"
      [txt journal; link (url "/search" clear) "All journals"]);
    (if result.papers = [] then p "No matching papers."
     else el "div" [classed "div" "ui-toolbar"
       [p (Printf.sprintf "%d papers on this page" (List.length result.papers));
        Ui.checkbox ~id:"show-abstracts" ~label:"Show abstracts" ~checked:true];
       classed "div" "results" (List.map entry result.papers)])] @
    match result.next with None -> [] | Some after ->
      [classed "p" "pagination" [link (url "/search" (("after",after) ::
        List.remove_assoc "after" params)) "Next page →"]])

let metadata rows =
  classed "dl" "metadata" (List.concat_map (fun (label, value) ->
    if value = "" then [] else [el "dt" [txt label]; el "dd" [txt value]])
    rows)

let panel_link kind href label =
  El.v "a" ~at:[at "href" href; at "data-panel" kind;
    at "aria-controls" "paper-panel"; at "aria-expanded" "false"] [txt label]

let side_panel uid tei =
  let tab kind href label = El.v "a" ~at:[at "href" href;
    at "id" ("panel-tab-" ^ kind); at "data-panel" kind;
    at "role" "tab"; at "aria-selected" "false";
    at "aria-controls" "panel-body"; at "tabindex" "-1"] [txt label] in
  El.v "aside" ~at:[at "id" "paper-panel"; at "class" "paper-panel";
    at "aria-labelledby" "panel-heading"; at "hidden" ""] [
    classed "div" "panel-heading" [
      El.v "h2" ~at:[at "id" "panel-heading"] [txt "Paper views"];
      El.v "button" ~at:[at "id" "panel-close"; at "class" "panel-close";
        at "type" "button"; at "aria-label" "Close side panel"] [txt "Close ×"]];
    El.v "nav" ~at:[at "class" "panel-tabs"; at "role" "tablist";
      at "aria-label" "Paper views"] [
      Option.fold ~none:El.void ~some:(fun (a : Manifest.Artifact.t) ->
        tab "tei" (url "/read" ["uid",uid; "file",a.file]) "Read TEI") tei;
      tab "history" (url "/events" ["uid",uid]) "History"];
    classed "div" "panel-context" [
      El.v "span" ~at:[at "id" "panel-filename"] [];
      El.v "a" ~at:[at "id" "panel-open-page"; at "href" (url "/paper" ["uid",uid]);
        at "target" "_blank"; at "rel" "noopener"] [txt "Open full page ↗"]];
    El.v "div" ~at:[at "id" "panel-body"; at "class" "panel-body";
      at "role" "tabpanel"; at "tabindex" "0"] []]

let fragment title body =
  El.to_string ~doctype:false (El.v "div" ~at:[at "data-panel-content" "";
    at "aria-label" title] body)

let paper_preview (summary : Summary.t) =
  let b = summary.bibliography in
  let uid = Uid.to_string summary.uid in
  let title = Option.value b.title ~default:uid in
  let details = url "/paper" ["uid",uid;"view","full"] in
  page ~kind:"paper-page" title [
    classed "div" "actions" [classed "span" "identity" [code uid]];
    metadata ["Authors", authors b.authors;
      "Journal", empty b.journal; "Publisher", empty b.publisher;
      "Year", Option.fold ~none:"" ~some:string_of_int b.year;
      "Published", empty b.published; "Volume", empty b.volume;
      "Issue", empty b.issue; "Pages", empty b.pages;
      "ISSN", empty b.issn];
    Option.fold ~none:El.void ~some:(fun abstract ->
      el "section" [el "h2" [txt "Abstract"];
        classed "p" "abstract" [txt abstract]]) b.abstract;
    El.v "section" ~at:[at "id" "paper-loading";
      at "data-details-url" details] [
      El.v "p" ~at:[at "role" "status"]
        [txt "Catalogue metadata. Load complete paper details for files, \
          reading and source information."];
      link details "Load complete paper details"]]

let paper ?catalogue_status (paper : P.t) =
  let m = paper.manifest in
  let meta = paper.metadata in
  let value = P.Field.value in
  let string field = empty (value field) in
  let uid = Uid.to_string m.id in
  let title = Option.value (value meta.title) ~default:uid in
  let artifact_url path (a : Manifest.Artifact.t) =
    url path ["uid",uid;"file",a.file] in
  let pdf = paper.preferred_pdf in
  let tei = paper.preferred_tei in
  let artifact (a : Manifest.Artifact.t) =
    let provenance = [
      "Recorded", a.at; "From", empty a.from;
      "URL", empty a.url; "Origin", empty a.origin;
      "Strategy", empty a.strategy;
      "Status", Option.fold ~none:"" ~some:string_of_int a.status;
      "Confidence", Option.fold ~none:""
        ~some:Manifest.Confidence.to_string a.confidence;
      "SHA-256", empty a.sha256;
      "Bytes", Int64.to_string a.bytes] in
    [Ui.cell a.file [
      El.v "a" ~at:[at "class" "file-name";
        at "href" (artifact_url "/artifact" a)] [txt a.file];
      el "details" [el "summary" [txt "Details"]; metadata provenance]];
      el "td" [Ui.badge (String.uppercase_ascii (Manifest.Format.to_string a.format))];
      el "td" [txt (joined [Manifest.Source.to_string a.source;
        empty a.converter])];
      el "td" [txt (Option.fold ~none:"" ~some:Manifest.Version.to_string
        a.version)];
      El.v "td" ~at:[at "class" "numeric";
        at "data-sort-value" (Int64.to_string a.bytes);
        at "title" (Int64.to_string a.bytes ^ " bytes")] [txt (size a.bytes)];
      classed "td" "file-actions"
        (if a.format = Manifest.Format.Tei then
          [panel_link "tei" (artifact_url "/read" a) "Read TEI"]
         else if a.format = Manifest.Format.Pdf then
          [link (artifact_url "/pdf" a) "View PDF"] else [])] in
  let abstract = string meta.abstract in
  let evidence label show field =
    List.map (fun (e : _ P.evidence) -> [
      el "td" [txt label]; el "td" [txt (show e.value)];
      el "td" [txt (P.source_name e.source)]]) (P.Field.candidates field) in
  let sources = List.concat [
    evidence "Title" Fun.id meta.title;
    evidence "Authors" authors meta.authors;
    evidence "Journal" Fun.id meta.journal;
    evidence "Publisher" Fun.id meta.publisher;
    evidence "Published" Fun.id meta.published;
    evidence "Year" string_of_int meta.year;
    evidence "Abstract" Fun.id meta.abstract;
    evidence "Volume" Fun.id meta.volume; evidence "Issue" Fun.id meta.issue;
    evidence "Pages" Fun.id meta.pages; evidence "ISSN" Fun.id meta.issn;
    evidence "DOI" Fun.id meta.doi; evidence "PMCID" Fun.id meta.pmcid;
    evidence "PMID" Fun.id meta.pmid] in
  let errors = List.filter_map (fun (d : P.document) ->
    match d.bibliography with Ok _ -> None | Error e ->
      Some (el "li" [code d.artifact.file; txt (": " ^ e)]))
    paper.documents in
  let body = [
    Option.fold ~none:El.void ~some:(fun s -> classed "p" "status" [txt s])
      catalogue_status;
    classed "div" "actions" [classed "span" "identity" [code uid];
      Option.fold ~none:El.void ~some:(fun a ->
        panel_link "tei" (artifact_url "/read" a) "Read TEI") tei;
      Option.fold ~none:El.void ~some:(fun _ ->
        link "#pdf" "PDF preview") pdf;
      panel_link "history" (url "/events" ["uid",uid]) "History"];
    metadata [
      "Authors", Option.fold ~none:"" ~some:authors (value meta.authors);
      "Journal", string meta.journal; "Publisher", string meta.publisher;
      "Year", Option.fold ~none:"" ~some:string_of_int (value meta.year);
      "Published", string meta.published; "Volume", string meta.volume;
      "Issue", string meta.issue; "Pages", string meta.pages;
      "ISSN", string meta.issn; "DOI", string meta.doi;
      "PMCID", string meta.pmcid; "PMID", string meta.pmid;
      "Updated", m.updated];
    (if abstract = "" then El.void else el "section" [
      el "h2" [txt "Abstract"]; classed "p" "abstract" [txt abstract]]);
    el "details" [el "summary" [txt "Metadata sources and alternatives"];
      Ui.table ["Field",false; "Value (preferred first)",false; "Source",false]
        sources;
      (if errors = [] then El.void else el "ul" errors)];
    el "h2" [txt "Files"];
    (if m.artifacts = [] then p "No files available." else
      Ui.table ["File",false;"Format",false;"Source / converter",false;
        "Version",false;"Size",true;"Open",false]
        (List.map artifact m.artifacts));
    Option.fold ~none:El.void ~some:(fun a ->
      El.v "section" ~at:[at "id" "pdf"] [
        classed "div" "preview-heading" [el "h2" [txt "PDF preview"];
          link (artifact_url "/artifact" a) "Open PDF"];
        El.v "iframe" ~at:[at "src" (artifact_url "/artifact" a);
          at "title" ("PDF: " ^ title); at "loading" "lazy"] []]) pdf] @
    (if m.corrections = [] then [] else [
      el "h2" [txt "Corrections"];
      el "ul" (List.map (fun (c : Manifest.Correction.t) ->
        el "li" [txt (Manifest.Field.to_string c.field ^ ": " ^ c.value);
          el "small" [txt (" · " ^ c.by ^ " · " ^ c.at)];
          nonempty (empty c.note)]) m.corrections)]) @
    (if m.acquisitions_failed = [] then [] else [
      el "h2" [txt "Acquisition refusals"];
      el "ul" (List.map (fun (r : Manifest.Refusal.t) ->
        el "li" [txt (r.strategy ^ ": " ^ r.reason);
          classed "p" "status" [txt (Printf.sprintf "%d attempts · %s%s%s"
            r.attempts r.at
            (Option.fold ~none:"" ~some:(fun n -> Printf.sprintf " · HTTP %d" n)
              r.status)
            (if r.permanent then " · Permanent" else ""))]])
        m.acquisitions_failed)]) @
    (if m.conversions_failed = [] then [] else [
      el "h2" [txt "Conversion failures"];
      el "ul" (List.map (fun (f : Manifest.Failure.t) ->
        el "li" [txt (Manifest.Route.to_string f.route ^ ": " ^ f.reason);
          classed "p" "status" [txt (Printf.sprintf "%d attempts · %s%s"
            f.attempts f.at (if f.permanent then " · Permanent" else ""))]])
        m.conversions_failed)]) in
  page ~kind:"paper-page" title [classed "div" "paper-layout" [
    classed "section" "paper-detail" body; side_panel uid tei]]

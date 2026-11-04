open View
module Ui = Taposaur_ui
module J = Taposaur_jobs.Inspect
let page ~jobs =
  let section id title body = Ui.node "section" ["class","activity-section"]
    [Ui.node "div" ["class","activity-heading"]
      [el "h2" [txt title];
       Ui.node "span" ["class","activity-count";"data-count-for",id] []];
     body] in
  View.page ~kind:"activity-page" "Activity" [
    Ui.state_sprite;
    Ui.node "div" ["id","activity";"data-activity-url","/activity?format=json"]
      [Ui.node "div" ["class","activity-status"]
        [Ui.node "span" ["class","live-dot";"aria-hidden","true"] [];
         Ui.node "span" ["id","activity-state"] [txt "Connecting"];
         Ui.node "span" ["id","activity-updated";"class","activity-updated"] []];
       Ui.node "div" ["class","ui-metrics activity-metrics"]
         [Ui.metric ~label:"Queued" (Ui.node "span" ["data-metric","queued"] [txt "0"]);
          Ui.metric ~label:"Running" (Ui.node "span" ["data-metric","running"] [txt "0"]);
          Ui.metric ~label:"Failed in view" (Ui.node "span" ["data-metric","failed"] [txt "0"]);
          Ui.metric ~label:"Pipelines in view" (Ui.node "span" ["data-metric","papers"] [txt "0"])];
       section "jobs" "Job pipelines"
         (if jobs then Ui.node "div" []
             [Ui.node "ol" ["id","activity-jobs";"class","pipelines";
                "aria-live","polite"] [];
              Ui.node "p" ["class","activity-more"]
                [Ui.node "button" ["type","button";"id","activity-older"]
                  [txt "Load older"]]]
          else p "Job events are not shown: start the server with --jobs-db.");
       Ui.node "details" ["class","activity-section";"id","activity-web-section"]
         [el "summary" [txt "Website requests";
            Ui.node "span" ["class","activity-count";"data-count-for","web"] []];
          Ui.node "div" ["id","activity-web"]
            [Ui.table ~class_name:"activity-table"
              ["Time",false;"Page",false;"Detail",false] []]];
       p "The page opens on the jobs still running and the most recently \
          touched ones, then polls every two seconds for new events only. \
          Load older pages back through the event log. Website requests \
          live in the running server's memory."]]
let quote s = match Jsont_bytesrw.encode_string Jsont.string s with
  | Ok s -> s | Error e -> failwith e
let stamp seconds =
  let t = Unix.gmtime seconds in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (t.tm_year + 1900)
    (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec
let clock t = let ((_, ((h, m, s), _))) = Ptime.to_date_time t in
  Printf.sprintf "%02d:%02d:%02d" h m s
let field k v = quote k ^ ":" ^ quote v
let optional k = function None -> [] | Some v -> [field k v]
let obj fields = "{" ^ String.concat "," fields ^ "}"
let line ~time ~event ~detail ~href = obj
  ([field "time" time; field "event" event; field "detail" detail]
   @ optional "href" href)

(* One attempt's events become "claimed 12:06:40 → succeeded 12:06:49 (9 s)"
   when the batch holds both ends, otherwise each kind with its time. *)
let timeline events =
  let attempts = List.sort_uniq compare (List.map Duraml.Event.attempt events) in
  String.concat "; " (List.map (fun attempt ->
    let own = List.filter (fun e -> Duraml.Event.attempt e = attempt) events in
    let kind e = Duraml.Event.kind e and at e = clock (Duraml.Event.time e) in
    let claimed = List.find_opt (fun e -> kind e = "claimed") own in
    let settled = List.find_opt (fun e -> List.mem (kind e)
      ["succeeded"; "failed"; "retry_scheduled"; "lease_expired"]) own in
    let body = match claimed, settled with
      | Some c, Some s ->
          let span = Ptime.Span.to_float_s
            (Ptime.diff (Duraml.Event.time s) (Duraml.Event.time c)) in
          Printf.sprintf "claimed %s \xe2\x86\x92 %s %s (%.0f s)%s" (at c)
            (kind s) (at s) span
            (if Duraml.Event.detail s = "" then ""
             else " " ^ Duraml.Event.detail s)
      | _ -> String.concat ", " (List.map (fun e ->
          kind e ^ " " ^ at e ^
          (if Duraml.Event.detail e = "" then "" else " " ^ Duraml.Event.detail e))
          own) in
    if attempt = 0 then body else Printf.sprintf "attempt %d: %s" attempt body)
    attempts)

let attempts events =
  let kinds = List.sort_uniq compare (List.map Duraml.Event.attempt events) in
  List.filter_map (fun attempt ->
    if attempt = 0 then None else
    let own = List.filter (fun e -> Duraml.Event.attempt e = attempt) events in
    let claimed = List.find_opt (fun e -> Duraml.Event.kind e = "claimed") own in
    let settled = List.find_opt (fun e -> List.mem (Duraml.Event.kind e)
      ["succeeded"; "failed"; "retry_scheduled"; "lease_expired"]) own in
    let time e = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 (Duraml.Event.time e) in
    let seconds = match claimed, settled with
      | Some c, Some s -> Some (Ptime.Span.to_float_s
          (Ptime.diff (Duraml.Event.time s) (Duraml.Event.time c)))
      | _ -> None in
    Some (obj ([field "attempt" (string_of_int attempt)]
      @ optional "started" (Option.map time claimed)
      @ optional "ended" (Option.map time settled)
      @ optional "kind" (Option.map Duraml.Event.kind settled)
      @ optional "detail" (Option.bind settled (fun s ->
          if Duraml.Event.detail s = "" then None
          else Some (Duraml.Event.detail s)))
      @ (match seconds with None -> []
         | Some s -> [quote "seconds" ^ ":" ^ Printf.sprintf "%.0f" s]))))
    kinds

(* Running jobs whose last event is older than the snapshot window still
   belong on the page, without a timeline. *)
let running_summary (running : J.job list) seen =
  List.filter_map (fun (j : J.job) ->
    if List.mem j.id seen then None else
    Some (obj ([field "id" j.id; field "queue" j.queue; field "state" j.state;
      field "attempt" (string_of_int j.attempt);
      field "updated" (Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 j.updated);
      field "timeline" ""; quote "attempts" ^ ":[]";
      field "href" (url "/job" ["id", j.id])]
      @ optional "parent" j.parent @ optional "uid" j.uid
      @ optional "paper" (J.paper_url j)))) running

let jobs_summary jobs =
  let seen = Hashtbl.create 16 and order = ref [] in
  List.iter (fun (e, j) ->
    let id = Duraml.Id.to_string (Duraml.Event.job e) in
    (match Hashtbl.find_opt seen id with
     | None -> Hashtbl.replace seen id (j, [e]); order := id :: !order
     | Some (j, events) -> Hashtbl.replace seen id (j, e :: events))) jobs;
  List.rev_map (fun id ->
    let (j : J.job option), events = Hashtbl.find seen id in
    let events = List.rev events in
    let latest = List.hd (List.rev events) in
    let enqueued = List.find_opt (fun e -> Duraml.Event.kind e = "enqueued")
      events in
    obj ([field "id" id;
      field "updated" (Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0
        (Duraml.Event.time latest));
      field "timeline" (timeline events);
      quote "attempts" ^ ":[" ^ String.concat "," (attempts events) ^ "]";
      field "href" (url "/job" ["id", id])]
     @ optional "enqueued" (Option.map (fun e ->
         Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 (Duraml.Event.time e))
         enqueued)
     @ (match j with
        | None -> [field "queue" "unknown"; field "state" "unknown";
            field "attempt" (string_of_int (Duraml.Event.attempt latest))]
        | Some j -> [field "queue" j.queue; field "state" j.state;
            field "attempt" (string_of_int j.attempt)]
            @ optional "parent" j.parent @ optional "uid" j.uid
            @ optional "paper" (J.paper_url j)
            @ (match j.review with
               | None -> []
               | Some r -> [field "review" (Printf.sprintf "%s #%d" r.slug
                   r.snapshot); field "review_href" (url "/review-snapshot"
                   ["id", string_of_int r.snapshot])])))) !order

let feed ~web ~web_cursor ~jobs ~running ~jobs_cursor ~older ~counts =
  let web = List.map (fun (e : Activity.entry) ->
    line ~time:(stamp e.time) ~event:(Printf.sprintf "%s %d" e.page e.status)
      ~detail:(Printf.sprintf "%.1f ms%s" e.elapsed_ms
        (if e.query = [] then "" else " " ^ String.concat "&"
          (List.map (fun (k, v) -> k ^ "=" ^ v) e.query)))
      ~href:None) web in
  let seen = List.map (fun (e, _) -> Duraml.Id.to_string (Duraml.Event.job e))
    jobs in
  let queued, running_count = counts in
  Printf.sprintf
    {|{"cursors":{"web":%d,"jobs":%Ld%s},"counts":{"queued":%d,"running":%d},"jobs":[%s],"web":[%s]}|}
    web_cursor jobs_cursor
    (match older with None -> "" | Some b -> Printf.sprintf {|,"before":%Ld|} b)
    queued running_count
    (String.concat "," (jobs_summary jobs @ running_summary running seen))
    (String.concat "," web)

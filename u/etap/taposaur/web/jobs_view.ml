open View
module Ui = Taposaur_ui
module J = Taposaur_jobs.Inspect
let stamp t = Ptime.to_rfc3339 ~frac_s:0 ~tz_offset_s:0 t
let state_badge state = Ui.node "span" ["class","state-cell"]
  [Ui.state_icon state; txt state]
let paper_cell (j : J.job) = match j.uid, J.paper_url j with
  | Some uid, Some href -> [link href uid]
  | _ -> [txt "Unattributed"]
let review_cell (j : J.job) = match j.review with
  | None -> [txt ""]
  | Some r -> [link (url "/review-snapshot" ["id",string_of_int r.snapshot])
      (Printf.sprintf "%s · snapshot %d" r.slug r.snapshot)]
let job_row (j : J.job) =
  [Ui.cell j.id [link (url "/job" ["id",j.id]) j.id];
   Ui.cell j.queue [txt j.queue];
   Ui.cell j.state [state_badge j.state];
   Ui.cell (string_of_int j.attempt) [txt (string_of_int j.attempt)];
   Ui.cell (Option.value ~default:"" j.uid) (paper_cell j);
   Ui.cell "" (review_cell j);
   Ui.cell (stamp j.updated) [txt (stamp j.updated)];
   Ui.cell (Option.value ~default:"" j.outcome)
     [txt (Option.value ~default:"" j.outcome)]]
let index params counts jobs =
  let value name = Option.value ~default:"" (List.assoc_opt name params) in
  let filters = List.filter (fun (k,_) -> k <> "after") params in
  let totals = Ui.table ["Queue",false;"Queued",true;"Running",true;
      "Done",true;"Failed",true;"Cancelled",true]
    (List.map (fun (c : J.counts) ->
      let n state count = Ui.cell (string_of_int count)
        [link (url "/jobs" ["queue",c.queue;"state",state])
          (View.count count)] in
      [Ui.cell c.queue [link (url "/jobs" ["queue",c.queue]) c.queue];
       n "queued" c.queued; n "running" c.running; n "done" c.done_;
       n "failed" c.failed; n "cancelled" c.cancelled]) counts) in
  page ~kind:"jobs-page" "Acquisition and conversion jobs" [
    Ui.state_sprite;
    p "Durable work recorded by the CLI and worker processes. This page \
       reads the queue database and changes nothing. Counts at 100,000 \
       are lower bounds.";
    totals;
    Ui.node "form" ["method","get";"action","/jobs"]
      [Ui.node "div" ["class","search-line"]
        [Ui.input ~name:"queue" ~label:"Queue" ~kind:"text"
           ~value:(value "queue");
         Ui.select ~name:"state" ~label:"State" ~value:(value "state")
           ["","Any";"queued","Queued";"running","Running";"done","Done";
            "failed","Failed";"cancelled","Cancelled"];
         el "button" [txt "Filter"]]];
    (if jobs = [] then p "No matching jobs." else
      Ui.table ["Job",true;"Queue",false;"State",false;"Attempt",true;
        "Paper",false;"Review",false;"Updated",false;"Outcome",false]
        (List.map job_row jobs));
    (match List.rev jobs with
     | last :: _ when List.length jobs >= 100 ->
         el "p" [link (url "/jobs" (("after",last.id)::filters))
           "Later jobs →"]
     | _ -> Htmlit.El.void)]
module A = Taposaur_audit
let details_codec = Jsont.Object.as_string_map Jsont.string
let details (e : A.event) =
  match Jsont_bytesrw.decode_string details_codec e.details with
  | Ok map -> List.rev (Jsont.String_map.fold (fun k v acc -> (k, v) :: acc) map [])
  | Error _ -> ["details", e.details]
let clock_of seconds =
  let t = Unix.gmtime seconds in
  Printf.sprintf "%02d:%02d:%02d" t.tm_hour t.tm_min t.tm_sec
(* Audit phases and queue transitions share the job state icons, so a
   page reads the same way from the activity tree down to one operation. *)
let phase_icon phase = Ui.state_icon ~label:(A.phase_name phase) (match phase with
  | A.Started -> "running" | Succeeded -> "done" | Failed -> "failed"
  | Cancelled -> "cancelled" | Notice -> "notice")
let transition_icon kind = Ui.state_icon ~label:kind (match kind with
  | "enqueued" | "retry_scheduled" -> "queued" | "claimed" -> "running"
  | "succeeded" -> "done" | "failed" | "lease_expired" -> "failed"
  | "cancelled" -> "cancelled" | _ -> "unknown")
let chips pairs = Ui.node "span" ["class","detail-chips"]
  (List.map (fun (k, v) -> Ui.node "span" ["class","detail-chip"]
    [el "b" [txt k]; txt " "; txt v]) pairs)

(* Operations form a tree through their parent identifiers. An operation
   is one started record plus its terminal record, or a single notice. *)
type operation = { records : A.event list; children : operation list }
let operations ~job (events : A.event list) =
  let mine (e : A.event) = List.assoc_opt "job" (details e) = Some job in
  let by_operation = Hashtbl.create 32 in
  List.iter (fun (e : A.event) ->
    Hashtbl.replace by_operation e.operation
      (e :: Option.value ~default:[] (Hashtbl.find_opt by_operation e.operation)))
    events;
  let records op = List.sort (fun (a : A.event) (b : A.event) ->
    compare a.id b.id) (Hashtbl.find by_operation op) in
  let parent_of op = List.find_map (fun (e : A.event) -> e.parent) (records op) in
  let ops = Hashtbl.fold (fun op _ acc -> op :: acc) by_operation [] in
  let rec build op = { records = records op;
    children = List.filter_map (fun child ->
      if parent_of child = Some op then Some (build child) else None) ops
      |> List.sort (fun a b -> compare (List.hd a.records).A.id
           (List.hd b.records).A.id) } in
  let roots = List.filter (fun op -> List.exists mine (records op) &&
    (List.hd (records op)).A.action <> "queue.event") ops in
  let roots = List.filter (fun op -> not (List.exists (fun other ->
    other <> op && parent_of op = Some other &&
    List.exists mine (records other)) roots)) roots in
  List.map build (List.sort (fun a b ->
    compare (List.hd (records a)).A.id (List.hd (records b)).A.id) roots)
let rec render_operation { records; children } =
  let first = List.hd records and last = List.hd (List.rev records) in
  let span = if List.length records > 1 then
      Printf.sprintf "%s to %s" (clock_of first.A.time) (clock_of last.A.time)
    else clock_of first.time in
  let detail (e : A.event) = List.filter (fun (k, _) ->
    not (List.mem k ["job"; "attempt"])) (details e) in
  Ui.node "li" ["class","operation"]
    [Ui.node "div" ["class","operation-line"]
      [phase_icon last.phase; el "code" [txt first.action];
       Ui.node "span" ["class","operation-time"] [txt span];
       chips (List.concat_map detail records)];
     (if children = [] then Htmlit.El.void
      else Ui.node "ul" ["class","operation-children"]
        (List.map render_operation children))]

let job (j : J.job) children audit =
  let row label value = el "tr" [el "th" [txt label]; el "td" value] in
  let timeline = List.filter (fun (e : A.event) ->
    e.action = "queue.event" && List.assoc_opt "job" (details e) = Some j.id)
    audit |> List.rev in
  let operations = operations ~job:j.id audit in
  let key = match j.key with Some k -> k | None -> "none" in
  let spec = j.spec in
  page ~kind:"jobs-page" ("Job " ^ j.id) [
    Ui.state_sprite;
    el "p" [link "/jobs" "← All jobs"];
    el "table" [
      row "Queue" [link (url "/jobs" ["queue",j.queue]) j.queue];
      row "State" [state_badge j.state];
      row "Attempt" [txt (string_of_int j.attempt)];
      row "Key" [el "code" [txt key]];
      row "Lane" [txt (if j.lane = "" then "unnamed" else j.lane)];
      row "Staged by" (match j.parent with
        | None -> [txt "Submitted directly"]
        | Some p -> [link (url "/job" ["id",p]) ("Job " ^ p)]);
      row "Paper" (paper_cell j);
      row "Review" (review_cell j);
      row "Created" [txt (stamp j.created)];
      row "Updated" [txt (stamp j.updated)];
      row "Available or lease deadline" [txt (stamp j.available)];
      row "Outcome" [txt (Option.value ~default:"none" j.outcome)];
      row "Kind and version" [txt (Printf.sprintf "%s v%d"
        (Duraml.Spec.kind spec) (Duraml.Spec.version spec))]];
    el "details" [el "summary" [txt "Request payload"];
      el "pre" [txt (Duraml.Spec.payload spec)]];
    el "h2" [txt "Successors"];
    (if children = [] then p "No successors were staged by this job." else
      Ui.table ["Job",true;"Queue",false;"State",false;"Attempt",true;
        "Paper",false;"Review",false;"Updated",false;"Outcome",false]
        (List.map job_row children));
    el "h2" [txt "Queue timeline"];
    (if timeline = [] then p "No bridged queue events for this job. Run \
        taposaur jobs audit-sync, or work with --sync, to record them."
     else Ui.table ["Time",false;"Transition",false;"Attempt",true;"Detail",false]
       (List.map (fun (e : A.event) ->
         let d = details e in
         let get k = Option.value ~default:"" (List.assoc_opt k d) in
         [Ui.cell (Printf.sprintf "%.6f" e.time) [txt (clock_of e.time)];
          Ui.cell (get "kind") [Ui.node "span" ["class","state-cell"]
            [transition_icon (get "kind"); txt (get "kind")]];
          Ui.cell (get "attempt") [txt (get "attempt")];
          Ui.cell (get "detail") [txt (get "detail")]]) timeline));
    el "h2" [txt "Operations"];
    p "Every audited operation run by this job's handlers, nested as it \
       happened. Notes on a handler record explain retries and failures; \
       transfer and conversion results carry HTTP statuses.";
    (if operations = [] then p "No audited operations name this job. \
        Handlers stamp their operations with the job identifier, so work \
        processed before that was recorded shows nothing here."
     else Ui.node "ul" ["class","operations"]
       (List.map render_operation operations))]

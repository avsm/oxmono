(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)

open Taposaur
module Q = Duraml_client.Queue
module Completion = Duraml_client.Completion
module A = Taposaur_archive
module Accept = Taposaur_accept
module Download = Taposaur_download

let version = 1
let get_ok what = function Ok v -> v | Error e -> failwith (what ^ ": " ^ e)

(* Payloads *)

module Attribution = struct
  type t = { slug : string; snapshot : int }
  let jsont =
    Jsont.Object.map ~kind:"Attribution" (fun slug snapshot -> {slug; snapshot})
    |> Jsont.Object.mem "slug" Jsont.string ~enc:(fun a -> a.slug)
    |> Jsont.Object.mem "snapshot" Jsont.int ~enc:(fun a -> a.snapshot)
    |> Jsont.Object.finish
end

module Artifact = struct
  type t = {
    file : string; format : Manifest.Format.t; sha256 : string; bytes : int64;
  }
  let jsont =
    Jsont.Object.map ~kind:"Artifact" (fun file format sha256 bytes ->
      {file; format; sha256; bytes})
    |> Jsont.Object.mem "file" Jsont.string ~enc:(fun a -> a.file)
    |> Jsont.Object.mem "format" Manifest.Format.jsont ~enc:(fun a -> a.format)
    |> Jsont.Object.mem "sha256" Jsont.string ~enc:(fun a -> a.sha256)
    |> Jsont.Object.mem "bytes" Jsont.int64 ~enc:(fun a -> a.bytes)
    |> Jsont.Object.finish
  let of_manifest (a : Manifest.Artifact.t) = Option.map (fun sha256 ->
    {file = a.file; format = a.format; sha256; bytes = a.bytes}) a.sha256
end

(* Publisher strategies come from the download library. The [oa] strategy
   names its open-access and CORE fallbacks for publishers without one. *)
let strategies = Download.strategies @ ["oa"]
let check_strategy strategy =
  if not (List.mem strategy strategies) then
    invalid_arg ("Unknown acquisition strategy " ^ strategy)
let check_route = function
  | Manifest.Route.S2orc -> invalid_arg "The s2orc route has no conversion queue"
  | Grobid | Pub2tei -> ()
(* Decoders refuse what the key functions refuse, so a malformed stored
   payload fails as invalid_payload instead of raising in a handler. *)
let refuse f x = try f x with Invalid_argument e -> Jsont.Error.msg Jsont.Meta.none e
(* Duraml keys are at most 256 bytes. Planning reports a longer key as an
   invalid member instead of failing inside a worker. *)
let check_key key =
  if String.length key > 256 then invalid_arg "Job key exceeds 256 bytes";
  key

module Acquisition = struct
  let strategies = strategies
  type request = {
    uid : Uid.t; strategy : string; revision : int; publisher : string;
    review : Attribution.t option; requested_by : string;
  }
  type result = {
    artifacts : Artifact.t list; attempts : (string * string) list;
  }
  let validate r =
    check_strategy r.strategy;
    if r.revision < 1 then invalid_arg "Strategy revision must be positive";
    r
  let key r =
    ignore (validate r);
    check_key (Printf.sprintf "acquire:%s:%s:v%d" (Uid.to_string r.uid)
      r.strategy r.revision)
  let queue ~strategy = check_strategy strategy; "acquire." ^ strategy
  let request_jsont =
    Jsont.Object.map ~kind:"Acquisition"
      (fun uid strategy revision publisher review requested_by ->
        refuse validate
          {uid; strategy; revision; publisher; review; requested_by})
    |> Jsont.Object.mem "uid" Uid.jsont ~enc:(fun r -> r.uid)
    |> Jsont.Object.mem "strategy" Jsont.string ~enc:(fun r -> r.strategy)
    |> Jsont.Object.mem "revision" Jsont.int ~enc:(fun r -> r.revision)
    |> Jsont.Object.mem "publisher" Jsont.string ~enc:(fun r -> r.publisher)
    |> Jsont.Object.opt_mem "review" Attribution.jsont ~enc:(fun r -> r.review)
    |> Jsont.Object.mem "requested_by" Jsont.string
         ~enc:(fun r -> r.requested_by)
    |> Jsont.Object.finish
  let attempt_jsont =
    Jsont.Object.map ~kind:"Attempt" (fun source message -> source, message)
    |> Jsont.Object.mem "source" Jsont.string ~enc:fst
    |> Jsont.Object.mem "message" Jsont.string ~enc:snd
    |> Jsont.Object.finish
  let result_jsont =
    Jsont.Object.map ~kind:"Acquired" (fun artifacts attempts ->
      {artifacts; attempts})
    |> Jsont.Object.mem "artifacts" (Jsont.list Artifact.jsont)
         ~enc:(fun r -> r.artifacts)
    |> Jsont.Object.mem "attempts" (Jsont.list attempt_jsont)
         ~enc:(fun r -> r.attempts)
    |> Jsont.Object.finish
end

module Conversion = struct
  type request = {
    uid : Uid.t; input : Artifact.t; route : Manifest.Route.t;
    converter : string;
  }
  type result = { output : Artifact.t; publisher : string option }
  let validate r = check_route r.route; r
  let key r =
    ignore (validate r);
    check_key (Printf.sprintf "convert:%s:%s:%s:%s" (Uid.to_string r.uid)
      (Manifest.Route.to_string r.route) r.input.Artifact.sha256 r.converter)
  let queue route = check_route route; "convert." ^ Manifest.Route.to_string route
  let request_jsont =
    Jsont.Object.map ~kind:"Conversion" (fun uid input route converter ->
      refuse validate {uid; input; route; converter})
    |> Jsont.Object.mem "uid" Uid.jsont ~enc:(fun r -> r.uid)
    |> Jsont.Object.mem "input" Artifact.jsont ~enc:(fun r -> r.input)
    |> Jsont.Object.mem "route" Manifest.Route.jsont ~enc:(fun r -> r.route)
    |> Jsont.Object.mem "converter" Jsont.string ~enc:(fun r -> r.converter)
    |> Jsont.Object.finish
  let result_jsont =
    Jsont.Object.map ~kind:"Converted" (fun output publisher ->
      {output; publisher})
    |> Jsont.Object.mem "output" Artifact.jsont ~enc:(fun r -> r.output)
    |> Jsont.Object.opt_mem "publisher" Jsont.string ~enc:(fun r -> r.publisher)
    |> Jsont.Object.finish
end

module Refresh = struct
  type request = { uid : Uid.t; manifest_sha256 : string }
  type result = { index : string; catalogue : string }
  let key r =
    Printf.sprintf "refresh:%s:%s" (Uid.to_string r.uid) r.manifest_sha256
  let queue = "catalogue.refresh"
  let request_jsont =
    Jsont.Object.map ~kind:"Refresh" (fun uid manifest_sha256 ->
      {uid; manifest_sha256})
    |> Jsont.Object.mem "uid" Uid.jsont ~enc:(fun r -> r.uid)
    |> Jsont.Object.mem "manifest_sha256" Jsont.string
         ~enc:(fun r -> r.manifest_sha256)
    |> Jsont.Object.finish
  let result_jsont =
    Jsont.Object.map ~kind:"Refreshed" (fun index catalogue ->
      {index; catalogue})
    |> Jsont.Object.mem "index" Jsont.string ~enc:(fun r -> r.index)
    |> Jsont.Object.mem "catalogue" Jsont.string ~enc:(fun r -> r.catalogue)
    |> Jsont.Object.finish
end

(* Queues *)

module Queues = struct
  type t = {
    store : Duraml.plain;
    acquisitions : (string * (Acquisition.request, Acquisition.result) Q.t) list;
    grobid : (Conversion.request, Conversion.result) Q.t;
    pub2tei : (Conversion.request, Conversion.result) Q.t;
    refresh : (Refresh.request, Refresh.result) Q.t;
  }
  let bind store ~name ~kind request result =
    Q.v store ~name ~kind ~version ~request:(Duraml_jsont.codec request)
      ~result:(Duraml_jsont.codec result)
  let v store =
    let store = (store :> Duraml.plain) in
    let acquisition strategy = strategy, bind store
      ~name:(Acquisition.queue ~strategy) ~kind:"acquisition"
      Acquisition.request_jsont Acquisition.result_jsont in
    let conversion route = bind store ~name:(Conversion.queue route)
      ~kind:"conversion" Conversion.request_jsont Conversion.result_jsont in
    {store; acquisitions = List.map acquisition strategies;
     grobid = conversion Grobid; pub2tei = conversion Pub2tei;
     refresh = bind store ~name:Refresh.queue ~kind:"refresh"
       Refresh.request_jsont Refresh.result_jsont}
  let store t = t.store
  let acquisition t ~strategy = match List.assoc_opt strategy t.acquisitions with
    | Some q -> q
    | None -> invalid_arg ("Unknown acquisition strategy " ^ strategy)
  let conversion t = function
    | Manifest.Route.Grobid -> t.grobid
    | Pub2tei -> t.pub2tei
    | S2orc -> invalid_arg "The s2orc route has no conversion queue"
  let refresh t = t.refresh
  let names =
    List.map (fun strategy -> Acquisition.queue ~strategy) strategies
    @ [Conversion.queue Grobid; Conversion.queue Pub2tei; Refresh.queue]
end

(* Planning and submission *)

module Plan = struct
  type target = Fulltext | Tei
  type decision =
    | Acquire of Acquisition.request
    | Convert of Conversion.request
    | Held
    | Refused of string
    | Unsupported of string
    | Unconfigured of string
    | Invalid of string
  type entry = { doi : string; publisher : string; decision : decision }
  type summary = {
    total : int; acquire : int; convert : int; held : int; refused : int;
    unsupported : int; unconfigured : int; invalid : int;
  }
  let summarize entries =
    List.fold_left (fun s e ->
      let s = {s with total = s.total + 1} in
      match e.decision with
      | Acquire _ -> {s with acquire = s.acquire + 1}
      | Convert _ -> {s with convert = s.convert + 1}
      | Held -> {s with held = s.held + 1}
      | Refused _ -> {s with refused = s.refused + 1}
      | Unsupported _ -> {s with unsupported = s.unsupported + 1}
      | Unconfigured _ -> {s with unconfigured = s.unconfigured + 1}
      | Invalid _ -> {s with invalid = s.invalid + 1})
      {total = 0; acquire = 0; convert = 0; held = 0; refused = 0;
       unsupported = 0; unconfigured = 0; invalid = 0} entries

  let digested format artifacts = List.find_map (fun (a : Manifest.Artifact.t) ->
    if a.format = format then Artifact.of_manifest a else None) artifacts
  let decide ?catalog ?archive ?(skip_refused = false) ~configured ~converters
      ~target ~revision ~requested_by ~review (o : Taposaur_review.observation) =
    let publisher = String.trim
      (Option.value ~default:"" o.bibliography.publisher) in
    let publisher = if publisher <> "" then publisher else
      Option.value ~default:"" (Download.prefix_publisher o.doi) in
    match Uid.of_input o.doi with
    | Error e -> Invalid e
    | Ok uid ->
        (* Manifests live on the archive filesystem, so they are read only
           when the catalogue has no row or a digest is needed. *)
        let manifest = lazy (Option.map (fun archive ->
          Option.map A.manifest (A.find archive uid)) archive) in
        let artifacts = lazy (Option.map (fun m -> match m with
          | None -> [] | Some (m : Manifest.t) -> m.artifacts)
          (Lazy.force manifest)) in
        let refused strategy =
          if not skip_refused then None else
          match Lazy.force manifest with
          | Some (Some (m : Manifest.t)) ->
              List.find_map (fun (r : Manifest.Refusal.t) ->
                if r.strategy = strategy && r.permanent
                then Some (Printf.sprintf "%s refused on %s: %s" strategy
                  r.at r.reason) else None) m.acquisitions_failed
          | _ -> None in
        let indexed = Option.bind catalog (fun catalog ->
          Taposaur_catalog.find catalog uid) in
        let frozen format = match format with
          | Manifest.Format.Pdf -> o.holdings.pdf
          | Xml -> o.holdings.xml
          | Tei -> o.holdings.tei
          | Json -> false in
        (* With a catalogue, an unindexed member is planned from its frozen
           holdings rather than a per-paper archive lookup, which on the
           archive filesystem costs tens of milliseconds each. *)
        let has format = match indexed, catalog with
          | Some (s : Summary.t), _ -> (match format with
              | Manifest.Format.Pdf -> s.holdings.pdf
              | Xml -> s.holdings.xml | Tei -> s.holdings.tei
              | Json -> s.holdings.s2orc)
          | None, Some _ -> frozen format
          | None, None -> match Lazy.force artifacts with
            | Some artifacts -> List.exists (fun (a : Manifest.Artifact.t) ->
                a.format = format) artifacts
            | None -> frozen format in
        match target with
        | Fulltext when has Pdf || has Xml -> Held
        | Fulltext when publisher = "" -> Unsupported "no publisher"
        | Fulltext -> (match Download.strategy_name publisher with
          | None when List.mem "oa" configured ->
              (match refused "oa" with
               | Some why -> Refused why
               | None -> Acquire {uid; strategy = "oa"; revision; publisher;
                   review = Some review; requested_by})
          | None -> Unsupported ("no implemented strategy for " ^ publisher)
          | Some strategy when not (List.mem strategy configured) ->
              Unconfigured strategy
          | Some strategy -> match refused strategy with
            | Some why -> Refused why
            | None -> Acquire {uid; strategy; revision; publisher;
                review = Some review; requested_by})
        | Tei when has Tei -> Held
        | Tei -> match Lazy.force artifacts with
          | None -> Invalid "archive required"
          | Some _ when not (has Pdf || has Xml) ->
              Unsupported "no PDF or publisher XML to convert"
          | Some artifacts ->
              let xml = digested Xml artifacts and pdf = digested Pdf artifacts in
              let choice = match xml, pdf with
                | Some x, _ when List.mem Manifest.Route.Pub2tei converters ->
                    Some (Manifest.Route.Pub2tei, x)
                | _, Some p -> Some (Grobid, p)
                | Some x, None -> Some (Pub2tei, x)
                | None, None -> None in
              match choice with
              | None -> Invalid "input has no digest"
              | Some (route, _) when not (List.mem route converters) ->
                  Unconfigured (Manifest.Route.to_string route)
              | Some (route, input) -> Convert {uid; input; route;
                  converter = Manifest.Route.to_string route ^ "-"
                    ^ string_of_int revision}
  let review ?limit ?members ?catalog ?archive ?skip_refused ~configured
      ~converters ~target ~revision ~requested_by store ~snapshot =
    if revision < 1 then invalid_arg "Strategy revision must be positive";
    let wanted = Option.map (List.map Uid.to_string) members in
    let selected (o : Taposaur_review.observation) = match wanted with
      | None -> true
      | Some wanted -> (match Uid.of_input o.doi with
          | Ok uid -> List.mem (Uid.to_string uid) wanted
          | Error _ -> false) in
    let review = {Attribution.slug = (Taposaur_review_store.snapshot store
      snapshot).slug; snapshot} in
    let limit = Option.value ~default:max_int limit in
    let rec pages after seen acc =
      if seen >= limit then List.rev acc else
      let page = Taposaur_review_store.rows ?after ~limit:(min 100 (limit - seen))
        store snapshot in
      match List.rev page with
      | [] -> List.rev acc
      | last :: _ ->
          let acc = List.fold_left (fun acc (o : Taposaur_review.observation) ->
            if not (selected o) then acc else
            let decision =
              try
                let d = decide ?catalog ?archive ?skip_refused ~configured
                  ~converters ~target ~revision ~requested_by ~review o in
                (match d with
                 | Acquire r -> ignore (Acquisition.key r)
                 | Convert r -> ignore (Conversion.key r)
                 | Held | Refused _ | Unsupported _ | Unconfigured _
                 | Invalid _ -> ());
                d
              with
              | A.Parse_error e -> Invalid (A.error_to_string e)
              | Invalid_argument e -> Invalid e in
            {doi = o.doi; publisher = String.trim (Option.value ~default:""
              o.bibliography.publisher); decision} :: acc) acc page in
          pages (Some last.doi) (seen + List.length page) acc in
    let entries = pages None 0 [] in
    (match wanted with
     | Some wanted when List.length entries < List.length
         (List.sort_uniq compare wanted) ->
         invalid_arg "A requested DOI is outside the snapshot membership"
     | _ -> ());
    entries

  let decision_name = function
    | Acquire _ -> "acquire" | Convert _ -> "convert" | Held -> "held"
    | Refused _ -> "refused"
    | Unsupported _ -> "unsupported" | Unconfigured _ -> "unconfigured"
    | Invalid _ -> "invalid"
  let entry_jsont =
    let reason = function
      | Unsupported s | Unconfigured s | Invalid s | Refused s -> Some s
      | Acquire _ | Convert _ | Held -> None in
    let dec doi publisher decision reason acquisition conversion =
      let decision = match decision, reason, acquisition, conversion with
        | "acquire", _, Some r, _ -> Acquire r
        | "convert", _, _, Some r -> Convert r
        | "held", _, _, _ -> Held
        | "unsupported", Some s, _, _ -> Unsupported s
        | "unconfigured", Some s, _, _ -> Unconfigured s
        | "invalid", Some s, _, _ -> Invalid s
        | _ -> Jsont.Error.msg Jsont.Meta.none "Incomplete plan decision" in
      {doi; publisher; decision} in
    Jsont.Object.map ~kind:"Plan entry" dec
    |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun e -> e.doi)
    |> Jsont.Object.mem "publisher" Jsont.string ~enc:(fun e -> e.publisher)
    |> Jsont.Object.mem "decision" Jsont.string
         ~enc:(fun e -> decision_name e.decision)
    |> Jsont.Object.opt_mem "reason" Jsont.string
         ~enc:(fun e -> reason e.decision)
    |> Jsont.Object.opt_mem "acquisition" Acquisition.request_jsont
         ~enc:(fun e -> match e.decision with Acquire r -> Some r | _ -> None)
    |> Jsont.Object.opt_mem "conversion" Conversion.request_jsont
         ~enc:(fun e -> match e.decision with Convert r -> Some r | _ -> None)
    |> Jsont.Object.finish
end

module Submit = struct
  type outcome = {
    added : int; existing : int; conflicts : int; skipped : int;
  }
  let entries ?not_before ?lane queues plan =
    let added = ref 0 and existing = ref 0 and conflicts = ref 0
    and skipped = ref 0 in
    let submit queue key request =
      match Q.submit ~key ?not_before ?lane queue request with
      | Q.Added job -> incr added; Some (Duraml_client.Job.id job)
      | Existing job -> incr existing; Some (Duraml_client.Job.id job)
      | Conflict id -> incr conflicts; Some id in
    let ids = List.map (fun (e : Plan.entry) -> e, match e.decision with
      | Plan.Acquire r -> submit (Queues.acquisition queues ~strategy:r.strategy)
          (Acquisition.key r) r
      | Convert r -> submit (Queues.conversion queues r.route)
          (Conversion.key r) r
      | Held | Refused _ | Unsupported _ | Unconfigured _ | Invalid _ ->
          incr skipped; None) plan in
    {added = !added; existing = !existing; conflicts = !conflicts;
     skipped = !skipped}, ids
end

(* Workers *)

let now clock = match Ptime.of_float_s (Eio.Time.now clock) with
  | Some t -> t
  | None -> failwith "Clock is outside the timestamp range"
let stamp t = Ptime.to_rfc3339 ~frac_s:3 ~tz_offset_s:0 t
let later t seconds =
  match Option.bind (Ptime.Span.of_float_s seconds) (Ptime.add_span t) with
  | Some t -> t
  | None -> t
(* Duraml reasons and details must be UTF-8 within its byte budget, so a
   cut never splits a multibyte sequence. *)
let truncate n s =
  if String.length s <= n then s else
  let rec back i = if i > 0 && Char.code s.[i] land 0xC0 = 0x80
    then back (i - 1) else i in
  String.sub s 0 (back n)
let sha256 s = Digestif.SHA256.(to_hex (digest_string s))
(* Handler results carry the audit outcome and the reason or code, so the
   operation record explains a retry or failure without reading the store. *)
let retry clock seconds reason =
  let seconds = Float.min 86400. (Float.max 30. seconds) in
  Completion.retry ~at:(later (now clock) seconds) ~reason, "retry", reason
let fail code detail =
  Completion.fail ~code ~detail:(truncate 1000 detail) (), "failed", code
let succeed ?(followups = []) value =
  Completion.succeed ~followups value, "succeeded", ""
let audited ?session ~action ~subject ~details f =
  let context = match Duraml_client.attempt () with
    | None -> []
    | Some (id, attempt) -> ["job", Duraml.Id.to_string id;
        "attempt", string_of_int attempt] in
  let (c, _, _) = Taposaur_audit.with_operation ?session ~action ~subject
    ~details:(details @ context)
    ~success:(fun (_, outcome, _) -> outcome <> "failed")
    ~result_details:(fun (_, outcome, note) ->
      ("outcome", outcome) :: (if note = "" then [] else ["note", note]))
    f in
  c

module Worker = struct
  type acquisition = {
    downloader : Download.t;
    reader : A.t;
    staging_root : Eio.Fs.dir_ty Eio.Path.t;
    sw : Eio.Switch.t;
    archive : Accept.t;
    converters : Manifest.Route.t list;
    converter : Manifest.Route.t -> string;
    clock : float Eio.Time.clock_ty Eio.Resource.t;
    audit : Taposaur_audit.session option;
    cmd : string;
  }

  (* Archive refusals are distinguished from staging refusals so an
     operator looks at the right tree. Both are permanent for this key. *)
  exception Archive_refused of string

  let refresh_successor queues uid manifest_sha256 =
    let r = {Refresh.uid; manifest_sha256} in
    Q.stage ~key:(Refresh.key r) (Queues.refresh queues) r

  let refusal (attempts : Download.attempt list) =
    let attempts = List.filter (fun (a : Download.attempt) ->
      a.failure <> Unavailable "No source found") attempts in
    if List.exists (fun (a : Download.attempt) ->
      a.failure = Unavailable "Publisher credential is not configured") attempts
    then "unconfigured"
    else match attempts with
      | [] -> "no_source"
      | a :: _ -> match a.failure with
        | Http n -> "http_" ^ string_of_int n
        | Invalid_content _ -> "invalid_content"
        | Unavailable _ -> "unavailable"
        | Timeout -> "timeout"
        | Transport -> "transport"

  (* Inputs the archive already holds. A replayed attempt succeeds with
     these instead of fetching watermarked bytes a second time. *)
  let held_inputs (c : acquisition) uid =
    match A.find c.reader uid with
    | None -> []
    | Some paper -> List.filter (fun (a : Manifest.Artifact.t) ->
        (a.format = Pdf || a.format = Xml) &&
        (match a.source with Publisher | Oa | Core | Url -> true
          | Tei | S2orc -> false)) (A.manifest paper).artifacts

  let acquired ?audit (c : acquisition) queues uid
      (artifacts : Manifest.Artifact.t list) attempts =
    let receipt = try
        if artifacts = [] then None else
        let staging = Taposaur_store.open_root ?audit ~sw:c.sw c.staging_root in
        let m = Taposaur_store.with_paper staging uid
          Taposaur_store.manifest in
        let doi = match m.doi with
          | Some _ as doi -> doi
          | None -> Option.map Doi.to_string (Uid.doi uid) in
        let identity = {Bibliography.empty with doi; title = m.title;
          publisher = m.publisher; year = m.year; pmid = m.pmid;
          pmcid = m.pmcid} in
        let directory = Eio.Path.(c.staging_root / Layout.paper uid) in
        let additions = List.map (fun (a : Manifest.Artifact.t) ->
          {Accept.source = File Eio.Path.(directory / a.file); artifact = a})
          artifacts in
        Some (Accept.accept ?audit c.archive ~identity ~cmd:c.cmd
          ~at:(stamp (now c.clock)) uid additions)
      with Failure e | Invalid_argument e -> raise (Archive_refused e) in
    let held, manifest_sha256 = match receipt with
      | Some receipt -> receipt.added @ receipt.existing, receipt.manifest_sha256
      | None ->
          let held = held_inputs c uid in
          let manifest = Option.get (Accept.manifest c.archive uid) in
          held, sha256 (get_ok "manifest" (Manifest.to_string manifest)) in
    let held = List.filter_map Artifact.of_manifest held in
    let conversions = List.filter_map (fun (input : Artifact.t) ->
      let route = match input.format with
        | Pdf -> Some Manifest.Route.Grobid
        | Xml -> Some Pub2tei
        | Tei | Json -> None in
      match route with
      | Some route when List.mem route c.converters ->
          let next = {Conversion.uid; input; route;
            converter = c.converter route} in
          Some (Q.stage ~key:(Conversion.key next)
            (Queues.conversion queues route) next)
      | _ -> None) held in
    let followups = conversions
      @ [refresh_successor queues uid manifest_sha256] in
    succeed ~followups {Acquisition.artifacts = held;
      attempts = List.map (fun (a : Download.attempt) ->
        a.source, Download.failure_message a.failure) attempts}

  let acquisition_handler (c : acquisition) queues ~sw:_
      (r : Acquisition.request) =
    audited ?session:c.audit ~action:"jobs.acquire"
      ~subject:(Uid.to_string r.uid)
      ~details:["strategy", r.strategy; "revision", string_of_int r.revision]
      (fun audit ->
        try
          if held_inputs c r.uid <> [] then
            acquired ?audit c queues r.uid [] []
          else
          let staging = Taposaur_store.open_root ?audit ~sw:c.sw
            c.staging_root in
          let result = Download.fetch ?audit c.downloader staging r.uid in
          if result.artifacts <> [] then
            acquired ?audit c queues result.uid result.artifacts
              result.attempts
          else
          (* Only the strategy's own sources decide whether to retry. A
             fallback that stumbled while the publisher refused outright
             must not spend the publisher again. The [oa] strategy owns
             every fallback source. *)
          let own (a : Download.attempt) = r.strategy = "oa" ||
            String.starts_with ~prefix:r.strategy a.source ||
            a.source = "lookup" in
          let refused = List.exists (fun (a : Download.attempt) ->
            own a && a.source <> "lookup" && (match a.failure with
              | Http (429 | 500 | 502 | 503 | 504) | Timeout | Transport -> false
              | Http _ | Invalid_content _ | Unavailable _ -> true))
            result.attempts in
          let considered = if refused then List.filter own result.attempts
            else result.attempts in
          let busy = List.find_map (fun (a : Download.attempt) ->
            match a.failure with
            | Http (429 | 500 | 502 | 503 | 504 as n) -> Some (a, n)
            | _ -> None) considered in
          let transient = List.find_opt (fun (a : Download.attempt) ->
            a.failure = Timeout || a.failure = Transport) considered in
          match busy, transient with
          | Some (a, n), _ ->
              retry c.clock (Option.value ~default:300. a.retry_after)
                (Printf.sprintf "busy:%s:%d" a.source n)
          | None, Some a ->
              retry c.clock 60. ((if a.failure = Timeout then "timeout:"
                else "transport:") ^ a.source)
          | None, None ->
              let code = refusal result.attempts in
              let reason = String.concat "; " (List.map
                (fun (a : Download.attempt) ->
                  a.source ^ ": " ^ Download.failure_message a.failure)
                result.attempts) in
              let status = List.find_map (fun (a : Download.attempt) ->
                match a.failure with Http n -> Some n | _ -> None)
                result.attempts in
              (* The archive remembers the refusal so later sweeps skip
                 this strategy, unless the credentials were simply absent. *)
              if code <> "unconfigured" then
                Accept.record_refusal ?audit c.archive ~cmd:c.cmd
                  ~at:(stamp (now c.clock)) r.uid
                  {strategy = r.strategy; at = stamp (now c.clock);
                   attempts = 1; status; reason = truncate 1000 reason;
                   permanent = true};
              fail code reason
        with
        | Accept.Busy _ -> retry c.clock 30. "archive_busy"
        | Taposaur_store.Busy -> retry c.clock 30. "staging_busy"
        | Archive_refused e -> fail "archive_refused" e
        | Failure e | Invalid_argument e -> fail "staging_refused" e
        | A.Parse_error e -> fail "invalid_manifest" (A.error_to_string e)
        | Eio.Io _ as e -> retry c.clock 120. ("io:" ^ Printexc.to_string e))

  type conversion = {
    reader : A.t;
    archive : Accept.t;
    grobid : Taposaur_services.Grobid.t option;
    pub2tei : Taposaur_services.Pub2tei.t option;
    timeout : float;
    max_bytes : int;
    clock : float Eio.Time.clock_ty Eio.Resource.t;
    mono_clock : Eio.Time.Mono.ty Eio.Resource.t;
    audit : Taposaur_audit.session option;
    cmd : string;
  }

  exception Envelope of string

  (* The response is classified inside the switch, so only its status,
     delay and validated body leave the request scope. *)
  let call (c : conversion) (r : Conversion.request) bytes =
    (* Fixed part names keep archive filenames out of multipart headers. *)
    let request ~sw = match r.route, c.grobid, c.pub2tei with
      | Grobid, Some g, _ -> Some (Taposaur_services.Grobid.process_fulltext ~sw g
          bytes ~filename:"input.pdf" ~consolidate_funders:Off)
      | Pub2tei, _, Some p ->
          (* Publisher envelopes are reduced to the article they carry. *)
          let article = match Taposaur_document.Publisher_xml.unwrap bytes with
            | Ok article -> article
            | Error e -> raise (Envelope e) in
          Some (Taposaur_services.Pub2tei.process_xml ~sw p article
            ~filename:"input.xml")
      | _ -> None in
    try Eio.Time.Timeout.run_exn
      (Eio.Time.Timeout.seconds c.mono_clock c.timeout) (fun () ->
      Eio.Switch.run (fun sw -> match request ~sw with
        | None -> `Unconfigured
        | Some response ->
            Eio.Switch.on_release sw (fun () -> Fetch.close response);
            match Fetch.status response with
            | 200 -> (try `Body (Eio.Buf_read.(take_all (of_flow
                  ~max_size:c.max_bytes (Fetch.body response))))
                with Eio.Buf_read.Buffer_limit_exceeded -> `Too_large)
            | n -> `Status (n, Download.retry_after response)))
    with
    | Eio.Time.Timeout -> `Timeout
    | Eio.Io _ -> `Transport
    | Envelope e -> `Envelope e

  let converted ?audit (c : conversion) queues (r : Conversion.request) ~at
      tei (b : Bibliography.t) =
    let digest = sha256 tei in
    let artifact = {Manifest.Artifact.file = "tei.xml"; format = Tei;
      source = Tei; version = None;
      converter = Some (Manifest.Route.to_string r.route);
      from = Some r.input.file; strategy = Some r.converter; status = Some 200;
      bytes = Int64.of_int (String.length tei); sha256 = Some digest;
      at = stamp at; url = None; origin = None; confidence = Some Observed} in
    let receipt = Accept.accept ?audit c.archive ~clear_failures:[r.route]
      ~cmd:c.cmd ~at:(stamp at) r.uid [{source = Bytes tei; artifact}] in
    let output = List.find_map Artifact.of_manifest
      (receipt.added @ receipt.existing) in
    let output = match output with
      | Some o -> o
      | None -> {file = "tei.xml"; format = Tei; sha256 = digest;
          bytes = artifact.bytes} in
    succeed ~followups:[refresh_successor queues r.uid receipt.manifest_sha256]
      {Conversion.output; publisher = b.publisher}

  let conversion_handler (c : conversion) queues ~sw:_
      (r : Conversion.request) =
    audited ?session:c.audit ~action:"jobs.convert"
      ~subject:(Uid.to_string r.uid)
      ~details:["route", Manifest.Route.to_string r.route;
        "converter", r.converter; "input", r.input.file]
      (fun audit ->
        let at = now c.clock in
        let permanent code reason =
          Accept.record_failure ?audit c.archive ~cmd:c.cmd ~at:(stamp at) r.uid
            {route = r.route; at = stamp at; attempts = 1; reason;
             permanent = true};
          fail code reason in
        try match A.find c.reader r.uid with
          | None -> fail "missing_paper" "The paper has no archived manifest"
          | Some paper ->
              let m = A.manifest paper in
              let listed = List.exists (fun (a : Manifest.Artifact.t) ->
                a.file = r.input.file && a.sha256 = Some r.input.sha256)
                m.artifacts in
              let previous = List.find_map (fun (a : Manifest.Artifact.t) ->
                if a.format = Tei && a.from = Some r.input.file &&
                   a.converter = Some (Manifest.Route.to_string r.route) &&
                   a.strategy = Some r.converter
                then Artifact.of_manifest a else None) m.artifacts in
              let bytes = if listed && previous = None then
                  Some (A.read_artifact ~max_bytes:c.max_bytes paper r.input.file)
                else None in
              match previous, bytes with
              | Some output, _ ->
                  (* Converter output is not byte-stable, so a replayed
                     attempt reuses the recorded conversion. *)
                  succeed ~followups:[refresh_successor queues r.uid
                    (sha256 (get_ok "manifest" (Manifest.to_string m)))]
                    {Conversion.output; publisher = m.publisher}
              | None, Some bytes when sha256 bytes = r.input.sha256 ->
                (match call c r bytes with
                 | `Unconfigured -> fail "unconfigured"
                     (Manifest.Route.to_string r.route ^ " is not configured")
                 | `Timeout -> retry c.clock 120. "timeout"
                 | `Transport -> retry c.clock 120. "transport"
                 | `Status ((429 | 502 | 503 | 504) as n, delay) ->
                     retry c.clock (Option.value ~default:120. delay)
                       ("busy:" ^ string_of_int n)
                 | `Status (204, _) -> permanent "no_content" "HTTP 204"
                 | `Status (n, _) ->
                     permanent "converter_error" ("HTTP " ^ string_of_int n)
                 | `Too_large -> permanent "invalid_tei" "TEI exceeds byte limit"
                 | `Envelope e -> permanent "invalid_input" e
                 | `Body tei ->
                     match Taposaur_document.Tei.validate_string
                       ~max_bytes:c.max_bytes tei with
                     | Error e -> permanent "invalid_tei" (truncate 1000 e)
                     | Ok () ->
                         match Taposaur_document.Tei.inspect_string tei with
                         | Error e -> permanent "invalid_tei" (truncate 1000 e)
                         | Ok {text = false; _} ->
                             (* A header without a body is a conversion of
                                nothing: an abstract-only or OCR-only
                                input. Holding it would count as full
                                text. *)
                             permanent "empty_body" "TEI body has no text"
                         | Ok {bibliography = b; _} ->
                             let expected = r.uid :: Option.to_list
                               (Option.bind m.doi (fun d ->
                                 Result.to_option (Uid.of_input d))) in
                             let named = Option.bind b.doi (fun d ->
                               Result.to_option (Uid.of_input d)) in
                             match named with
                             | None when r.route = Pub2tei ->
                                 (* Pub2TEI transforms our own publisher XML,
                                    whose identity acquisition established,
                                    and older articles carry no DOI in the
                                    header it emits. *)
                                 converted ?audit c queues r ~at tei b
                             | None -> permanent "identity_mismatch"
                                 "TEI header names no DOI"
                             | Some u when not (List.exists
                                 (fun e -> Uid.compare e u = 0) expected) ->
                                 permanent "identity_mismatch"
                                   "TEI header names another DOI"
                             | Some _ -> converted ?audit c queues r ~at tei b)
              | _ -> fail "input_changed"
                  "The archived input no longer has the requested digest"
        with
        | Accept.Busy _ -> retry c.clock 30. "archive_busy"
        | A.Parse_error e -> fail "invalid_manifest" (A.error_to_string e)
        | Eio.Buf_read.Buffer_limit_exceeded ->
            fail "input_too_large" "The archived input exceeds the byte limit"
        | Failure e | Invalid_argument e -> fail "archive_refused" e
        | Eio.Io _ as e -> retry c.clock 120. ("io:" ^ Printexc.to_string e))

  type refresh = {
    reader : A.t;
    archive : Accept.t;
    catalog : Taposaur_catalog.write Taposaur_catalog.t;
    publisher_lookup : Doi.t -> Publisher_evidence.t option;
    abstract_lookup : Doi.t -> Abstract_evidence.t option;
    clock : float Eio.Time.clock_ty Eio.Resource.t;
    audit : Taposaur_audit.session option;
  }

  let refresh_handler (c : refresh) _queues ~sw:_ (r : Refresh.request) =
    let clock_retry reason = retry c.clock 60. reason in
    audited ?session:c.audit ~action:"jobs.refresh"
      ~subject:(Uid.to_string r.uid)
      ~details:["manifest_sha256", r.manifest_sha256]
      (fun audit ->
        (* Every archive step may find the paper or shard locked by a
           conversion or another refresh, so the whole body retries on
           Busy instead of letting it stop the pool. *)
        try
          let change = Accept.refresh_index ?audit c.archive r.uid in
          let index = match change with
            | Accept.Added -> "added" | Updated -> "updated"
            | Unchanged -> "unchanged" in
          let shard = Layout.index r.uid in
          (* One row is published directly. A shard the catalogue has
             never imported falls back to a whole-shard import. *)
          let published = match Accept.index_row c.archive r.uid with
            | None -> Error "manifest_unavailable"
            | Some row ->
                let size, mtime = A.shard_stat c.reader shard in
                (match Taposaur_catalog.Shard.upsert c.catalog ~shard
                  {size; mtime} row with
                 | `Inserted | `Updated | `Unchanged -> Ok ()
                 | exception Invalid_argument _ ->
                     (match Taposaur_indexing.Import.shard ~force:true
                        c.catalog c.reader ~shard with
                      | Failed _ -> Error "import_failed"
                      | Unchanged | Loaded _ -> Ok ())) in
          match published with
          | Error reason -> clock_retry reason
          | Ok () ->
              let result = ref None in
              ignore (Taposaur_indexing.Enrich.run ?audit
                ~publisher_lookup:c.publisher_lookup
                ~abstract_lookup:c.abstract_lookup ~uid:r.uid ~force:true
                ~jobs:1 ~progress:(fun _ p -> result := Some p)
                c.catalog c.reader);
              match !result with
              | Some Updated ->
                  succeed {Refresh.index; catalogue = "updated"}
              | Some (Partial _) ->
                  succeed {Refresh.index; catalogue = "partial"}
              | Some (Unavailable _) -> clock_retry "unavailable"
              | Some Superseded -> clock_retry "superseded"
              | None -> clock_retry "not_resolved"
        with
        | Accept.Busy _ -> retry c.clock 30. "archive_busy"
        | Failure e -> fail "archive_refused" e
        | A.Parse_error e -> fail "invalid_manifest" (A.error_to_string e)
        | Sqlite3.Error e | Sqlite3.SqliteError e ->
            retry c.clock 60. ("catalogue:" ^ truncate 200 e)
        | Eio.Io _ as e ->
            clock_retry ("io:" ^ truncate 200 (Printexc.to_string e)))

  let own_queue ?component ~prefix queue =
    Taposaur_lock.acquire ?component (prefix ^ "." ^ queue ^ ".lock")

  let span = Ptime.Span.of_int_s
  let acquire ~sw ~mono_clock ?(concurrency = 1) ?(lease = span 300)
      ?(max_attempts = 8) ?stop c queues ~strategy =
    Q.start ~sw ~mono_clock ~concurrency ~lease ~max_attempts ?stop
      (Queues.acquisition queues ~strategy) (acquisition_handler c queues)
  let convert ~sw ~mono_clock ?(concurrency = 2) ?(lease = span 600)
      ?(max_attempts = 5) ?stop c queues route =
    Q.start ~sw ~mono_clock ~concurrency ~lease ~max_attempts ?stop
      (Queues.conversion queues route) (conversion_handler c queues)
  let refresh ~sw ~mono_clock ?(concurrency = 1) ?(lease = span 300)
      ?(max_attempts = 10) ?stop c queues =
    Q.start ~sw ~mono_clock ~concurrency ~lease ~max_attempts ?stop
      (Queues.refresh queues) (refresh_handler c queues)
end

(* Inspection *)

module Inspect = struct
  type counts = {
    queue : string; queued : int; running : int; done_ : int;
    failed : int; cancelled : int;
  }
  let counts store =
    List.map (fun queue ->
      let count state = Duraml.count store (Duraml.query ~queue ~state ()) in
      {queue; queued = count Queued; running = count Running;
       done_ = count Done; failed = count Failed_job;
       cancelled = count Cancelled}) Queues.names
  let state_of_string = function
    | "queued" -> Some Duraml.Queued | "running" -> Some Running
    | "done" -> Some Done | "failed" -> Some Failed_job
    | "cancelled" -> Some Cancelled | _ -> None
  type job = {
    id : string; queue : string; key : string option; state : string;
    attempt : int; uid : string option; review : Attribution.t option;
    created : Ptime.t; updated : Ptime.t; available : Ptime.t;
    outcome : string option; spec : Duraml.Spec.t; parent : string option;
    lane : string;
  }
  let attribution spec =
    let decode codec = Result.to_option
      (Jsont_bytesrw.decode_string codec (Duraml.Spec.payload spec)) in
    match Duraml.Spec.kind spec with
    | "acquisition" -> Option.map (fun (r : Acquisition.request) ->
        r.uid, r.review) (decode Acquisition.request_jsont)
    | "conversion" -> Option.map (fun (r : Conversion.request) ->
        r.uid, None) (decode Conversion.request_jsont)
    | "refresh" -> Option.map (fun (r : Refresh.request) -> r.uid, None)
        (decode Refresh.request_jsont)
    | _ -> None
  let describe j =
    let spec = Duraml.Job.spec j in
    let attributed = attribution spec in
    {id = Duraml.Id.to_string (Duraml.Job.id j); queue = Duraml.Spec.queue spec;
     key = Duraml.Spec.key spec;
     state = Duraml.state_name (Duraml.Job.state j);
     attempt = Duraml.Job.attempt j;
     uid = Option.map (fun (uid, _) -> Uid.to_string uid) attributed;
     review = Option.bind attributed snd;
     created = Duraml.Job.created_at j; updated = Duraml.Job.updated_at j;
     available = Duraml.Job.available_at j;
     outcome = Option.map (function
       | Duraml.Succeeded _ -> "succeeded"
       | Retry {reason; _} -> "retry: " ^ reason
       | Failed {code; detail} -> Printf.sprintf "failed %s: %s" code detail)
       (Duraml.Job.outcome j); spec;
     parent = Option.map Duraml.Id.to_string (Duraml.Job.parent j);
     lane = Duraml.Spec.lane spec}
  let list ?queue ?state ?after ?(limit = 100) store =
    List.map describe (Duraml.list store
      (Duraml.query ?queue ?state ?after ~limit ()))
  let find store id = Option.map describe (Duraml.find store id)
  let children ?(limit = 100) store parent =
    List.map describe (Duraml.list store (Duraml.query ~parent ~limit ()))
  let paper_url j = Option.map (fun uid ->
    "/paper?uid=" ^ Uriz.pct_encode ~component:`Unreserved uid) j.uid
end

(* Audit *)

module Audit_bridge = struct
  type cursor = { identity : string; sequence : int64 }
  let cursor_jsont =
    Jsont.Object.map ~kind:"Cursor" (fun identity sequence ->
      {identity; sequence})
    |> Jsont.Object.mem "identity" Jsont.string ~enc:(fun c -> c.identity)
    |> Jsont.Object.mem "sequence" Jsont.int64 ~enc:(fun c -> c.sequence)
    |> Jsont.Object.finish
  let load path =
    match In_channel.with_open_bin path In_channel.input_all with
    | exception Sys_error _ -> None
    | text -> Result.to_option (Jsont_bytesrw.decode_string cursor_jsont text)
  let save path cursor =
    let text = get_ok "cursor" (Jsont_bytesrw.encode_string cursor_jsont
      cursor) in
    let tmp = path ^ ".tmp" in
    Out_channel.with_open_gen [Open_wronly; Open_creat; Open_trunc; Open_binary]
      0o600 tmp (fun oc -> Out_channel.output_string oc (text ^ "\n");
        Out_channel.flush oc; Unix.fsync (Unix.descr_of_out_channel oc));
    Unix.rename tmp path
  let quote s = get_ok "quote" (Jsont_bytesrw.encode_string Jsont.string s)
  let find_from text needle =
    let n = String.length needle in
    let rec loop i = if i + n > String.length text then None
      else if String.sub text i n = needle then Some (i + n) else loop (i + 1) in
    loop 0
  (* Audit details are a flat JSON object of strings written by
     {!Taposaur_audit.record}, so the store and sequence members are
     recognised by their exact encoding. *)
  let audited store_identity session =
    let marker = "\"store\":" ^ quote store_identity in
    let sequence (e : Taposaur_audit.event) =
      if find_from e.details marker = None then None else
      match find_from e.details "\"sequence\":\"" with
      | None -> None
      | Some i ->
          let j = match String.index_from_opt e.details i '"' with
            | Some j -> j | None -> i in
          Int64.of_string_opt (String.sub e.details i (j - i)) in
    (* Records are newest first, so the first match for this store is its
       latest delivery. Other stores' records are paged past, bounded. *)
    let rec page before remaining =
      if remaining = 0 then 0L else
      let events = Taposaur_audit.query ~action:"queue.event" ~limit:500
        ?before (Taposaur_audit.store session) in
      match List.find_map sequence events with
      | Some n -> n
      | None -> match List.rev events with
        | last :: _ when List.length events = 500 ->
            page (Some last.id) (remaining - 1)
        | _ -> 0L in
    page None 20
  let deliver ?(limit = 1000) ~cursor store session =
    Taposaur_lock.with_lock ~component:"taposaur.jobs.audit"
      (cursor ^ ".lock") @@ fun () ->
    let identity = Duraml.identity store in
    let stored = match load cursor with
      | Some c when c.identity = identity -> c.sequence
      | _ -> 0L in
    let after = ref (max stored (audited identity session)) in
    let jobs = Hashtbl.create 16 in
    let job id = match Hashtbl.find_opt jobs id with
      | Some j -> j
      | None ->
          let j = Option.map Inspect.describe (Duraml.find store id) in
          Hashtbl.replace jobs id j; j in
    let delivered = ref 0 in
    let checkpoint () = save cursor {identity; sequence = !after} in
    let rec loop () =
      if !delivered < limit then begin
        let want = min 1000 (limit - !delivered) in
        let events = Duraml.events ~after:!after ~limit:want store in
        List.iter (fun e ->
          let id = Duraml.Event.job e in
          let j = job id in
          let field name = Option.bind j name in
          let uid = field (fun j -> j.Inspect.uid) in
          let review = field (fun j -> j.review) in
          let details = List.filter (fun (_, v) -> v <> "") [
            "store", identity;
            "sequence", Int64.to_string (Duraml.Event.sequence e);
            "queue", Option.fold ~none:"" ~some:(fun j -> j.Inspect.queue) j;
            "kind", Duraml.Event.kind e;
            "job", Duraml.Id.to_string id;
            "attempt", string_of_int (Duraml.Event.attempt e);
            "detail", truncate 1000 (Duraml.Event.detail e);
            "key", Option.value ~default:"" (field (fun j -> j.key));
            "uid", Option.value ~default:"" uid;
            "review_slug", Option.fold ~none:""
              ~some:(fun (a : Attribution.t) -> a.slug) review;
            "review_snapshot", Option.fold ~none:""
              ~some:(fun (a : Attribution.t) -> string_of_int a.snapshot) review]
          in
          Taposaur_audit.record session ~action:"queue.event"
            ~subject:(Option.value ~default:("job:" ^ Duraml.Id.to_string id)
              uid) ~details ();
          after := Duraml.Event.sequence e;
          incr delivered;
          if !delivered mod 100 = 0 then checkpoint ()) events;
        if List.length events = want then loop ()
      end in
    loop ();
    checkpoint ();
    !delivered
  let run ~mono_clock ?(interval = Ptime.Span.of_int_s 5) ?stop ~cursor store
      session =
    let stop = match stop with
      | Some stop -> stop
      | None -> fst (Eio.Promise.create ()) in
    let seconds = Ptime.Span.to_float_s interval in
    let rec loop () =
      if not (Eio.Promise.is_resolved stop) then begin
        ignore (deliver ~cursor store session);
        match Eio.Fiber.first
          (fun () -> Eio.Promise.await stop; `Stop)
          (fun () -> Eio.Time.Mono.sleep mono_clock seconds; `Next) with
        | `Stop -> ()
        | `Next -> loop ()
      end in
    loop ()
end

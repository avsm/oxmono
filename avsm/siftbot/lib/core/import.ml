module P = Jmap.Proto
module M = Siftbot_model
module Mail = Jmap_eio.Mail

let id s =
  match P.Id.of_string_received s with
  | Ok x -> x
  | Error _ -> invalid_arg "Invalid email ID"

let date s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> t
  | Error _ -> invalid_arg "Use an RFC3339 date"

let hash s = Digestif.SHA256.(to_hex (digest_string s))

let strip text =
  let rec lines acc = function
    | [] -> List.rev acc
    | line :: _ when String.trim line = "--" -> List.rev acc
    | line :: rest when String.starts_with ~prefix:">" (String.trim line) ->
        lines acc rest
    | line :: rest -> lines (line :: acc) rest
  in
  String.concat "\n" (lines [] (String.split_on_char '\n' text))

let message ~service ~account ~since ~until (e : P.Email.t) =
  let required = function
    | Some x -> x
    | None -> invalid_arg "Missing imported email identity or date"
  in
  let email_id = P.Id.to_string (required e.id) in
  let thread = P.Id.to_string (required e.thread_id) in
  let received = required e.received_at in
  let values =
    Option.value ~default:[] e.body_values
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let text_values =
    List.filter_map
      (fun (p : P.Email_body.Part.t) ->
        Option.bind p.part_id (fun id -> List.assoc_opt id values))
      (Option.value ~default:[] e.text_body)
  in
  let original =
    String.concat "\n"
      (List.map (fun (_, (v : P.Email_body.Value.t)) -> v.value) values)
  in
  let text =
    if text_values = [] then
      List.filter_map
        (fun (p : P.Email_body.Part.t) ->
          Option.bind p.part_id (fun id -> List.assoc_opt id values))
        (Option.value ~default:[] e.html_body)
      |> List.map (fun (v : P.Email_body.Value.t) ->
             Jmap_eio.Mail_text.render v.value)
      |> String.concat "\n"
    else
      String.concat "\n"
        (List.map (fun (v : P.Email_body.Value.t) -> v.value) text_values)
  in
  let addresses xs =
    List.map
      (fun (a : P.Email_address.t) -> a.email)
      (Option.value ~default:[] xs)
  in
  let m =
    M.
      {
        source = { service; account; id = email_id };
        thread;
        date = Ptime.to_rfc3339 ~tz_offset_s:0 received;
        sender = String.concat ", " (addresses e.from);
        recipients = addresses e.to_ @ addresses e.cc @ addresses e.bcc;
        subject = Option.value ~default:"" e.subject;
        original;
        text = strip text;
        headers =
          List.map
            (fun (h : P.Email_header.t) -> (h.name, h.value))
            (Option.value ~default:[] e.headers);
        message_ids = Option.value ~default:[] e.message_id;
        reply_ids =
          Option.value ~default:[] e.in_reply_to
          @ Option.value ~default:[] e.references;
        truncated =
          e.body_values = None
          || List.exists
               (fun (_, (v : P.Email_body.Value.t)) ->
                 v.is_truncated || v.is_encoding_problem)
               values;
        outside_interval =
          Ptime.compare received (date since) < 0
          || Ptime.compare received (date until) >= 0;
        revision = "";
      }
  in
  {
    m with
    revision = hash ("extract/v1:" ^ Jmap_eio.Codec.encode_exn M.message_jsont m);
  }

type remote = {
  service : string;
  account : string;
  username : string;
  batch_size : int;
  state : unit -> string;
  query : since:string -> until:string -> position:int -> Mail.query_page;
  read : string list -> P.Email.t P.Method.get_response;
  thread : string -> position:int -> Mail.thread_page;
  changes : string -> P.Method.changes_response;
}

let remote ~service ~body_limit reader =
  let service =
    match Fetch.Middleware.Url.of_string service with
    | Ok url -> Fetch.Middleware.Url.to_string url
    | Error _ -> invalid_arg "Invalid service URL"
  in
  {
    service;
    account = (Mail.identity reader).account;
    username = (Mail.identity reader).username;
    batch_size = Mail.batch_size reader;
    state = (fun () -> Mail.state reader);
    query =
      (fun ~since ~until ~position ->
        Mail.query reader
          ~filter:(P.Email.filter ~after:(date since) ~before:(date until) ())
          ~sort:[ P.Filter.comparator ~is_ascending:true "receivedAt" ]
          ~position ~limit:(Mail.batch_size reader) ~collapse_threads:false ());
    read =
      (fun ids ->
        Mail.read_batch reader ~max_body_value_bytes:body_limit
          (List.map id ids));
    thread =
      (fun thread ~position ->
        Mail.thread ~max_body_value_bytes:body_limit ~allow_missing:true reader
          ~id:(id thread) ~position ~limit:10);
    changes =
      (fun since_state -> Mail.changes reader ~since_state ~max_changes:50);
  }

type cursor = {
  since : string;
  until : string;
  anchor : string;
  phase : string;
  position : int;
  query_state : string;
}

let codec =
  let make since until anchor phase position query_state =
    { since; until; anchor; phase; position; query_state }
  in
  Jsont.Object.map make
  |> Jsont.Object.mem "since" Jsont.string ~enc:(fun x -> x.since)
  |> Jsont.Object.mem "until" Jsont.string ~enc:(fun x -> x.until)
  |> Jsont.Object.mem "anchor" Jsont.string ~enc:(fun x -> x.anchor)
  |> Jsont.Object.mem "phase" Jsont.string ~enc:(fun x -> x.phase)
  |> Jsont.Object.mem "position" Jsont.int ~enc:(fun x -> x.position)
  |> Jsont.Object.mem "queryState" Jsont.string ~enc:(fun x -> x.query_state)
  |> Jsont.Object.finish

let job r =
  "import:" ^ M.key { service = r.service; account = r.account; id = "" }

let encode = Jmap_eio.Codec.encode_exn codec
let decode = Jmap_eio.Codec.decode_exn codec

let interval db r =
  Option.map
    (fun s ->
      let c = decode s in
      (c.since, c.until))
    (Store.checkpoint db (job r))

let start db r ~since ~until =
  let binding =
    "binding:" ^ M.key { service = r.service; account = r.account; id = "" }
  in
  (match Store.checkpoint db binding with
  | Some name when name <> r.username ->
      invalid_arg "The configured JMAP account changed authenticated identity"
  | Some _ -> ()
  | None ->
      Store.commit_batch db ~job:binding ~expected:None ~cursor:r.username [] []);
  if Ptime.compare (date since) (date until) >= 0 then
    invalid_arg "Import interval must have positive duration";
  if r.batch_size < 1 || r.batch_size > 10 then invalid_arg "Invalid batch size";
  if Store.checkpoint db (job r) = None then
    let c =
      {
        since;
        until;
        anchor = r.state ();
        phase = "listing";
        position = 0;
        query_state = "";
      }
    in
    Store.commit_batch db ~job:(job r) ~expected:None ~cursor:(encode c) [] []

let take n xs = List.filteri (fun i _ -> i < n) xs

let scoped r (m : M.message) =
  m.source.service = r.service && m.source.account = r.account

let source (r : remote) id = M.{ service = r.service; account = r.account; id }

let step db r =
  let job = job r in
  let expected = Store.checkpoint db job in
  let c =
    match expected with
    | Some s -> decode s
    | None -> invalid_arg "Start the import first"
  in
  let save ?hydrate ?hydrated c ms deleted =
    Store.commit_batch ?hydrate ?hydrated db ~job ~expected ~cursor:(encode c)
      ms deleted
  in
  let convert =
    message ~service:r.service ~account:r.account ~since:c.since ~until:c.until
  in
  let read ids =
    let rec loop ms gone = function
      | [] -> (ms, gone)
      | xs ->
          let batch = take r.batch_size xs in
          let rest = List.filteri (fun i _ -> i >= List.length batch) xs in
          let result = r.read batch in
          loop
            (List.map convert result.list @ ms)
            (List.map (fun id -> source r (P.Id.to_string id)) result.not_found
            @ gone)
            rest
    in
    loop [] [] ids
  in
  let reset () =
    save
      {
        c with
        phase = "reconcile";
        position = 0;
        query_state = "";
        anchor = r.state ();
      }
      [] [];
    false
  in
  try
    match c.phase with
    | "listing" ->
        let page = r.query ~since:c.since ~until:c.until ~position:c.position in
        if c.query_state <> "" && c.query_state <> page.query.query_state then (
          save { c with position = 0; query_state = "" } [] [];
          false)
        else
          let ms, gone = read (List.map P.Id.to_string page.query.ids) in
          let next =
            match page.next_position with
            | Some n ->
                {
                  c with
                  position = Int64.to_int n;
                  query_state = page.query.query_state;
                }
            | None ->
                { c with phase = "hydrate"; position = 0; query_state = "" }
          in
          let hydrate =
            List.map (fun (m : M.message) -> { m.source with id = m.thread }) ms
          in
          save ~hydrate next ms gone;
          false
    | "reconcile" ->
        let ids =
          Store.messages db
          |> List.filter (scoped r)
          |> List.map (fun m -> m.M.source.id)
          |> List.sort String.compare
          |> List.filter (fun id -> id > c.query_state)
          |> take r.batch_size
        in
        (if ids = [] then
           save { c with phase = "listing"; query_state = "" } [] []
         else
           let ms, gone = read ids in
           save { c with query_state = List.hd (List.rev ids) } ms gone);
        false
    | "hydrate" ->
        (match Store.hydration db ~service:r.service ~account:r.account with
        | None ->
            save
              { c with phase = "changes"; position = 0; query_state = "" }
              [] []
        | Some (thread, position) ->
            let page = r.thread thread.id ~position in
            let ms, gone =
              match page.emails with
              | None -> ([], [])
              | Some result ->
                  ( List.map convert result.list,
                    List.map
                      (fun id -> source r (P.Id.to_string id))
                      result.not_found )
            in
            save ~hydrated:(thread, page.next_position) c ms gone);
        false
    | "changes" | "done" ->
        let changes = r.changes c.anchor in
        let ids =
          List.sort_uniq P.Id.compare (changes.created @ changes.updated)
          |> List.map P.Id.to_string
        in
        let ms, gone = read ids in
        let ms =
          List.filter
            (fun (m : M.message) ->
              Ptime.compare (date m.date) (date c.since) >= 0
              || Store.get db m.source <> None
              || List.exists
                   (fun (old : M.message) ->
                     scoped r old && old.thread = m.thread)
                   (Store.messages db))
            ms
        in
        let hydrate =
          ms
          |> List.filter (fun (m : M.message) ->
                 match Store.get db m.source with
                 | Some old -> old.revision <> m.revision
                 | None -> true)
          |> List.map (fun (m : M.message) -> { m.source with id = m.thread })
        in
        save ~hydrate
          {
            c with
            anchor = changes.new_state;
            phase = (if changes.has_more_changes then "changes" else "done");
          }
          ms
          (List.map (fun id -> source r (P.Id.to_string id)) changes.destroyed
          @ gone);
        if Store.hydration db ~service:r.service ~account:r.account <> None then (
          let expected = Store.checkpoint db job in
          let current = Option.get expected |> decode in
          Store.commit_batch db ~job ~expected
            ~cursor:(encode { current with phase = "hydrate" })
            [] [];
          false)
        else not changes.has_more_changes
    | _ -> failwith "Unknown import phase"
  with
  | Jmap_eio.Client.Jmap_client_error (Method_error e)
  when e.P.Error.Method_error.type_ = `Cannot_calculate_changes
  ->
    reset ()

let evaluation_cutoff db =
  let intervals =
    Store.checkpoints db
    |> List.filter_map (fun (job, raw) ->
           if String.starts_with ~prefix:"import:" job then
             Some (decode raw).until
           else None)
    |> List.sort String.compare
  in
  match List.rev intervals with
  | [] -> invalid_arg "Import first or provide an explicit --cutoff"
  | latest :: _ ->
      let (year, month, day), time = Ptime.to_date_time (date latest) in
      let year, month =
        if month > 3 then (year, month - 3) else (year - 1, month + 9)
      in
      let rec valid day =
        match Ptime.of_date_time ((year, month, day), time) with
        | Some t -> Ptime.to_rfc3339 ~tz_offset_s:0 t
        | None when day > 1 -> valid (day - 1)
        | None -> invalid_arg "Cannot calculate evaluation boundary"
      in
      valid day

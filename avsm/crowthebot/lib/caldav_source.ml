module C = Caldav_eio.Client
module D = Httpz_dav

type settings = {
  url : string;
  user : string;
  password : string;
  max_bytes : int;
}

let codec =
  Jsont.Object.map (fun url user password max_bytes ->
      { url; user; password; max_bytes })
  |> Jsont.Object.mem "url" Jsont.string ~enc:(fun s -> s.url)
  |> Jsont.Object.mem "user" Jsont.string ~enc:(fun s -> s.user)
  |> Jsont.Object.mem "password" Jsont.string ~enc:(fun s -> s.password)
  |> Jsont.Object.mem "max_bytes" Jsont.int ~enc:(fun s -> s.max_bytes)
  |> Jsont.Object.error_unknown |> Jsont.Object.finish

let validate s =
  ignore (Tool_config.endpoint ~allow_http:false s.url);
  if s.user = "" || s.password = "" then
    invalid_arg "CalDAV needs a login and app password.";
  ignore (Fetch.Credential.basic ~user:s.user ~password:s.password);
  if s.max_bytes < 1048576 || s.max_bytes > 268435456 then
    invalid_arg "CalDAV response limit must be 1 to 256 MiB."

let configuration =
  let open Cmdliner in
  Tool_config.v ~name:"caldav"
    ~doc:"Named read-only CalDAV calendar connections."
    Term.(
      const (fun url user password_file max_mb () ->
          if max_mb < 1 || max_mb > 256 then
            invalid_arg "Response limit must be 1 to 256 MiB.";
          let password =
            Tool_config.secret ~label:"CalDAV app password" password_file
            |> String.trim
          in
          let s = { url; user; password; max_bytes = max_mb * 1048576 } in
          validate s;
          Tool_config.encode codec s)
      $ Arg.(
          value
          & opt string "https://caldav.fastmail.com/"
          & info [ "url" ] ~docv:"URL" ~doc:"HTTPS CalDAV discovery URL.")
      $ Arg.(
          required
          & opt (some string) None
          & info [ "user" ] ~docv:"LOGIN" ~doc:"Full calendar login address.")
      $ Arg.(
          value
          & opt (some string) None
          & info [ "password-file" ] ~docv:"FILE"
              ~doc:
                "0600 file containing an app password. Otherwise prompt \
                 without echo. Fastmail JMAP API tokens cannot be used here.")
      $ Arg.(
          value & opt int 32
          & info [ "max-response-mib" ]
              ~doc:"Maximum response size, default 32 MiB."))

exception Error of C.error
exception Invalid_sync_token

let result = function Ok v -> v | Error e -> raise (Error e)

let error = function
  | Error (C.Http (status, _) | Dav (status, _)) ->
      Printf.sprintf
        "CalDAV HTTP %d. Check the login, calendar app password and endpoint."
        status
  | Error (C.Not_found _) ->
      "CalDAV HTTP 404. The requested discovery or calendar resource was not \
       found."
  | Error (C.Precondition_failed _) -> "CalDAV HTTP 412. A precondition failed."
  | Error (C.Discovery _) ->
      "CalDAV discovery failed. Check the endpoint and calendar access."
  | Error (C.Xml _ | Data _) ->
      "CalDAV returned invalid or oversized data. The saved cursor was \
       retained."
  | Error (C.Transport (Fetch.Denied _, _)) ->
      "CalDAV read-only policy rejected the request before transport."
  | Error (C.Transport _) -> "CalDAV transport failed. Sync will retry."
  | Eio.Time.Timeout -> "CalDAV request timed out. Sync will retry."
  | Invalid_argument message -> message
  | exn ->
      Printf.sprintf "CalDAV operation failed (%s)." (Diagnostics.error exn)

type t = {
  connect : unit -> C.t;
  timeout : 'a. (unit -> 'a) -> 'a;
  key : string -> string;
}

type identity = { principal : string; key : string }

include Caldav_data

let initialize ~sw ~fetch ~clock json =
  let s = Tool_config.decode codec json in
  validate s;
  let fetch = Caldav_http.read_only ~url:s.url fetch in
  let timeout f = Eio.Time.with_timeout_exn clock 30. f in
  let cached = ref None and mutex = Eio.Mutex.create () in
  let connect () =
    Persistence.locked mutex @@ fun () ->
    match !cached with
    | Some c -> c
    | None ->
        let c =
          timeout (fun () ->
              C.connect ~sw
                ~credentials:
                  [ Fetch.Credential.basic ~user:s.user ~password:s.password ]
                ~limits:{ D.default_limits with max_bytes = s.max_bytes }
                fetch s.url
              |> result)
        in
        cached := Some c;
        c
  in
  let key principal =
    Digestif.SHA256.(
      to_hex (digest_string (s.url ^ "\000" ^ s.user ^ "\000" ^ principal)))
  in
  { connect; timeout; key }

let identity t =
  let principal = C.principal (t.connect ()) in
  { principal; key = t.key principal }

let checked r =
  match r.D.outcome with
  | D.Status n when n >= 200 && n < 300 -> ()
  | Properties ps
    when List.for_all
           (fun (p : D.propstat) ->
             p.status = 404 || (p.status >= 200 && p.status < 300))
           ps ->
      ()
  | _ -> raise (Error (C.Data "Incomplete DAV listing"))

let resolve base href =
  match D.resolve_href ~base href with
  | Ok h -> h
  | Error _ -> raise (Error (C.Data "Invalid href"))

let member collection href =
  let href = resolve collection href in
  let module U = Fetch.Middleware.Url in
  let url value =
    match U.of_string value with
    | Ok u -> u
    | Error _ -> raise (Error (C.Data "Invalid calendar URL"))
  in
  let base = url collection
  and target = url href
  and uri = Uri.of_string href in
  if
    (not (U.under ~prefix:base target))
    || U.path_segments base = U.path_segments target
    || Uri.query uri <> []
    || Uri.fragment uri <> None
  then raise (Error (C.Data "Foreign calendar member"));
  U.to_string target

let discover t =
  t.timeout @@ fun () ->
  let c = t.connect () in
  List.concat_map
    (fun home ->
      let query =
        match Caldav.Calendar.propfind with
        | D.Prop names -> D.Allprop names
        | query -> query
      in
      let m = C.propfind c ~depth:`One home query |> result in
      List.iter checked m.responses;
      List.filter_map
        (fun (r : D.response) ->
          match Caldav.Calendar.of_response r with
          | None -> None
          | Some cal ->
              let props =
                match r.outcome with
                | D.Properties ps ->
                    List.concat_map (fun (p : D.propstat) -> p.properties) ps
                | _ -> []
              in
              Some
                {
                  href = resolve home cal.href;
                  title = Option.value ~default:"Calendar" cal.display_name;
                  properties =
                    D.encode_xml
                      (D.element (D.dav "prop")
                         (List.map (fun p -> D.Element p) props));
                  sync = List.mem (D.dav "sync-collection") cal.reports;
                })
        m.responses)
    (C.home_sets c)

let next t (collection : collection) ~token =
  t.timeout @@ fun () ->
  let c = t.connect () in
  if collection.sync then begin
    let s =
      match
        Fetch_dav.Session.sync (C.session c) ?token ~limit:20 collection.href
      with
      | Error (C.Dav (_, conditions))
        when token <> None
             && List.exists
                  (fun e -> e.D.name = D.dav "valid-sync-token")
                  conditions ->
          raise Invalid_sync_token
      | r -> result r
    in
    if s.token = None || (s.truncated && s.token = token) then
      raise (Error (C.Data "Non-advancing sync token"));
    let changes =
      List.filter_map
        (function
          | D.Sync.Changed r ->
              checked r;
              if D.is_collection r then None
              else
                Some
                  {
                    href = member collection.href (D.href r);
                    etag = D.etag r;
                    removed = false;
                  }
          | Removed href ->
              Some
                {
                  href = member collection.href href;
                  etag = None;
                  removed = true;
                }
          | Unsupported _ -> raise (Error (C.Data "Unsupported sync member")))
        s.changes
    in
    { token = s.token; more = s.truncated; inventory = false; changes }
  end
  else begin
    let m =
      C.propfind c ~depth:`One collection.href
        (D.Prop D.Prop.[ resourcetype; getetag ])
      |> result
    in
    List.iter checked m.responses;
    let changes =
      List.filter_map
        (fun r ->
          if D.same_href (D.href r) collection.href || D.is_collection r then
            None
          else
            Some
              {
                href = member collection.href (D.href r);
                etag = D.etag r;
                removed = false;
              })
        m.responses
    in
    { token = None; more = false; inventory = true; changes }
  end

let index raw = Caldav_text.index raw

let get t ~collection href =
  t.timeout @@ fun () ->
  let href = member collection href in
  match C.get Caldav.Data.raw (t.connect ()) href with
  | Error (C.Not_found _) -> None
  | r ->
      let e = result r in
      let search, parsed = index e.value in
      Some { href; etag = e.etag; raw = e.value; search; parsed }

let agenda t (collection : collection) window =
  t.timeout @@ fun () ->
  let response =
    C.report (t.connect ()) ~depth:`One collection.href
      (Caldav.Report.query_to_xml (Caldav_agenda.query window))
    |> result
  in
  let outcome =
    Caldav.Report.outcome_of_multistatus ~base:collection.href response
  in
  if outcome.truncated then
    invalid_arg "Agenda response was truncated. Ask for a shorter date range.";
  List.iter checked response.responses;
  let resources =
    List.filter
      (fun r -> not (D.same_href (D.href r) collection.href))
      response.responses
  in
  if List.length outcome.entries <> List.length resources then
    invalid_arg
      "Incomplete agenda response. Some calendar resources could not be read.";
  List.map
    (fun (e : Caldav.Report.entry) ->
      let raw =
        match e.data with
        | Some raw -> raw
        | None -> invalid_arg "Agenda response omitted calendar data."
      in
      Caldav_agenda.parse ~href:(member collection.href e.href) ~etag:e.etag raw)
    outcome.entries

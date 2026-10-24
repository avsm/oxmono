open Taposaur
module S = Taposaur_services
module Store = Taposaur_store

type failure = Http of int | Timeout | Invalid_content of string
  | Transport | Unavailable of string
type attempt = { source : string; failure : failure }
type result = {
  uid : Uid.t; artifacts : Manifest.Artifact.t list; attempts : attempt list;
}
type t = {
  http : Fetch.plain; clock : float Eio.Time.clock_ty Eio.Resource.t;
  timeout : float; lookup_timeout : float; max_bytes : int;
  busy_retries : int; busy_wait : float;
  cup : S.Cup.t; plos : S.Plos.t; tandf : S.Tandf.t;
  springer_pdf : S.Springer.t option;
  glutton : S.Glutton.t option; datacite : S.Datacite.t option;
  elsevier : S.Elsevier.t option; springer : S.Springer.t option;
  wiley : S.Wiley.t option; core : S.Core.t option;
}
let failure_message = function
  | Http n -> Printf.sprintf "HTTP %d" n
  | Timeout -> "Transfer deadline exceeded"
  | Invalid_content s -> "Invalid content: " ^ s
  | Transport -> "HTTP transport failed"
  | Unavailable s -> s
exception Remote of failure
let create ?(timeout = 120.) ?(max_bytes = 128 * 1024 * 1024)
    ?lookup_timeout ?(busy_retries = 3) ?(busy_wait = 300.)
    ?cup ?plos ?tandf ?springer_pdf
    ?glutton ?datacite ?elsevier ?springer ?wiley ?core ~clock http =
  if not (timeout > 0. && Float.is_finite timeout) then
    invalid_arg "timeout must be positive and finite";
  if max_bytes < 1 then invalid_arg "max_bytes must be positive";
  let lookup_timeout = Option.value ~default:(min timeout 15.) lookup_timeout in
  if not (lookup_timeout > 0. && Float.is_finite lookup_timeout) then
    invalid_arg "lookup_timeout must be positive and finite";
  if busy_retries < 0 || busy_retries > 100 || busy_wait < 0.
     || not (Float.is_finite busy_wait) then invalid_arg "Invalid busy retry policy";
  {http = Fetch.read_only http;
   clock = (clock :> float Eio.Time.clock_ty Eio.Resource.t); timeout; max_bytes;
   lookup_timeout; busy_retries; busy_wait; springer_pdf;
   cup = Option.value ~default:(S.Cup.client http) cup;
   plos = Option.value ~default:(S.Plos.client http) plos;
   tandf = Option.value ~default:(S.Tandf.client http) tandf;
   glutton; datacite; elsevier; springer; wiley; core}
let now t = match Ptime.of_float_s (Eio.Time.now t.clock) with
  | Some time -> Ptime.to_rfc3339 ~frac_s:3 ~tz_offset_s:0 time
  | None -> failwith "Clock is outside RFC3339 range"
let request f = try f () with Eio.Io _ -> raise (Remote Transport)
let attempt ?timeout t f =
  try Ok (Eio.Time.with_timeout_exn t.clock
    (Option.value ~default:t.timeout timeout)
    (fun () -> Eio.Switch.run f)) with
  | Remote failure -> Error failure
  | Eio.Time.Timeout -> Error Timeout
  | Store.Empty_artifact -> Error (Invalid_content "Empty artifact")
  | Store.Limit_exceeded -> Error (Invalid_content "Artifact exceeds byte limit")
let status response =
  let status = Fetch.status response in
  if status < 200 || status >= 300 then raise (Remote (Http status))
let json t codec f = attempt ~timeout:t.lookup_timeout t (fun sw ->
  let response = request (fun () -> f sw) in
  Eio.Switch.on_release sw (fun () -> Fetch.close response);
  status response;
  let body = try request (fun () -> Eio.Buf_read.(take_all
    (of_flow ~max_size:(1024 * 1024) (Fetch.body response))))
    with Eio.Buf_read.Buffer_limit_exceeded ->
      raise (Remote (Invalid_content "Metadata exceeds 1 MiB")) in
  match Jsont_bytesrw.decode_string codec body with
  | Ok value -> value
  | Error _ -> raise (Remote (Invalid_content "Invalid metadata JSON")))
let contains text needle =
  let rec loop i = i + String.length needle <= String.length text &&
    (String.sub text i (String.length needle) = needle || loop (i + 1)) in
  loop 0
let validate format file =
  let bad s = raise (Remote (Invalid_content s)) in
  match format with
  | Manifest.Format.Pdf ->
      let size = Optint.Int63.to_int (Eio.File.size file) in
      if size < 12 then bad "Truncated PDF";
      let head = Cstruct.create 5 in
      Eio.File.pread_exact file ~file_offset:Optint.Int63.zero [head];
      if Cstruct.to_string head <> "%PDF-" then bad "Missing PDF signature";
      let n = min 2048 size in
      let tail = Cstruct.create n in
      Eio.File.pread_exact file ~file_offset:(Optint.Int63.of_int (size - n)) [tail];
      if not (contains (Cstruct.to_string tail) "%%EOF") then
        bad "Missing PDF end marker"
  | Xml ->
      let reader = Eio.Buf_read.of_flow ~max_size:65536 file in
      let input = Xmlm.make_input (`Fun (fun () ->
        Char.code (Eio.Buf_read.any_char reader))) in
      let first = ref true and depth = ref 0 in
      let wrapper = ref false and articles = ref 0 in
      (try while not (Xmlm.eoi input) do
        match Xmlm.input input with
        | `El_start ((_, name), _) ->
            if !depth = 0 && not !first then bad "Multiple XML roots";
            if !first then begin
              first := false;
              wrapper := name = "response";
              if not (List.mem (String.lowercase_ascii name)
                ["article"; "full-text-retrieval-response"; "articles";
                 "article-set"; "book-part-wrapper"; "book-part"; "response"]) then
                bad "Unsupported publisher XML root"
            end;
            if name = "article" then incr articles;
            incr depth;
            if !depth > 256 then bad "XML nesting exceeds 256"
        | `El_end -> decr depth
        | `Dtd _ | `Data _ -> ()
      done with Xmlm.Error _ -> bad "Malformed XML");
      if !first then bad "Empty XML";
      if !wrapper && !articles <> 1 then
        bad "Publisher response must contain exactly one article"
  | _ -> invalid_arg "Only PDF and publisher XML can be fetched"
let public_url url =
  let u = Uri.of_string url in
  let query = List.filter (fun (key, _) -> not (List.mem
    (String.lowercase_ascii key) ["api_key"; "apikey"; "token";
      "access_token"; "key"])) (Uri.query u) in
  Uri.to_string (Uri.with_userinfo (Uri.with_query u query) None)
let transfer t paper ~source ~format ~strategy f = attempt t (fun sw ->
  let response = request (fun () -> f sw) in
  Eio.Switch.on_release sw (fun () -> Fetch.close response);
  status response;
  let media = Option.map (fun s -> String.lowercase_ascii
    (String.trim (List.hd (String.split_on_char ';' s))))
    (Http.Header.get (Fetch.headers response) "content-type") in
  (match format, media with
   | Manifest.Format.Pdf, Some "application/pdf" -> ()
   | Xml, Some ("application/xml" | "text/xml" | "application/jats+xml") -> ()
   | _ -> raise (Remote (Invalid_content "Unexpected Content-Type")));
  (* Only input failures become remote errors. Storage failures propagate. *)
  let module Input = struct
    type t = Eio.Flow.source_ty Eio.Resource.t
    let single_read flow (buffer @ local) =
      try Eio.Flow.single_read flow buffer with Eio.Io _ -> raise (Remote Transport)
    let read_methods = []
  end in
  let flow = Eio.Resource.T (Fetch.body response,
    Eio.Flow.Pi.source (module Input)) in
  Store.add ~max_bytes:t.max_bytes paper ~at:(now t) ~source ~format ~strategy
    ~status:(Fetch.status response) ~url:(public_url (Fetch.url response))
    ~validate:(validate format) flow)
let record t paper attempts source failure =
  attempts := {source; failure} :: !attempts;
  Store.record_error paper ~at:(now t) ~source (failure_message failure)
let fetch_url t store uid ~format url =
  if format <> Manifest.Format.Pdf && format <> Xml then
    invalid_arg "Only PDF and publisher XML can be fetched";
  Store.with_paper store uid (fun paper ->
    let attempts = ref [] in
    let artifacts = match transfer t paper ~source:Manifest.Source.Url
      ~format ~strategy:"url" (fun sw -> Fetch.get ~sw t.http url) with
      | Ok a -> [a]
      | Error e -> record t paper attempts "url" e; [] in
    {uid; artifacts; attempts = List.rev !attempts})
let fetch t store input =
  let lookup = match t.glutton with
    | None -> Error (Unavailable "Glutton is not configured")
    | Some g -> json t S.Glutton.Record.jsont (fun sw ->
        match Uid.doi input with
        | Some doi -> S.Glutton.by_doi ~sw g doi
        | None -> S.Glutton.by_pmcid ~sw g (Uid.id input)) in
  let lookup = match lookup, t.datacite, Uid.doi input with
    | Error (Http 404), Some d, Some doi ->
        json t S.Glutton.Record.jsont (fun sw -> S.Datacite.by_doi ~sw d doi)
    | result, _, _ -> result in
  let identity = match lookup with
    | Error e -> Error e
    | Ok r -> (match Option.bind r.doi (fun s -> Result.to_option (Doi.of_string s)) with
        | None -> Error (Invalid_content "Record has no valid DOI")
        | Some doi when Option.fold ~none:false
            ~some:(fun expected -> not (Doi.equal expected doi)) (Uid.doi input) ->
            Error (Invalid_content "Record names a different DOI")
        | Some _ when r.kind = Some "journal" || r.kind = Some "journal-issue" ->
            Error (Unavailable "Identifier names a journal or issue")
        | Some doi -> Ok (doi, r)) in
  match identity with
  | Error failure ->
      Store.with_paper store input (fun paper ->
        Store.record_error paper ~at:(now t) ~source:"lookup"
          (failure_message failure));
      {uid = input; artifacts = []; attempts = [{source = "lookup"; failure}]}
  | Ok (doi, r) ->
      let uid = match Uid.of_id Uid.Scheme.Doi (Doi.to_string doi) with
        | Ok uid -> uid | Error e -> failwith e in
      Store.with_paper store uid (fun paper ->
        Store.identify paper ~at:(now t) {Bibliography.empty with
          doi = Some (Doi.to_string doi); title = r.title; publisher = r.publisher;
          year = r.year; pmid = r.pmid; pmcid = r.pmcid; journal = r.journal};
        let attempts = ref [] and artifacts = ref [] in
        let try_transfer source format strategy f =
          let rec run remaining =
            match transfer t paper ~source ~format ~strategy f with
            | Error (Http (429 | 500 | 502 | 503 | 504))
                when source = Manifest.Source.Publisher && remaining > 0 ->
                Eio.Time.sleep t.clock t.busy_wait;
                run (remaining - 1)
            | result -> result in
          match run t.busy_retries with
          | Ok a -> artifacts := a :: !artifacts; true
          | Error e -> record t paper attempts strategy e; false in
        let publisher = String.lowercase_ascii (Option.value ~default:"" r.publisher) in
        let missing name = record t paper attempts name
          (Unavailable "Publisher credential is not configured"); false in
        let xml, publisher_pdf =
          if contains publisher "elsevier" then
            (Option.fold ~none:(fun () -> missing "elsevier")
              ~some:(fun c () -> try_transfer Publisher Xml "elsevier"
                (fun sw -> S.Elsevier.article ~sw c doi)) t.elsevier (), None)
          else if contains publisher "springer" then
            (Option.fold ~none:(fun () -> missing "springer")
              ~some:(fun c () -> try_transfer Publisher Xml "springer"
                (fun sw -> S.Springer.articles ~sw c [doi])) t.springer (),
             Option.map (fun c -> "springer-pdf", fun sw ->
               S.Springer.pdf ~sw c doi) t.springer_pdf)
          else if contains publisher "public library of science" then
            (try_transfer Publisher Xml "plos"
               (fun sw -> S.Plos.article ~sw t.plos doi),
             Some ("plos-pdf", fun sw -> S.Plos.pdf ~sw t.plos doi))
          else if contains publisher "wiley" then begin
            if t.wiley = None then ignore (missing "wiley");
            false, Option.map (fun c ->
              "wiley", fun sw -> S.Wiley.pdf ~sw c doi) t.wiley
          end
          else if contains publisher "informa" || contains publisher "taylor" then
            (false, Some ("tandf", fun sw -> S.Tandf.pdf ~sw t.tandf doi))
          else if contains publisher "cambridge university press" then
            (false, Some ("cup", fun sw ->
              let response = S.Cup.locate ~sw t.cup doi in
              let id = Fun.protect ~finally:(fun () -> Fetch.close response)
                (fun () -> status response; S.Cup.id_of_url (Fetch.url response)) in
              match id with
              | Some id -> S.Cup.pdf ~sw t.cup id
              | None -> raise (Remote (Invalid_content
                  "Cambridge did not resolve to an article page"))))
          else false, None in
        let pdf = Option.fold ~none:false ~some:(fun (strategy, f) ->
          try_transfer Publisher Pdf strategy f) publisher_pdf in
        let oa () =
          let link = match r.oa, t.glutton with
            | Some url, _ -> Ok (Some url)
            | None, Some g -> json t S.Glutton.oa_link_jsont
                (fun sw -> S.Glutton.oa_by_doi ~sw g doi)
            | None, None -> Ok None in
          match link with
          | Error e -> record t paper attempts "glutton-oa" e; false
          | Ok None -> false
          | Ok (Some url) -> try_transfer Oa Pdf "glutton-oa"
              (fun sw -> Fetch.get ~sw t.http url) in
        if not pdf && not (oa ()) then begin
          match t.core with
          | None -> ()
          | Some c ->
              match json t S.Core.Search.jsont (fun sw -> S.Core.search ~sw c doi) with
              | Error e -> record t paper attempts "core" e
              | Ok results ->
                  match List.find_map (fun (r : S.Core.Search.result) ->
                    r.download_url) results.results with
                  | None ->
                      (match List.find_map (fun (r : S.Core.Search.result) ->
                         r.id) results.results with
                       | None -> ()
                       | Some id ->
                           match json t S.Core.Output.jsont (fun sw ->
                             S.Core.metadata ~sw c id) with
                           | Error e -> record t paper attempts "core-metadata" e
                           | Ok output ->
                               let matches = Option.bind output.doi (fun value ->
                                 Result.to_option (Doi.of_string value))
                                 |> Option.fold ~none:false ~some:(Doi.equal doi) in
                               if matches then ignore (try_transfer Core Pdf "core"
                                 (fun sw -> S.Core.download ~sw c id))
                               else record t paper attempts "core-metadata"
                                 (Invalid_content "CORE output DOI is missing or different"))
                  | Some url -> ignore (try_transfer Core Pdf "core"
                      (fun sw -> Fetch.get ~sw t.http url))
        end;
        if not xml && !artifacts = [] && !attempts = [] then
          record t paper attempts "sources" (Unavailable "No source found");
        {uid; artifacts = List.rev !artifacts; attempts = List.rev !attempts})

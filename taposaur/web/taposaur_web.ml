open Taposaur
open Proffer
module A = Taposaur_archive
module C = Taposaur_catalog

type page = Home | Search | Paper | Events | Download | Pdf | Read

type t = {
  handle : page -> (Req.t @ local -> Resp.respond @ local -> unit) @ local;
}

let get params key = Option.value (List.assoc_opt key params) ~default:""
let uid params =
  match Uid.of_string (get params "uid") with
  | Ok uid -> uid | Error e -> invalid_arg e
let number params key = match get params key with
  | "" -> None
  | s -> match int_of_string_opt s with
    | Some n -> Some n | None -> invalid_arg (key ^ " must be an integer")

let search_query params =
  let scheme = match get params "scheme" with
    | "" -> None | s -> (match Uid.Scheme.of_string s with
        | Ok s -> Some s | Error e -> invalid_arg e) in
  let holdings = match get params "has" with
    | "" -> C.Query.Any | "pdf" -> C.Query.Pdf
    | "xml" -> C.Query.Xml | "tei" -> C.Query.Tei
    | "s2orc" -> C.Query.S2orc | "unconverted" -> C.Query.Unconverted
    | _ -> invalid_arg "Unknown content filter" in
  let metadata = match get params "metadata" with
    | "" -> []
    | "title" -> [C.Query.Title, C.Query.Present]
    | "abstract" -> [C.Query.Abstract, C.Query.Present]
    | "publisher" -> [C.Query.Publisher, C.Query.Present]
    | "no-title" -> [C.Query.Title, C.Query.Missing]
    | "no-abstract" -> [C.Query.Abstract, C.Query.Missing]
    | "no-publisher" -> [C.Query.Publisher, C.Query.Missing]
    | _ -> invalid_arg "Unknown metadata filter" in
  { (C.Query.make ~text:(get params "q") ()) with scheme; holdings; metadata;
    publisher = get params "publisher"; journal = get params "journal";
    year_from = number params "from"; year_to = number params "to";
    after = List.assoc_opt "after" params }

type response = Html of Status.t * string
  | Artifact of A.paper * Manifest.Artifact.t

let create ~archive ~catalog =
  let prepare page params =
    let html s = Html (Httpz.Res.Success, s) in
    if page = Home then
      html (View.home (C.stats catalog) (C.Shard.errors catalog))
    else if page = Search then
      html (View.search params (C.search catalog (search_query params)))
    else
      let id = uid params in
      match A.find archive id with
      | None -> Html (Httpz.Res.Not_found, View.page "Paper not found" [])
      | Some paper ->
          let m = A.manifest paper in
          if page = Paper then
            html (View.paper (Taposaur_reader.load
              ?index:(C.find_index catalog id) paper))
          else if page = Events then begin
            let events = ref [] and bytes = ref 0 in
            let exception Enough in
            (try A.iter_events ~max_line:(1024 * 1024) paper (fun line e ->
              if line > 1000 then raise Enough;
              let s = match Event.to_string e with Ok s -> s | Error e -> e in
              bytes := !bytes + String.length s;
              if !bytes > 1024 * 1024 then raise Enough;
              events := View.el "pre" [View.txt s] :: !events)
             with Enough -> ());
            html (View.page "Event history" (View.p
              "Up to 1,000 events or 1 MiB in file order. \
               Sequence numbers restart per run."
              :: List.rev !events))
          end else
            let file = get params "file" in
            match List.find_opt (fun (a : Manifest.Artifact.t) -> a.file = file)
              m.artifacts with
            | None ->
                Html (Httpz.Res.Not_found, View.page "Artifact not found" [])
            | Some a ->
                if page = Download then Artifact (paper, a)
                else if page = Pdf && a.format = Manifest.Format.Pdf then
                  html (View.page file [Htmlit.El.v "iframe"
                    ~at:[Htmlit.At.v "src" (View.url "/artifact" params);
                      Htmlit.At.v "title" "Paper PDF"] []])
                else if page = Read && a.format = Manifest.Format.Tei then
                  (match A.with_artifact paper file
                    Taposaur_document.Tei.render_html with
                   | Ok body -> html (View.page "Read paper"
                       [View.link (View.url "/paper" ["uid",Uid.to_string id])
                          "← Paper and sources"; Htmlit.El.unsafe_raw body])
                   | Error e -> Html (Httpz.Res.Unprocessable_entity,
                       View.page "Cannot render TEI" [View.p e]))
                else Html (Httpz.Res.Bad_request,
                  View.page "Unexpected artifact format" [])
  in
  let handle page req (respond @ local) =
    let params = Req.query req in
    let response = try prepare page params with
      | Invalid_argument e -> Html (Httpz.Res.Bad_request,
          View.page "Invalid request" [View.p e])
      | A.Parse_error e -> Html (Httpz.Res.Unprocessable_entity,
          View.page "Invalid archive record" [View.p (A.error_to_string e)])
      | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
          Html (Httpz.Res.Not_found, View.page "File not found" [])
    in
    match response with
    | Html (status, body) -> Resp.html respond ~status
        ~headers:[Resp.other "X-Content-Type-Options" "nosniff";
          Resp.other "Content-Security-Policy"
            "default-src 'none'; style-src 'unsafe-inline'; \
             frame-src 'self'; form-action 'self'; base-uri 'none'"] body
    | Artifact (paper, a) ->
        Artifact.send paper a req respond
  in { handle }

let site =
  let open Route in
  Site.of_routes [
    get root (fun env req respond -> env.handle Home req respond);
    get (s "search") (fun env req respond -> env.handle Search req respond);
    get (s "paper") (fun env req respond -> env.handle Paper req respond);
    get (s "events") (fun env req respond -> env.handle Events req respond);
    get (s "artifact") (fun env req respond -> env.handle Download req respond);
    get (s "pdf") (fun env req respond -> env.handle Pdf req respond);
    get (s "read") (fun env req respond -> env.handle Read req respond)]

type t = Fetch.plain

(* Springer Nature retired spdi.public.springernature.app on 7 August
   2026 and moved the Full Text API to api.springernature.com. The host
   is the only thing that changed: the path, the query parameters, the
   key and its metric suffix and the response are all as they were, and
   the endpoint serves subscribed content as well as open access. The
   retired host answers every path with an HTML error page behind a
   404, which is what a stale integration sees.

   The key carries its metric as a suffix ([<hex>/articles-api]), and
   the metric decides what the key may read: [/articles-api] covers all
   journal articles, where an older institutional metric does not. A
   key without a metric is a 400 saying so. *)
let origin = "https://api.springernature.com"

let base = origin ^ "/xmldata/jats"

let url ~dois =
  (match dois with
   | [] -> invalid_arg "Taposaur_services.Springer.url: no DOIs"
   | _ when List.length dois > 25 ->
     invalid_arg "Taposaur_services.Springer.url: at most 25 DOIs per query"
   | _ -> ());
  let q =
    dois
    |> List.map (fun d -> "doi:" ^ Taposaur.Doi.to_string d)
    |> String.concat " OR "
  in
  Uri.to_string
    (Uri.with_query' (Uri.of_string base) [ "q", "(" ^ q ^ ")"; "p", "25" ])

let client ~api_key t =
  let scope = [ origin ] in
  Fetch.read_only
    (Fetch.restrict ~under:scope
       (Fetch.with_credentials ~scope
          Fetch.Credential.[ Query [ ("api_key", api_key) ] ]
          t))


let articles ~sw t dois = Fetch.get ~sw t (url ~dois)

(* The PDF route needs no API key: SpringerLink and nature.com serve
   PDFs by network entitlement at pattern-derived URLs, the same way
   Taylor & Francis does. Nature Portfolio DOIs (10.1038) live on
   nature.com under the registered suffix; everything else Springer
   publishes is at link.springer.com/content/pdf. *)

let pdf_origin = "https://link.springer.com"
let nature_origin = "https://www.nature.com"

(* Entitled access bounces through the identity provider before the
   PDF is served: content host → idp/authorize (sets a cookie) → back
   to the content host. The client must be allowed to follow. *)
let idp_origins = [ "https://idp.springer.com"; "https://idp.nature.com" ]

let pdf_url ~doi =
  let name = Taposaur.Doi.to_string doi in
  match String.length name > 8 && String.sub name 0 8 = "10.1038/" with
  | true ->
    let suffix = String.sub name 8 (String.length name - 8) in
    nature_origin ^ "/articles/" ^ suffix ^ ".pdf"
  | false -> pdf_origin ^ "/content/pdf/" ^ Taposaur.Doi.path doi ^ ".pdf"

let pdf_client t =
  Fetch.read_only
    (Fetch.restrict ~under:([ pdf_origin; nature_origin ] @ idp_origins) t)

let pdf ~sw t doi = Fetch.get ~sw t (pdf_url ~doi)

let retry =
  Fetch.Retry.v ~max_retries:3
    ~backoff_factor:(Duration.of_sec 1800)
    ~backoff_max:(Duration.of_sec 1800)
    ~status_forcelist:[ 429; 500 ] ~jitter:false ()

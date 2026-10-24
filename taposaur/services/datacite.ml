type t = { server : string; c : Fetch.plain }

let json = Fetch.Header.[ accept, [ pref "application/json" ] ]

(* The identifier travels as a query parameter, so a DOI's ["/"] needs
   no escaping and the path stays one segment. *)
let lookup_url ~server ~doi =
  Uri.to_string
    (Uri.with_query'
       (Uri.of_string (Service.root server ^ "/lookup"))
       [ ("doi", Taposaur.Doi.to_string doi) ])

let stats_url ~server = Service.root server ^ "/stats"

let client ~server t =
  { server = Service.server ~caller:"Datacite.client" server;
    c = Service.plain t }


let server t = t.server

let by_doi ~sw t doi =
  Fetch.get ~sw t.c ~headers:json (lookup_url ~server:t.server ~doi)

let stats ~sw t = Fetch.get ~sw t.c ~headers:json (stats_url ~server:t.server)

type t = Fetch.plain

let origin = "https://api.core.ac.uk"

let base = origin ^ "/v3/outputs/"

let metadata_url ~id = base ^ Uri.pct_encode ~component:`Path id

let download_url ~id = metadata_url ~id ^ "/download"

(* The DOI is quoted, so its punctuation is read as part of one term
   rather than as query syntax. The path ends in a slash because the
   API answers the bare path with a 301 to this one, which costs a
   round trip and loses the query on any client that will not carry it
   across. *)
let search_url ~doi =
  Uri.to_string
    (Uri.with_query'
       (Uri.of_string (origin ^ "/v3/search/works/"))
       [ ("q", "doi:\"" ^ Taposaur.Doi.to_string doi ^ "\"") ])

let client ~token t =
  Fetch.read_only
    (Fetch.with_credentials ~scope:[ origin ] Fetch.Credential.[ Bearer token ]
       t)


let download ~sw t id = Fetch.get ~sw t (download_url ~id)

let metadata ~sw t id =
  Fetch.get ~sw t (metadata_url ~id)
    ~headers:Fetch.Header.[ accept, [ pref "application/json" ] ]

let search ~sw t doi =
  Fetch.get ~sw t (search_url ~doi)
    ~headers:Fetch.Header.[ accept, [ pref "application/json" ] ]

module Search = struct
  type result = { id : string option; download_url : string option }
  type t = { total_hits : int; results : result list }

  (* An output id is a number in a search result and a path segment
     everywhere else, which is why both spellings read as the
     identifier. *)
  let id =
    Jsont.any ~kind:"Id"
      ~dec_number:(Jsont.map ~dec:string_of_int Jsont.int)
      ~dec_string:Jsont.string
      ~enc:(fun _ -> Jsont.string)
      ()

  (* A result with no file carries [""] rather than omitting the
     member, so a blank reads back as no URL. *)
  let result_jsont =
    Jsont.Object.map ~kind:"Result" (fun id download_url ->
        { id; download_url = (match download_url with
                              | Some "" -> None
                              | u -> u) })
    |> Jsont.Object.opt_mem "id" id ~enc:(fun r -> r.id)
    |> Jsont.Object.opt_mem "downloadUrl" Jsont.string
         ~enc:(fun r -> r.download_url)
    |> Jsont.Object.finish

  let jsont =
    Jsont.Object.map ~kind:"Search" (fun total_hits results ->
        { total_hits; results })
    |> Jsont.Object.mem "totalHits" Jsont.int ~dec_absent:(fun () -> 0)
         ~enc:(fun s -> s.total_hits)
    |> Jsont.Object.mem "results" (Jsont.list result_jsont) ~dec_absent:(fun () -> [])
         ~enc:(fun s -> s.results)
    |> Jsont.Object.finish
end

module Output = struct
  type t = { doi : string option; title : string option }

  let jsont =
    Jsont.Object.map ~kind:"Output" (fun doi title -> { doi; title })
    |> Jsont.Object.opt_mem "doi" Jsont.string ~enc:(fun o -> o.doi)
    |> Jsont.Object.opt_mem "title" Jsont.string ~enc:(fun o -> o.title)
    |> Jsont.Object.finish
end

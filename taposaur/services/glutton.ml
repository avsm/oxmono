type t = { server : string; c : Fetch.plain }

(* The service stores PMC identifiers under their ["PMC"] prefix. *)
let pmc id =
  let n = String.length id in
  if n >= 3 && String.uppercase_ascii (String.sub id 0 3) = "PMC" then
    "PMC" ^ String.sub id 3 (n - 3)
  else "PMC" ^ id

let query base ps =
  let ps = List.filter_map (fun (k, v) -> Option.map (fun v -> (k, v)) v) ps in
  Uri.to_string (Uri.with_query' (Uri.of_string base) ps)

let json = Fetch.Header.[ accept, [ pref "application/json" ] ]

let lookup_url ~server ?doi ?pmcid ?pmid ?pii ?istexid ?halid ?first_author
    ?atitle ?jtitle ?volume ?first_page ?year ?biblio ?parse_reference () =
  query
    (Service.root server ^ "/service/lookup")
    [ ("doi", Option.map Taposaur.Doi.to_string doi);
      ("halid", halid);
      ("pmid", pmid);
      ("pmc", Option.map pmc pmcid);
      ("pii", pii);
      ("istexid", istexid);
      ("firstAuthor", first_author);
      ("atitle", atitle);
      ("jtitle", jtitle);
      ("volume", volume);
      ("firstPage", first_page);
      ("year", year);
      ("biblio", biblio);
      ("parseReference", Option.map string_of_bool parse_reference) ]

let oa_url ~server ?doi ?pmcid ?pmid ?pii () =
  query
    (Service.root server ^ "/service/oa")
    [ ("doi", Option.map Taposaur.Doi.to_string doi);
      ("pmid", pmid);
      ("pmc", Option.map pmc pmcid);
      ("pii", pii) ]

let data_url ~server = Service.root server ^ "/service/data"

let client ~server t =
  { server = Service.server ~caller:"Glutton.client" server;
    c = Service.plain t }


let server t = t.server

let by_doi ~sw ?first_author ?atitle ?year t doi =
  Fetch.get ~sw t.c ~headers:json
    (lookup_url ~server:t.server ~doi ?first_author ?atitle ?year ())

let by_pmid ~sw ?first_author ?atitle ?year t pmid =
  Fetch.get ~sw t.c ~headers:json
    (lookup_url ~server:t.server ~pmid ?first_author ?atitle ?year ())

let by_pmcid ~sw ?first_author ?atitle ?year t pmcid =
  Fetch.get ~sw t.c ~headers:json
    (lookup_url ~server:t.server ~pmcid ?first_author ?atitle ?year ())

let by_pii ~sw ?first_author ?atitle ?year t pii =
  Fetch.get ~sw t.c ~headers:json
    (lookup_url ~server:t.server ~pii ?first_author ?atitle ?year ())

let by_metadata ~sw ?first_author ?atitle ?jtitle ?volume ?first_page
    ?year t () =
  Fetch.get ~sw t.c ~headers:json
    (lookup_url ~server:t.server ?first_author ?atitle ?jtitle ?volume
       ?first_page ?year ())

let by_reference ~sw ?parse_reference ?first_author ?atitle ?jtitle
    ?year t biblio =
  Fetch.get ~sw t.c ~headers:json
    (lookup_url ~server:t.server ~biblio ?parse_reference ?first_author
       ?atitle ?jtitle ?year ())

let oa_by_doi ~sw t doi =
  Fetch.get ~sw t.c ~headers:json (oa_url ~server:t.server ~doi ())

let oa_by_pmcid ~sw t pmcid =
  Fetch.get ~sw t.c ~headers:json (oa_url ~server:t.server ~pmcid ())

let oa_by_pmid ~sw t pmid =
  Fetch.get ~sw t.c ~headers:json (oa_url ~server:t.server ~pmid ())

let data ~sw t = Fetch.get ~sw t.c ~headers:json (data_url ~server:t.server)

(* Reading the answers *)

let first = function [] -> None | x :: _ -> Some x

module Record = struct
  type t = {
    doi : string option;
    title : string option;
    journal : string option;
    year : int option;
    publisher : string option;
    pmid : string option;
    pmcid : string option;
    oa : string option;
    kind : string option;
        (* Crossref's own word for what the record is:
           ["journal-article"], ["journal-issue"], ["journal"] and the
           rest. A journal list carries issue and journal DOIs among its
           articles, and asking a publisher for the full text of an
           issue is a request that cannot be met. *)
  }

  (* A cross-identifier is a number in some records and a string in
     others, and both spell the same one. *)
  let id =
    Jsont.any ~kind:"Id"
      ~dec_number:(Jsont.map ~dec:string_of_int Jsont.int)
      ~dec_string:Jsont.string
      ~enc:(fun _ -> Jsont.string)
      ()

  (* Crossref gives a title and a journal name as arrays, since a work
     can carry a translated title beside its own and a journal a former
     name. A record that writes one as a bare string is read as the one
     entry it is, rather than failing the whole parse over a field
     nothing dispatches on. *)
  let texts =
    Jsont.any ~kind:"Texts"
      ~dec_string:(Jsont.map ~dec:(fun s -> [ s ]) Jsont.string)
      ~dec_array:(Jsont.list Jsont.string)
      ~enc:(fun _ -> Jsont.list Jsont.string)
      ()

  (* A Crossref date is [{"date-parts": [[2013, 7, 31]]}], a list of
     dates because one can be a range, each a list running year, month,
     day and stopping at the precision known. A part is null in a record
     that gives none, so the year is an option even when the member is
     there. *)
  let date =
    Jsont.Object.map ~kind:"Date" (function (y :: _) :: _ -> y | _ -> None)
    |> Jsont.Object.mem "date-parts"
         (Jsont.list (Jsont.list (Jsont.option Jsont.int)))
         ~dec_absent:(fun () -> [])
         ~enc:(fun y -> [ [ y ] ])
    |> Jsont.Object.finish

  (* [published] is Crossref's earliest date and the two others are the
     print and online ones, which is all a record without it carries. On
     encoding the year is written back as [published] alone. *)
  let jsont =
    Jsont.Object.map ~kind:"Record"
      (fun doi title journal published print online publisher pmid pmcid oa
           kind ->
        { doi; title = first title; journal = first journal;
          year = List.find_map Fun.id [ published; print; online ]; publisher;
          pmid; pmcid; oa; kind })
    |> Jsont.Object.opt_mem "DOI" Jsont.string ~enc:(fun r -> r.doi)
    |> Jsont.Object.mem "title" texts ~dec_absent:(fun () -> [])
         ~enc:(fun r -> Option.to_list r.title)
         ~enc_omit:(fun t -> t = [])
    |> Jsont.Object.mem "container-title" texts ~dec_absent:(fun () -> [])
         ~enc:(fun r -> Option.to_list r.journal)
         ~enc_omit:(fun t -> t = [])
    |> Jsont.Object.mem "published" date ~dec_absent:(fun () -> None)
         ~enc:(fun r -> r.year) ~enc_omit:Option.is_none
    |> Jsont.Object.mem "published-print" date ~dec_absent:(fun () -> None)
         ~enc:(fun _ -> None) ~enc_omit:Option.is_none
    |> Jsont.Object.mem "published-online" date ~dec_absent:(fun () -> None)
         ~enc:(fun _ -> None) ~enc_omit:Option.is_none
    |> Jsont.Object.opt_mem "publisher" Jsont.string ~enc:(fun r -> r.publisher)
    |> Jsont.Object.opt_mem "pmid" id ~enc:(fun r -> r.pmid)
    |> Jsont.Object.opt_mem "pmcid" id ~enc:(fun r -> r.pmcid)
    |> Jsont.Object.opt_mem "oaLink" Jsont.string ~enc:(fun r -> r.oa)
    |> Jsont.Object.opt_mem "type" Jsont.string ~enc:(fun r -> r.kind)
    |> Jsont.Object.finish
end

let oa_link_jsont =
  Jsont.Object.map ~kind:"Oa" Fun.id
  |> Jsont.Object.opt_mem "oaLink" Jsont.string ~enc:Fun.id
  |> Jsont.Object.finish

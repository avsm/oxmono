type venue = {id:string; name:string option; issn_l:string option;
  issns:string list; kind:string option; host:string option}
let venue_jsont =
  Jsont.Object.map (fun id name issn_l issns kind host ->
    {id;name;issn_l;issns;kind;host})
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun v -> v.id)
  |> Jsont.Object.opt_mem "name" Jsont.string ~enc:(fun v -> v.name)
  |> Jsont.Object.opt_mem "issn_l" Jsont.string ~enc:(fun v -> v.issn_l)
  |> Jsont.Object.mem "issns" (Jsont.list Jsont.string)
       ~dec_absent:(fun () -> []) ~enc:(fun v -> v.issns)
  |> Jsont.Object.opt_mem "kind" Jsont.string ~enc:(fun v -> v.kind)
  |> Jsont.Object.opt_mem "host" Jsont.string ~enc:(fun v -> v.host)
  |> Jsont.Object.finish
type t = {doi:string; bibliography:Bibliography.t; venue:venue option;
  provider:string; endpoint:string; sha256:string; retrieved:float}
let jsont =
  Jsont.Object.map (fun doi bibliography venue provider endpoint sha256 retrieved ->
    {doi;bibliography;venue;provider;endpoint;sha256;retrieved})
  |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun r -> r.doi)
  |> Jsont.Object.mem "bibliography" Bibliography.jsont ~enc:(fun r -> r.bibliography)
  |> Jsont.Object.opt_mem "venue" venue_jsont ~enc:(fun r -> r.venue)
  |> Jsont.Object.mem "provider" Jsont.string ~enc:(fun r -> r.provider)
  |> Jsont.Object.mem "endpoint" Jsont.string ~enc:(fun r -> r.endpoint)
  |> Jsont.Object.mem "sha256" Jsont.string ~enc:(fun r -> r.sha256)
  |> Jsont.Object.mem "retrieved" Jsont.number ~enc:(fun r -> r.retrieved)
  |> Jsont.Object.finish
let label r = r.provider ^ " · " ^ r.sha256
let fields r =
  let b = r.bibliography in
  let text name = function
    | Some s when String.trim s <> "" -> Some name | _ -> None in
  List.filter_map Fun.id [
    text "title" b.title;
    (if b.authors = [] then None else Some "authors");
    text "journal" b.journal; text "publisher" b.publisher;
    text "published" b.published;
    Option.map (fun _ -> "year") b.year;
    text "volume" b.volume; text "issue" b.issue; text "pages" b.pages;
    text "issn" b.issn; text "doi" b.doi; text "pmcid" b.pmcid;
    text "pmid" b.pmid]

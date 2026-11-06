type t = {doi:string; bibliography:Bibliography.t; provider:string;
  endpoint:string; sha256:string; retrieved:float}
let jsont =
  Jsont.Object.map (fun doi bibliography provider endpoint sha256 retrieved ->
    {doi;bibliography;provider;endpoint;sha256;retrieved})
  |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun r -> r.doi)
  |> Jsont.Object.mem "bibliography" Bibliography.jsont ~enc:(fun r -> r.bibliography)
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

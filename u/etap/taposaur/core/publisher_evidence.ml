type t = {doi:string; publisher:string; provider:string; endpoint:string;
  sha256:string; retrieved:float}
let jsont =
  Jsont.Object.map (fun doi publisher provider endpoint sha256 retrieved ->
    {doi;publisher;provider;endpoint;sha256;retrieved})
  |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun r -> r.doi)
  |> Jsont.Object.mem "publisher" Jsont.string ~enc:(fun r -> r.publisher)
  |> Jsont.Object.mem "provider" Jsont.string ~enc:(fun r -> r.provider)
  |> Jsont.Object.mem "endpoint" Jsont.string ~enc:(fun r -> r.endpoint)
  |> Jsont.Object.mem "sha256" Jsont.string ~enc:(fun r -> r.sha256)
  |> Jsont.Object.mem "retrieved" Jsont.number ~enc:(fun r -> r.retrieved)
  |> Jsont.Object.finish
let label r = r.provider ^ " · " ^ r.sha256

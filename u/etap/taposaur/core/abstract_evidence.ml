type t = {doi:string; abstract:string; provider:string; endpoint:string;
  sha256:string; retrieved:float}
let jsont =
  Jsont.Object.map (fun doi abstract provider endpoint sha256 retrieved ->
    {doi;abstract;provider;endpoint;sha256;retrieved})
  |> Jsont.Object.mem "doi" Jsont.string ~enc:(fun r -> r.doi)
  |> Jsont.Object.mem "abstract" Jsont.string ~enc:(fun r -> r.abstract)
  |> Jsont.Object.mem "provider" Jsont.string ~enc:(fun r -> r.provider)
  |> Jsont.Object.mem "endpoint" Jsont.string ~enc:(fun r -> r.endpoint)
  |> Jsont.Object.mem "sha256" Jsont.string ~enc:(fun r -> r.sha256)
  |> Jsont.Object.mem "retrieved" Jsont.number ~enc:(fun r -> r.retrieved)
  |> Jsont.Object.finish
let label r = r.provider ^ " · " ^ r.sha256

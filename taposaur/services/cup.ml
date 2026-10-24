type t = Fetch.plain

let origin = "https://www.cambridge.org"
let identifier_url ~doi =
  origin ^ "/core/product/identifier/"
  ^ Uriz.pct_encode ~component:`Unreserved
      (String.uppercase_ascii (Taposaur.Doi.suffix doi))
  ^ "/type/journal_article"
let valid_id id =
  String.length id >= 8 && String.length id <= 128 &&
  String.for_all (function 'A'..'Z' | 'a'..'z' | '0'..'9' -> true | _ -> false) id
let content_url ~id =
  if not (valid_id id) then invalid_arg "Cup.content_url: invalid article id";
  origin ^ "/core/services/aop-cambridge-core/content/view/" ^ id
let id_of_url url =
  let u = Uri.of_string url in
  if Uri.scheme u <> Some "https" || Uri.host u <> Some "www.cambridge.org"
     || Uri.userinfo u <> None || (Uri.port u <> None && Uri.port u <> Some 443)
  then None
  else match String.split_on_char '/' (Uri.path u) with
    | "" :: "core" :: "journals" :: rest when List.mem "article" rest ->
        (match List.rev rest with id :: _ when valid_id id -> Some id | _ -> None)
    | _ -> None
let client t = Fetch.read_only (Fetch.restrict ~under:[origin] t)
let locate ~sw t doi = Fetch.get ~sw t (identifier_url ~doi)
let pdf ~sw t id = Fetch.get ~sw t (content_url ~id)

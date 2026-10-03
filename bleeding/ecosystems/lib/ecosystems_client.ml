let pages ?(per_page = 100) f =
  if per_page < 1 then invalid_arg "Ecosystems_client.pages: per_page < 1";
  let rec from page () =
    match f ~page:(string_of_int page) ~per_page:(string_of_int per_page) with
    | [] -> Seq.Nil
    | items -> Seq.append (List.to_seq items) (from (page + 1)) ()
  in
  from 1

let default_base_url = "https://packages.ecosyste.ms/api/v1"

let create ?(user_agent = "ocaml-ecosystems") ?(base_url = default_base_url)
    ~sw env =
  let session =
    Fetch_cookies.std ~cookies:`Off env (Fetch_curl.v ~sw ~user_agent ())
  in
  Ecosystems.create ~session ~sw env ~base_url

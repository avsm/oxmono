let pages ?(per_page = 100) f =
  let rec from page () =
    match f ~page:(string_of_int page) ~per_page:(string_of_int per_page) with
    | [] -> Seq.Nil
    | items ->
        let rest =
          if List.length items < per_page then Seq.empty else from (page + 1)
        in
        Seq.append (List.to_seq items) rest ()
  in
  from 1

let default_base_url = "https://packages.ecosyste.ms/api/v1"

let create ?(user_agent = "ocaml-ecosystems") ?(base_url = default_base_url)
    ~sw env =
  let session = Fetch_curl.v ~sw ~user_agent () in
  Ecosystems.create ~session ~sw env ~base_url

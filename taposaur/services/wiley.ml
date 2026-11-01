type t = Fetch.plain

let origin = "https://api.wiley.com"

let url ~doi = origin ^ "/onlinelibrary/tdm/v1/articles/" ^ Taposaur.Doi.path doi

let client ~tokens t =
  if tokens = [] then invalid_arg "Taposaur_services.Wiley.client: no tokens";
  let pool = Array.of_list tokens in
  let next = ref 0 in
  let token _req =
    let i = !next in
    next := (i + 1) mod Array.length pool;
    pool.(i)
  in
  Fetch.read_only
    (Fetch.with_credentials ~scope:[ origin ]
       Fetch.Credential.[ Header ("Wiley-TDM-Client-Token", token) ]
       t)

let pdf ~sw t doi = Fetch.get ~sw t (url ~doi)

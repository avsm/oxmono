type t = Fetch.plain

let origin = "https://api.elsevier.com"

let url ~doi = origin ^ "/content/article/doi/" ^ Taposaur.Doi.path doi

let client ~api_key ?inst_token t =
  let scope = [ origin ] in
  let open Fetch.Credential in
  let creds =
    Header ("X-ELS-APIKey", fun _ -> api_key ())
    :: (match inst_token with
        | None -> []
        | Some tok -> [ Header ("X-ELS-Insttoken", fun _ -> tok ()) ])
  in
  Fetch.read_only
    (Fetch.restrict ~under:scope (Fetch.with_credentials ~scope creds t))


let article ~sw t doi =
  Fetch.get ~sw t (url ~doi)
    ~headers:Fetch.Header.[ accept, [ pref "application/xml" ] ]

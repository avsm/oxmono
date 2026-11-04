(* Explicitly configured service clients, built once per CLI invocation.
   Credentials come from the Taposaur keychain, with environment variables
   as fallbacks. Service addresses come from the environment or flags.
   Library code never reads either. *)
module S = Taposaur_services
module Secrets = Taposaur_secrets
let getenv name = match Sys.getenv_opt name with
  | None | Some "" -> None | Some value -> Some value
let secrets ~sw env = Secrets.open_ ~sw ~fs:env#fs ()
let user_agent = "u/etap/taposaur/0.1"
let repository_agent =
  "Mozilla/5.0 (compatible; u/etap/taposaur/1.0; +mailto:avsm2@cam.ac.uk)"

(* Strategies whose prerequisites this process has. PLOS, Taylor & Francis
   and Cambridge need network entitlement rather than a credential, so they
   count as configured. Springer PDF entitlement is a separate opt-in. *)
let configured secrets =
  let has c = Secrets.find secrets c <> None in
  List.filter (fun strategy -> match strategy with
    | "elsevier" -> has Elsevier_api_key
    | "springer" -> has Springer_api_key
    | "wiley" -> has Wiley_tokens
    | "oa" -> getenv "GLUTTON_SERVER_URL" <> None
    | _ -> true) Taposaur_jobs.Acquisition.strategies

let http ~sw ?(max_concurrent=6) env =
  Fetch_main.std ~sw ~user_agent ~max_concurrent
    ~retry:(Fetch.Retry.v ~max_retries:0 ()) env

(* Some publishers admit one whitelisted address. TANDF_PROXY names a SOCKS
   or HTTP proxy URL, such as socks5h://127.0.0.1:1080 for an SSH tunnel to
   that address, and Taylor & Francis requests travel through it. *)
let tandf_proxy () = getenv "TANDF_PROXY"
let proxied ~sw ~proxy env =
  Fetch_cookies.std ~retry:(Fetch.Retry.v ~max_retries:0 ()) ~max_concurrent:1
    env (Fetch_curl.v ~sw ~proxy ~user_agent ())

(* [downloader ~sw ~audit env] configures acquisition. [busy_retries]
   defaults to the library's in-process retry loop. Queue workers pass zero
   so busy publishers become scheduled retries instead of leased sleeps. *)
let downloader ?busy_retries ?(publisher_pdf=false) ?(timeout=120.)
    ?(max_bytes=128*1024*1024) ~sw ~audit ~secrets env =
  let http = http ~sw env in
  let secret c = Secrets.value secrets c in
  let paced seconds = Fetch.with_limits ~clock:env#mono_clock
    ~max_concurrent:1 ~min_interval:(Duration.of_sec seconds) http in
  let glutton = Option.map (fun server -> S.Glutton.client ~server http)
    (getenv "GLUTTON_SERVER_URL") in
  let datacite = Option.map (fun server -> S.Datacite.client ~server http)
    (getenv "DATACITE_SERVER_URL") in
  let elsevier = Option.map (fun api_key ->
    S.Elsevier.client ~api_key ?inst_token:(secret Elsevier_inst_token)
      (paced 2)) (secret Elsevier_api_key) in
  let springer = Option.map (fun key ->
    S.Springer.client ~api_key:(key ()) (paced 2)) (secret Springer_api_key) in
  let wiley = Option.bind (secret Wiley_tokens) (fun values ->
    let tokens = String.split_on_char ',' (values ()) |> List.map String.trim
      |> List.filter ((<>) "") in
    if tokens = [] then None else Some (S.Wiley.client ~tokens (paced 2))) in
  let core = Option.map (fun token -> S.Core.client ~token (paced 2))
    (secret Core_api_token) in
  let cup = S.Cup.client (paced 30) in
  let plos = S.Plos.client (paced 2) in
  let tandf = match tandf_proxy () with
    | None -> S.Tandf.client (paced 2)
    | Some proxy -> S.Tandf.client (Fetch.with_limits ~clock:env#mono_clock
        ~max_concurrent:1 ~min_interval:(Duration.of_sec 2)
        (proxied ~sw ~proxy env)) in
  let springer_pdf = if publisher_pdf then
    Some (S.Springer.pdf_client (paced 2)) else None in
  let repositories = Fetch.with_headers Fetch.Header.[
    user_agent, repository_agent ] (paced 2) in
  Taposaur_download.create ~audit ~timeout ~max_bytes ?busy_retries
    ?glutton ?datacite ?elsevier ?springer ?wiley ?core
    ~cup ~plos ~tandf ?springer_pdf ~clock:env#clock repositories

(* Converter services are self-hosted. One request in flight per service
   keeps GROBID and Pub2TEI responsive, as their interfaces recommend. *)
let grobid ~sw ?server env =
  Option.map (fun server -> S.Grobid.client ~server
    (http ~sw ~max_concurrent:2 env))
    (match server with Some s -> Some s | None -> getenv "GROBID_SERVER_URL")
let pub2tei ~sw ?server env =
  Option.map (fun server -> S.Pub2tei.client ~server
    (http ~sw ~max_concurrent:1 env))
    (match server with Some s -> Some s | None -> getenv "PUB2TEI_SERVER_URL")

type backend = [ `Curl | `Macos ]

let env_var = "FETCH_BACKEND"

let pp ppf = function
  | `Curl -> Fmt.string ppf "curl"
  | `Macos -> Fmt.string ppf "macos"

let of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "curl" -> Some `Curl
  | "macos" -> Some `Macos
  | _ -> None

(* A typo in the variable must not silently fall back to the default:
   whoever set it meant to steer the choice, so tell them it missed. *)
let of_env () =
  match Sys.getenv_opt env_var with
  | None | Some "" -> None
  | Some s ->
    match of_string s with
    | Some b -> Some b
    | None ->
      invalid_arg
        (Fmt.str "Fetch_main: %s=%S: unknown backend (expected \"curl\" or \
                  \"macos\")" env_var s)

let select ?backend () =
  let b =
    match backend with
    | Some b -> b
    | None ->
      match of_env () with
      | Some b -> b
      | None -> if Fetch_macos.available then `Macos else `Curl
  in
  match b with
  | `Macos when not Fetch_macos.available ->
    invalid_arg
      "Fetch_main: the macos backend needs NSURLSession, which this platform \
       does not have"
  | b -> b

let std ~sw ?backend ?user_agent ?headers ?cookies ?retry ?max_concurrent
    ?min_interval env =
  let t =
    match select ?backend () with
    | `Macos ->
      Fetch_macos.std ?cookies ?retry ?max_concurrent ?min_interval env
    | `Curl ->
      Fetch_curl.std ~sw ?cookies ?retry ?max_concurrent ?min_interval env
  in
  (* Defaults, not policy: a header the request already carries wins,
     and the wrappers apply to every backend alike — the backend's own
     built-in User-Agent only ever fires when neither the request nor
     [user_agent] named one. *)
  let t =
    match headers with
    | None -> t
    | Some hs -> Fetch.with_headers ~mode:`If_absent hs t
  in
  match user_agent with
  | None -> t
  | Some ua -> Fetch.with_headers ~mode:`If_absent Fetch.Header.[ user_agent, ua ] t

let std_ro ~sw ?backend ?user_agent ?headers ?cookies ?retry ?max_concurrent
    ?min_interval env =
  Fetch.read_only
    (std ~sw ?backend ?user_agent ?headers ?cookies ?retry ?max_concurrent
       ?min_interval env)

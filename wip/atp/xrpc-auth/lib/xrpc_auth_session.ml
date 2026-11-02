(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type oauth = {
  issuer : string;
  token_endpoint : string;
  revocation_endpoint : string option;
  client_id : string;
  dpop_key : string;
  scope : string;
  expires_at : string;
}

type t = {
  access_jwt : string;
  refresh_jwt : string;
  did : string;
  handle : string;
  pds : string;
  created_at : string;
  oauth : oauth option;
}

let oauth_jsont =
  Jsont.Object.map ~kind:"OAuth"
    (fun issuer token_endpoint revocation_endpoint client_id dpop_key scope
         expires_at ->
      { issuer; token_endpoint; revocation_endpoint; client_id; dpop_key;
        scope; expires_at })
  |> Jsont.Object.mem "issuer" Jsont.string ~enc:(fun o -> o.issuer)
  |> Jsont.Object.mem "token_endpoint" Jsont.string
       ~enc:(fun o -> o.token_endpoint)
  |> Jsont.Object.opt_mem "revocation_endpoint" Jsont.string
       ~enc:(fun o -> o.revocation_endpoint)
  |> Jsont.Object.mem "client_id" Jsont.string ~enc:(fun o -> o.client_id)
  |> Jsont.Object.mem "dpop_key" Jsont.string ~enc:(fun o -> o.dpop_key)
  |> Jsont.Object.mem "scope" Jsont.string ~enc:(fun o -> o.scope)
  |> Jsont.Object.mem "expires_at" Jsont.string ~enc:(fun o -> o.expires_at)
  |> Jsont.Object.finish

let jsont =
  Jsont.Object.map ~kind:"Session"
    (fun access_jwt refresh_jwt did handle pds created_at oauth ->
      { access_jwt; refresh_jwt; did; handle; pds; created_at; oauth })
  |> Jsont.Object.mem "access_jwt" Jsont.string ~enc:(fun s -> s.access_jwt)
  |> Jsont.Object.mem "refresh_jwt" Jsont.string ~enc:(fun s -> s.refresh_jwt)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun s -> s.did)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun s -> s.handle)
  |> Jsont.Object.mem "pds" Jsont.string ~enc:(fun s -> s.pds)
  |> Jsont.Object.mem "created_at" Jsont.string ~enc:(fun s -> s.created_at)
  |> Jsont.Object.opt_mem "oauth" oauth_jsont ~enc:(fun s -> s.oauth)
  |> Jsont.Object.finish

(* App config stores the current profile *)
type app_config = { current_profile : string }

let app_config_jsont =
  Jsont.Object.map ~kind:"AppConfig" (fun current_profile ->
      { current_profile })
  |> Jsont.Object.mem "current_profile" Jsont.string ~enc:(fun c ->
      c.current_profile)
  |> Jsont.Object.finish

let default_profile = "default"

exception Invalid_session of string

let validate_name name =
  if name = "" || name = "." || name = ".." ||
     String.exists (fun c -> c = '/' || c = '\\' || Char.code c < 32 || Char.code c = 127) name then
    invalid_arg "Profile and application names must be non-empty path components"

let mkdir_if_missing ~perm path =
  match Eio.Path.kind ~follow:false path with
  | `Not_found -> Eio.Path.mkdir ~perm path
  | `Directory -> ()
  | _ -> invalid_arg "Configuration path must be a directory, not a symlink or file"

let base_config_dir fs ~app_name =
  validate_name app_name;
  let root = match Sys.getenv_opt "XDG_CONFIG_HOME" with
    | Some value when value <> "" && not (Filename.is_relative value) -> value
    | _ -> Filename.concat (Sys.getenv "HOME") ".config" in
  let path = Eio.Path.(fs / root) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 path;
  let path = Eio.Path.(path / app_name) in
  mkdir_if_missing ~perm:0o700 path;
  path

let profiles_dir fs ~app_name =
  let path = Eio.Path.(base_config_dir fs ~app_name / "profiles") in
  mkdir_if_missing ~perm:0o700 path;
  path

let read_json codec path =
  try
    match Jsont_bytesrw.decode_string codec (Eio.Path.load path) with
    | Ok value -> Some value
    | Error _ -> raise (Invalid_session "Malformed saved configuration or session")
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save_private path content =
  let parent, name = match Eio.Path.split path with
    | Some parts -> parts | None -> invalid_arg "Cannot save over a directory root" in
  let rec write attempt =
    if attempt = 100 then failwith "Unable to allocate temporary config file";
    let temp = Eio.Path.(parent / Printf.sprintf ".%s.%d.%d.tmp" name (Unix.getpid ()) attempt) in
    try
      Eio.Switch.run (fun sw ->
        let file = Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) temp in
        Fun.protect ~finally:(fun () ->
          try Eio.Path.unlink temp with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ())
          (fun () -> Eio.Flow.copy_string content file;
            Eio.File.sync file;
            Eio.Path.rename temp path))
    with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> write (attempt + 1)
  in
  write 0

let encode_save codec path value =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec value with
  | Ok content -> save_private path content
  | Error msg -> raise (Invalid_session msg)

let app_config_file fs ~app_name = Eio.Path.(base_config_dir fs ~app_name / "config.json")

let get_current_profile fs ~app_name =
  let profile = match read_json app_config_jsont (app_config_file fs ~app_name) with
    | Some config -> config.current_profile | None -> default_profile in
  validate_name profile;
  profile

let set_current_profile fs ~app_name current_profile =
  validate_name current_profile;
  encode_save app_config_jsont (app_config_file fs ~app_name) { current_profile }

let config_dir fs ~app_name ?profile () =
  let profile = match profile with Some p -> p | None -> get_current_profile fs ~app_name in
  validate_name profile;
  let path = Eio.Path.(profiles_dir fs ~app_name / profile) in
  mkdir_if_missing ~perm:0o700 path;
  path

let list_profiles fs ~app_name =
  let root = profiles_dir fs ~app_name in
  Eio.Path.read_dir root |> List.filter (fun name ->
    let path = Eio.Path.(root / name) in
    Eio.Path.kind ~follow:false path = `Directory &&
    Eio.Path.kind ~follow:false Eio.Path.(path / "session.json") = `Regular_file)
  |> List.sort String.compare

let session_file fs ~app_name ?profile () = Eio.Path.(config_dir fs ~app_name ?profile () / "session.json")
let load fs ~app_name ?profile () = read_json jsont (session_file fs ~app_name ?profile ())
let save fs ~app_name ?profile session = encode_save jsont (session_file fs ~app_name ?profile ()) session
let clear fs ~app_name ?profile () =
  try Eio.Path.unlink (session_file fs ~app_name ?profile ())
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let is_expired ?(leeway = Ptime.Span.of_int_s 60) session =
  match session.oauth with
  | None -> Xrpc.Jwt.is_expired ~leeway session.access_jwt
  | Some oauth -> (
      match Ptime.of_rfc3339 oauth.expires_at with
      | Ok (expires, _, _) ->
          let now = Ptime_clock.now () in
          not (Ptime.is_later expires ~than:(Option.value ~default:now
            (Ptime.add_span now leeway)))
      | Error _ -> true)

let pp ppf session =
  Fmt.pf ppf "@[<v>Handle: %s@,DID: %s@,PDS: %s@,Created: %s" session.handle
    session.did session.pds session.created_at;
  (match session.oauth with
   | None -> Fmt.pf ppf "@,Login: password"
   | Some o ->
       Fmt.pf ppf "@,Login: OAuth via %s@,Scope: %s" o.issuer o.scope);
  Fmt.pf ppf "@]"

let of_xrpc ~pds (xrpc : Xrpc.Types.session) =
  {
    access_jwt = xrpc.access_jwt;
    refresh_jwt = xrpc.refresh_jwt;
    did = xrpc.did;
    handle = xrpc.handle;
    pds;
    created_at = Ptime.to_rfc3339 (Ptime_clock.now ());
    oauth = None;
  }

let to_xrpc session : Xrpc.Types.session =
  {
    access_jwt = session.access_jwt;
    refresh_jwt = session.refresh_jwt;
    did = session.did;
    handle = session.handle;
    pds_uri = Some session.pds;
    email = None;
    email_confirmed = None;
    email_auth_factor = None;
    active = None;
    status = None;
  }

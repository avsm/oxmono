(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  actor_uri : string;
  (* HTTP Signature auth (optional for OAuth-only sessions) *)
  key_id : string option;
  private_key_pem : string option;
  (* Mastodon OAuth (optional for signature-only sessions) *)
  oauth_instance : string option;
  oauth_access_token : string option;
  oauth_client_id : string option;
  oauth_client_secret : string option;
  created_at : string;
}

let jsont =
  Jsont.Object.map ~kind:"Session"
    (fun actor_uri key_id private_key_pem oauth_instance oauth_access_token
         oauth_client_id oauth_client_secret created_at ->
      { actor_uri; key_id; private_key_pem; oauth_instance; oauth_access_token;
        oauth_client_id; oauth_client_secret; created_at })
  |> Jsont.Object.mem "actor_uri" Jsont.string ~enc:(fun s -> s.actor_uri)
  |> Jsont.Object.opt_mem "key_id" Jsont.string ~enc:(fun s -> s.key_id)
  |> Jsont.Object.opt_mem "private_key_pem" Jsont.string
       ~enc:(fun s -> s.private_key_pem)
  |> Jsont.Object.opt_mem "oauth_instance" Jsont.string
       ~enc:(fun s -> s.oauth_instance)
  |> Jsont.Object.opt_mem "oauth_access_token" Jsont.string
       ~enc:(fun s -> s.oauth_access_token)
  |> Jsont.Object.opt_mem "oauth_client_id" Jsont.string
       ~enc:(fun s -> s.oauth_client_id)
  |> Jsont.Object.opt_mem "oauth_client_secret" Jsont.string
       ~enc:(fun s -> s.oauth_client_secret)
  |> Jsont.Object.mem "created_at" Jsont.string ~enc:(fun s -> s.created_at)
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

let save_document fs ~app_name ?profile ~id json =
  let root = Eio.Path.(config_dir fs ~app_name ?profile () / "objects") in
  mkdir_if_missing ~perm:0o700 root;
  let name = Digestif.SHA256.(to_hex (digest_string id)) ^ ".json" in
  encode_save Jsont.json Eio.Path.(root / name) json

let pp ppf session =
  Fmt.pf ppf "@[<v>Actor: %s@," session.actor_uri;
  Option.iter (fun k -> Fmt.pf ppf "Key ID: %s@," k) session.key_id;
  Option.iter (fun i -> Fmt.pf ppf "OAuth Instance: %s@," i) session.oauth_instance;
  (match session.oauth_access_token with
   | Some _ -> Fmt.pf ppf "OAuth: Configured@,"
   | None -> ());
  Fmt.pf ppf "Created: %s@]" session.created_at

(* Create a signature-based session from components *)
let create ~actor_uri ~key_id ~private_key_pem =
  {
    actor_uri;
    key_id = Some key_id;
    private_key_pem = Some private_key_pem;
    oauth_instance = None;
    oauth_access_token = None;
    oauth_client_id = None;
    oauth_client_secret = None;
    created_at = Ptime.to_rfc3339 (Ptime_clock.now ());
  }

(* Create an OAuth-based session *)
let create_oauth ~actor_uri ~instance ~access_token ~client_id ~client_secret =
  {
    actor_uri;
    key_id = None;
    private_key_pem = None;
    oauth_instance = Some instance;
    oauth_access_token = Some access_token;
    oauth_client_id = Some client_id;
    oauth_client_secret = Some client_secret;
    created_at = Ptime.to_rfc3339 (Ptime_clock.now ());
  }

(* Merge OAuth credentials into an existing session (for hybrid auth) *)
let add_oauth session ~instance ~access_token ~client_id ~client_secret =
  { session with
    oauth_instance = Some instance;
    oauth_access_token = Some access_token;
    oauth_client_id = Some client_id;
    oauth_client_secret = Some client_secret;
  }

(* Check if session has signature auth *)
let has_signature session =
  Option.is_some session.key_id && Option.is_some session.private_key_pem

(* Check if session has OAuth auth *)
let has_oauth session =
  Option.is_some session.oauth_access_token && Option.is_some session.oauth_instance

(* Extract a profile name from an actor URI *)
let profile_name_of_actor_uri uri =
  (* Convert https://example.com/users/alice to alice@example.com *)
  match Uriz.of_string uri with
  | Null -> "default"
  | This uri -> match Uriz.host uri with
  | This host ->
      let path = Uriz.path uri in
      let name = Filename.basename path in
      if name = "" || name = "/" then host else name ^ "@" ^ host
  | Null -> "default"

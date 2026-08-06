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

(* Helper to create directory if it doesn't exist *)
let mkdir_if_missing ~perm path =
  try Eio.Path.mkdir ~perm path
  with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ()

(* Base config directory for the app *)
let base_config_dir fs ~app_name =
  let home = Sys.getenv "HOME" in
  (* Ensure ~/.config exists first *)
  let dot_config = Eio.Path.(fs / home / ".config") in
  mkdir_if_missing ~perm:0o755 dot_config;
  (* Then create the app-specific directory *)
  let config_path = Eio.Path.(dot_config / app_name) in
  mkdir_if_missing ~perm:0o700 config_path;
  config_path

(* Profiles directory *)
let profiles_dir fs ~app_name =
  let base = base_config_dir fs ~app_name in
  let profiles = Eio.Path.(base / "profiles") in
  mkdir_if_missing ~perm:0o700 profiles;
  profiles

(* Config directory for a specific profile *)
let config_dir fs ~app_name ?profile () =
  let profile_name = Option.value ~default:default_profile profile in
  let profiles = profiles_dir fs ~app_name in
  let profile_dir = Eio.Path.(profiles / profile_name) in
  mkdir_if_missing ~perm:0o700 profile_dir;
  profile_dir

(* App config file (stores current profile) *)
let app_config_file fs ~app_name =
  Eio.Path.(base_config_dir fs ~app_name / "config.json")

let load_app_config fs ~app_name =
  let path = app_config_file fs ~app_name in
  try
    Eio.Path.load path
    |> Jsont_bytesrw.decode_string app_config_jsont
    |> Result.to_option
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save_app_config fs ~app_name config =
  let path = app_config_file fs ~app_name in
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent app_config_jsont config
  with
  | Ok content -> Eio.Path.save ~create:(`Or_truncate 0o600) path content
  | Error e -> failwith ("Failed to encode app config: " ^ e)

(* Get the current profile name *)
let get_current_profile fs ~app_name =
  match load_app_config fs ~app_name with
  | Some config -> config.current_profile
  | None -> default_profile

(* Set the current profile *)
let set_current_profile fs ~app_name profile =
  save_app_config fs ~app_name { current_profile = profile }

(* List all available profiles *)
let list_profiles fs ~app_name =
  let profiles = profiles_dir fs ~app_name in
  try
    Eio.Path.read_dir profiles
    |> List.filter (fun name ->
           (* Check if it's a directory with a session.json *)
           let dir = Eio.Path.(profiles / name) in
           let session = Eio.Path.(dir / "session.json") in
           try
             ignore (Eio.Path.load session);
             true
           with _ -> false)
    |> List.sort String.compare
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> []

(* Session file within a profile directory *)
let session_file fs ~app_name ?profile () =
  Eio.Path.(config_dir fs ~app_name ?profile () / "session.json")

let load fs ~app_name ?profile () =
  let profile =
    match profile with
    | Some p -> Some p
    | None ->
        (* Use current profile if none specified *)
        let current = get_current_profile fs ~app_name in
        Some current
  in
  let path = session_file fs ~app_name ?profile () in
  try
    Eio.Path.load path |> Jsont_bytesrw.decode_string jsont |> Result.to_option
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save fs ~app_name ?profile session =
  let profile =
    match profile with
    | Some p -> Some p
    | None -> Some (get_current_profile fs ~app_name)
  in
  let path = session_file fs ~app_name ?profile () in
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont session with
  | Ok content -> Eio.Path.save ~create:(`Or_truncate 0o600) path content
  | Error e -> failwith ("Failed to encode session: " ^ e)

let clear fs ~app_name ?profile () =
  let profile =
    match profile with
    | Some p -> Some p
    | None -> Some (get_current_profile fs ~app_name)
  in
  let path = session_file fs ~app_name ?profile () in
  try Eio.Path.unlink path
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

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
  match Uri.of_string uri |> fun u -> (Uri.host u, Uri.path u) with
  | Some host, path ->
      let name = Filename.basename path in
      if name = "" || name = "/" then host else name ^ "@" ^ host
  | None, _ -> "default"

(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  access_jwt : string;
  refresh_jwt : string;
  did : string;
  handle : string;
  pds : string;
  created_at : string;
}

let jsont =
  Jsont.Object.map ~kind:"Session"
    (fun access_jwt refresh_jwt did handle pds created_at ->
      { access_jwt; refresh_jwt; did; handle; pds; created_at })
  |> Jsont.Object.mem "access_jwt" Jsont.string ~enc:(fun s -> s.access_jwt)
  |> Jsont.Object.mem "refresh_jwt" Jsont.string ~enc:(fun s -> s.refresh_jwt)
  |> Jsont.Object.mem "did" Jsont.string ~enc:(fun s -> s.did)
  |> Jsont.Object.mem "handle" Jsont.string ~enc:(fun s -> s.handle)
  |> Jsont.Object.mem "pds" Jsont.string ~enc:(fun s -> s.pds)
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

(* Base config directory for the app *)
let base_config_dir fs ~app_name =
  let home = Sys.getenv "HOME" in
  let config_path = Eio.Path.(fs / home / ".config" / app_name) in
  (try Eio.Path.mkdir ~perm:0o700 config_path
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  config_path

(* Profiles directory *)
let profiles_dir fs ~app_name =
  let base = base_config_dir fs ~app_name in
  let profiles = Eio.Path.(base / "profiles") in
  (try Eio.Path.mkdir ~perm:0o700 profiles
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  profiles

(* Config directory for a specific profile *)
let config_dir fs ~app_name ?profile () =
  let profile_name = Option.value ~default:default_profile profile in
  let profiles = profiles_dir fs ~app_name in
  let profile_dir = Eio.Path.(profiles / profile_name) in
  (try Eio.Path.mkdir ~perm:0o700 profile_dir
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
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

let is_expired ?(leeway = Ptime.Span.of_int_s 60) session =
  Xrpc.Jwt.is_expired ~leeway session.access_jwt

let pp ppf session =
  Fmt.pf ppf "@[<v>Handle: %s@,DID: %s@,PDS: %s@,Created: %s@]" session.handle
    session.did session.pds session.created_at

let of_xrpc ~pds (xrpc : Xrpc.Types.session) =
  {
    access_jwt = xrpc.access_jwt;
    refresh_jwt = xrpc.refresh_jwt;
    did = xrpc.did;
    handle = xrpc.handle;
    pds;
    created_at = Ptime.to_rfc3339 (Ptime_clock.now ());
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

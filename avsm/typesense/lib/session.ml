(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Session data for Typesense authentication. *)
type t = {
  server_url : string;
  api_key : string;
  created_at : string;
}

let jsont =
  Jsont.Object.map ~kind:"Session"
    (fun server_url api_key created_at -> { server_url; api_key; created_at })
  |> Jsont.Object.mem "server_url" Jsont.string ~enc:(fun s -> s.server_url)
  |> Jsont.Object.mem "api_key" Jsont.string ~enc:(fun s -> s.api_key)
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
let app_name = "typesense"

(* Base config directory for the app *)
let base_config_dir fs =
  let home = Sys.getenv "HOME" in
  let config_path = Eio.Path.(fs / home / ".config" / app_name) in
  (try Eio.Path.mkdir ~perm:0o700 config_path
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  config_path

(* Profiles directory *)
let profiles_dir fs =
  let base = base_config_dir fs in
  let profiles = Eio.Path.(base / "profiles") in
  (try Eio.Path.mkdir ~perm:0o700 profiles
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  profiles

(* Config directory for a specific profile *)
let config_dir fs ?profile () =
  let profile_name = Option.value ~default:default_profile profile in
  let profiles = profiles_dir fs in
  let profile_dir = Eio.Path.(profiles / profile_name) in
  (try Eio.Path.mkdir ~perm:0o700 profile_dir
   with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ());
  profile_dir

(* App config file (stores current profile) *)
let app_config_file fs =
  Eio.Path.(base_config_dir fs / "config.json")

let load_app_config fs =
  let path = app_config_file fs in
  try
    Eio.Path.load path
    |> Jsont_bytesrw.decode_string app_config_jsont
    |> Result.to_option
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save_app_config fs config =
  let path = app_config_file fs in
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent app_config_jsont config
  with
  | Ok content -> Eio.Path.save ~create:(`Or_truncate 0o600) path content
  | Error e -> failwith ("Failed to encode app config: " ^ e)

(* Get the current profile name *)
let get_current_profile fs =
  match load_app_config fs with
  | Some config -> config.current_profile
  | None -> default_profile

(* Set the current profile *)
let set_current_profile fs profile =
  save_app_config fs { current_profile = profile }

(* List all available profiles *)
let list_profiles fs =
  let profiles = profiles_dir fs in
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
let session_file fs ?profile () =
  Eio.Path.(config_dir fs ?profile () / "session.json")

let load fs ?profile () =
  let profile =
    match profile with
    | Some p -> Some p
    | None ->
        (* Use current profile if none specified *)
        let current = get_current_profile fs in
        Some current
  in
  let path = session_file fs ?profile () in
  try
    Eio.Path.load path |> Jsont_bytesrw.decode_string jsont |> Result.to_option
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save fs ?profile session =
  let profile =
    match profile with
    | Some p -> Some p
    | None -> Some (get_current_profile fs)
  in
  let path = session_file fs ?profile () in
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent jsont session with
  | Ok content -> Eio.Path.save ~create:(`Or_truncate 0o600) path content
  | Error e -> failwith ("Failed to encode session: " ^ e)

let clear fs ?profile () =
  let profile =
    match profile with
    | Some p -> Some p
    | None -> Some (get_current_profile fs)
  in
  let path = session_file fs ?profile () in
  try Eio.Path.unlink path
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

(* Styled output helpers *)
let label_style = Fmt.(styled `Faint string)
let value_style = Fmt.(styled (`Fg `Cyan) string)

let pp ppf session =
  Fmt.pf ppf "@[<v>%a %a@,%a %a@,%a %a@]"
    label_style "Server:" value_style session.server_url
    label_style "API Key:" value_style (String.sub session.api_key 0 (min 8 (String.length session.api_key)) ^ "...")
    label_style "Created:" value_style session.created_at

let server_url t = t.server_url
let api_key t = t.api_key
let created_at t = t.created_at

let create ~server_url ~api_key () =
  { server_url; api_key; created_at = Ptime.to_rfc3339 (Ptime_clock.now ()) }

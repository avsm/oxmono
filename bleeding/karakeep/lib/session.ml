(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type credentials = {
  api_key : string;
  base_url : string;
}

let app_name = "karakeep"
let default_base_url = "https://hoard.recoil.org"
let default_profile = "default"

let credentials_tomlt =
  Tomlt.(Table.(
    obj (fun api_key base_url -> { api_key; base_url })
    |> mem "api_key" string ~enc:(fun c -> c.api_key)
    |> mem "base_url" string ~enc:(fun c -> c.base_url) ~dec_absent:default_base_url
    |> finish
  ))

type app_config = { current_profile : string }

let app_config_tomlt =
  Tomlt.(Table.(
    obj (fun current_profile -> { current_profile })
    |> mem "current_profile" string ~enc:(fun c -> c.current_profile) ~dec_absent:default_profile
    |> finish
  ))

let mkdir_if_not_exists path =
  try Eio.Path.mkdir ~perm:0o700 path
  with Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ()

let base_config_dir fs =
  let home = Sys.getenv "HOME" in
  let config_path = Eio.Path.(fs / home / ".config" / app_name) in
  mkdir_if_not_exists Eio.Path.(fs / home / ".config");
  mkdir_if_not_exists config_path;
  config_path

let profiles_dir fs =
  let base = base_config_dir fs in
  let profiles = Eio.Path.(base / "profiles") in
  mkdir_if_not_exists profiles;
  profiles

let profile_dir fs profile =
  let profiles = profiles_dir fs in
  let dir = Eio.Path.(profiles / profile) in
  mkdir_if_not_exists dir;
  dir

let app_config_file fs = Eio.Path.(base_config_dir fs / "config.toml")
let credentials_file fs profile = Eio.Path.(profile_dir fs profile / "credentials.toml")

let load_app_config fs =
  let path = app_config_file fs in
  try
    match Tomlt_eio.decode_file app_config_tomlt path with
    | Ok config -> Some config
    | Error _ -> None
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save_app_config fs config =
  let path = app_config_file fs in
  Tomlt_eio.encode_file app_config_tomlt config path

let get_current_profile fs =
  match load_app_config fs with
  | Some config -> config.current_profile
  | None -> default_profile

let set_current_profile fs profile =
  save_app_config fs { current_profile = profile }

let list_profiles fs =
  let profiles = profiles_dir fs in
  try
    Eio.Path.read_dir profiles
    |> List.filter (fun name ->
        let dir = Eio.Path.(profiles / name) in
        let creds = Eio.Path.(dir / "credentials.toml") in
        try
          ignore (Eio.Path.load creds);
          true
        with _ -> false)
    |> List.sort String.compare
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> []

let load fs ?profile () =
  let profile = match profile with
    | Some p -> p
    | None -> get_current_profile fs
  in
  let path = credentials_file fs profile in
  try
    match Tomlt_eio.decode_file credentials_tomlt path with
    | Ok creds -> Some creds
    | Error _ -> None
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

let save fs ?profile creds =
  let profile = match profile with
    | Some p -> p
    | None -> get_current_profile fs
  in
  let path = credentials_file fs profile in
  Tomlt_eio.encode_file credentials_tomlt creds path

let clear fs ?profile () =
  let profile = match profile with
    | Some p -> p
    | None -> get_current_profile fs
  in
  let path = credentials_file fs profile in
  try Eio.Path.unlink path
  with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let label_style = Fmt.(styled `Faint string)
let value_style = Fmt.(styled (`Fg `Cyan) string)

let pp ppf creds =
  Fmt.pf ppf "@[<v>%a %a@,%a %a@]"
    label_style "Server:" value_style creds.base_url
    label_style "API Key:" value_style (String.sub creds.api_key 0 (min 4 (String.length creds.api_key)) ^ "...")

let base_url t = t.base_url
let api_key t = t.api_key

let create ~base_url ~api_key () =
  { base_url; api_key }
